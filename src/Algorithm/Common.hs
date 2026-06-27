module Algorithm.Common
  ( VertexUpdate (..),
    validateWeightedGraph,
    extractPathResult,
    extractComponentResult,
    extractRankingsResult,
    extractLabelResult,
    reconstructPath,
    runVertexUpdate,
    stepResult,
    bfsCandidates,
    tryImproveDistance,
    tryRelabel,
    labelsFromMessages,
    labelBootstrap,
    emitLabelMessages,
    minimumWithSelf,
    pathInitState,
    pathBootstrap,
    pathMaxSupersteps,
    labelMaxSupersteps,
    pageRankMaxSupersteps,
  )
where

import Algorithm.Error (AlgorithmError (..))
import Algorithm.State
  ( LabelState (..),
    PathState (..),
    RankState (..),
    emptyPathState,
  )
import Data.List (minimumBy, sort)
import Data.Maybe (isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Graph.Types
import Graph.VertexContext (VertexContext, outNeighbors, vcNodeId)
import Pregel.Types

data VertexUpdate state msg
  = Unchanged
  | Updated state [LogEntry msg]
  deriving (Eq, Show)

validateWeightedGraph :: Graph -> Either AlgorithmError ()
validateWeightedGraph graph
  | null (graphEdges graph) =
      Left WeightedGraphRequired
  | all (isJust . edgeWeight) (graphEdges graph) =
      Right ()
  | otherwise =
      Left WeightedGraphRequired

extractPathResult :: Map NodeId PathState -> RunConfig -> Either AlgorithmError Result
extractPathResult states cfg =
  case rcTarget cfg of
    Nothing ->
      Left MissingPathTarget
    Just target ->
      case Map.lookup target states of
        Nothing ->
          Left (TargetNodeMissing target)
        Just vertexState ->
          case psDistance vertexState of
            Nothing -> Right NoPath
            Just dist ->
              let path = reconstructPath states target (rcSource cfg)
               in if null path
                    then Right NoPath
                    else Right (PathFound path dist)

extractComponentResult :: Map NodeId LabelState -> RunConfig -> Either AlgorithmError Result
extractComponentResult states _cfg =
  Right
    ( Components
        ( sort
            [ (label, sort members)
              | (label, members) <- Map.toList (groupByLabel states)
            ]
        )
    )

groupByLabel :: Map NodeId LabelState -> Map NodeId [NodeId]
groupByLabel states =
  Map.fromListWith
    (++)
    [ (lsLabel vertexState, [nodeId])
      | (nodeId, vertexState) <- Map.toList states
    ]

extractRankingsResult :: Map NodeId RankState -> RunConfig -> Either AlgorithmError Result
extractRankingsResult states _cfg =
  Right
    ( Rankings
        ( sort
            [ (nodeId, rsRank vertexState)
              | (nodeId, vertexState) <- Map.toList states
            ]
        )
    )

extractLabelResult :: Map NodeId LabelState -> RunConfig -> Either AlgorithmError Result
extractLabelResult states _cfg =
  Right
    ( NodeLabels
        ( sort
            [ (nodeId, lsLabel vertexState)
              | (nodeId, vertexState) <- Map.toList states
            ]
        )
    )

reconstructPath :: Map NodeId PathState -> NodeId -> NodeId -> [NodeId]
reconstructPath states target source = go target []
  where
    go node acc
      | node == source = node : acc
      | otherwise =
          case Map.lookup node states >>= psPredecessor of
            Just predecessor -> go predecessor (node : acc)
            Nothing -> []

runVertexUpdate ::
  VertexContext ->
  state ->
  [msg] ->
  ([msg] -> state -> VertexUpdate state msg) ->
  (VertexContext -> state -> [(NodeId, msg)]) ->
  VertexStepResult state msg
runVertexUpdate vtx state messages update emit =
  let nodeId = vcNodeId vtx
   in case update messages state of
        Unchanged ->
          stepResult nodeId state [] []
        Updated newState logs ->
          stepResult nodeId newState (emit vtx newState) logs

stepResult ::
  NodeId ->
  state ->
  [(NodeId, msg)] ->
  [LogEntry msg] ->
  VertexStepResult state msg
stepResult nodeId newState outgoing logs =
  let sentLogs =
        [ MessageSent nodeId to msg
          | (to, msg) <- outgoing
        ]
   in VertexStepResult
        { vsrState = newState,
          vsrOutgoing = outgoing,
          vsrLogs = logs ++ sentLogs
        }

bfsCandidates :: [DistanceMsg] -> [(Int, NodeId)]
bfsCandidates messages =
  [ (dmDistance message + 1, dmFrom message)
    | message <- messages
  ]

tryImproveDistance ::
  NodeId ->
  [(Int, NodeId)] ->
  PathState ->
  VertexUpdate PathState msg
tryImproveDistance _ [] _ =
  Unchanged
tryImproveDistance nodeId candidates state =
  let (newDist, predecessor) =
        minimumBy (comparing fst <> comparing snd) candidates
   in case psDistance state of
        Just current | newDist >= current ->
          Unchanged
        _ ->
          Updated
            ( state
                { psDistance = Just newDist,
                  psPredecessor = Just predecessor
                }
            )
            [VertexUpdated nodeId newDist]

tryRelabel ::
  NodeId ->
  NodeId ->
  LabelState ->
  VertexUpdate LabelState msg
tryRelabel nodeId newLabel state =
  if newLabel == lsLabel state
    then Unchanged
    else
      Updated
        (LabelState newLabel)
        [VertexLabelUpdated nodeId newLabel]

labelsFromMessages :: [LabelMsg] -> [NodeId]
labelsFromMessages messages =
  [ lmLabel message
    | message <- messages
  ]

minimumWithSelf :: NodeId -> [NodeId] -> NodeId
minimumWithSelf self labels =
  minimum (self : labels)

labelBootstrap :: RunConfig -> [(NodeId, LabelMsg)]
labelBootstrap cfg =
  let graph = rcGraph cfg
   in [ (to, LabelMsg nodeId)
        | nodeId <- graphNodes graph,
          (to, _) <- neighbors graph nodeId
      ]

emitLabelMessages :: VertexContext -> LabelState -> [(NodeId, LabelMsg)]
emitLabelMessages vtx state =
  [ (to, LabelMsg (lsLabel state))
    | to <- outNeighbors vtx
  ]

pathInitState :: NodeId -> RunConfig -> PathState
pathInitState nodeId cfg
  | nodeId == rcSource cfg =
      emptyPathState {psDistance = Just 0}
  | otherwise =
      emptyPathState

pathBootstrap :: (Maybe Int -> Bool) -> RunConfig -> [(NodeId, DistanceMsg)]
pathBootstrap acceptWeight cfg =
  [ (to, DistanceMsg (rcSource cfg) 0)
    | (to, weight) <- neighbors (rcGraph cfg) (rcSource cfg),
      acceptWeight weight
  ]

pathMaxSupersteps :: Int -> Int
pathMaxSupersteps n = max 1 n

labelMaxSupersteps :: Int -> Int
labelMaxSupersteps n = max 1 n

pageRankMaxSupersteps :: Int -> Int
pageRankMaxSupersteps n =
  max 1 (n * n)
