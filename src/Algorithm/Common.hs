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
import Data.List (minimumBy, sort)
import Data.Maybe (isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Graph.Types
import Graph.VertexContext (VertexContext, outNeighbors, vcNodeId)
import Pregel.Types

data VertexUpdate
  = Unchanged
  | Updated VertexState [LogEntry]
  deriving (Eq, Show)

validateWeightedGraph :: Graph -> Either AlgorithmError ()
validateWeightedGraph graph
  | null (graphEdges graph) =
      Left WeightedGraphRequired
  | all (isJust . edgeWeight) (graphEdges graph) =
      Right ()
  | otherwise =
      Left WeightedGraphRequired

extractPathResult :: Map NodeId VertexState -> RunConfig -> Result
extractPathResult states cfg =
  case rcTarget cfg of
    Nothing ->
      InputError MissingTarget
    Just target ->
      case Map.lookup target states of
        Nothing ->
          InputError (TargetNodeMissing target)
        Just vertexState ->
          case vsDistance vertexState of
            Nothing -> NoPath
            Just dist ->
              let path = reconstructPath states target (rcSource cfg)
               in if null path
                    then NoPath
                    else PathFound path dist

extractComponentResult :: Map NodeId VertexState -> RunConfig -> Result
extractComponentResult states _cfg =
  Components
    ( sort
        [ (label, sort members)
          | (label, members) <- Map.toList (groupByLabel states)
        ]
    )

groupByLabel :: Map NodeId VertexState -> Map NodeId [NodeId]
groupByLabel states =
  Map.fromListWith
    (++)
    [ (label, [nodeId])
      | (nodeId, vertexState) <- Map.toList states,
        Just label <- [vsLabel vertexState]
    ]

extractRankingsResult :: Map NodeId VertexState -> RunConfig -> Result
extractRankingsResult states _cfg =
  Rankings
    ( sort
        [ (nodeId, rank)
          | (nodeId, vertexState) <- Map.toList states,
            Just rank <- [vsRank vertexState]
        ]
    )

extractLabelResult :: Map NodeId VertexState -> RunConfig -> Result
extractLabelResult states _cfg =
  NodeLabels
    ( sort
        [ (nodeId, label)
          | (nodeId, vertexState) <- Map.toList states,
            Just label <- [vsLabel vertexState]
        ]
    )

reconstructPath :: Map NodeId VertexState -> NodeId -> NodeId -> [NodeId]
reconstructPath states target source = go target []
  where
    go node acc
      | node == source = node : acc
      | otherwise =
          case Map.lookup node states >>= vsPredecessor of
            Just predecessor -> go predecessor (node : acc)
            Nothing -> []

runVertexUpdate ::
  VertexContext ->
  VertexState ->
  [Message] ->
  ([Message] -> VertexState -> VertexUpdate) ->
  (VertexContext -> VertexState -> [(NodeId, Message)]) ->
  VertexStepResult
runVertexUpdate vtx state messages update emit =
  let nodeId = vcNodeId vtx
   in case update messages state of
        Unchanged ->
          stepResult nodeId state [] []
        Updated newState logs ->
          stepResult nodeId newState (emit vtx newState) logs

stepResult ::
  NodeId ->
  VertexState ->
  [(NodeId, Message)] ->
  [LogEntry] ->
  VertexStepResult
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

-- | Extract hop-distance offers from incoming BFS messages.
bfsCandidates :: [Message] -> [(Int, NodeId)]
bfsCandidates messages =
  [ (dist + 1, from)
    | MsgDistance from dist <- messages
  ]

-- | Apply the best distance offer when it strictly improves the current value.
tryImproveDistance ::
  NodeId ->
  [(Int, NodeId)] ->
  VertexState ->
  VertexUpdate
tryImproveDistance _ [] _ =
  Unchanged
tryImproveDistance nodeId candidates state =
  let (newDist, predecessor) =
        minimumBy (comparing fst <> comparing snd) candidates
   in case vsDistance state of
        Just current | newDist >= current ->
          Unchanged
        _ ->
          Updated
            ( state
                { vsDistance = Just newDist,
                  vsPredecessor = Just predecessor
                }
            )
            [VertexUpdated nodeId newDist]

-- | Relabel a vertex when the proposed label differs from the current one.
tryRelabel ::
  NodeId ->
  NodeId ->
  VertexState ->
  VertexUpdate
tryRelabel nodeId newLabel state =
  let current = maybe nodeId id (vsLabel state)
   in if newLabel == current
        then Unchanged
        else
          Updated
            (state {vsLabel = Just newLabel})
            [VertexLabelUpdated nodeId newLabel]

labelsFromMessages :: [Message] -> [NodeId]
labelsFromMessages messages =
  [ label
    | MsgLabel label <- messages
  ]

minimumWithSelf :: NodeId -> [NodeId] -> NodeId
minimumWithSelf self labels =
  minimum (self : labels)

labelBootstrap :: RunConfig -> [(NodeId, Message)]
labelBootstrap cfg =
  let graph = rcGraph cfg
   in [ (to, MsgLabel nodeId)
        | nodeId <- graphNodes graph,
          (to, _) <- neighbors graph nodeId
      ]

emitLabelMessages :: VertexContext -> VertexState -> [(NodeId, Message)]
emitLabelMessages vtx state =
  [ (to, MsgLabel label)
    | Just label <- [vsLabel state],
      to <- outNeighbors vtx
  ]

pathInitState :: NodeId -> RunConfig -> VertexState
pathInitState nodeId cfg
  | nodeId == rcSource cfg =
      initialVertexState {vsDistance = Just 0}
  | otherwise =
      initialVertexState

pathBootstrap :: (Maybe Int -> Bool) -> RunConfig -> [(NodeId, Message)]
pathBootstrap acceptWeight cfg =
  [ (to, MsgDistance (rcSource cfg) 0)
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
