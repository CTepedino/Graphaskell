module Algorithm.Common
  ( extractPathResult,
    extractComponentResult,
    extractRankingsResult,
    extractLabelResult,
    reconstructPath,
    runVertexUpdate,
    labelVertexUpdateAlwaysEmit,
    bfsCandidates,
    tryImproveDistance,
    tryRelabel,
    labelsFromMessages,
    labelBootstrap,
    labelInitState,
    emitLabelMessages,
    emitDistanceMessages,
    minimumWithSelf,
    pathInitState,
    pathBootstrap,
    atLeastOneSuperstep,
    pageRankMaxSupersteps,
  )
where

import Algorithm.Error (AlgorithmError (..))
import Algorithm.Messages (DistanceMsg (..), LabelMsg (..))
import Algorithm.Result (Result (..))
import Algorithm.State
  ( LabelState (..),
    PathState (..),
    RankState (..),
    emptyPathState,
  )
import Data.List (minimumBy, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Graph.Types
import Graph.VertexContext (VertexContext, outNeighbors, vcNodeId)
import Pregel.Types (RunConfig (..), VertexStepResult (..))

extractPathResult :: Map NodeId PathState -> RunConfig -> Either AlgorithmError Result
extractPathResult states cfg =
  case (rcTarget cfg, rcSource cfg) of
    (Nothing, _) ->
      Left MissingPathTarget
    (_, Nothing) ->
      Left MissingPathSource
    (Just target, Just source) ->
      case Map.lookup target states of
        Nothing ->
          Left (TargetNodeMissing target)
        Just vertexState ->
          case psDistance vertexState of
            Nothing -> Right NoPath
            Just dist ->
              let path = reconstructPath states target source
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
  ([msg] -> state -> Maybe state) ->
  (VertexContext -> state -> [(NodeId, msg)]) ->
  VertexStepResult state msg
runVertexUpdate vtx state messages update emit =
  case update messages state of
    Nothing ->
      VertexStepResult state []
    Just newState ->
      VertexStepResult newState (emit vtx newState)

bfsCandidates :: [DistanceMsg] -> [(Distance, NodeId)]
bfsCandidates messages =
  [ (succDistance (dmDistance message), dmFrom message)
    | message <- messages
  ]

tryImproveDistance ::
  NodeId ->
  [(Distance, NodeId)] ->
  PathState ->
  Maybe PathState
tryImproveDistance _ [] _ =
  Nothing
tryImproveDistance _nodeId candidates state =
  let (newDist, predecessor) =
        minimumBy (comparing fst <> comparing snd) candidates
   in case psDistance state of
        Just current | newDist >= current ->
          Nothing
        _ ->
          Just
            ( state
                { psDistance = Just newDist,
                  psPredecessor = Just predecessor
                }
            )

tryRelabel :: NodeId -> NodeId -> LabelState -> Maybe LabelState
tryRelabel _nodeId newLabel state =
  if newLabel == lsLabel state
    then Nothing
    else Just (LabelState newLabel)

labelsFromMessages :: [LabelMsg] -> [NodeId]
labelsFromMessages messages =
  [ lmLabel message
    | message <- messages
  ]

minimumWithSelf :: NodeId -> [NodeId] -> NodeId
minimumWithSelf self labels =
  foldl min self labels

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

emitDistanceMessages ::
  (VertexContext -> [NodeId]) ->
  VertexContext ->
  PathState ->
  [(NodeId, DistanceMsg)]
emitDistanceMessages outTargets vtx state =
  case psDistance state of
    Nothing -> []
    Just dist ->
      [ (to, DistanceMsg (vcNodeId vtx) dist)
        | to <- outTargets vtx
      ]

labelInitState :: NodeId -> RunConfig -> LabelState
labelInitState nodeId _cfg =
  LabelState nodeId

labelVertexUpdateAlwaysEmit ::
  (NodeId -> [LabelMsg] -> LabelState -> Maybe LabelState) ->
  VertexContext ->
  LabelState ->
  [LabelMsg] ->
  VertexStepResult LabelState LabelMsg
labelVertexUpdateAlwaysEmit update vtx state messages =
  let nodeId = vcNodeId vtx
   in case update nodeId messages state of
        Nothing ->
          VertexStepResult state (emitLabelMessages vtx state)
        Just newState ->
          VertexStepResult newState (emitLabelMessages vtx newState)

pathInitState :: NodeId -> RunConfig -> PathState
pathInitState nodeId cfg =
  case rcSource cfg of
    Just source
      | nodeId == source ->
          emptyPathState {psDistance = Just zeroDistance}
    _ ->
      emptyPathState

pathBootstrap :: RunConfig -> [(NodeId, DistanceMsg)]
pathBootstrap cfg =
  case rcSource cfg of
    Nothing ->
      []
    Just source ->
      [ (to, DistanceMsg source zeroDistance)
        | (to, _) <- neighbors (rcGraph cfg) source
      ]

atLeastOneSuperstep :: Int -> Int
atLeastOneSuperstep n = max 1 n

pageRankMaxSupersteps :: Int -> Int
pageRankMaxSupersteps n =
  max 50 (n * n)
