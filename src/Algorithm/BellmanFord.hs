module Algorithm.BellmanFord
  ( bellmanFordSpec,
  )
where

import Algorithm.Common (UpdateM, extractPathResult, runVertexUpdate)
import Algorithm.Types (AlgorithmSpec (..))
import Control.Monad.State.Strict (get, put)
import Control.Monad.Writer.Strict (tell)
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Graph.Types
import Graph.VertexContext
  ( VertexContext (..),
    lookupIncomingWeight,
    weightedOutNeighbors,
  )
import Pregel.Types

bellmanFordSpec :: AlgorithmSpec
bellmanFordSpec =
  AlgorithmSpec
    { specInitState = initState,
      specBootstrap = bootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractPathResult
    }

initState :: NodeId -> RunConfig -> VertexState
initState nodeId cfg
  | nodeId == rcSource cfg =
      initialVertexState {vsDistance = Just 0}
  | otherwise =
      initialVertexState

bootstrap :: RunConfig -> [(NodeId, Message)]
bootstrap cfg =
  [ (to, MsgDistance (rcSource cfg) 0)
    | (to, Just _) <- neighbors (rcGraph cfg) (rcSource cfg)
  ]

vertexUpdate ::
  VertexContext ->
  VertexState ->
  [Message] ->
  VertexStepResult
vertexUpdate vtx state messages =
  runVertexUpdate vtx state messages (bellmanFordUpdate vtx messages) emitOutgoing

bellmanFordUpdate :: VertexContext -> [Message] -> UpdateM Bool
bellmanFordUpdate vtx messages = do
  state <- get
  let nodeId = vcNodeId vtx
      candidates =
        mapMaybe
          (candidate vtx)
          messages
  case candidates of
    [] -> pure False
    _ -> do
      let (newDist, predecessor) =
            minimumBy (comparing fst) candidates
      case vsDistance state of
        Just current | newDist >= current -> pure False
        _ -> do
          tell [VertexUpdated nodeId newDist]
          put
            state
              { vsDistance = Just newDist,
                vsPredecessor = Just predecessor
              }
          pure True

candidate :: VertexContext -> Message -> Maybe (Int, NodeId)
candidate _ (MsgLabel _) = Nothing
candidate _ (MsgRank _) = Nothing
candidate vtx (MsgDistance from dist) = do
  weight <- lookupIncomingWeight vtx from
  Just (dist + weight, from)

emitOutgoing :: VertexContext -> VertexState -> [(NodeId, Message)]
emitOutgoing vtx state =
  case vsDistance state of
    Nothing -> []
    Just dist ->
      [ (to, MsgDistance (vcNodeId vtx) dist)
        | to <- weightedOutNeighbors vtx
      ]
