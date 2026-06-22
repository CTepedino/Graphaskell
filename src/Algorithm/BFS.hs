module Algorithm.BFS
  ( bfsSpec,
  )
where

import Algorithm.Common (UpdateM, extractPathResult, runVertexUpdate)
import Algorithm.Types (AlgorithmSpec (..))
import Control.Monad.State.Strict (get, put)
import Control.Monad.Writer.Strict (tell)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Graph.Types
import Graph.VertexContext (VertexContext (..), outNeighbors)
import Pregel.Types

bfsSpec :: AlgorithmSpec
bfsSpec =
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
    | (to, _) <- neighbors (rcGraph cfg) (rcSource cfg)
  ]

vertexUpdate ::
  VertexContext ->
  VertexState ->
  [Message] ->
  VertexStepResult
vertexUpdate vtx state messages =
  let nodeId = vcNodeId vtx
   in runVertexUpdate vtx state messages (bfsUpdate nodeId messages) emitOutgoing

bfsUpdate :: NodeId -> [Message] -> UpdateM Bool
bfsUpdate nodeId messages = do
  state <- get
  let candidates =
        [ (dist + 1, from)
          | MsgDistance from dist <- messages
        ]
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

emitOutgoing :: VertexContext -> VertexState -> [(NodeId, Message)]
emitOutgoing vtx state =
  case vsDistance state of
    Nothing -> []
    Just dist ->
      [ (to, MsgDistance (vcNodeId vtx) dist)
        | to <- outNeighbors vtx
      ]
