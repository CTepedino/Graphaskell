module Algorithm.PageRank
  ( pageRankSpec,
  )
where

import Algorithm.Common (UpdateM, extractRankingsResult, runVertexUpdate)
import Algorithm.Types (AlgorithmSpec (..))
import Control.Monad.State.Strict (get, put)
import Control.Monad.Writer.Strict (tell)
import Graph.Types
import Graph.VertexContext (VertexContext (..), outNeighbors, outDegree)
import Pregel.Types

damping :: Double
damping = 0.85

rankEpsilon :: Double
rankEpsilon = 1e-9

pageRankSpec :: AlgorithmSpec
pageRankSpec =
  AlgorithmSpec
    { specInitState = initState,
      specBootstrap = bootstrap,
      specVertexUpdate = vertexUpdate,
      specExtractResult = extractRankingsResult
    }

initState :: NodeId -> RunConfig -> VertexState
initState _nodeId cfg =
  let n = fromIntegral (nodeCount (rcGraph cfg))
   in initialVertexState {vsRank = Just (1 / n)}

bootstrap :: RunConfig -> [(NodeId, Message)]
bootstrap cfg =
  let graph = rcGraph cfg
      n = fromIntegral (nodeCount graph)
      initRank = 1 / n
   in [ (to, MsgRank (initRank / fromIntegral od))
        | nodeId <- graphNodes graph,
          let od = length (neighbors graph nodeId),
          od > 0,
          (to, _) <- neighbors graph nodeId
      ]

vertexUpdate ::
  VertexContext ->
  VertexState ->
  [Message] ->
  VertexStepResult
vertexUpdate vtx state messages =
  let n = fromIntegral (vcNodeCount vtx)
      nodeId = vcNodeId vtx
   in runVertexUpdate vtx state messages (pageRankUpdate n nodeId messages) emitOutgoing

pageRankUpdate :: Double -> NodeId -> [Message] -> UpdateM Bool
pageRankUpdate n nodeId messages = do
  state <- get
  let oldRank = maybe (1 / n) id (vsRank state)
      incoming = sum [contribution | MsgRank contribution <- messages]
      newRank = (1 - damping) / n + damping * incoming
  if abs (newRank - oldRank) <= rankEpsilon
    then pure False
    else do
      tell [VertexRankUpdated nodeId newRank]
      put state {vsRank = Just newRank}
      pure True

emitOutgoing :: VertexContext -> VertexState -> [(NodeId, Message)]
emitOutgoing vtx state =
  case vsRank state of
    Nothing -> []
    Just rank ->
      let od = outDegree vtx
       in if od == 0
            then []
            else
              [ (to, MsgRank (rank / fromIntegral od))
                | to <- outNeighbors vtx
              ]
