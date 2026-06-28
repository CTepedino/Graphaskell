module Algorithm.PageRank
  ( pageRankSpec,
  )
where

import Algorithm.Common (extractRankingsResult, runVertexUpdate)
import Algorithm.Messages (RankMsg (..))
import Algorithm.State (RankState (..))
import Algorithm.Types (AlgorithmSpec, RankLog, mkRankSpec)
import Graph.Types
import Graph.VertexContext (VertexContext (..), outNeighbors, outDegree)
import Pregel.Types

damping :: Double
damping = 0.85

rankEpsilon :: Double
rankEpsilon = 1e-9

pageRankSpec :: AlgorithmSpec RankState RankMsg RankLog
pageRankSpec =
  mkRankSpec initState bootstrap vertexUpdate extractRankingsResult

initState :: NodeId -> RunConfig -> RankState
initState _nodeId cfg =
  let n = fromIntegral (nodeCount (rcGraph cfg))
   in RankState (1 / n)

bootstrap :: RunConfig -> [(NodeId, RankMsg)]
bootstrap cfg =
  let graph = rcGraph cfg
      n = fromIntegral (nodeCount graph)
      initRank = 1 / n
   in [ (to, RankMsg (initRank / fromIntegral od))
        | nodeId <- graphNodes graph,
          let od = length (neighbors graph nodeId),
          od > 0,
          (to, _) <- neighbors graph nodeId
      ]

vertexUpdate ::
  VertexContext ->
  RankState ->
  [RankMsg] ->
  VertexStepResult RankState RankMsg
vertexUpdate vtx state messages =
  let n = fromIntegral (vcNodeCount vtx)
      nodeId = vcNodeId vtx
   in runVertexUpdate
        vtx
        state
        messages
        (pageRankUpdate n nodeId)
        emitOutgoing

pageRankUpdate :: Double -> NodeId -> [RankMsg] -> RankState -> Maybe RankState
pageRankUpdate n _nodeId messages state =
  let oldRank = rsRank state
      incoming = sum [rmRank message | message <- messages]
      newRank = (1 - damping) / n + damping * incoming
   in if abs (newRank - oldRank) <= rankEpsilon
        then Nothing
        else Just (RankState newRank)

emitOutgoing :: VertexContext -> RankState -> [(NodeId, RankMsg)]
emitOutgoing vtx state =
  let rank = rsRank state
      od = outDegree vtx
   in if od == 0
        then []
        else
          [ (to, RankMsg (rank / fromIntegral od))
            | to <- outNeighbors vtx
          ]
