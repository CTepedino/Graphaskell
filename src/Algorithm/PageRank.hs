module Algorithm.PageRank
  ( pageRankSpec,
  )
where

import Algorithm.Common (extractRankingsResult)
import Algorithm.Messages (RankMsg (..))
import Algorithm.State (RankState (..))
import Algorithm.Types (AlgorithmSpec, RankLog, mkRankSpec)
import Graph.Types (NodeId, graphNodes, neighbors, nodeCount)
import Graph.VertexContext (VertexContext (..), allNodes, outNeighbors, outDegree)
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
      nodes = graphNodes graph
      n = fromIntegral (nodeCount graph)
      initRank = 1 / n
      rankMessages =
        concat
          [ if od > 0
              then
                [ (to, RankMsg (initRank / fromIntegral od))
                  | (to, _) <- neighbors graph nodeId
                ]
              else
                [ (to, RankMsg (initRank / n)) | to <- nodes
                ]
          | nodeId <- nodes,
            let od = length (neighbors graph nodeId)
          ]
   in rankMessages ++ activationMessages nodes

activationMessages :: [NodeId] -> [(NodeId, RankMsg)]
activationMessages nodes =
  [(to, RankMsg 0) | _ <- nodes, to <- nodes]

vertexUpdate ::
  VertexContext ->
  RankState ->
  [RankMsg] ->
  VertexStepResult RankState RankMsg
vertexUpdate vtx state messages =
  let n = fromIntegral (vcNodeCount vtx)
      oldRank = rsRank state
      incoming = sum [rmRank message | message <- messages]
      newRank = (1 - damping) / n + damping * incoming
      newState = RankState newRank
      outgoing = emitOutgoing vtx newState
   in if abs (newRank - oldRank) <= rankEpsilon
        then VertexStepResult state outgoing
        else VertexStepResult newState outgoing

emitOutgoing :: VertexContext -> RankState -> [(NodeId, RankMsg)]
emitOutgoing vtx state =
  let rank = rsRank state
      od = outDegree vtx
      n = fromIntegral (vcNodeCount vtx)
      rankMessages =
        if od == 0
          then
            [ (to, RankMsg (rank / n)) | to <- allNodes vtx
            ]
          else
            [ (to, RankMsg (rank / fromIntegral od))
              | to <- outNeighbors vtx
            ]
   in rankMessages ++ activationMessages (allNodes vtx)
