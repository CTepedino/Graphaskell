module Algorithm.PageRank
  ( pageRankSpec,
    pageRankReference,
  )
where

import Algorithm.Common (extractRankingsResult)
import Algorithm.Messages (RankMsg (..))
import Algorithm.State (RankState (..))
import Algorithm.Types (AlgorithmSpec, RankLog, mkRankSpec)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Graph.Types (NodeId, ValidGraph, graphNodes, neighbors, nodeCount)
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
   in concat
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
   in if od == 0
        then
          [ (to, RankMsg (rank / n)) | to <- allNodes vtx
          ]
        else
          [ (to, RankMsg (rank / fromIntegral od))
            | to <- outNeighbors vtx
          ]

pageRankReference :: ValidGraph -> [(NodeId, Double)]
pageRankReference graph =
  let nodes = graphNodes graph
      n = fromIntegral (length nodes)
      outDeg nodeId = length (neighbors graph nodeId)
      outTargets nodeId = map fst (neighbors graph nodeId)
      initial = Map.fromList [(nodeId, 1 / n) | nodeId <- nodes]
      converged old new =
        all (\nodeId -> abs (old Map.! nodeId - new Map.! nodeId) <= rankEpsilon) nodes
      step ranks =
        let danglingMass =
              sum [ranks Map.! nodeId | nodeId <- nodes, outDeg nodeId == 0]
            incoming nodeId =
              sum
                [ ranks Map.! fromNode / fromIntegral (outDeg fromNode)
                  | fromNode <- nodes,
                    nodeId `elem` outTargets fromNode
                ]
                + danglingMass / n
            newRank nodeId = (1 - damping) / n + damping * incoming nodeId
         in Map.fromList [(nodeId, newRank nodeId) | nodeId <- nodes]
      go ranks
        | converged ranks (step ranks) =
            sort [(nodeId, ranks Map.! nodeId) | nodeId <- nodes]
        | otherwise =
            go (step ranks)
   in go initial
