module PageRankOracle (pageRankReference) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Graph.Types (NodeId, ValidGraph, graphNodes, neighbors)

referenceDamping :: Double
referenceDamping = 0.85

referenceRankEpsilon :: Double
referenceRankEpsilon = 1e-9

pageRankReference :: ValidGraph -> [(NodeId, Double)]
pageRankReference graph =
  let nodes = graphNodes graph
      n = fromIntegral (length nodes)
      outDeg nodeId = length (neighbors graph nodeId)
      initial = Map.fromList [(nodeId, 1 / n) | nodeId <- nodes]
      converged old new =
        all
          ( \nodeId ->
              abs (old Map.! nodeId - new Map.! nodeId) <= referenceRankEpsilon
          )
          nodes
      step ranks =
        let danglingMass =
              sum [ranks Map.! nodeId | nodeId <- nodes, outDeg nodeId == 0]
            incoming nodeId =
              sum
                [ ranks Map.! fromNode / fromIntegral (outDeg fromNode)
                  | fromNode <- nodes,
                    (target, _) <- neighbors graph fromNode,
                    target == nodeId
                ]
                + danglingMass / n
            newRank nodeId =
              (1 - referenceDamping) / n + referenceDamping * incoming nodeId
         in Map.fromList [(nodeId, newRank nodeId) | nodeId <- nodes]
      go ranks
        | converged ranks (step ranks) =
            sort [(nodeId, ranks Map.! nodeId) | nodeId <- nodes]
        | otherwise =
            go (step ranks)
   in go initial
