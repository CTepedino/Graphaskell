module Algorithm.Common
  ( lookupEdgeWeight,
    extractPathResult,
    extractVisitedResult,
    reconstructPath,
    isVisited,
    firstUnvisitedNeighbor,
  )
where

import Data.List (find, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graph.Types
import Pregel.Types

lookupEdgeWeight :: Graph -> NodeId -> NodeId -> Maybe Int
lookupEdgeWeight graph from to =
  case [weight | (neighbor, weight) <- neighbors graph from, neighbor == to] of
    (weight : _) -> weight
    [] -> Nothing

isVisited :: VertexStates -> NodeId -> Bool
isVisited states nodeId =
  maybe False vsVisited (Map.lookup nodeId states)

firstUnvisitedNeighbor :: Graph -> VertexStates -> NodeId -> Maybe NodeId
firstUnvisitedNeighbor graph states nodeId =
  find (not . isVisited states) (sortedNeighbors graph nodeId)

sortedNeighbors :: Graph -> NodeId -> [NodeId]
sortedNeighbors graph nodeId =
  sort (map fst (neighbors graph nodeId))

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

extractVisitedResult :: Map NodeId VertexState -> RunConfig -> Result
extractVisitedResult states cfg =
  case rcTarget cfg of
    Nothing ->
      InputError MissingTarget
    Just target ->
      case Map.lookup target states of
        Nothing ->
          InputError (TargetNodeMissing target)
        Just vertexState
          | not (vsVisited vertexState) ->
              NoPath
          | otherwise ->
              let path = reconstructPath states target (rcSource cfg)
                  dist = maybe 0 id (vsDistance vertexState)
               in if null path
                    then NoPath
                    else PathFound path dist

reconstructPath :: Map NodeId VertexState -> NodeId -> NodeId -> [NodeId]
reconstructPath states target source = go target []
  where
    go node acc
      | node == source = node : acc
      | otherwise =
          case Map.lookup node states >>= vsPredecessor of
            Just predecessor -> go predecessor (node : acc)
            Nothing -> []
