module Graph.Parser
  ( parseGraphFile,
  )
where

import Control.Monad ((<=<), foldM)
import Data.Bifunctor (first)
import Graph.ParseError
import Util.Reading (readNonNegativeInt, readPositiveInt, trim)
import Graph.Types
  ( Edge (..),
    GraphEndpoint (..),
    GraphError (..),
    NodeId (..),
    ValidGraph,
    Weight (..),
    buildGraph,
    defaultEdgeWeight,
  )

parseGraphFile :: String -> Either ParseError ValidGraph
parseGraphFile =
  finalize <=< foldM step initialState . prepareLines

data ParseState = ParseState
  { psNodeCount :: Maybe Int,
    psWeighted :: Bool,
    psInEdges :: Bool,
    psEdges :: [Edge]
  }

initialState :: ParseState
initialState =
  ParseState
    { psNodeCount = Nothing,
      psWeighted = False,
      psInEdges = False,
      psEdges = []
    }

prepareLines :: String -> [String]
prepareLines =
  filter (not . null)
    . map trim
    . lines

step :: ParseState -> String -> Either ParseError ParseState
step st line =
  case words line of
    ["NODES", nStr] -> do
      n <- parsePositive CtxNodes nStr
      Right st {psNodeCount = Just n, psInEdges = False}
    ["EDGES"] ->
      Right st {psInEdges = True}
    ["WEIGHTED"] ->
      Right st {psWeighted = True, psInEdges = False}
    ws | psInEdges st ->
      parseEdgeLine st ws
    _ ->
      Left (UnknownLine line)

parseEdgeLine :: ParseState -> [String] -> Either ParseError ParseState
parseEdgeLine st ws
  | psWeighted st =
      case ws of
        [fromStr, toStr, weightStr] -> do
          (from, to) <- parseEndpoints fromStr toStr
          weight <- parseWeight CtxEdgeWeight weightStr
          let edge = Edge from to weight
          Right st {psEdges = edge : psEdges st}
        _ -> Left InvalidWeightedEdge
  | otherwise =
      case ws of
        [fromStr, toStr] -> do
          (from, to) <- parseEndpoints fromStr toStr
          let edge = Edge from to defaultEdgeWeight
          Right st {psEdges = edge : psEdges st}
        [_, _, _] ->
          Left (WeightOnUnweightedGraph (unwords ws))
        _ -> Left InvalidUnweightedEdge

parseEndpoints :: String -> String -> Either ParseError (NodeId, NodeId)
parseEndpoints fromStr toStr = do
  from <- parseNodeId CtxEdgeFrom fromStr
  to <- parseNodeId CtxEdgeTo toStr
  pure (from, to)

finalize :: ParseState -> Either ParseError ValidGraph
finalize st = do
  nodeTotal <-
    maybe (Left (MissingDirective DirNodes)) Right (psNodeCount st)
  if null (psEdges st)
    then Left NoEdges
    else do
      let edges = reverse (psEdges st)
      first graphErrorToParseError (buildGraph nodeTotal edges)

graphErrorToParseError :: GraphError -> ParseError
graphErrorToParseError err =
  case err of
    GraphBuildInvalidNodeCount n ->
      InvalidNodeCount n
    GraphBuildInvalidNodeId endpoint nodeId maxNode ->
      NodeOutOfRange (graphEndpointToParseContext endpoint) nodeId maxNode

graphEndpointToParseContext :: GraphEndpoint -> ParseContext
graphEndpointToParseContext endpoint =
  case endpoint of
    EdgeFrom -> CtxEdgeFrom
    EdgeTo -> CtxEdgeTo

parsePositive :: ParseContext -> String -> Either ParseError Int
parsePositive ctx raw =
  case readPositiveInt raw of
    Left _ -> Left (InvalidPositiveInteger ctx raw)
    Right n -> Right n

parseWeight :: ParseContext -> String -> Either ParseError Weight
parseWeight ctx raw =
  case readPositiveInt raw of
    Left _ -> Left (InvalidPositiveInteger ctx raw)
    Right n -> Right (Weight n)

parseNodeId :: ParseContext -> String -> Either ParseError NodeId
parseNodeId ctx raw =
  case readNonNegativeInt raw of
    Left _ -> Left (InvalidNodeId ctx raw)
    Right n -> Right (NodeId n)
