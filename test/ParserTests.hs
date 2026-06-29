module ParserTests (parserTests) where

import Fixtures
  ( disconnectedGraphText,
    simpleGraphText,
    weightedGraphText,
  )
import Graph.ParseError
  ( Directive (..),
    ParseContext (..),
    ParseError (..),
  )
import Graph.Parser (loadGraphFile, parseGraphFile, validateRunNodes)
import Graph.Types (NodeId (..), nodeCount)
import System.Directory (doesFileExist)
import Test.HUnit
import TestSupport (examplesGraphPaths, readExampleGraph)

parserTests :: Test
parserTests =
  TestList
    ( [ "parses simple valid graph" ~: do
          case parseGraphFile simpleGraphText of
            Right graph -> nodeCount graph @?= 5
            Left err -> assertFailure (show err),
        "parses weighted graph" ~: do
          case parseGraphFile weightedGraphText of
            Right graph -> nodeCount graph @?= 4
            Left err -> assertFailure (show err),
        "missing NODES" ~: do
          let text =
                unlines
                  [ "EDGES",
                    "0 1"
                  ]
          parseGraphFile text @?= Left (MissingDirective DirNodes),
        "missing edges" ~: do
          parseGraphFile "NODES 2\n" @?= Left NoEdges,
        "source node out of range in validateRunNodes" ~: do
          case parseGraphFile simpleGraphText of
            Right graph ->
              validateRunNodes graph (NodeId 9) Nothing
                @?= Left (NodeOutOfRange CtxSource (NodeId 9) 4)
            Left err -> assertFailure (show err),
        "target node out of range in validateRunNodes" ~: do
          case parseGraphFile simpleGraphText of
            Right graph ->
              validateRunNodes graph (NodeId 0) (Just (NodeId 9))
                @?= Left (NodeOutOfRange CtxTarget (NodeId 9) 4)
            Left err -> assertFailure (show err),
        "edge endpoint out of range" ~: do
          let text =
                unlines
                  [ "NODES 2",
                    "EDGES",
                    "0 5"
                  ]
          case parseGraphFile text of
            Left (NodeOutOfRange CtxEdgeTo (NodeId 5) 1) -> return ()
            other -> assertFailure (show other),
        "weighted edge without WEIGHTED directive" ~: do
          let text =
                unlines
                  [ "NODES 2",
                    "EDGES",
                    "0 1 5"
                  ]
          parseGraphFile text
            @?= Left (WeightOnUnweightedGraph "0 1 5"),
        "WEIGHTED edge line requires three tokens" ~: do
          let text =
                unlines
                  [ "NODES 2",
                    "WEIGHTED",
                    "EDGES",
                    "0 1"
                  ]
          parseGraphFile text @?= Left InvalidWeightedEdge,
        "invalid unweighted edge line" ~: do
          let text =
                unlines
                  [ "NODES 2",
                    "EDGES",
                    "0"
                  ]
          parseGraphFile text @?= Left InvalidUnweightedEdge,
        "disconnected graph parses ok" ~: do
          case parseGraphFile disconnectedGraphText of
            Right _ -> return ()
            Left err -> assertFailure (show err),
        "comment lines are rejected" ~: do
          let text = "# comment\n" ++ simpleGraphText
          case parseGraphFile text of
            Left (UnknownLine _) -> return ()
            other -> assertFailure (show other),
        "validateRunNodes accepts valid source and target" ~: do
          case parseGraphFile simpleGraphText of
            Right graph ->
              validateRunNodes graph (NodeId 0) (Just (NodeId 4)) @?= Right ()
            Left err -> assertFailure (show err),
        "loadGraphFile reads existing example graph" ~: do
          exists <- doesFileExist "examples/grafo-simple.txt"
          if exists
            then do
              result <- loadGraphFile "examples/grafo-simple.txt"
              case result of
                Right graph -> nodeCount graph @?= 5
                Left err -> assertFailure (show err)
            else assertFailure "missing examples/grafo-simple.txt"
      ]
        ++ map exampleGraphParses examplesGraphPaths
    )

exampleGraphParses :: FilePath -> Test
exampleGraphParses path =
  "example graph parses: " ++ path ~: do
    graph <- readExampleGraph path
    assertBool "graph has nodes" (nodeCount graph > 0)
