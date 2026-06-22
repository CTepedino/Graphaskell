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
import Graph.Parser (parseGraphFile, validateRunNodes)
import Graph.Types (nodeCount)
import Test.HUnit

parserTests :: Test
parserTests =
  TestList
    [ "parses simple valid graph" ~: do
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
      "SOURCE in graph file is rejected" ~: do
        let text = simpleGraphText ++ "\nSOURCE 0"
        parseGraphFile text @?= Left (LegacyCliDirective "SOURCE"),
      "node out of range in validateRunNodes" ~: do
        case parseGraphFile simpleGraphText of
          Right graph ->
            validateRunNodes graph 9 Nothing
              @?= Left (NodeOutOfRange CtxSource 9 4)
          Left err -> assertFailure (show err),
      "weighted edge without WEIGHTED directive" ~: do
        let text =
              unlines
                [ "NODES 2",
                  "EDGES",
                  "0 1 5"
                ]
        parseGraphFile text
          @?= Left (WeightOnUnweightedGraph "0 1 5"),
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
            validateRunNodes graph 0 (Just 4) @?= Right ()
          Left err -> assertFailure (show err)
    ]
