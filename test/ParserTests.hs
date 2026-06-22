module ParserTests (parserTests) where

import Fixtures
  ( disconnectedGraphText,
    noTargetGraphText,
    simpleGraphText,
    weightedGraphText,
  )
import Graph.ParseError
  ( Directive (..),
    ParseContext (..),
    ParseError (..),
  )
import Graph.Parser (GraphFile (..), parseGraphFile)
import Graph.Types (Algorithm (..), nodeCount)
import Test.HUnit

parserTests :: Test
parserTests =
  TestList
    [ "parse grafo valido simple" ~: do
        case parseGraphFile simpleGraphText of
          Right graphFile -> do
            gfSource graphFile @?= 0
            gfTarget graphFile @?= Just 4
            gfAlgorithm graphFile @?= BFS
            nodeCount (gfGraph graphFile) @?= 5
          Left err -> assertFailure (show err),
      "parse grafo weighted" ~: do
        case parseGraphFile weightedGraphText of
          Right graphFile ->
            gfAlgorithm graphFile @?= Dijkstra
          Left err -> assertFailure (show err),
      "falta SOURCE" ~: do
        let text =
              unlines
                [ "NODES 2",
                  "EDGES",
                  "0 1"
                ]
        parseGraphFile text @?= Left (MissingDirective DirSource),
      "falta NODES" ~: do
        let text =
              unlines
                [ "EDGES",
                  "0 1",
                  "SOURCE 0"
                ]
        parseGraphFile text @?= Left (MissingDirective DirNodes),
      "algoritmo desconocido" ~: do
        let text = simpleGraphText ++ "\nALGORITHM FLOYD"
        parseGraphFile text @?= Left (UnknownAlgorithm "FLOYD"),
      "nodo fuera de rango en SOURCE" ~: do
        let text =
              unlines
                [ "NODES 2",
                  "EDGES",
                  "0 1",
                  "SOURCE 9"
                ]
        parseGraphFile text
          @?= Left (NodeOutOfRange CtxSource 9 1),
      "arista con peso sin WEIGHTED" ~: do
        let text =
              unlines
                [ "NODES 2",
                  "EDGES",
                  "0 1 5",
                  "SOURCE 0"
                ]
        parseGraphFile text
          @?= Left (WeightOnUnweightedGraph "0 1 5"),
      "grafo desconectado parsea ok" ~: do
        case parseGraphFile disconnectedGraphText of
          Right _ -> return ()
          Left err -> assertFailure (show err),
      "grafo sin TARGET parsea ok" ~: do
        case parseGraphFile noTargetGraphText of
          Right graphFile -> gfTarget graphFile @?= Nothing
          Left err -> assertFailure (show err)
    ]
