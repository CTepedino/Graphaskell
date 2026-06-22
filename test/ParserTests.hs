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
    [ "parse grafo valido simple" ~: do
        case parseGraphFile simpleGraphText of
          Right graph -> nodeCount graph @?= 5
          Left err -> assertFailure (show err),
      "parse grafo weighted" ~: do
        case parseGraphFile weightedGraphText of
          Right graph -> nodeCount graph @?= 4
          Left err -> assertFailure (show err),
      "falta NODES" ~: do
        let text =
              unlines
                [ "EDGES",
                  "0 1"
                ]
        parseGraphFile text @?= Left (MissingDirective DirNodes),
      "SOURCE en archivo rechazado" ~: do
        let text = simpleGraphText ++ "\nSOURCE 0"
        parseGraphFile text @?= Left (LegacyCliDirective "SOURCE"),
      "nodo fuera de rango en validateRunNodes" ~: do
        case parseGraphFile simpleGraphText of
          Right graph ->
            validateRunNodes graph 9 Nothing
              @?= Left (NodeOutOfRange CtxSource 9 4)
          Left err -> assertFailure (show err),
      "arista con peso sin WEIGHTED" ~: do
        let text =
              unlines
                [ "NODES 2",
                  "EDGES",
                  "0 1 5"
                ]
        parseGraphFile text
          @?= Left (WeightOnUnweightedGraph "0 1 5"),
      "grafo desconectado parsea ok" ~: do
        case parseGraphFile disconnectedGraphText of
          Right _ -> return ()
          Left err -> assertFailure (show err),
      "validateRunNodes acepta origen y destino validos" ~: do
        case parseGraphFile simpleGraphText of
          Right graph ->
            validateRunNodes graph 0 (Just 4) @?= Right ()
          Left err -> assertFailure (show err)
    ]
