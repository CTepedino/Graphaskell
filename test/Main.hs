module Main where

import AlgorithmTests (algorithmTests)
import ParserTests (parserTests)
import TraceTests (traceTests)
import Test.HUnit (Test (..), failures, errors, runTestTT)

main :: IO ()
main = do
  counts <- runTestTT (TestList [parserTests, algorithmTests, traceTests])
  if failures counts + errors counts == 0
    then pure ()
    else fail "Tests fallidos"
