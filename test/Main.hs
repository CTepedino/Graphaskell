module Main where

import AlgorithmTests (algorithmTests)
import ParserTests (parserTests)
import PropertyTests (propertyTests)
import TraceTests (traceTests)
import Test.HUnit (Test (..), failures, errors, runTestTT)

main :: IO ()
main = do
  counts <-
    runTestTT
      (TestList [parserTests, algorithmTests, traceTests, propertyTests])
  if failures counts + errors counts == 0
    then pure ()
    else fail "Tests failed"
