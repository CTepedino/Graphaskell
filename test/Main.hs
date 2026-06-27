module Main where

import AlgorithmTests (algorithmTests)
import CommonTests (commonTests)
import ParserTests (parserTests)
import PropertyTests (propertyTests)
import SuperstepTests (superstepTests)
import TraceTests (traceTests)
import Test.HUnit (Test (..), failures, errors, runTestTT)

main :: IO ()
main = do
  counts <-
    runTestTT
      (TestList [parserTests, algorithmTests, commonTests, superstepTests, traceTests, propertyTests])
  if failures counts + errors counts == 0
    then pure ()
    else fail "Tests failed"
