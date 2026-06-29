module SomePregelRun
  ( SomePregelRun (..),
    somePregelResult,
  )
where

import Algorithm.Log (DescribeLogEntry)
import Algorithm.Result (Result)
import Pregel.Types (PregelRun (..))

data SomePregelRun where
  SomePregelRun :: DescribeLogEntry log => PregelRun log -> SomePregelRun

somePregelResult :: SomePregelRun -> Result
somePregelResult (SomePregelRun run) =
  prResult run
