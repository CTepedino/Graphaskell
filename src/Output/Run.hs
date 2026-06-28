module Output.Run
  ( SomePregelRun (..),
    somePregelResult,
  )
where

import Algorithm.Result (Result)
import Output.Log (DescribeLogEntry)
import Pregel.Types (PregelRun (..))

data SomePregelRun where
  SomePregelRun :: DescribeLogEntry log => PregelRun log -> SomePregelRun

somePregelResult :: SomePregelRun -> Result
somePregelResult (SomePregelRun run) =
  prResult run
