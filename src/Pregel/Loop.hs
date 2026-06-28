module Pregel.Loop
  ( loopRunner,
    finalizeRun,
  )
where

import Algorithm.Types (AlgorithmSpec (..))
import Graph.Types (NodeId)
import Pregel.Error (PregelError (..))
import Pregel.Superstep (SuperstepResult (..), mkSuperstepLog)
import Pregel.Types

loopRunner ::
  Monad m =>
  RunConfig ->
  m [NodeId] ->
  (Int -> VertexStates state -> [NodeId] -> m (Either PregelError (SuperstepResult state msg log))) ->
  ([(NodeId, msg)] -> m (Either PregelError ())) ->
  Int ->
  VertexStates state ->
  m (Either PregelError (VertexStates state, [SuperstepLog log], Int, Bool))
loopRunner cfg getActives runSuperstep deliver step states = go step states
  where
    go step' states'
      | step' >= rcMaxSteps cfg =
          pure (Right (states', [], step', True))
      | otherwise = do
          actives <- getActives
          if null actives
            then pure (Right (states', [], step', False))
            else do
              superstepResult <- runSuperstep step' states' actives
              case superstepResult of
                Left err ->
                  pure (Left err)
                Right result -> do
                  let logEntry =
                        mkSuperstepLog
                          step'
                          actives
                          (ssOutgoing result)
                          (ssEntries result)
                  if null (ssOutgoing result)
                    then
                      pure
                        ( Right
                            ( ssNewStates result,
                              [logEntry],
                              step' + 1,
                              False
                            )
                        )
                    else do
                      deliverResult <- deliver (ssOutgoing result)
                      case deliverResult of
                        Left err ->
                          pure (Left err)
                        Right () -> do
                          loopResult <- go (step' + 1) (ssNewStates result)
                          pure
                            ( fmap
                                ( \(finalStates, restLogs, finalStep, maxStepsReached) ->
                                    (finalStates, logEntry : restLogs, finalStep, maxStepsReached)
                                )
                                loopResult
                            )

finalizeRun ::
  RunConfig ->
  AlgorithmSpec state msg log ->
  (VertexStates state, [SuperstepLog log], Int, Bool) ->
  Either PregelError (PregelRun log)
finalizeRun cfg spec (finalStates, logs, steps, maxStepsReached) =
  case specExtractResult spec finalStates cfg of
    Left algoErr ->
      Left (ResultExtraction algoErr)
    Right result ->
      Right
        PregelRun
          { prSupersteps = steps,
            prLogs = logs,
            prResult = result,
            prMaxStepsReached = maxStepsReached
          }
