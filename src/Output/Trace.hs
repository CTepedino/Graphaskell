module Output.Trace
  ( describeResult,
    describeRun,
  )
where

import Data.List (sortBy)
import Pregel.Engine (PregelRun (..))
import Pregel.Types

describeRun :: Bool -> PregelRun -> String
describeRun verbose run =
  unlines
    ( map (describeSuperstep verbose) (prLogs run)
        ++ [""]
        ++ ["Convergio en " ++ show (prSupersteps run) ++ " supersteps.", ""]
        ++ [describeResult (prResult run)]
    )

describeSuperstep :: Bool -> SuperstepLog -> String
describeSuperstep verbose stepLog =
  unlines
    ( header
        : if verbose
          then map ("    " ++) (map describeLogEntry (sortedEntries stepLog))
          else []
    )
  where
    header =
      "Superstep "
        ++ show (sslStep stepLog)
        ++ ": "
        ++ show (sslActiveVertices stepLog)
        ++ " vertices activos, "
        ++ show (sslMessagesSent stepLog)
        ++ " mensajes emitidos"

sortedEntries :: SuperstepLog -> [LogEntry]
sortedEntries stepLog =
  sortBy compareLogEntry (sslEntries stepLog)

compareLogEntry :: LogEntry -> LogEntry -> Ordering
compareLogEntry left right =
  case (left, right) of
    (VertexUpdated n1 _, VertexUpdated n2 _) ->
      compare n1 n2
    (MessageSent _ _ _, VertexUpdated _ _) ->
      GT
    (VertexUpdated _ _, MessageSent _ _ _) ->
      LT
    (MessageSent f1 t1 _, MessageSent f2 t2 _) ->
      compare f1 f2 <> compare t1 t2

describeLogEntry :: LogEntry -> String
describeLogEntry entry =
  case entry of
    VertexUpdated nodeId distance ->
      "vertice "
        ++ show nodeId
        ++ " actualizado: distancia "
        ++ show distance
    MessageSent from to message ->
      "vertice "
        ++ show from
        ++ " -> "
        ++ show to
        ++ ": "
        ++ describeMessage message

describeMessage :: Message -> String
describeMessage (MsgDistance from distance) =
  "MsgDistance(from="
    ++ show from
    ++ ", dist="
    ++ show distance
    ++ ")"
describeMessage (MsgVisit from) =
  "MsgVisit(from=" ++ show from ++ ")"

describeResult :: Result -> String
describeResult result =
  case result of
    PathFound path dist ->
      unlines
        [ "Resultado: camino encontrado",
          "  Distancia: " ++ show dist,
          "  Camino:    " ++ show path
        ]
    NoPath ->
      "Resultado: no hay camino entre origen y destino"
    InputError err ->
      "Resultado: entrada invalida — " ++ displayInputError err

displayInputError :: InputError -> String
displayInputError err =
  case err of
    MissingTarget ->
      "se requiere TARGET para calcular un camino"
    TargetNodeMissing nodeId ->
      "nodo destino " ++ show nodeId ++ " inexistente"
