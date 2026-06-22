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
        ++ superstepSummary run
        ++ [describeResult (prResult run)]
    )

superstepSummary :: PregelRun -> [String]
superstepSummary run =
  [ "Convergio en " ++ show (prSupersteps run) ++ " supersteps.",
    ""
  ]
    ++ if prMaxStepsReached run
      then
        [ "Advertencia: se alcanzo el limite maximo de supersteps.",
          ""
        ]
      else []

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
    (VertexLabelUpdated n1 _, VertexLabelUpdated n2 _) ->
      compare n1 n2
    (VertexRankUpdated n1 _, VertexRankUpdated n2 _) ->
      compare n1 n2
    (MessageSent _ _ _, VertexUpdated _ _) ->
      GT
    (MessageSent _ _ _, VertexLabelUpdated _ _) ->
      GT
    (MessageSent _ _ _, VertexRankUpdated _ _) ->
      GT
    (VertexUpdated _ _, MessageSent _ _ _) ->
      LT
    (VertexLabelUpdated _ _, MessageSent _ _ _) ->
      LT
    (VertexRankUpdated _ _, MessageSent _ _ _) ->
      LT
    (MessageSent f1 t1 _, MessageSent f2 t2 _) ->
      compare f1 f2 <> compare t1 t2
    (VertexUpdated _ _, VertexLabelUpdated _ _) ->
      LT
    (VertexLabelUpdated _ _, VertexUpdated _ _) ->
      GT
    (VertexUpdated _ _, VertexRankUpdated _ _) ->
      LT
    (VertexRankUpdated _ _, VertexUpdated _ _) ->
      GT
    (VertexLabelUpdated _ _, VertexRankUpdated _ _) ->
      LT
    (VertexRankUpdated _ _, VertexLabelUpdated _ _) ->
      GT

describeLogEntry :: LogEntry -> String
describeLogEntry entry =
  case entry of
    VertexUpdated nodeId distance ->
      "vertice "
        ++ show nodeId
        ++ " actualizado: distancia "
        ++ show distance
    VertexLabelUpdated nodeId label ->
      "vertice "
        ++ show nodeId
        ++ " actualizado: etiqueta "
        ++ show label
    VertexRankUpdated nodeId rank ->
      "vertice "
        ++ show nodeId
        ++ " actualizado: rank "
        ++ show rank
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
describeMessage (MsgLabel label) =
  "MsgLabel(label=" ++ show label ++ ")"
describeMessage (MsgRank rank) =
  "MsgRank(rank=" ++ show rank ++ ")"

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
    ComponentFound label members ->
      unlines
        [ "Resultado: componente conexa",
          "  Etiqueta:  " ++ show label,
          "  Nodos:     " ++ show members
        ]
    Rankings pairs ->
      unlines
        ( "Resultado: PageRank"
            : map
              ( \(nodeId, rank) ->
                  "  nodo "
                    ++ show nodeId
                    ++ ": "
                    ++ show rank
              )
              pairs
        )
    NodeLabels pairs ->
      unlines
        ( "Resultado: propagacion de etiquetas"
            : map
              ( \(nodeId, label) ->
                  "  nodo "
                    ++ show nodeId
                    ++ " -> etiqueta "
                    ++ show label
              )
              pairs
        )
    InputError err ->
      "Resultado: entrada invalida — " ++ displayInputError err

displayInputError :: InputError -> String
displayInputError err =
  case err of
    MissingTarget ->
      "se requiere --target para calcular un camino"
    TargetNodeMissing nodeId ->
      "nodo " ++ show nodeId ++ " inexistente"
