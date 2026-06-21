module Graph.Parser
  ( GraphFile (..),
    loadGraphFile,
    parseGraphFile,
    describeGraphFile,
  )
where

import Control.Monad ((<=<), foldM, when)
import Data.Char (isSpace, toUpper)
import Data.Foldable (traverse_)
import Graph.Types

data GraphFile = GraphFile
  { gfGraph :: Graph,
    gfSource :: NodeId,
    gfTarget :: Maybe NodeId,
    gfAlgorithm :: Algorithm
  }
  deriving (Eq, Show)

loadGraphFile :: FilePath -> IO (Either String GraphFile)
loadGraphFile path = do
  contents <- readFile path
  pure (parseGraphFile contents)

parseGraphFile :: String -> Either String GraphFile
parseGraphFile =
  finalize <=< foldM step initialState . prepareLines

describeGraphFile :: GraphFile -> String
describeGraphFile gf =
  unlines
    [ "  Nodos:      " ++ show (nodeCount (gfGraph gf)),
      "  Aristas:    " ++ show (length (graphEdges (gfGraph gf))),
      "  Origen:     " ++ show (gfSource gf),
      "  Destino:    " ++ maybe "—" show (gfTarget gf),
      "  Algoritmo:  " ++ show (gfAlgorithm gf),
      "",
      adjSummary (gfGraph gf)
    ]

-- | Internal parse state

data ParseState = ParseState
  { psNodeCount :: Maybe Int,
    psWeighted :: Bool,
    psInEdges :: Bool,
    psEdges :: [Edge],
    psSource :: Maybe NodeId,
    psTarget :: Maybe NodeId,
    psAlgorithm :: Maybe Algorithm
  }

initialState :: ParseState
initialState =
  ParseState
    { psNodeCount = Nothing,
      psWeighted = False,
      psInEdges = False,
      psEdges = [],
      psSource = Nothing,
      psTarget = Nothing,
      psAlgorithm = Nothing
    }

prepareLines :: String -> [String]
prepareLines =
  filter (not . null)
    . map trim
    . map stripComment
    . lines

stripComment :: String -> String
stripComment = takeWhile (/= '#')

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

step :: ParseState -> String -> Either String ParseState
step st line =
  case words line of
    ["NODES", nStr] -> do
      n <- parsePositive "NODES" nStr
      Right st {psNodeCount = Just n, psInEdges = False}
    ["EDGES"] ->
      Right st {psInEdges = True}
    ["WEIGHTED"] ->
      Right st {psWeighted = True, psInEdges = False}
    ["SOURCE", sStr] -> do
      source <- parseNodeId "SOURCE" sStr
      Right st {psSource = Just source, psInEdges = False}
    ["TARGET", tStr] -> do
      target <- parseNodeId "TARGET" tStr
      Right st {psTarget = Just target, psInEdges = False}
    ["ALGORITHM", algStr] -> do
      algorithm <- parseAlgorithm algStr
      Right st {psAlgorithm = Just algorithm, psInEdges = False}
    ws | psInEdges st ->
      parseEdgeLine st ws
    _ ->
      Left $
        "Linea desconocida o fuera de la seccion EDGES: "
          ++ line

parseEdgeLine :: ParseState -> [String] -> Either String ParseState
parseEdgeLine st ws
  | psWeighted st =
      case ws of
        [fromStr, toStr, weightStr] -> do
          from <- parseNodeId "arista (origen)" fromStr
          to <- parseNodeId "arista (destino)" toStr
          weight <- parsePositive "arista (peso)" weightStr
          let edge = Edge from to (Just weight)
          Right st {psEdges = psEdges st ++ [edge]}
        _ ->
          Left $
            "En modo WEIGHTED cada arista debe tener formato: \
            \<origen> <destino> <peso>"
  | otherwise =
      case ws of
        [fromStr, toStr] -> do
          from <- parseNodeId "arista (origen)" fromStr
          to <- parseNodeId "arista (destino)" toStr
          let edge = Edge from to Nothing
          Right st {psEdges = psEdges st ++ [edge]}
        [_, _, _weightStr] ->
          Left $
            "Arista con peso encontrada pero falta la directiva WEIGHTED: "
              ++ unwords ws
        _ ->
          Left $
            "Cada arista debe tener formato: <origen> <destino>"

finalize :: ParseState -> Either String GraphFile
finalize st = do
  nodeTotal <-
    maybe (Left "Falta la directiva NODES") Right (psNodeCount st)
  when (null (psEdges st)) $
    Left "Falta la seccion EDGES o no hay aristas definidas"
  source <-
    maybe (Left "Falta la directiva SOURCE") Right (psSource st)
  let graph = buildGraph nodeTotal (psEdges st)
  validateNode graph "SOURCE" source
  traverse_ (validateNode graph "TARGET") (psTarget st)
  mapM_ (validateEdge graph) (psEdges st)
  when (psWeighted st && any (== Nothing) (map edgeWeight (psEdges st))) $
    Left "Modo WEIGHTED requiere peso en todas las aristas"
  pure
    GraphFile
      { gfGraph = graph,
        gfSource = source,
        gfTarget = psTarget st,
        gfAlgorithm = maybe BFS id (psAlgorithm st)
      }

validateNode :: Graph -> String -> NodeId -> Either String ()
validateNode graph label nodeId
  | isValidNode graph nodeId = Right ()
  | otherwise =
      Left $
        label
          ++ " "
          ++ show nodeId
          ++ " fuera de rango [0, "
          ++ show (nodeCount graph - 1)
          ++ "]"

validateEdge :: Graph -> Edge -> Either String ()
validateEdge graph edge = do
  validateNode graph "arista (origen)" (edgeFrom edge)
  validateNode graph "arista (destino)" (edgeTo edge)

parsePositive :: String -> String -> Either String Int
parsePositive label raw =
  case reads raw of
    [(n, "")] | n > 0 -> Right n
    _ -> Left $ label ++ " debe ser un entero positivo: " ++ raw

parseNodeId :: String -> String -> Either String NodeId
parseNodeId label raw =
  case reads raw of
    [(n, "")] | n >= 0 -> Right n
    _ -> Left $ label ++ " debe ser un entero >= 0: " ++ raw

parseAlgorithm :: String -> Either String Algorithm
parseAlgorithm raw =
  case map toUpper raw of
    "BFS" -> Right BFS
    "DFS" -> Right DFS
    "DIJKSTRA" -> Right Dijkstra
    _ -> Left $ "Algoritmo desconocido: " ++ raw ++ " (use BFS, DFS o DIJKSTRA)"

adjSummary :: Graph -> String
adjSummary graph =
  unlines
    ( "  Adyacencia:"
        : map formatAdj (graphNodes graph)
    )
  where
    formatAdj nodeId =
      let nbs = neighbors graph nodeId
       in "    "
            ++ show nodeId
            ++ " -> "
            ++ if null nbs
              then "[]"
              else unwords (map formatNeighbor nbs)
    formatNeighbor (to, Nothing) = show to
    formatNeighbor (to, Just weight) = show to ++ "(" ++ show weight ++ ")"
