module Graph.Load
  ( LoadGraphError (..),
    loadGraphFile,
    displayLoadGraphError,
  )
where

import Control.Exception (IOException, try)
import Graph.ParseError (ParseError, displayParseError)
import Graph.Parser (parseGraphFile)
import Graph.Types (ValidGraph)

data LoadGraphError
  = LoadReadError FilePath String
  | LoadParseError ParseError
  deriving (Eq, Show)

displayLoadGraphError :: LoadGraphError -> String
displayLoadGraphError err =
  case err of
    LoadReadError path message ->
      "Could not read file "
        ++ path
        ++ ": "
        ++ message
    LoadParseError parseError ->
      displayParseError parseError

loadGraphFile :: FilePath -> IO (Either LoadGraphError ValidGraph)
loadGraphFile path = do
  result <- try (readFile path) :: IO (Either IOException String)
  pure $
    case result of
      Left exception ->
        Left (LoadReadError path (show exception))
      Right contents ->
        case parseGraphFile contents of
          Left parseError -> Left (LoadParseError parseError)
          Right graph -> Right graph
