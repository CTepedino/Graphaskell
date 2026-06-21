module Main where

import Cli.Options (Options (..), parseOptions)
import Graph.Parser (describeGraphFile, loadGraphFile)
import System.Exit (die)

main :: IO ()
main = do
  opts <- parseOptions
  run opts

run :: Options -> IO ()
run opts = do
  putStrLn "Graphaskell"
  putStrLn ""
  putStrLn $ "  Grafo:        " ++ optGraphPath opts
  putStrLn $
    "  Threads:      "
      ++ show (optThreads opts)
      ++ " / "
      ++ show (optMaxCapabilities opts)
      ++ " capacidades"
  putStrLn ""

  result <- loadGraphFile (optGraphPath opts)
  case result of
    Left err ->
      die $ "Error al cargar el grafo: " ++ err
    Right graphFile -> do
      putStrLn "Grafo cargado:"
      putStrLn ""
      putStrLn (describeGraphFile graphFile)
