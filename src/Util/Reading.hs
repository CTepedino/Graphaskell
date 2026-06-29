module Util.Reading
  ( readNonNegativeInt,
    readPositiveInt,
    trim,
  )
where

import Data.Char (isSpace)
import Text.Read (readMaybe)

readNonNegativeInt :: String -> Either String Int
readNonNegativeInt raw =
  case readExactInt raw of
    Nothing ->
      Left ("must be an integer >= 0: " ++ raw)
    Just n | n >= 0 ->
      Right n
    Just _ ->
      Left ("must be an integer >= 0: " ++ raw)

readPositiveInt :: String -> Either String Int
readPositiveInt raw =
  case readExactInt raw of
    Nothing ->
      Left ("must be a positive integer: " ++ raw)
    Just n | n > 0 ->
      Right n
    Just _ ->
      Left ("must be a positive integer: " ++ raw)

readExactInt :: String -> Maybe Int
readExactInt raw =
  let trimmed = trim raw
   in if null trimmed
        then Nothing
        else readMaybe trimmed

trim :: String -> String
trim =
  reverse . dropWhile isSpace . reverse . dropWhile isSpace
