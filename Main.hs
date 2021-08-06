module Main where

import Data.Maybe
import System.Environment

import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Data.Text.Internal.Search as T

processLine :: FilePath -> (Int, T.Text) -> Maybe T.Text
processLine fileName (line, contents)
  | null indices = Nothing
  | otherwise    = Just $ T.pack position <> T.drop column contents
  where
    indices  = T.indices (T.pack "TODO: ") contents
    column   = head indices + 6
    position = concat [fileName, ":", show line, ":", show column, ": "]

findTodos :: FilePath -> T.Text -> [T.Text]
findTodos fileName = mapMaybe (processLine fileName)
  . zip [1..]
  . T.lines

searchFile :: FilePath -> IO ()
searchFile fileName = T.readFile fileName >>=
  T.putStrLn
  . T.unlines
  . findTodos fileName

main :: IO ()
main = getArgs >>= mapM_ searchFile
