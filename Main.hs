module Main where

import Data.Maybe
import System.Environment

import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import qualified Data.Text.Internal.Search as Text

processLine :: FilePath -> (Int, Text.Text) -> Maybe Text.Text
processLine fileName (line, contents) = result
  where indices  = Text.indices (Text.pack "TODO: ") contents
        column   = head indices + 6

        location = ":" ++ show line ++ ":" ++ show column ++ ": "
        position = Text.pack $ fileName ++ location

        message  = Text.concat [position, Text.drop column contents]
        result   = if null indices then Nothing else Just message

findTodos :: FilePath -> Text.Text -> [Text.Text]
findTodos fileName = map fromJust
  . filter isJust
  . map (processLine fileName)
  . zip [1..]
  . Text.lines

searchFile :: FilePath -> IO ()
searchFile fileName = Text.readFile fileName >>=
  Text.putStrLn
  . Text.unlines
  . findTodos fileName

main :: IO ()
main = getArgs >>= mapM_ searchFile
