module Main where

import Data.Maybe
import System.Environment

import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import qualified Data.Text.Internal.Search as Text

processLine :: String -> (Int, Text.Text) -> Maybe Text.Text
processLine fileName (line, contents) = result
  where indices  = Text.indices (Text.pack "TODO: ") contents
        column   = head indices + 6

        location = ":" ++ show line ++ ":" ++ show column ++ ": "
        position = Text.pack $ fileName ++ location
        message  = Text.concat [position, Text.drop column contents]

        result   = if null indices then Nothing else Just message

findTodos :: String -> Text.Text -> [Text.Text]
findTodos fileName = map fromJust
  . filter isJust
  . init
  . map (processLine fileName)
  . zip [1..]
  . Text.lines

searchFile :: String -> IO ()
searchFile fileName = Text.readFile fileName >>= \lines ->
    Text.putStrLn
    $ Text.init
    $ Text.unlines
    $ findTodos fileName lines

main :: IO [()]
main = getArgs >>= mapM searchFile
