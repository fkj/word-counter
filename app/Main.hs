module Main where

import Control.Exception (IOException, displayException, try)
import Control.Monad.ST
import Data.Either ()
import Data.HashTable.Class qualified as C
import Data.HashTable.ST.Basic qualified as H
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      textOrError <- try @IOException $ T.IO.readFile filename
      case textOrError of
        Left err ->
          putStrLn (displayException err) >> exitFailure
        Right text ->
          T.IO.putStr $ T.unlines $ map formatCount $ countAndSort $ T.words $ T.toCaseFold text
    _ -> T.IO.putStrLn (T.pack "Expected a file name as the only argument.") >> exitFailure

formatCount :: (T.Text, Int) -> T.Text
formatCount (word, count) = T.pack (show count <> ": ") <> word

countAndSort :: [T.Text] -> [(T.Text, Int)]
countAndSort wordList = sortOn ordering $ countOccurrences wordList
  where
    -- We have to customize the comparator to sort in reverse order and descending on the counts
    ordering (word, count) = (Down count, word)

countOccurrences :: [T.Text] -> [(T.Text, Int)]
countOccurrences wordList = runST $ do
  table <- H.new
  mapM_ (\word -> C.mutate table word update) wordList
  C.toList table
  where
    update existing = case existing of
      Nothing -> (Just 1, ())
      Just oldCount -> (Just (oldCount + 1), ())
