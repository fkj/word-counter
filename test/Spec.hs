{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Data.Text.Arbitrary
import Data.Text.IO qualified as T.IO
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import WordCounter (processText)

main :: IO ()
main = hspec $ do
  describe "the program" $ do
    it "works on the given example" $ do
      text <- T.IO.readFile "test-data/given-text.txt"
      processText text
        `shouldBe` [ ("do", 2),
                     ("that", 2),
                     ("go", 1),
                     ("so", 1),
                     ("thing", 1),
                     ("well", 1),
                     ("you", 1)
                   ]
    it "runs on a large file" $ do
      text <- T.IO.readFile "test-data/bible.txt"
      processText text `shouldStartWith` [("the", 61672)]
    it "works on an empty text" $ do
      processText T.empty `shouldBe` []
    it "works on a single word" $ do
      processText "ghost" `shouldBe` [("ghost", 1)]
    it "works on a text of just symbols" $ do
      processText ",.-. → £]$" `shouldBe` [(",.-.", 1), ("£]$", 1), ("→", 1)]
    modifyMaxSize (const 100000) $ it "sorts results correctly" $ do
      property $ \text -> isSortedCorrectly $ processText text
    modifyMaxSize (const 100000) $ it "does not output words that are not in the text" $ do
      property $ \text -> all (\(word, _) -> word `T.isInfixOf` T.toCaseFold text) (processText text)
    modifyMaxSize (const 100000) $ it "outputs all words in the text" $ do
      property $ \text ->
        let outputWords = map fst $ processText text
         in all (`elem` outputWords) $ T.words $ T.toCaseFold text

isSortedCorrectly :: [(Text, Int)] -> Bool
isSortedCorrectly [] = True
isSortedCorrectly [_] = True
isSortedCorrectly ((word1, count1) : (word2, count2) : counts)
  | count1 >= count2 = isSortedCorrectly ((word2, count2) : counts)
  | word1 <= word2 = isSortedCorrectly ((word2, count2) : counts)
  | otherwise = False
