module Document(
 Document(Document),
 createDocument,
 words,
 wordCounts,
 WordCountMap
) where

import Data.Map
import Prelude hiding (lookup, words)

data Document = Document
  { words :: [String],
    wordCounts :: WordCountMap
  }
  deriving (Show, Eq)

type WordCountMap = Map String Int

createDocument :: [String] -> Document
createDocument words = Document words (countWords words empty)

countWords :: [String] -> WordCountMap -> WordCountMap
countWords [] wordCountMap = wordCountMap
countWords (word : words) wordCountMap = case lookup word wordCountMap of
  Just count -> recurse (count + 1)
  Nothing -> recurse 1
  where
    recurse count = countWords words $ insert word count wordCountMap
