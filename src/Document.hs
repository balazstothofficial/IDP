module Document
  ( Document (..),
    WordCountMap
  )
where

import Data.Map (Map)
import Prelude hiding (lookup, words)

data Document = Document
  { title :: String,
    words :: [String],
    wordCounts :: WordCountMap
  }
  deriving (Show)
  
-- TODO: Not completely safe
instance Eq Document where
  (==) first second = title first == title second
  
instance Ord Document where
  (<=) first second = title first <= title second

type WordCountMap = Map String Int
