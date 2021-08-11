module Document
  ( Document (..),
    WordCountMap,
  )
where

import Data.Map (Map)
import Prelude hiding (words)

data Document = Document
  { title :: String,
    words :: [String],
    wordCounts :: WordCountMap
  }
  deriving (Show)

type WordCountMap = Map String Int

-- TODO: Use secure id
instance Eq Document where
  (==) first second = title first == title second

instance Ord Document where
  (<=) first second = title first <= title second
