module Vocabulary
  ( fromDocuments,
    Vocabulary,
  )
where

import Data.List (nub)
import Data.Set (Set, fromList)
import Document
import Prelude hiding (words)

type Vocabulary = Set String

fromDocuments :: [Document] -> Vocabulary
fromDocuments = fromList . nub . foldr appendWords []
  where
    appendWords = (++) . words
