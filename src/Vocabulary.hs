module Vocabulary
  ( vocabularyFactory,
    Vocabulary,
    Factory,
    create,
  )
where

import Data.List (nub)
import Data.Set (Set, fromList)
import Document
import Factory (Factory (..))
import Prelude hiding (words)

type Vocabulary = Set String

vocabularyFactory :: Factory [Document] Vocabulary
vocabularyFactory = Factory fromDocuments
  where
    fromDocuments = fromList . nub . foldr appendWords []
      where
        appendWords = (++) . words
