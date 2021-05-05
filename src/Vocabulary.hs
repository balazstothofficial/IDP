module Vocabulary
  ( fromDocuments,
    Vocabulary,
  )
where

import Data.Set hiding(foldr)
import Document
import Prelude hiding(words)

type Vocabulary = Set String

fromDocuments :: [Document] -> Vocabulary
fromDocuments = foldr unionWords empty
  where 
    unionWords = union . fromList . words
