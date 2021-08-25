{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vocabulary
  ( Vocabulary,
    Factory,
    create,
  )
where

import Data.List (nub)
import Data.Set (Set, fromList)
import Document
import Prelude hiding (words)

type Vocabulary = Set String

-- TODO: New Type?
instance Factory [Document] Vocabulary where
  create = fromList . nub . foldr appendWords []
    where
      appendWords = (++) . words
