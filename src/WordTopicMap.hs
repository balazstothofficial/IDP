{-# OPTIONS -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module WordTopicMap (WordTopicMap, Factory (..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Document (Document, words)
import Factory
import Prelude hiding (words)

-- TODO: Use new type
type WordTopicMap = Map (String, Int) Int

instance Factory [Document] ([Int] -> WordTopicMap) where
  create documents = create allWords
    where
      allWords = concatMap words documents

instance Factory [String] ([Int] -> WordTopicMap) where
  create words topics = foldr increaseCount Map.empty assignedTopics
    where
      assignedTopics = zip words topics

      increaseCount = Map.alter increase

      increase Nothing = Just 1
      increase (Just n) = Just (n + 1)
