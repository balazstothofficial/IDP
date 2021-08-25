{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DocumentTopicMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Document (Document)
import Factory
import TopicAssignments (TopicAssignments)

-- TODO: New Type
type DocumentTopicMap = Map (Document, Int) Int

instance Factory TopicAssignments ([Document] -> DocumentTopicMap) where
  create topicAssignments documents = foldr unionWithPlus Map.empty documentTopicMaps
    where
      unionWithPlus = Map.unionWith (+)
      documentTopicMaps = zipWith create topicAssignments documents

instance Factory [Int] (Document -> DocumentTopicMap) where
  create topics document = foldr increaseCount Map.empty topics
    where
      increaseCount topic = Map.alter increase (document, topic)

      -- TODO: Use Nat + generalize this:
      increase Nothing = Just 1
      increase (Just n) = Just (n + 1)
