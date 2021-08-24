module TopicCountsFactory
  ( topicCountsFactory,
    Factory,
    create,
  )
where

import qualified Data.Map as Map
import Factory (Factory (..))
import Model (TopicCounts)

topicCountsFactory :: Factory [Int] TopicCounts
topicCountsFactory = Factory countTopics
  where
    countTopics = foldr increaseCount Map.empty

    increaseCount = Map.alter increase

    increase Nothing = Just 1
    increase (Just n) = Just (n + 1)
