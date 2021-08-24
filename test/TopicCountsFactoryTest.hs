module TopicCountsFactoryTest where

import qualified Data.Map as Map
import Test.HUnit
import Test.Hspec
import TestData
import TopicCountsFactory

createTopicCounts :: Spec
createTopicCounts = do
  describe "Create Topic Counts" $ do
    it "Unit Test 1" $ do
      create topicCountsFactory testTopics
        @?= Map.fromList [(0, 4), (1, 8), (2, 4), (3, 4)]
