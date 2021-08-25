module TopicCountsTest where

import qualified Data.Map as Map
import Test.HUnit
import Test.Hspec
import TestData
import TopicCounts

createTopicCounts :: Spec
createTopicCounts = do
  describe "Create Topic Counts" $ do
    it "Unit Test 1" $ do
      -- TODO: Why is this needed? Can this be removed if TopicCounts are a new type?
      (create rawTestInitialTopics :: TopicCounts)
        @?= Map.fromList [(0, 4), (1, 8), (2, 4), (3, 4)]
