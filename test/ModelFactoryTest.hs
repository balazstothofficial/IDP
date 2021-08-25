{-# LANGUAGE DisambiguateRecordFields #-}

module ModelFactoryTest where

import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
import Factory
import HyperParameter
import Model
import ModelFactory
import Test.HUnit
import Test.Hspec
import TestData

createModels :: Spec
createModels = do
  describe "Create Models" $ do
    it "Unit Test 1" $ do
      create
        Input
          { documents = testDocuments,
            numberOfTopics = 4,
            topics = testTopics
          }
        @?= Model
          { hyperParameter =
              HyperParameter
                { alpha = 50 / 4,
                  beta = 0.1
                },
            numberOfTopics = 4,
            numberOfWords = 16,
            numberOfDocuments = 4,
            numberOfUpdates = 0,
            documents = testDocuments,
            theta = Matrix.zero 4 4,
            phi = Matrix.zero 4 16,
            vocabulary =
              Set.fromList
                [ "the",
                  "tree",
                  "runs",
                  "quickly",
                  "far",
                  "away",
                  "lions",
                  "yell",
                  "she",
                  "shoots",
                  "shapes",
                  "now",
                  "he",
                  "hits",
                  "hops",
                  "happily"
                ],
            wordTopicMap =
              Map.fromList
                [ (("the", 1), 1),
                  (("the", 2), 1),
                  (("the", 3), 1),
                  (("tree", 1), 1),
                  (("runs", 3), 1),
                  (("quickly", 0), 1),
                  (("far", 2), 1),
                  (("away", 1), 1),
                  (("lions", 0), 1),
                  (("yell", 0), 1),
                  (("yell", 1), 2),
                  (("she", 1), 1),
                  (("shoots", 3), 1),
                  (("shapes", 2), 1),
                  (("now", 0), 1),
                  (("he", 2), 1),
                  (("hits", 3), 1),
                  (("hops", 1), 1),
                  (("happily", 1), 1)
                ],
            documentTopicMap =
              Map.fromList
                [ ((head testDocuments, 0), 1),
                  ((head testDocuments, 1), 2),
                  ((head testDocuments, 3), 1),
                  ((testDocuments !! 1, 0), 2),
                  ((testDocuments !! 1, 1), 2),
                  ((testDocuments !! 1, 2), 1),
                  ((testDocuments !! 2, 0), 1),
                  ((testDocuments !! 2, 1), 2),
                  ((testDocuments !! 2, 2), 1),
                  ((testDocuments !! 2, 3), 1),
                  ((testDocuments !! 3, 1), 2),
                  ((testDocuments !! 3, 2), 2),
                  ((testDocuments !! 3, 3), 2)
                ],
            topicAssignments =
              [ [1, 1, 3, 0],
                [2, 1, 0, 1, 0],
                [1, 3, 1, 2, 0],
                [2, 3, 1, 1, 3, 2]
              ],
            topicCounts = Map.fromList [(0, 4), (1, 8), (2, 4), (3, 4)]
          }
