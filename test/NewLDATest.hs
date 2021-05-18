module NewLDATest where

import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
import HyperParameter
import Model
import NewLDA
import Test.HUnit
import Test.Hspec
import TestData

initialModelTest :: Spec
initialModelTest = do
  describe "Initial Model" $ do
    it "Unit Test 1" $ do
      initialModel testDocuments 3
        @?= Model
          { hyperParameter =
              HyperParameter
                { alpha = 50 / 3,
                  beta = 0.1
                },
            numberOfTopics = 3,
            numberOfWords = 16,
            numberOfDocuments = 4,
            numberOfUpdates = 0,
            theta = Matrix.zero 4 3,
            phi = Matrix.zero 3 16,
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
                [ (("the", 1), 2),
                  (("the", 0), 1),
                  (("tree", 1), 1),
                  (("runs", 3), 1),
                  (("quickly", 0), 1),
                  (("far", 1), 1),
                  (("away", 3), 1),
                  (("lions", 0), 1),
                  (("yell", 1), 1),
                  (("yell", 2), 2),
                  (("she", 3), 1),
                  (("shoots", 0), 1),
                  (("shapes", 1), 1),
                  (("now", 0), 1),
                  (("he", 0), 1),
                  (("hits", 2), 1),
                  (("hops", 1), 1),
                  (("happily", 0), 1)
                ],
            documentTopicMap =
              Map.fromList
                [ ((head testDocuments, 0), 1),
                  ((head testDocuments, 1), 2),
                  ((head testDocuments, 3), 1),
                  ((testDocuments !! 1, 0), 1),
                  ((testDocuments !! 1, 1), 2),
                  ((testDocuments !! 1, 2), 1),
                  ((testDocuments !! 1, 3), 1),
                  ((testDocuments !! 2, 0), 2),
                  ((testDocuments !! 2, 1), 1),
                  ((testDocuments !! 2, 2), 1),
                  ((testDocuments !! 2, 3), 1),
                  ((testDocuments !! 3, 0), 3),
                  ((testDocuments !! 3, 1), 2),
                  ((testDocuments !! 3, 2), 1)
                ],
            topicAssignments =
              [ [1, 1, 3, 0],
                [1, 3, 0, 2, 1],
                [3, 0, 2, 1, 0],
                [0, 2, 1, 0, 1, 0]
              ]
          }
