{-# LANGUAGE DisambiguateRecordFields #-}

module DocumentFactoryTest where

import Data.Map (fromList)
import Document
import DocumentFactory
import Test.HUnit
import Test.Hspec
import Prelude hiding (words)

createDocuments :: Spec
createDocuments = do
  describe "Create Documents" $ do
    it "Unit Test 1" $ do
      create (Input "Doc1" ["the", "tree", "runs", "quickly"])
        @?= Document
          { title = "Doc1",
            words = ["the", "tree", "runs", "quickly"],
            wordCounts = fromList [("the", 1), ("tree", 1), ("runs", 1), ("quickly", 1)]
          }
    it "Unit Test 2" $ do
      create (Input "Doc2" ["far", "away", "lions", "yell", "away", "lions"])
        @?= Document
          { title = "Doc2",
            words = ["far", "away", "lions", "yell", "away", "lions"],
            wordCounts = fromList [("far", 1), ("away", 2), ("lions", 2), ("yell", 1)]
          }
    it "Unit Test 3" $ do
      create (Input "Doc3" ["hallo", "d", "d", "d", "quall", "qualle", "halle", "hallo"])
        @?= Document
          { title = "Doc3",
            words = ["hallo", "d", "d", "d", "quall", "qualle", "halle", "hallo"],
            wordCounts =
              fromList [("hallo", 2), ("d", 3), ("quall", 1), ("qualle", 1), ("halle", 1)]
          }
