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
      create ["the", "tree", "runs", "quickly"]
        @?= Document
          { title = "",
            words = ["the", "tree", "runs", "quickly"],
            wordCounts = fromList [("the", 1), ("tree", 1), ("runs", 1), ("quickly", 1)]
          }
    it "Unit Test 2" $ do
      create (Input "LionDocument" ["far", "away", "lions", "yell", "away", "lions"])
        @?= Document
          { title = "LionDocument",
            words = ["far", "away", "lions", "yell", "away", "lions"],
            wordCounts = fromList [("far", 1), ("away", 2), ("lions", 2), ("yell", 1)]
          }
    it "Unit Test 3" $ do
      create ["hallo", "d", "d", "d", "quall", "qualle", "halle", "hallo"]
        @?= Document
          { title = "",
            words = ["hallo", "d", "d", "d", "quall", "qualle", "halle", "hallo"],
            wordCounts =
              fromList [("hallo", 2), ("d", 3), ("quall", 1), ("qualle", 1), ("halle", 1)]
          }
