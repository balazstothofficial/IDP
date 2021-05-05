module DocumentTest where

import Data.Map (fromList)
import Document
import Test.HUnit
import Test.Hspec
import Prelude hiding (words)

createDocumentTests :: Spec
createDocumentTests = do
  describe "Create Documents" $ do
    it "Unit Test 1" $ do
      createDocument ["the", "tree", "runs", "quickly"]
        @?= Document
          { words = ["the", "tree", "runs", "quickly"],
            wordCounts = fromList [("the", 1), ("tree", 1), ("runs", 1), ("quickly", 1)]
          }
    it "Unit Test 2" $ do
      createDocument ["far", "away", "lions", "yell", "away", "lions"]
        @?= Document
          { words = ["far", "away", "lions", "yell", "away", "lions"],
            wordCounts = fromList [("far", 1), ("away", 2), ("lions", 2), ("yell", 1)]
          }
    it "Unit Test 3" $ do
      createDocument ["hallo", "d", "d", "d", "quall", "qualle", "halle", "hallo"]
        @?= Document
          { words = ["hallo", "d", "d", "d", "quall", "qualle", "halle", "hallo"],
            wordCounts =
              fromList [("hallo", 2), ("d", 3), ("quall", 1), ("qualle", 1), ("halle", 1)]
          }
