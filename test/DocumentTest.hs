module DocumentTest where

import Data.Map (fromList)
import Document
import Test.HUnit
import TestUtils
import Prelude hiding (words)

createDocumentTests :: Test
createDocumentTests =
  testList
    [ createDocument ["the", "tree", "runs", "quickly"]
        @?= Document
          { words = ["the", "tree", "runs", "quickly"],
            wordCounts = fromList [("the", 1), ("tree", 1), ("runs", 1), ("quickly", 1)]
          },
      createDocument ["far", "away", "lions", "yell", "away", "lions"]
        @?= Document
          { words = ["far", "away", "lions", "yell", "away", "lions"],
            wordCounts = fromList [("far", 1), ("away", 2), ("lions", 2), ("yell", 1)]
          },
      createDocument ["hallo", "d", "d", "d", "quall", "qualle", "halle", "hallo"]
        @?= Document
          { words = ["hallo", "d", "d", "d", "quall", "qualle", "halle", "hallo"],
            wordCounts =
              fromList [("hallo", 3), ("d", 3), ("quall", 1), ("qualle", 1), ("halle", 1)]
          }
    ]
