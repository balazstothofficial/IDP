module TestData
  ( testDocuments,
    testTopics,
  )
where

import Document

-- TODO: Move to test package

testDocuments :: [Document]
testDocuments =
  [ create "Doc1" ["the", "tree", "runs", "quickly"],
    create "Doc2" ["far", "away", "lions", "yell", "yell"],
    create "Doc3" ["she", "shoots", "yell", "shapes", "now"],
    create "Doc4" ["he", "hits", "hops", "happily", "the", "the"]
  ]

testTopics :: [Int]
testTopics = [1, 1, 3, 0, 2, 1, 0, 1, 0, 1, 3, 1, 2, 0, 2, 3, 1, 1, 3, 2]
