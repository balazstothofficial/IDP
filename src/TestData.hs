module TestData
  ( testDocuments,
    testTopics,
  )
where

import Document
import DocumentFactory

-- TODO: Move to test package

testDocuments :: [Document]
testDocuments =
  [ createDocument (Input "Doc1" ["the", "tree", "runs", "quickly"]),
    createDocument (Input "Doc2" ["far", "away", "lions", "yell", "yell"]),
    createDocument (Input "Doc3" ["she", "shoots", "yell", "shapes", "now"]),
    createDocument (Input "Doc4" ["he", "hits", "hops", "happily", "the", "the"])
  ]
  where
    createDocument = create documentFactory

testTopics :: [Int]
testTopics = [1, 1, 3, 0, 2, 1, 0, 1, 0, 1, 3, 1, 2, 0, 2, 3, 1, 1, 3, 2]
