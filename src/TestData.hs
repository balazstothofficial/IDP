module TestData
  ( testDocuments,
  )
where

import Document
import DocumentFactory

testDocuments :: [Document]
testDocuments =
  [ create (Input "Doc1" ["the", "tree", "runs", "quickly"]),
    create (Input "Doc2" ["far", "away", "lions", "yell", "yell"]),
    create (Input "Doc3" ["she", "shoots", "yell", "shapes", "now"]),
    create (Input "Doc4" ["he", "hits", "hops", "happily", "the", "the"])
  ]
