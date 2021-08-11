module TestData
  ( testDocuments,
  )
where

import Document
import DocumentFactory

testDocuments :: [Document]
testDocuments =
  [ create ["the", "tree", "runs", "quickly"],
    create ["far", "away", "lions", "yell", "yell"],
    create ["she", "shoots", "yell", "shapes", "now"],
    create ["he", "hits", "hops", "happily", "the", "the"]
  ]
