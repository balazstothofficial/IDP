module TestData
  ( testDocuments,
  )
where

import Document

testDocuments :: [Document]
testDocuments =
  [ createDocument ["the", "tree", "runs", "quickly"],
    createDocument ["far", "away", "lions", "yell", "yell"],
    createDocument ["she", "shoots", "yell", "shapes", "now"],
    createDocument ["he", "hits", "hops", "happily", "the", "the"]
  ]
