module TestData
  ( testDocuments,
  )
where

import Document

testDocuments :: [Document]
testDocuments =
  [ createDocument ["the", "tree", "runs", "quickly"],
    createDocument ["far", "away", "lions", "yell"],
    createDocument ["she", "shoots", "shapes", "now"],
    createDocument ["he", "hits", "hops", "happily"]
  ]
