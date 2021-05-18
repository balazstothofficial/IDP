module Model
  ( Model (Model),
    WordTopicMap,
    DocumentTopicMap,
    hyperParameter,
    numberOfTopics,
    numberOfWords,
    numberOfDocuments,
    numberOfUpdates,
    vocabulary,
    theta,
    phi,
    wordTopicMap,
    documentTopicMap,
    topicAssignments,
  )
where

import Data.Map (Map)
import Data.Matrix (Matrix, ncols, nrows)
import Document
import HyperParameter
import Vocabulary

data Model = Model
  { hyperParameter :: HyperParameter,
    numberOfTopics :: Int,
    numberOfWords :: Int,
    numberOfDocuments :: Int,
    numberOfUpdates :: Int,
    -- TODO: Find better names:
    theta :: Matrix Double, -- Size: NumberOfDocuments x NumberOfTopics
    phi :: Matrix Double, -- Size: NumberOfTopics x NumberOfWords
    vocabulary :: Vocabulary, -- Size: NumberOfWords
    wordTopicMap :: WordTopicMap, -- Number of words associated to specific topic
    documentTopicMap :: DocumentTopicMap, -- Number of words with topic in specific document
    topicAssignments :: [[Int]] -- Size: NumberOfDocuments x Document size
  }
  deriving (Eq)

type WordTopicMap = Map (String, Int) Int

type DocumentTopicMap = Map (Document, Int) Int

-- For debugging purposes:
instance Show Model where
  show model =
    "Model {"
      ++ "\n\tHyperParamter = "
      ++ show (hyperParameter model)
      ++ "\n\tNumber of Topics = "
      ++ show (numberOfTopics model)
      ++ "\n\tNumber of Words = "
      ++ show (numberOfWords model)
      ++ "\n\tNumber of Documents = "
      ++ show (numberOfDocuments model)
      ++ "\n\tNumber of Updates = "
      ++ show (numberOfUpdates model)
      ++ "\n\tTheta = "
      ++ show (theta model)
      ++ "\n\tPhi = "
      ++ show (phi model)
      ++ "\n\tVocabulary = "
      ++ show (vocabulary model)
      ++ "\n\twordTopicMap = "
      ++ show (wordTopicMap model)
      ++ "\n\tdocumentTopicMap = "
      ++ show (documentTopicMap model)
      ++ "\n\ttopicAssignments = "
      ++ show (topicAssignments model)
      ++ "\n}\n"

showDimensions :: Matrix a -> String
showDimensions matrix =
  "Matrix " ++ show rows ++ "x" ++ show cols
    ++ " (Rows x Cols)"
  where
    rows = nrows matrix
    cols = ncols matrix
