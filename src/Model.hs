{-# LANGUAGE RecordWildCards #-}

module Model
  ( Model (..),
    WordTopicMap,
    DocumentTopicMap,
    TopicCounts,
    TopicAssignments,
  )
where

import Data.Matrix (Matrix, ncols, nrows)
import Document
import DocumentTopicMap
import HyperParameter
import TopicAssignments
import TopicCounts (TopicCounts)
import Vocabulary
import WordTopicMap (WordTopicMap)

-- TODO: Use some Nat type instead of Ints
data Model = Model
  { hyperParameter :: HyperParameter,
    numberOfTopics :: Int,
    vocabularySize :: Int,
    numberOfWords :: Int,
    numberOfDocuments :: Int,
    numberOfUpdates :: Int,
    documents :: [Document],
    -- TODO: Find better names:
    theta :: Matrix Double, -- Size: NumberOfDocuments x NumberOfTopics
    phi :: Matrix Double, -- Size: NumberOfTopics x NumberOfWords
    vocabulary :: Vocabulary, -- Size: NumberOfWords
    wordTopicMap :: WordTopicMap, -- Number of words associated to specific topic
    topicCounts :: TopicCounts,
    documentTopicMap :: DocumentTopicMap, -- Number of words with topic in specific document
    topicAssignments :: TopicAssignments -- Size: NumberOfDocuments x Document size
  }
  deriving (Eq)

-- For debugging purposes:
instance Show Model where
  show Model{..} =
    "Model {"
      ++ "\n\tHyperParamter = "
      ++ show hyperParameter
      ++ "\n\tNumber of Topics = "
      ++ show numberOfTopics
      ++ "\n\tNumber of Words in Vocabulary = "
      ++ show vocabularySize
      ++ "\n\tTotal Number of Words = "
      ++ show numberOfWords
      ++ "\n\tNumber of Documents = "
      ++ show numberOfDocuments
      ++ "\n\tNumber of Updates = "
      ++ show numberOfUpdates
      ++ "\n\tTheta = "
      ++ show theta
      ++ "\n\tPhi = "
      ++ show phi
      ++ "\n\tVocabulary = "
      ++ show vocabulary
      ++ "\n\twordTopicMap = "
      ++ show wordTopicMap
      ++ "\n\tdocumentTopicMap = "
      ++ show documentTopicMap
      ++ "\n\ttopicAssignments = "
      ++ show topicAssignments
      ++ "\n\ttopicCounts = "
      ++ show topicCounts
      ++ "\n}\n"

showDimensions :: Matrix a -> String
showDimensions matrix =
  "Matrix " ++ show rows ++ "x" ++ show cols
    ++ " (Rows x Cols)"
  where
    rows = nrows matrix
    cols = ncols matrix
