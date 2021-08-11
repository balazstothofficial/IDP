{-# LANGUAGE RecordWildCards #-}

module ModelFactory where

import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import Document (Document)
import qualified Document
import HyperParameter
import Model
import qualified Vocabulary

class ModelFactory a where
  create :: a -> Model.Model

data Input = Input
  { documents :: [Document],
    numberOfTopics :: Int,
    topics :: [Int]
  }

instance ModelFactory Input where
  create Input {..} =
    Model
      { numberOfTopics = numberOfTopics,
        numberOfWords = numberOfWords,
        numberOfDocuments = numberOfDocuments,
        numberOfUpdates = 0,
        documents = documents,
        theta = Matrix.zero numberOfDocuments numberOfTopics,
        phi = Matrix.zero numberOfTopics numberOfWords,
        vocabulary = vocabulary,
        hyperParameter =
          HyperParameter
            { alpha = 50 / fromIntegral numberOfTopics,
              beta = 0.1
            },
        wordTopicMap = wordTopicMap,
        documentTopicMap = documentTopicMap,
        topicAssignments = topicAssignments
      }
    where
      vocabulary = Vocabulary.fromDocuments documents
      numberOfDocuments = length documents
      numberOfWords = length vocabulary
      (topicAssignments, wordTopicMap, documentTopicMap) =
        initializeTopicMaps numberOfTopics topics documents

-- TODO: Make easier!!
initializeTopicMaps :: Int -> [Int] -> [Document] -> ([[Int]], WordTopicMap, DocumentTopicMap)
initializeTopicMaps _ _ [] = ([], Map.empty, Map.empty)
initializeTopicMaps numberOfTopics topics (document : documents) =
  let (topicAssignments', wordTopicMap', documentTopicMap') =
        initializeTopicMaps numberOfTopics nextTopics documents
   in ( fmap snd assignedTopics : topicAssignments',
        Map.unionWith (+) wordTopicMap wordTopicMap',
        Map.union documentTopicMap documentTopicMap'
      )
  where
    nextTopics = drop (length assignedTopics) topics

    assignedTopics = zip (Document.words document) topics

    wordTopicMap = Map.fromListWith (+) $ assignOne assignedTopics

    documentTopicMap = Map.fromListWith (+) $ assignOne assignedDocuments
      where
        assignedDocuments = map assignToDocument assignedTopics
        assignToDocument (_, topic) = (document, topic)

    assignOne xs = map assign xs
      where
        assign x = (x, 1)
