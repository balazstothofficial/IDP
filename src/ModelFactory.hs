{-# LANGUAGE RecordWildCards #-}

module ModelFactory
  ( Input (..),
    modelFactory,
    create,
  )
where

import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import Document (Document (..))
import Factory
import HyperParameter
import Model
import TopicCountsFactory
import Vocabulary
import Prelude hiding (words)

-- TODO: Make sure that right amount of topics is provided
data Input = Input
  { documents :: [Document],
    numberOfTopics :: Int,
    topics :: [Int]
  }

modelFactory :: Factory Input Model
modelFactory = Factory {create = create}
  where
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
          topicCounts = topicCounts,
          documentTopicMap = documentTopicMap,
          topicAssignments = topicAssignments
        }
      where
        neededTopics = take (totalNumberOfWords documents) topics

        topicCounts = Factory.create topicCountsFactory neededTopics
        vocabulary = Factory.create vocabularyFactory documents
        numberOfDocuments = length documents
        numberOfWords = length vocabulary
        (topicAssignments, wordTopicMap, documentTopicMap) =
          initializeTopicMaps topics documents

totalNumberOfWords :: [Document] -> Int
totalNumberOfWords = foldl sumNumberOfWords 0
  where
    sumNumberOfWords acc document = acc + numberOfWords document
    numberOfWords Document {..} = length words

-- TODO: Make easier!!
initializeTopicMaps :: [Int] -> [Document] -> ([[Int]], WordTopicMap, DocumentTopicMap)
initializeTopicMaps _ [] = ([], Map.empty, Map.empty)
initializeTopicMaps topics (document : documents) =
  let (topicAssignments', wordTopicMap', documentTopicMap') =
        initializeTopicMaps nextTopics documents
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
