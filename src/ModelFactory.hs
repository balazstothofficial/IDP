{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module ModelFactory
  ( Input (..),
    create,
  )
where

import qualified Data.Matrix as Matrix
import Document (Document (..))
import Factory
import HyperParameter
import Model
import Prelude hiding (words)

-- TODO: Make sure that right amount of topics is provided
data Input = Input
  { documents :: [Document],
    numberOfTopics :: Int,
    topics :: [Int]
  }

instance Factory Input Model where
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
        wordTopicMap = create documents neededTopics,
        topicCounts = create neededTopics,
        documentTopicMap = documentTopicMap,
        topicAssignments = topicAssignments
      }
    where
      neededTopics = take (totalNumberOfWords documents) topics

      vocabulary = create documents
      topicAssignments = create documents neededTopics
      documentTopicMap = create topicAssignments documents

      numberOfDocuments = length documents
      numberOfWords = length vocabulary

totalNumberOfWords :: [Document] -> Int
totalNumberOfWords = foldr sumNumberOfWords 0
  where
    sumNumberOfWords document acc = acc + numberOfWords document
    numberOfWords Document {..} = length words
