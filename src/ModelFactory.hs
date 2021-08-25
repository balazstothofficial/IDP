{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module ModelFactory
  ( Input (..),
    create,
  )
where

import qualified Data.Matrix as Matrix
import Document (Document (..))
import Factory
import InitialTopics
import HyperParameter
import Model
import Prelude hiding (words)

data Input = Input
  { documents :: [Document],
    numberOfTopics :: Int,
    topics :: InitialTopics
  }

instance Factory Input Model where
  create Input {..} =
    Model
      { numberOfTopics = numberOfTopics,
        vocabularySize = length vocabulary,
        numberOfWords = numberOfWords,
        numberOfDocuments = numberOfDocuments,
        numberOfUpdates = 0,
        documents = documents,
        theta = Matrix.zero numberOfDocuments numberOfTopics,
        phi = Matrix.zero numberOfTopics vocabularySize,
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
      numberOfDocuments = length documents
      numberOfWords = totalNumberOfWords documents
      vocabularySize = length vocabulary
      
      neededTopics = rawTopics topics numberOfWords

      vocabulary = create documents
      topicAssignments = create documents neededTopics
      documentTopicMap = create topicAssignments documents

totalNumberOfWords :: [Document] -> Int
totalNumberOfWords = foldr sumNumberOfWords 0
  where
    sumNumberOfWords document acc = acc + numberOfWords document
    numberOfWords Document {..} = length words
