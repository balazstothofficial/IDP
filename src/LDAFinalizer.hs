{-# LANGUAGE RecordWildCards #-}

module LDAFinalizer where

import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
import qualified Document
import qualified HyperParameter
import Model (Model (..))

computePhi :: Model -> Model
computePhi Model {..} = Model {..} {Model.phi = newPhi}
  where
    newPhi = Matrix.matrix numberOfTopics vocabularySize fill
    beta = HyperParameter.beta hyperParameter

    fill :: (Int, Int) -> Double
    fill (topic, wordIndex) = (wordCount + beta) / (topicCount + fromIntegral vocabularySize * beta)
      where
        topicCount = fromIntegral $ Map.findWithDefault 0 (topic - 1) topicCounts
        wordCount = fromIntegral $ Map.findWithDefault 0 (word, topic - 1) wordTopicMap
        word = Set.elemAt (wordIndex - 1) vocabulary

computeTheta :: Model -> Model
computeTheta Model {..} = Model {..} {Model.theta = newTheta}
  where
    newTheta = Matrix.matrix numberOfDocuments numberOfTopics fill
    alpha = HyperParameter.alpha hyperParameter

    fill :: (Int, Int) -> Double
    fill (documentIndex, topic) =
      (fromIntegral topicCount + alpha)
        / (fromIntegral documentSize + fromIntegral numberOfTopics * alpha)
      where
        topicCount = Map.findWithDefault 0 (document, topic - 1) documentTopicMap
        document = documents !! (documentIndex - 1)
        documentSize = length $ Document.words document
