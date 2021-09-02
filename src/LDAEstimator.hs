{-# LANGUAGE RecordWildCards #-}

module LDAEstimator (Input (..), estimate) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Debug ((?))
import Document
import HyperParameter
import List (mapToStructure)
import Model
import Prelude hiding (words)

data Input = Input
  { model :: Model,
    iterations :: Int,
    factors :: [Double]
  }

-- TODO: Try to replace foldl with foldr and topics ++ [topic] with topic : topics
estimate :: Input -> Model
estimate Input {..} = case iterations ? "Iteration: " ++ show iterations of
  0 -> model
  _ ->
    estimate
      Input
        { model = iteratedModel,
          iterations = iterations - 1,
          factors = drop numberOfWords factors
        }
  where
    numberOfWords = Model.numberOfWords model
    iteratedModel = sampleDocuments model factors

sampleDocuments :: Model -> [Double] -> Model
sampleDocuments Model {..} factors = outputModel {topicAssignments = outputTopics}
  where
    distributedFactors = mapToStructure factors topicAssignments

    (outputModel, outputTopics) =
      foldl
        (flip reduction)
        (Model {..}, [])
        $ zip3 documents topicAssignments distributedFactors

    reduction triple (model, topics) =
      let (newModel, newTopics) = sampleDocument model triple
       in (newModel, topics ++ [newTopics])

sampleDocument :: Model -> (Document, [Int], [Double]) -> (Model, [Int])
sampleDocument Model {..} (document, topics, factors) = (outputModel, outputTopics)
  where
    words = Document.words document

    (outputModel, outputTopics) =
      foldl
        (flip reduction)
        (Model {..}, [])
        $ zip3 words topics factors

    reduction triple (model, topics) =
      let (newModel, topic) = sample triple model
       in (newModel, topics ++ [topic])

    sample (word, topic, factor) model = (modelWithNewTopic, newTopic)
      where
        modelWithNewTopic =
          modelWithRemovedTopic
            { Model.wordTopicMap =
                Map.alter increase (word, newTopic) $
                  Model.wordTopicMap modelWithRemovedTopic,
              Model.documentTopicMap =
                Map.alter increase (document, newTopic) $
                  Model.documentTopicMap modelWithRemovedTopic,
              Model.topicCounts =
                Map.alter increase newTopic $
                  Model.topicCounts modelWithRemovedTopic
            }

        newTopic = sampleWord modelWithRemovedTopic document word factor

        modelWithRemovedTopic =
          model
            { Model.wordTopicMap =
                Map.alter decrease (word, topic) $ Model.wordTopicMap model,
              Model.documentTopicMap =
                Map.alter decrease (document, topic) $ Model.documentTopicMap model,
              Model.topicCounts = Map.alter decrease topic $ Model.topicCounts model
            }

        decrease Nothing = Nothing
        decrease (Just 1) = Nothing
        decrease (Just n) = Just (n - 1)

        increase Nothing = Just 1
        increase (Just n) = Just (n + 1)

sampleWord :: Model -> Document -> String -> Double -> Int
sampleWord model document word factor = Maybe.fromJust $ List.findIndex predicate p
  where
    predicate value = value > u
    u = factor * last p
    p = multinomialSampling model word document

multinomialSampling :: Model -> String -> Document -> [Double]
multinomialSampling Model {..} word Document {..} =
  scanl1 (+) (fmap sampling [0 .. (numberOfTopics - 1)])
  where
    document = Document {..}

    kAlpha = alpha * fromIntegral numberOfTopics
    alpha = HyperParameter.alpha hyperParameter

    vBeta = beta * fromIntegral vocabularySize
    beta = HyperParameter.beta hyperParameter

    sampling topic =
      (numberOfWordsWithTopic + beta) / (occurrencesOfTopic + vBeta)
        * (occurrencesOfTopicInDocument + alpha) / (numberOfWordsInDocument + kAlpha)
      where
        numberOfWordsWithTopic = fromIntegral $ Map.findWithDefault 0 (word, topic) wordTopicMap
        occurrencesOfTopic = fromIntegral $ Map.findWithDefault 0 topic topicCounts
        occurrencesOfTopicInDocument =
          fromIntegral $ Map.findWithDefault 0 (document, topic) documentTopicMap
        numberOfWordsInDocument = fromIntegral (length words) - 1
