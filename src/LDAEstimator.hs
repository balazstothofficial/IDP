{-# LANGUAGE RecordWildCards #-}

module LDAEstimator where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Debug ((?))
import Document
import HyperParameter
import Model
import Prelude hiding (words)

newtype Estimator a = Estimator {estimate :: a -> Model}

data Input = Input
  { model :: Model,
    iterations :: Int,
    factors :: [Double]
  }

-- TODO: Simplify!!
estimator :: Estimator Input
estimator = Estimator {estimate = estimate}
  where
    estimate Input {..} = case iterations of
      0 -> model
      _ ->
        -- TODO: computePhi $
        -- TODO: computeTheta $
        estimate
          Input
            { model = outputModel,
              iterations = iterations - 1,
              factors = drop totalDocumentsLength factors
            }
          ? show (Model.topicAssignments outputModel)
      where
        outputModel = tmpModel {topicAssignments = topicAssignments}

        (tmpModel, topicAssignments) = sampleDocuments model documents (Model.topicAssignments model) factors

        documents = Model.documents model

        totalDocumentsLength = foldr sumLength 0 documents
          where
            sumLength document acc = acc + length (Document.words document)

--sampleDocuments2 :: Model -> [Double] -> [Document] -> [[Int]]  -> [(Model, [Int])]
--sampleDocuments2 model factors = zipWith (sample2 model factors)

sampleDocuments :: Model -> [Document] -> [[Int]] -> [Double] -> (Model, [[Int]])
sampleDocuments model [] _ _ = (model, [])
sampleDocuments model (document : documents) (topicAssignment : topicAssignments) factors =
  (recursedModel, outputTopics : recursedTopicAssignments)
  where
    (recursedModel, recursedTopicAssignments) =
      sampleDocuments outputModel documents topicAssignments (drop (length topicAssignment) factors)

    (outputModel, outputTopics) = sample model (Document.words document) topicAssignment document factors

--sample2 :: Model -> [Double] -> Document -> [Int] -> (Model, [Int])
--sample2 model factors document topics = _
-- where
--    words = Document.words document

-- sample3 :: Model -> Document -> [String]  -> [Double] -> [Int] -> (Model, [Int])
-- sample3 model document = zipWith3 (sampling model document)

sample :: Model -> [String] -> [Int] -> Document -> [Double] -> (Model, [Int])
sample model [] _ _ _ = (model, [])
sample model (word : words) (topic : topics) document (factor : factors) =
  (recursedModel, newTopic : recursedTopics)
  where
    (recursedModel, recursedTopics) = sample outputModel words topics document factors

    outputModel =
      inputModel
        { Model.wordTopicMap =
            Map.alter increase (word, newTopic) $ Model.wordTopicMap inputModel,
          Model.documentTopicMap =
            Map.alter increase (document, newTopic) $ Model.documentTopicMap inputModel,
          Model.topicCounts = Map.alter increase newTopic $ Model.topicCounts inputModel
        }

    newTopic = sampling inputModel document word factor

    inputModel =
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

sampling :: Model -> Document -> String -> Double -> Int
sampling model document word randomFactor = Maybe.fromJust $ List.findIndex predicate p
  where
    predicate value = value > u
    u = randomFactor * last p
    p = multinomialSampling model word document

multinomialSampling :: Model -> String -> Document -> [Double]
multinomialSampling Model {..} word Document {..} = scanl1 (+) (fmap sampling [0 .. (numberOfTopics - 1)])
  where
    -- TODO: Does this work?
    document = Document {..}

    kAlpha = alpha * fromIntegral numberOfTopics
    alpha = HyperParameter.alpha hyperParameter

    vBeta = beta * fromIntegral numberOfWords
    beta = HyperParameter.beta hyperParameter

    sampling topic =
      samplingResult
        ? "\nSampling Result: " ++ show samplingResult
          ++ "\nTopic: "
          ++ show topic
          ++ "\nNoWordForTopic: "
          ++ show numberOfWordsWithTopic
          ++ "\nNoTopic: "
          ++ show occurrencesOfTopic
          ++ "\nNoTopicInDoc: "
          ++ show occurrencesOfTopicInDocument
          ++ "\nNoWordInDoc: "
          ++ show numberOfWordsInDocument
      where
        samplingResult =
          (numberOfWordsWithTopic + beta) / (occurrencesOfTopic + vBeta)
            * (occurrencesOfTopicInDocument + alpha) / (numberOfWordsInDocument + kAlpha)

        numberOfWordsWithTopic = fromIntegral (Map.findWithDefault 0 (word, topic) wordTopicMap)
        occurrencesOfTopic = fromIntegral (Map.findWithDefault 0 topic topicCounts)
        occurrencesOfTopicInDocument = fromIntegral (Map.findWithDefault 0 (document, topic) documentTopicMap)
        numberOfWordsInDocument = fromIntegral (length words) - 1

count :: (k -> Bool) -> Map k Int -> Int
count predicate = Map.foldrWithKey f 0
  where
    f k a acc = if predicate k then acc else a + acc

-- TODO: Extract from this file
computePhi :: Model -> Model
computePhi model = model {Model.phi = phi}
  where
    phi = Matrix.matrix numberOfTopics numberOfWords fill

    numberOfWords = Model.numberOfWords model
    numberOfTopics = Model.numberOfTopics model
    wordTopicMap = Model.wordTopicMap model
    vocabulary = Model.vocabulary model
    beta = HyperParameter.beta $ Model.hyperParameter model

    fill :: (Int, Int) -> Double
    fill (topic, wordIndex) =
      (fromIntegral wordCount + beta)
        / (fromIntegral (count isTopic wordTopicMap) + fromIntegral numberOfWords * beta)
      where
        wordCount = Map.findWithDefault 0 (word, topic) wordTopicMap
        word = Set.elemAt (wordIndex - 1) vocabulary
        isTopic (_, topic') = topic == topic'

computeTheta :: Model -> Model
computeTheta model = model {Model.theta = theta}
  where
    theta = Matrix.matrix numberOfDocuments numberOfTopics fill

    numberOfDocuments = Model.numberOfDocuments model
    numberOfTopics = Model.numberOfTopics model
    documents = Model.documents model
    documentTopicMap = Model.documentTopicMap model
    alpha = HyperParameter.alpha $ Model.hyperParameter model

    fill :: (Int, Int) -> Double
    fill (documentIndex, topic) =
      (fromIntegral topicCount + alpha)
        / (fromIntegral documentSize + fromIntegral numberOfTopics * alpha)
      where
        topicCount = Map.findWithDefault 0 (document, topic) documentTopicMap
        document = documents !! (documentIndex - 1)
        documentSize = length $ Document.words document
