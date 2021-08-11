{-# LANGUAGE RecordWildCards #-}

module LDAEstimator where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Document
import HyperParameter
import Model

class Estimator a where
  estimate :: a -> Model

data Input = Input
  { model :: Model,
    iterations :: Int,
    factors :: [Double]
  }

-- TODO: Simplify!!
instance Estimator Input where
  estimate Input {..} = case iterations of
    0 -> model
    _ ->
      computePhi $
        computeTheta $
          estimate
            Input
              { model = outputModel,
                iterations = iterations - 1,
                factors = drop totalDocumentsLength factors
              }
    where
      outputModel = tmpModel {topicAssignments = topicAssignments}

      (tmpModel, topicAssignments) = sampleDocuments model documents (Model.topicAssignments model) factors

      documents = Model.documents model

      totalDocumentsLength = foldr sumLength 0 documents
        where
          sumLength document acc = acc + length (Document.words document)

sampleDocuments :: Model -> [Document] -> [[Int]] -> [Double] -> (Model, [[Int]])
sampleDocuments model [] _ _ = (model, [])
sampleDocuments model (document : documents) (topicAssignment : topicAssignments) factors =
  (recursedModel, outputTopics : recursedTopicAssignments)
  where
    (recursedModel, recursedTopicAssignments) =
      sampleDocuments outputModel documents topicAssignments (drop (length topicAssignment) factors)

    (outputModel, outputTopics) = sample model (Document.words document) topicAssignment document factors

sample :: Model -> [String] -> [Int] -> Document -> [Double] -> (Model, [Int])
sample model [] _ _ _ = (model, [])
sample model (word : words) (topic : topics) document (factor : factors) =
  (recursedModel, newTopic : recursedTopics)
  where
    (recursedModel, recursedTopics) = sample outputModel words topics document factors

    outputModel =
      model
        { Model.wordTopicMap =
            Map.alter increase (word, newTopic) $ Model.wordTopicMap model,
          Model.documentTopicMap =
            Map.alter increase (document, newTopic) $ Model.documentTopicMap model
        }

    newTopic = sampling inputModel word document factor

    inputModel =
      model
        { Model.wordTopicMap =
            Map.alter decrease (word, topic) $ Model.wordTopicMap model,
          Model.documentTopicMap =
            Map.alter decrease (document, topic) $ Model.documentTopicMap model
        }

    decrease Nothing = Nothing
    decrease (Just 0) = Nothing
    decrease (Just n) = Just (n - 1)

    increase Nothing = Just 1
    increase (Just n) = Just (n + 1)

sampling :: Model -> String -> Document -> Double -> Int
sampling model word document randomFactor = Maybe.fromJust $ List.findIndex predicate p
  where
    predicate value = value > u
    u = randomFactor * last p
    p = multinomialSampling model word document

multinomialSampling :: Model -> String -> Document -> [Double]
multinomialSampling model word document = scanl1 (+) (fmap sampling [0 .. (Model.numberOfTopics model - 1)])
  where
    vBeta = beta * fromIntegral (Model.numberOfWords model)
    beta = HyperParameter.beta $ Model.hyperParameter model

    kAlpha = alpha * fromIntegral (Model.numberOfTopics model)
    alpha = HyperParameter.alpha $ Model.hyperParameter model

    wordTopicMap = Model.wordTopicMap model
    documentTopicMap = Model.documentTopicMap model

    sampling topic =
      (fromIntegral (Map.findWithDefault 0 (word, topic) wordTopicMap) + beta)
        / (fromIntegral (count isTopic wordTopicMap) + vBeta)
        * (fromIntegral (Map.findWithDefault 0 (document, topic) documentTopicMap) + alpha)
        / (fromIntegral (length $ Document.words document) + kAlpha)
      where
        isTopic (_, topic') = topic == topic'

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
