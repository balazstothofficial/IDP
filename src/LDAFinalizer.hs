module LDAFinalizer where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
import qualified HyperParameter
import Model (Model)
import qualified Model
import qualified Document

-- TODO: Extract from this file
computePhi :: Model -> Model
computePhi model = model {Model.phi = phi}
  where
    phi = Matrix.matrix numberOfTopics numberOfWords fill

    numberOfWords = Model.vocabularySize model
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

count :: (k -> Bool) -> Map k Int -> Int
count predicate = Map.foldrWithKey f 0
  where
    f k a acc = if predicate k then acc else a + acc
