module LDA
  ( Model (Model),
    initialModel,
    run,
    randomFactors,
  )
where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Debug.Trace (trace)
import Document (Document)
import qualified Document
import qualified HyperParameter
import Model (DocumentTopicMap, Model, WordTopicMap)
import qualified Model
import System.Random
import Vocabulary
import Prelude hiding (words)

run :: [Document] -> Int -> Int -> IO ()
run documents numberOfTopics seed = do
  let topics = randomTopics (mkStdGen seed) numberOfTopics
  let model = initialModel documents numberOfTopics topics
  putStr $ show model
  let factors = randomFactors (mkStdGen (seed + 1))
  let estimatedModel = estimate model 2000 factors
  putStr $ show $ computeTheta . computePhi $ estimatedModel

initialModel :: [Document] -> Int -> [Int] -> Model
initialModel documents numberOfTopics topics =
  Model.Model
    { Model.numberOfTopics = numberOfTopics,
      Model.numberOfWords = numberOfWords,
      Model.numberOfDocuments = numberOfDocuments,
      Model.numberOfUpdates = 0,
      Model.documents = documents,
      Model.theta = Matrix.zero numberOfDocuments numberOfTopics,
      Model.phi = Matrix.zero numberOfTopics numberOfWords,
      Model.vocabulary = vocabulary,
      Model.hyperParameter =
        HyperParameter.HyperParameter
          { HyperParameter.alpha = 50 / fromIntegral numberOfTopics,
            HyperParameter.beta = 0.1
          },
      Model.wordTopicMap = wordTopicMap,
      Model.documentTopicMap = documentTopicMap,
      Model.topicAssignments = topicAssignments
    }
  where
    vocabulary = Vocabulary.fromDocuments documents
    numberOfDocuments = length documents
    numberOfWords = length vocabulary
    (topicAssignments, wordTopicMap, documentTopicMap) =
      initializeTopicMaps numberOfTopics topics documents

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

updateModel :: Model -> Model
updateModel = incrementUpdateCounter
  where
    incrementUpdateCounter model = model {Model.numberOfUpdates = numberOfUpdates + 1}
      where
        numberOfUpdates = Model.numberOfUpdates model

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
        word =  Set.elemAt (wordIndex - 1) vocabulary
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

estimate :: Model -> Int -> [Double] -> Model
estimate model 0 _ = model
estimate model iterations factors =
  estimate outputModel (iterations - 1) (drop totalDocumentsLength factors)
  where
    outputModel = tmpModel {Model.topicAssignments = topicAssignments}

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

randomTopics :: StdGen -> Int -> [Int]
randomTopics randomGenerator numberOfTopics = randomRs range randomGenerator
  where
    range = (0, numberOfTopics)

randomFactors :: StdGen -> [Double]
randomFactors randomGenerator = randomRs (0, 1) randomGenerator

(?) :: a -> String -> a
(?) = flip trace

infixr 1 ?
