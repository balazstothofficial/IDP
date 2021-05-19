module NewLDA
  ( Model (Model),
    initialModel,
    run,
  )
where

import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
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
  let model = initialModel documents numberOfTopics seed
  putStr $ show model
  putStr $ show $ updateModel model

initialModel :: [Document] -> Int -> Int -> Model
initialModel documents numberOfTopics seed =
  Model.Model
    { Model.numberOfTopics = numberOfTopics,
      Model.numberOfWords = numberOfWords,
      Model.numberOfDocuments = numberOfDocuments,
      Model.numberOfUpdates = 0,
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
    (topicAssignments, wordTopicMap, documentTopicMap) = initializeTopicMaps numberOfTopics topics documents
    topics = randomTopics (mkStdGen seed) numberOfTopics

initializeTopicMaps :: Int -> [Int] -> [Document] -> ([[Int]], WordTopicMap, DocumentTopicMap)
initializeTopicMaps _ _ [] = ([], Map.empty, Map.empty)
initializeTopicMaps numberOfTopics topics (document : documents) =
  let (topicAssignments', wordTopicMap', documentTopicMap') = initializeTopicMaps numberOfTopics nextTopics documents
   in ( fmap snd assignedTopics : topicAssignments',
        Map.unionWith (+) wordTopicMap wordTopicMap',
        Map.union documentTopicMap documentTopicMap'
      )
  where
    nextTopics = drop (length $ Document.words document) topics

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

randomTopics :: StdGen -> Int -> [Int]
randomTopics randomGenerator numberOfTopics = randomRs range randomGenerator
  where
    range = (0, numberOfTopics)

(?) :: a -> String -> a
(?) = flip trace

infixr 1 ?
