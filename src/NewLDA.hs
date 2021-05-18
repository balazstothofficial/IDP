module NewLDA
  ( Model (Model),
    initialModel,
    run,
    randomTopic,
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

run :: [Document] -> Int -> IO ()
run documents numberOfTopics = do
  let model = initialModel documents numberOfTopics
  putStr $ show model
  putStr $ show $ updateModel model

initialModel :: [Document] -> Int -> Model
initialModel documents numberOfTopics =
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
    (topicAssignments, wordTopicMap, documentTopicMap, _) = initializeTopicMaps (mkStdGen 42) numberOfTopics documents

initializeTopicMaps :: StdGen -> Int -> [Document] -> ([[Int]], WordTopicMap, DocumentTopicMap, StdGen)
initializeTopicMaps rGen _ [] = ([], Map.empty, Map.empty, rGen)
initializeTopicMaps rGenerator numberOfTopics (document : documents) =
  let (t', nw', nd', gen') = assignRandomTopics rGenerator (Document.words document) Map.empty Map.empty
   in let (ts, nw, nd, gen'') = initializeTopicMaps gen' numberOfTopics documents
       in (t' : ts, Map.unionWith (+) nw' nw, Map.union nd' nd, gen'')
  where
    assignRandomTopics :: StdGen -> [String] -> WordTopicMap -> DocumentTopicMap -> ([Int], WordTopicMap, DocumentTopicMap, StdGen)
    assignRandomTopics randomGenerator words wordTopicMap documentTopicMap = case words of
      [] -> ([], wordTopicMap, documentTopicMap, randomGenerator)
      (word : otherWords) -> case assignRandomTopics nextRandomGenerator otherWords wordTopicMap documentTopicMap of
        (z', nw', nd', _) -> (topic : z', Map.alter increase (word, topic) nw', Map.alter increase (document, topic) nd', nextRandomGenerator)
      where
        (topic, nextRandomGenerator) = randomTopic randomGenerator numberOfTopics

        increase :: Maybe Int -> Maybe Int
        increase Nothing = Just 1
        increase (Just x) = Just (x + 1)

updateModel :: Model -> Model
updateModel = incrementUpdateCounter
  where
    incrementUpdateCounter model = model {Model.numberOfUpdates = numberOfUpdates + 1}
      where
        numberOfUpdates = Model.numberOfUpdates model

randomTopic :: StdGen -> Int -> (Int, StdGen)
randomTopic randomGenerator numberOfTopics = randomR range randomGenerator
  where
    range = (0, numberOfTopics)

(?) :: a -> String -> a
(?) = flip trace
infixr 1 ?
