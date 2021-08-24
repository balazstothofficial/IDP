{-# LANGUAGE RecordWildCards #-}

module LDARunner where

import Document (Document)
import Factory
import LDAEstimator (estimate, estimator)
import qualified LDAEstimator
import Model (Model (..))
import qualified ModelFactory
import System.Random (mkStdGen, randomRs)

newtype LDARunner a = LDARunner {run :: a -> Model}

data Input = Input
  { documents :: [Document],
    iterations :: Int,
    numberOfTopics :: Int,
    seed :: Int
  }
  deriving (Show, Eq)

ldaRunner :: LDARunner Input
ldaRunner = LDARunner {run = run}
  where
    run Input {..} = estimatedModel
      where
        topics = randomTopics seed numberOfTopics

        initialModel =
          create
            ModelFactory.modelFactory
            ModelFactory.Input
              { documents = documents,
                numberOfTopics = numberOfTopics,
                topics = topics
              }

        factors = randomFactors (seed + 1)

        estimatedModel =
          estimate
            estimator
            LDAEstimator.Input
              { model = initialModel,
                iterations = iterations,
                factors = factors
              }

randomTopics :: Int -> Int -> [Int]
randomTopics seed numberOfTopics = randomRs range randomGenerator
  where
    randomGenerator = mkStdGen seed
    range = (0, numberOfTopics)

randomFactors :: Int -> [Double]
randomFactors seed = randomRs (0, 1) randomGenerator
  where
    randomGenerator = mkStdGen seed
