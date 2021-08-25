{-# LANGUAGE RecordWildCards #-}

module LDARunner where

import Document (Document)
import Factory
import LDAEstimator (estimate)
import qualified LDAEstimator
import Model (Model (..))
import qualified ModelFactory
import System.Random (mkStdGen, randomRs)

class LDARunner a where
  run :: a -> Model

data Input = Input
  { documents :: [Document],
    iterations :: Int,
    numberOfTopics :: Int,
    seed :: Int
  }
  deriving (Show, Eq)

instance LDARunner Input where
  run Input {..} = estimatedModel
    where
      factors = randomFactors (seed + 1)

      initialModel =
        create
          ModelFactory.Input
            { documents = documents,
              numberOfTopics = numberOfTopics,
              topics = create seed numberOfTopics
            }

      estimatedModel =
        estimate
          LDAEstimator.Input
            { model = initialModel,
              iterations = iterations,
              factors = factors
            }

randomFactors :: Int -> [Double]
randomFactors seed = randomRs (0, 1) randomGenerator
  where
    randomGenerator = mkStdGen seed
