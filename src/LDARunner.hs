{-# LANGUAGE RecordWildCards #-}

module LDARunner where

import Debug (showResult, (?))
import Document (Document)
import Factory
import LDAEstimator (estimate)
import qualified LDAEstimator
import LDAFinalizer
import Model (Model (..))
import qualified ModelFactory
import System.Random (mkStdGen, randomRs)
import Prelude hiding (iterate)

class LDARunner a where
  run :: a -> [Model]

data Input = Input
  { documents :: [Document],
    saveIterations :: Int,
    saveInterval :: Int,
    numberOfTopics :: Int,
    seed :: Int
  }
  deriving (Show, Eq)

instance LDARunner Input where
  run Input {..} = scanl iterate initialModel [0 .. saveIterations - 1]
    where
      initialModel =
        create
          ModelFactory.Input
            { documents = documents,
              numberOfTopics = numberOfTopics,
              topics = create seed numberOfTopics
            }

      iterate model iteration = estimatedModel
        where
          factors = randomFactors (seed + iteration)
          estimatedModel =
            computeTheta $
              computePhi $
                estimate
                  LDAEstimator.Input
                    { model = model,
                      iterations = saveInterval,
                      factors = factors
                    }

randomFactors :: Int -> [Double]
randomFactors seed = randomRs (0, 1) randomGenerator
  where
    randomGenerator = mkStdGen seed
