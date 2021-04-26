module NewLDA where

import Control.Monad.State (replicateM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Data.Random (MonadRandom, gamma, sample)
import Data.Random.RVar (RVar)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (words)
import Document

-- | HyperParameters for an LDA model.
--    * alpha Prior on topic weights Theta
--    * eta   E[log(Beta)], where Beta is a matrix of p(w|topic)
--    * rho
--    * kappa learning parameter; decay factor for influence of batches
--    * tau   learning parameter to downweight early documents
data HyperParameter = HyperParameter
  { alpha :: Double,
    eta :: Double,
    rho :: Double,
    kappa :: Double,
    tau :: Double
  }
  deriving (Show)

data Model = Model
  { hyperParameter :: HyperParameter,
    numberOfTopics :: Int,
    numberOfWords :: Int,
    numberOfDocuments :: Int,
    numberOfUpdates :: Int,
    -- TODO: Find out what they are used for
    gamma2 :: Matrix Double,
    eLogBeta :: Matrix Double,
    expELogBeta :: Matrix Double,
    lambda :: Matrix Double,
    -- TODO: Rename to something understandable
    sstats :: Matrix Double,
    vocabulary :: Set String
  }
  deriving (Show)

-- initialModel :: Set String -> Int -> Int -> Model
-- initialModel vocabulary numberOfTopics numberOfDocuments = Model {}

gammaRandomVariable :: RVar Double
gammaRandomVariable = gamma 100.0 (1.0 / 100.0)

randomGammaMatrix :: (MonadRandom m) => Int -> Int -> m (Matrix Double)
randomGammaMatrix rows columns = matrixFromList <$> randomVariables
  where
    matrixFromList xs = Matrix.fromList rows columns xs

    randomVariables = replicateM matrixSize sampleRandomVariable
      where
        matrixSize = rows * columns
        sampleRandomVariable = sample gammaRandomVariable

testVocabulary :: Set String
testVocabulary =
  Set.fromList
    [ "the",
      "tree",
      "runs",
      "quickly",
      "far",
      "away",
      "lions",
      "yell",
      "she",
      "shoots",
      "shapes",
      "now",
      "he",
      "hits",
      "hops",
      "happily"
    ]

testDocuments :: [Document]
testDocuments =
  [ createDocument ["the", "tree", "runs", "quickly"],
    createDocument ["far", "away", "lions", "yell"],
    createDocument ["she", "shoots", "shapes", "now"],
    createDocument ["he", "hits", "hops", "happily"]
  ]
