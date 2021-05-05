module Lambda
  ( randomGammaMatrix,
  )
where

import Control.Monad.State
import Data.Matrix
import Data.Random

randomGammaMatrix :: (MonadRandom m) => Int -> Int -> m (Matrix Double)
randomGammaMatrix rows columns = matrixFromList <$> randomVariables
  where
    matrixFromList xs = fromList rows columns xs

    randomVariables = replicateM matrixSize sampleRandomVariable
      where
        matrixSize = rows * columns
        sampleRandomVariable = sample gammaRandomVariable

gammaRandomVariable :: RVar Double
gammaRandomVariable = gamma 100.0 (1.0 / 100.0)
