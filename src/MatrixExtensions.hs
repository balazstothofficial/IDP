module MatrixExtensions
  ( dirichletExpectation,
    scalarAdd,
    expOf
  )
where

import Data.Matrix (Matrix, elementwise, fromList, getRow, ncols, nrows, toList, (<|>))
import qualified Data.Vector as Vector
import Numeric.SpecFunctions (digamma)

-- For a vector theta ~ Dir(alpha), computes E[log(theta)] given alpha.
dirichletExpectation :: Matrix Double -> Matrix Double
dirichletExpectation matrix = mapAll digamma $ subtractFromColumns summedRows matrix
  where
    summedRows = sumRows matrix

-- Add a scalar to every element of a matrix.
scalarAdd :: Double -> Matrix Double -> Matrix Double
scalarAdd x = mapAll $ \y -> y + x

-- e^ each element of the matrix.
expOf :: Matrix Double -> Matrix Double
expOf = mapAll $ \x -> exp 1 ** x

-- Subtract a column vector from each column.
subtractFromColumns :: Num a => [a] -> Matrix a -> Matrix a
subtractFromColumns column matrix = elementwise (-) matrix matrixFromColumn
  where
    matrixFromColumn = repeatColumns column $ ncols matrix

-- Sums the rows of a Matrix.
sumRows :: Num a => Matrix a -> [a]
sumRows m = map (sum . rowList) [1 .. nrows m]
  where
    rowList i' = Vector.toList $ getRow i' m

-- Repeat a column vector n times to create a matrix
repeatColumns :: [a] -> Int -> Matrix a
repeatColumns col n = horizN initial initial (n - 1)
  where
    initial = fromList (length col) 1 col

    -- TODO: Name correctly
    horizN :: Matrix a -> Matrix a -> Int -> Matrix a
    horizN x _ 0 = x
    horizN x y m = horizN (x <|> y) y (m - 1)

-- Map over every element of a matrix.
mapAll :: (a -> b) -> Matrix a -> Matrix b
mapAll f m = fromList (nrows m) (ncols m) $ map f $ toList m
