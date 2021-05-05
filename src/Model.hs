module Model
  ( Model (Model),
    hyperParameter,
    numberOfTopics,
    numberOfWords,
    numberOfDocuments,
    numberOfUpdates,
    gamma,
    eLogBeta,
    expELogBeta,
    lambda,
    sstats,
    vocabulary,
  )
where

import Data.Matrix (Matrix, ncols, nrows)
import HyperParameter
import Vocabulary

data Model = Model
  { hyperParameter :: HyperParameter,
    numberOfTopics :: Int,
    numberOfWords :: Int,
    numberOfDocuments :: Int,
    numberOfUpdates :: Int,
    -- TODO: Find out what they are used for
    gamma :: Matrix Double,
    eLogBeta :: Matrix Double,
    expELogBeta :: Matrix Double,
    lambda :: Matrix Double,
    -- TODO: Rename to something understandable
    sstats :: Matrix Double,
    vocabulary :: Vocabulary
  }

-- For debugging purposes:
instance Show Model where
  show model =
    "Model {"
      ++ "\n\tHyperParamter = "
      ++ show (hyperParameter model)
      ++ "\n\tNumber of Topics = "
      ++ show (numberOfTopics model)
      ++ "\n\tNumber of Words = "
      ++ show (numberOfWords model)
      ++ "\n\tNumber of Documents = "
      ++ show (numberOfDocuments model)
      ++ "\n\tNumber of Updates = "
      ++ show (numberOfUpdates model)
      ++ "\n\tGamma = "
      ++ showDimensions (gamma model)
      ++ "\n\teLogBeta = "
      ++ show (eLogBeta model)
      ++ "\n\texpELogBeta = "
      ++ show (expELogBeta model)
      ++ "\n\tLambda = "
      ++ showDimensions (lambda model)
      ++ "\n\tSstats = "
      ++ showDimensions (sstats model)
      ++ "\n\tVocabulary = "
      ++ show (vocabulary model)
      ++ "\n}\n"

showDimensions :: Matrix a -> String
showDimensions matrix =
  "Matrix " ++ show rows ++ "x" ++ show cols
    ++ " (Rows x Cols)"
  where
    rows = nrows matrix
    cols = ncols matrix
