module NewLDA
  ( Model (Model),
    initialModel,
    run,
  )
where

import Data.Matrix (Matrix, scaleMatrix)
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
import Document
import qualified HyperParameter
import Lambda
import MatrixExtensions
import Model (Model)
import qualified Model
import Vocabulary
import Prelude hiding (words)

run :: [Document] -> Int -> IO ()
run documents numberOfTopics = do
  let vocabulary = Vocabulary.fromDocuments documents
  let numberOfWords = Set.size vocabulary
  initialLambda <- randomGammaMatrix numberOfTopics numberOfWords
  let model = initialModel documents numberOfTopics initialLambda
  putStr $ show model
  putStr $ show $ updateModel model

initialModel :: [Document] -> Int -> Matrix Double -> Model
initialModel documents numberOfTopics lambda =
  Model.Model
    { Model.numberOfTopics = numberOfTopics,
      Model.numberOfWords = numberOfWords,
      Model.numberOfDocuments = numberOfDocuments,
      Model.numberOfUpdates = 0,
      Model.gamma = Matrix.zero numberOfDocuments numberOfTopics,
      Model.eLogBeta = Matrix.zero numberOfTopics numberOfWords,
      Model.expELogBeta = Matrix.zero numberOfTopics numberOfWords,
      Model.lambda = lambda,
      Model.sstats = Matrix.zero numberOfTopics numberOfWords,
      Model.vocabulary = vocabulary,
      Model.hyperParameter =
        HyperParameter.HyperParameter
          { HyperParameter.alpha = 1.0 / fromIntegral numberOfTopics,
            HyperParameter.eta = 1.0 / fromIntegral numberOfTopics,
            HyperParameter.rho = 0.7,
            HyperParameter.kappa = 0.7,
            HyperParameter.tau = 1024
          }
    }
  where
    vocabulary = Vocabulary.fromDocuments documents
    numberOfDocuments = length documents
    numberOfWords = Set.size vocabulary

updateModel :: Model -> Model
updateModel = updateRho . updateBeta . updateLambda . incrementUpdateCounter
  where
    incrementUpdateCounter model = model {Model.numberOfUpdates = numberOfUpdates + 1}
      where
        numberOfUpdates = Model.numberOfUpdates model

updateLambda :: Model -> Model
updateLambda model = model {Model.lambda = newLambda}
  where
    lambda = Model.lambda model
    hyperParameter = Model.hyperParameter model
    sstats = Model.sstats model
    rho = HyperParameter.rho hyperParameter
    eta = HyperParameter.eta hyperParameter
    numberOfDocuments = fromIntegral $ Model.numberOfDocuments model

    scaledLambda = scaleMatrix (1.0 - rho) lambda
    scaledSStats = scaleMatrix rho $ scalarAdd eta $ scaleMatrix numberOfDocuments sstats
    newLambda = Matrix.elementwise (+) scaledLambda scaledSStats

updateBeta :: Model -> Model
updateBeta model = model {Model.eLogBeta = eLogBeta, Model.expELogBeta = expELogBeta}
  where
    lambda = Model.lambda model
    eLogBeta = dirichletExpectation lambda
    expELogBeta = expOf eLogBeta

updateRho :: Model -> Model
updateRho model = model {Model.hyperParameter = hyperParameter {HyperParameter.rho = rho}}
  where
    numberOfUpdates = Model.numberOfUpdates model
    hyperParameter = Model.hyperParameter model
    tau = HyperParameter.tau hyperParameter
    kappa = HyperParameter.kappa hyperParameter

    rho = rhot tau numberOfUpdates kappa

-- How much to weight the information from a mini-batch.
-- p_t = (t_0 + t)^{-k}
-- TODO: Rename to something understandable
rhot :: Double -> Int -> Double -> Double
rhot tau count kap = (tau + fromIntegral count) ** (- kap)
