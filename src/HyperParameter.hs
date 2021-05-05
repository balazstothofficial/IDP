module HyperParameter
  ( HyperParameter(HyperParameter),
    alpha,
    eta,
    rho,
    kappa,
    tau,
  )
where

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
