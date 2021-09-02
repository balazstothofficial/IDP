{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module InitialTopics (InitialTopics, rawTopics, Factory (..)) where

import Factory
import System.Random

newtype InitialTopics = InitialTopics [Int]

instance Factory [Int] InitialTopics where
  create = InitialTopics

-- TODO: Make seed and numberOfTopics type safe?
instance Factory Int (Int -> InitialTopics) where
  create seed numberOfTopics = InitialTopics $ randomRs range randomGenerator
    where
      randomGenerator = mkStdGen seed
      range = (0, numberOfTopics)

rawTopics :: InitialTopics -> Int -> [Int]
rawTopics (InitialTopics topics) n = take n topics
