{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TopicCounts (Factory(..), TopicCounts) where

import Data.Map (Map)
import qualified Data.Map as Map
import Factory

-- TODO: use new type!
type TopicCounts = Map Int Int

instance Factory [Int] TopicCounts where
  create = foldr increaseCount Map.empty
    where
      increaseCount = Map.alter increase

      increase Nothing = Just 1
      increase (Just n) = Just (n + 1)
