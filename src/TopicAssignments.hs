{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TopicAssignments (TopicAssignments, Factory (..)) where

import Data.Functor ((<&>))
import Document (Document (..))
import Factory
import List
import Prelude hiding (words)

-- TODO: New Type
type TopicAssignments = [[Int]]

instance Factory [Document] ([Int] -> TopicAssignments) where
  create documents = create $ documents <&> words

instance Factory [[String]] ([Int] -> TopicAssignments) where
  create = flip mapToStructure
