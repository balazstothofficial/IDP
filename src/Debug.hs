{-# LANGUAGE RecordWildCards #-}

module Debug where

import qualified Data.List as List
import qualified Data.Matrix as Matrix
import Data.Ord (comparing)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Document
import Model

(?) :: a -> String -> a
(?) = flip trace

infixr 1 ?

showResult :: Model -> String
showResult Model {..} = iterate documents thetaLists []
  where
    thetaLists = Matrix.toLists theta
    phiLists = Matrix.toLists phi

    iterate [] [] _ = ""
    iterate (document : ds) (topics : ts) usedTopics =
      "\nDocument: " ++ title ++ "\n"
        ++ "Best topic: "
        ++ show topicWords
        ++ iterate ds ts (maxTopic : usedTopics)
      where
        title = Document.title document

        maxTopic = recurse (maxIndex topics)
          where
            recurse [] = 0
            recurse (s : sortedTopics) = if s `elem` usedTopics then recurse sortedTopics else s

        topicWords = fmap getWord $ take 10 $ List.sortBy (comparing fst) $ zip (phiLists !! maxTopic) [0 ..]

    getWord (_, i) = Set.elemAt i vocabulary
    maxIndex xs = fmap snd (List.sortBy (comparing fst) (zip xs [0 ..]))
