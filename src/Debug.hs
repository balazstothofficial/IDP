{-# LANGUAGE RecordWildCards #-}

module Debug where

import Data.List (sortOn)
import qualified Data.List as List
import qualified Data.Matrix as Matrix
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Document
import Model

(?) :: a -> String -> a
(?) = flip trace

infixr 1 ?

-- TODO: Make nicer!
showResult :: Model -> String
showResult Model {..} = iterate documents thetaLists
  where
    thetaLists = Matrix.toLists theta -- Document x Topic
    phiLists = Matrix.toLists phi -- Topic x Word
    iterate [] [] = ""
    iterate (document : ds) (topics : ts) =
      "\n\nDocument: " ++ title ++ "\n"
        ++ "Best topics:"
        ++ "\n1:"
        ++ show (topicWords (bestTopics !! 0))
        ++ "\n2:"
        ++ show (topicWords (bestTopics !! 1))
        ++ "\n3:"
        ++ show (topicWords (bestTopics !! 2))
        ++ "\n4:"
        ++ show (topicWords (bestTopics !! 3))
        ++ "\n5:"
        ++ show (topicWords (bestTopics !! 4))
        ++ iterate ds ts
      where
        title = Document.title document

        bestTopics :: [Int]
        bestTopics = maxIndex topics

        topicWords topic = fmap getWord $ take 10 $ sortOn (negate . fst) $ zip (phiLists !! topic) [0 ..]

    getWord (_, i) = Set.elemAt i vocabulary
    maxIndex xs = fmap snd (sortOn (negate . fst) (zip xs [0 ..]))
