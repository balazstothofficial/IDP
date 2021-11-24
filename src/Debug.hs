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
showResult Model {..} = "\n\nAfter Iteration: " ++ show numberOfUpdates ++ "\n" ++ iterate documents thetaLists
  where
    thetaLists = Matrix.toLists theta -- Document x Topic
    phiLists = Matrix.toLists phi -- Topic x Word
    iterate [] [] = ""
    iterate (document : ds) (topics : ts) =
      "\n\nDocument: " ++ title ++ "\n"
        ++ "Best topics:"
        ++ "\n1:"
        ++ concatToString (topicWords (bestTopics !! 0))
        ++ "\n2:"
        ++ concatToString (topicWords (bestTopics !! 1))
        ++ "\n3:"
        ++ concatToString (topicWords (bestTopics !! 2))
        ++ "\n4:"
        ++ concatToString (topicWords (bestTopics !! 3))
        ++ "\n5:"
        ++ concatToString (topicWords (bestTopics !! 4))
        ++ "\n6:"
        ++ concatToString (topicWords (bestTopics !! 5))
        ++ "\n7:"
        ++ concatToString (topicWords (bestTopics !! 6))
        ++ "\n8:"
        ++ concatToString (topicWords (bestTopics !! 7))
        ++ "\n9:"
        ++ concatToString (topicWords (bestTopics !! 8))
        ++ "\n10:"
        ++ concatToString (topicWords (bestTopics !! 9))
        ++ iterate ds ts
      where
        title = Document.title document

        bestTopics :: [Int]
        bestTopics = maxIndex topics

        topicWords topic = fmap getWord $ take 15 $ sortOn (negate . fst) $ zip (phiLists !! topic) [0 ..]

    getWord (_, i) = Set.elemAt i vocabulary
    maxIndex xs = fmap snd (sortOn (negate . fst) (zip xs [0 ..]))
    
concatToString :: [String] -> String
concatToString strings = "[" ++ List.intercalate ", " strings ++ "]"
