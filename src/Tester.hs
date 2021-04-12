module Tester (test) where

import Data.Function
import Data.Functor
import Data.List.Split
import InterviewReader

test :: [Interview] -> IO ()
test interviews = wordCounts & print . sum
  where
    wordCounts = interviews <&> countWords

countWords :: Interview -> Int
countWords interview = length words
  where
    interviewContent = content interview
    words = splitOn " " interviewContent
    
test2 :: (b -> c) -> (a -> b) -> a -> c
test2 = (.)

