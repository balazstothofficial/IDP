{-# OPTIONS -Wno-name-shadowing #-}

module Main where

import Document
import InterviewReader
import LDARunner

main :: IO ()
main = readInterviews directory >>= print . runLDA . test
  where
    runLDA documents = run (Input documents 1 6 2323453)

test :: [Interview] -> [Document]
test = fmap create

directory :: Directory
directory = Relative "interviews"
