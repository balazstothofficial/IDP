{-# OPTIONS -Wno-name-shadowing #-}

module Main where

import Document
import InterviewReader
import LDARunner
import Debug

main :: IO ()
main = readInterviews directory >>= putStrLn . showResult . runLDA . test
  where
    runLDA documents = run (Input documents 10 50 2323453)

test :: [Interview] -> [Document]
test = fmap create

directory :: Directory
directory = Relative "interviews"
