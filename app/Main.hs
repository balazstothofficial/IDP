{-# OPTIONS -Wno-name-shadowing #-}

module Main where

import Document
import InterviewReader
import LDARunner
import Debug

main :: IO ()
main = readInterviews directory >>= putStrLn . showResult . runLDA . createDocuments
  where
    runLDA documents = run (Input documents 500 75 42069420)

createDocuments :: [Interview] -> [Document]
createDocuments = fmap create

directory :: Directory
directory = Relative "interviews"
