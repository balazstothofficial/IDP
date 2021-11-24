{-# OPTIONS -Wno-name-shadowing #-}

module Main where

import Debug
import Document
import InterviewReader
import Configuration
import LDARunner hiding (numberOfTopics, saveInterval, saveIterations, seed)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = readInterviews interviewDirectory >>= sequence_ . write . transform
  where
    write models = setLocaleEncoding utf8 : writeFile resultFile "TITEL:" : fmap (appendFile resultFile . showResult) models
    transform = runLDA . createDocuments
    runLDA documents = run $ Input documents saveIterations saveInterval numberOfTopics seed

createDocuments :: [Interview] -> [Document]
createDocuments = fmap create
