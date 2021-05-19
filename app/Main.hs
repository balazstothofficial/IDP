module Main where

import InterviewReader
import LDA
import NewLDA
import TestData

main :: IO ()
main = run testDocuments 10 69

directory :: Directory
directory = Relative "interviews"
