module Main where

import Document
import DocumentFactory
import InterviewReader
import LDARunner
import TestData

main :: IO ()
main = readInterviews directory >>= print . (\documents -> run (LDARunner.Input documents 1 6 2323453)) . test

test :: [Interview] -> [Document]
test interviews = fmap create interviews

directory :: Directory
directory = Relative "interviews"
