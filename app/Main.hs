module Main where

import Document
import DocumentFactory (create, interviewBasedDocumentFactory)
import InterviewReader
import LDARunner (ldaRunner, run)
import qualified LDARunner

main :: IO ()
main = readInterviews interviewReader directory >>= print . runLDA . test
  where
    runLDA documents = run ldaRunner (LDARunner.Input documents 1 6 2323453)

test :: [Interview] -> [Document]
test = fmap $ create interviewBasedDocumentFactory

directory :: Directory
directory = Relative "interviews"
