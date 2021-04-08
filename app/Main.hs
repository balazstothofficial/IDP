module Main where

import InterviewReader

main :: IO ()
main = readInterviewsRelativeToCurrentDirectory directory >>= print

directory :: String
directory = "interviews"
