module Main where

import InterviewReader

main :: IO ()
main = readInterviews directory >>= print

directory :: Directory
directory = Relative "interviews"
