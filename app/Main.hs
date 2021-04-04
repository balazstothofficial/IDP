module Main where

import InterviewReader

main :: IO ()
main = readInterviews directory >>= print

directory :: String
directory = "/Users/balazstoth/IDP/interviews/"
