module Configuration where

import Data.Time.Calendar
import Data.Time.Clock
import InterviewReader
import Data.Functor ((<&>))

interviewDirectory :: Directory
interviewDirectory = Relative "interviews"

resultFile :: String
resultFile = "result.txt"

date :: IO (Integer, Int, Int)
date = getCurrentTime <&> toGregorian . utctDay

seed :: Int
seed = 69

numberOfTopics :: Int
numberOfTopics = 50

saveIterations :: Int
saveIterations = 1

saveInterval :: Int
saveInterval = 1
