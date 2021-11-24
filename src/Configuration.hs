module Configuration where

import Data.Functor ((<&>))
import Data.Time.Clock
import InterviewReader
import System.FilePath (pathSeparator)
import System.Directory (createDirectoryIfMissing)

interviewDirectory :: Directory
interviewDirectory = Relative "interviews"

resultFile :: String
resultFile = "result.txt"

resultDirectory :: String -> IO String
resultDirectory name = do
  _ <- createDirectoryIfMissing True directoryName
  getCurrentTime <&> fileWithDirectory
  where
    fileWithDirectory time = directoryName ++ [pathSeparator] ++ fileName time ++ ".txt"

    fileName time =
      fmap replace $
        show time ++ "_" ++ name ++ "_Topics_" ++ show numberOfTopics ++ "_Iterations_" ++ show iterations

    iterations = saveInterval * saveIterations

    replace ' ' = '_'
    replace '.' = '_'
    replace ':' = '-'
    replace c = c

    directoryName = "result"

seed :: Int
seed = 69

numberOfTopics :: Int
numberOfTopics = 50

saveIterations :: Int
saveIterations = 1

saveInterval :: Int
saveInterval = 1
