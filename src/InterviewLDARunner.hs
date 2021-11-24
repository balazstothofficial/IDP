module InterviewLDARunner where

import Configuration
import Debug
import Document
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import InterviewReader
import LDARunner hiding (numberOfTopics, saveInterval, saveIterations, seed, documents)
import Control.Concurrent.ParallelIO.Global (parallel_)  
  
runLDAOnFullDataSet :: IO ()
runLDAOnFullDataSet = readInterviews interviewDirectory >>= processInterviews
  where
    processInterviews interviews = resultDirectory fullDataSet >>= flip processInterviewsToFiles interviews

    processInterviewsToFiles fileName = sequence_ . writeToFile fileName . runLDAOnInterviews

    writeToFile fileName models =
      setLocaleEncoding utf8 :
      writeFile fileName fullDataSet :
      fmap (appendFile fileName . showResult) models

    runLDAOnInterviews = runLDA . createDocuments

    runLDA documents = run $ Input documents saveIterations saveInterval numberOfTopics seed

runLDAPerSubfolder :: IO ()
runLDAPerSubfolder = readInterviewsPerSubfolder interviewDirectory >>= runParallel
  where
    runParallel interviews = parallel_ $ fmap processInterviews interviews
    
    processInterviews interviews = resultDirectory groupName >>= flip (processInterviewsToFiles groupName) interviews
      where
        groupName = group $ head interviews

    processInterviewsToFiles groupName fileName = sequence_ . writeToFile groupName fileName . runLDAOnInterviews

    writeToFile groupName fileName models =
      setLocaleEncoding utf8 :
      writeFile fileName groupName :
      fmap (appendFile fileName . showResult) models

    runLDAOnInterviews = runLDA . createDocuments

    runLDA documents = run $ Input documents saveIterations saveInterval numberOfTopics seed

fullDataSet :: String
fullDataSet = "Full-Data-Set"

createDocuments :: [Interview] -> [Document]
createDocuments = fmap create
