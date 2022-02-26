module InterviewLDARunner where

import Configuration
import Control.Concurrent.ParallelIO.Global (parallel_)
import Data.Functor ((<&>))
import Data.List (intercalate, nub)
import qualified Data.Map as Map
import Debug
import Document
import GHC.IO.Encoding (setLocaleEncoding)
import InterviewReader
import LDARunner hiding (documents, numberOfTopics, saveInterval, saveIterations, seed)
import System.Directory.Internal.Prelude (catMaybes)
import System.IO

runLDAOnFullDataSetWithCombinedSubfolders :: IO ()
runLDAOnFullDataSetWithCombinedSubfolders = readInterviewsPerSubfolder interviewDirectory >>= processInterviews
  where
    
    processInterviews interviews = resultDirectory fullDataSet >>= flip processInterviewsToFiles interviews

    processInterviewsToFiles fileName = sequence_ . writeToFile fileName . runLDAOnInterviews

    writeToFile fileName models =
      setLocaleEncoding utf8 :
      writeFile fileName fullDataSet :
      fmap (appendFile fileName . showResult) models
        
    runLDAOnInterviews = runLDA . fmap toDocuments
      where
        toDocuments interviews = combineDocuments groupName $ createDocuments interviews
          where
            groupName = group $ head interviews

    runLDA documents = run $ Input documents saveIterations saveInterval numberOfTopics seed

runLDAOnCombinedFullDataSet :: IO ()
runLDAOnCombinedFullDataSet = readInterviews interviewDirectory >>= processInterviews
  where
    processInterviews interviews = resultDirectory fullDataSet >>= flip processInterviewsToFiles interviews

    processInterviewsToFiles fileName = sequence_ . writeToFile fileName . runLDAOnInterviews

    writeToFile fileName models =
      setLocaleEncoding utf8 :
      writeFile fileName fullDataSet :
      fmap (appendFile fileName . showResult) models

    runLDAOnInterviews = runLDA . singletonList . combineDocuments "Full-Data-Set" . createDocuments 
    
    singletonList x = [x]

    runLDA documents = run $ Input documents saveIterations saveInterval numberOfTopics seed

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

runLDAPerCombinedSubfolder :: IO ()
runLDAPerCombinedSubfolder = readInterviewsPerSubfolder interviewDirectory >>= runParallel
  where
    runParallel interviews = parallel_ $ fmap processInterviews interviews

    processInterviews interviews = resultDirectory groupName >>= flip (processInterviewsToFiles groupName) interviews
      where
        groupName = group $ head interviews

    processInterviewsToFiles groupName fileName = sequence_ . writeToFile groupName fileName . runLDAOnInterviews groupName

    writeToFile groupName fileName models =
      setLocaleEncoding utf8 :
      writeFile fileName groupName :
      fmap (appendFile fileName . showResult) models

    runLDAOnInterviews groupName = runLDA . singletonList . combineDocuments groupName . createDocuments 
        
    singletonList x = [x]

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

combineDocuments :: String -> [Document] -> Document
combineDocuments name = foldl1 combine
  where
    combine d1 d2 = Document name (Document.words d1 ++ Document.words d2) (Map.unionWith (+) (Document.wordCounts d1) (Document.wordCounts d2))

fullDataSet :: String
fullDataSet = "Full-Data-Set"

createDocuments :: [Interview] -> [Document]
createDocuments = fmap create

decideWords :: IO ()
decideWords = readInterviews interviewDirectory >>= decideDocuments . createDocuments >>= writeToFile
  where
    writeToFile text = do
      _ <- setLocaleEncoding utf8
      writeFile "Stoppwords.txt" text

    decideDocuments documents =
      sequence (documents <&> decideDocument)
        <&> concat
        <&> catMaybes
        <&> nub
        <&> fmap ('\"' :)
        <&> fmap (++ "\",")
        <&> intercalate "\n"

    decideDocument document = sequence $ decideWord <$> nub (Document.words document)

    decideWord word = do
      putStrLn $ "\n" ++ word
      hFlush stdout
      line <- getLine
      return $ if 'y' `elem` line then Just word else Nothing
