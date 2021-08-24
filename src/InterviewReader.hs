{-# LANGUAGE FlexibleInstances #-}

module InterviewReader
  ( InterviewReader (..),
    Directory (Relative, Absolute),
    Interview (..),
    interviewReader,
  )
where

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.List.Split
import Directory
import Interview
import System.Directory (doesFileExist)
import System.FilePath (pathSeparator)

data InterviewReader a = InterviewReader
  { readInterview :: a -> IO Interview,
    readInterviews :: a -> IO [Interview]
  }

interviewReader :: InterviewReader Directory
interviewReader = InterviewReader readInterviewFromDirectory readInterviewsFromDirectory
  where
    readInterviewFromDirectory = withAbsolutePath readInterview
    readInterviewsFromDirectory = withAbsolutePath readInterviews

    withAbsolutePath f = directoryToAbsolutePath >=> f filePathBasedInterviewReader

filePathBasedInterviewReader :: InterviewReader FilePath
filePathBasedInterviewReader = InterviewReader readInterviewFromPath readInterviewsFromPath
  where
    readInterviewFromPath path = readFile path <&> Interview fileName
      where
        fileName = last splitPath
        splitPath = splitOn [pathSeparator] path

    readInterviewsFromPath path = doesFileExist path >>= recurse
      where
        recurse isFile =
          if isFile
            then readInterviewFromPath path <&> (: [])
            else listDirectoryWithFullPath path >>= fmap concat . mapM readInterviewsFromPath
