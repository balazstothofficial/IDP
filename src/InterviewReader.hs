{-# LANGUAGE FlexibleInstances #-}

module InterviewReader
  ( InterviewReader (..),
    Directory (Relative, Absolute),
    Interview (..),
  )
where

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.List.Split
import Directory
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Interview
import System.Directory (doesFileExist)
import System.FilePath (pathSeparator)

class InterviewReader a where
  readInterview :: a -> IO Interview
  readInterviews :: a -> IO [Interview]
  readInterviewsPerSubfolder :: a -> IO [[Interview]]

instance InterviewReader Directory where
  readInterview = directoryToAbsolutePath >=> readInterview
  readInterviews = directoryToAbsolutePath >=> readInterviews
  readInterviewsPerSubfolder = directoryToAbsolutePath >=> readInterviewsPerSubfolder

instance InterviewReader FilePath where
  readInterview path = do
    _ <- setLocaleEncoding utf8
    readFile path <&> Interview fileName groupName
    where
      fileName = last splitPath
      groupName = last $ init splitPath
      splitPath = splitOn [pathSeparator] path

  readInterviews path = doesFileExist path >>= recurse
    where
      recurse isFile =
        if isFile
          then readInterview path <&> (: [])
          else listDirectoryWithFullPath path >>= fmap concat . mapM readInterviews
          
  readInterviewsPerSubfolder path = listDirectoryWithFullPath path >>= mapM readInterviews
