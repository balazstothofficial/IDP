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
import Interview
import System.Directory (doesFileExist)
import System.FilePath (pathSeparator)

class InterviewReader a where
  readInterview :: a -> IO Interview
  readInterviews :: a -> IO [Interview]

instance InterviewReader Directory where
  readInterview = directoryToAbsolutePath >=> readInterview
  readInterviews = directoryToAbsolutePath >=> readInterviews

instance InterviewReader FilePath where
  readInterview path = readFile path <&> Interview fileName
    where
      fileName = last splitPath
      splitPath = splitOn [pathSeparator] path

  readInterviews path = doesFileExist path >>= recurse
    where
      recurse isFile =
        if isFile
          then readInterview path <&> (: [])
          else listDirectoryWithFullPath path >>= fmap concat . mapM readInterviews
