{-# LANGUAGE FlexibleInstances #-}

module InterviewReader
  ( readInterview,
    readInterviews,
    Directory (Relative, Absolute),
    Interview (..)
  )
where

import Data.Functor
import Data.List.Split
import System.Directory
import System.FilePath (pathSeparator)
import Control.Monad ((>=>))

data Interview = Interview
  { title :: String,
    content :: String
  }
  deriving (Eq)

-- Just for debugging purposes
instance Show Interview where
  show interview = "Interview: " ++ title interview

data Directory = Relative FilePath | Absolute FilePath

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

directoryToAbsolutePath :: Directory -> IO FilePath
directoryToAbsolutePath (Absolute path) = pure path
directoryToAbsolutePath (Relative path) = getCurrentDirectory <&> appendRelativePath
  where
    appendRelativePath = flip concatPaths path

listDirectoryWithFullPath :: FilePath -> IO [FilePath]
listDirectoryWithFullPath path = listDirectory path <&> fmap prependPath
  where
    prependPath = concatPaths path

concatPaths :: FilePath -> FilePath -> FilePath
concatPaths first second = first ++ [pathSeparator] ++ second
