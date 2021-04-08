module InterviewReader
  ( readInterview,
    readInterviews,
    Directory (Relative, Absolute),
    Interview,
    title,
    content,
  )
where

import Data.Functor
import Data.List.Split
import System.Directory
import System.FilePath (pathSeparator)

data Interview = Interview
  { title :: String,
    content :: String
  }
  deriving (Eq)

instance Show Interview where
  show interview = "Interview: " ++ title interview

data Directory = Relative FilePath | Absolute FilePath

readInterviews :: Directory -> IO [Interview]
readInterviews = useDirectoryFor readInterviewsFromPath

readInterview :: Directory -> IO Interview
readInterview = useDirectoryFor readInterviewFromPath

readInterviewsFromPath :: FilePath -> IO [Interview]
readInterviewsFromPath path = doesFileExist path >>= recurse
  where
    recurse isFile =
      if isFile
        then readInterviewFromPath path <&> (: [])
        else listDirectoryWithFullPath path >>= fmap concat . mapM readInterviewsFromPath

readInterviewFromPath :: FilePath -> IO Interview
readInterviewFromPath path = readFile path <&> Interview fileName
  where
    fileName = last splitPath
    splitPath = splitOn [pathSeparator] path

useDirectoryFor :: (FilePath -> IO a) -> (Directory -> IO a)
useDirectoryFor operation = transformedOperation
  where
    transformedOperation (Absolute path) = operation path
    transformedOperation (Relative path) = getCurrentDirectory >>= operation . appendPath
      where
        appendPath = flip concatPaths path

listDirectoryWithFullPath :: FilePath -> IO [FilePath]
listDirectoryWithFullPath path = listDirectory path <&> fmap prependPath
  where
    prependPath = concatPaths path

concatPaths :: FilePath -> FilePath -> FilePath
concatPaths first second = first ++ [pathSeparator] ++ second
