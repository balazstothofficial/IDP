module InterviewReader
  ( readInterview,
    readInterviews,
    Interview,
    title,
    content,
  )
where

import Data.Functor
import Data.List.Split
import System.Directory

data Interview = Interview
  { title :: String,
    content :: String
  }
  deriving (Eq)

instance Show Interview where
  show interview = "Interview: " ++ title interview

readInterviews :: FilePath -> IO [Interview]
readInterviews path = doesFileExist path >>= recurse
  where
    recurse isFile =
      if isFile
        then readInterview path <&> (: [])
        else listDirectoryWithFullPath path >>= fmap concat . mapM readInterviews

readInterview :: FilePath -> IO Interview
readInterview path = readFile path <&> Interview fileName
  where
    fileName = last splitPath
    splitPath = splitOn pathSeparator path

listDirectoryWithFullPath :: FilePath -> IO [FilePath]
listDirectoryWithFullPath path = listDirectory path <&> fmap concatWithPath
  where
    concatWithPath fileName = path ++ pathSeparator ++ fileName

-- TODO: Use platform specific path separator
pathSeparator :: String
pathSeparator = "/"
