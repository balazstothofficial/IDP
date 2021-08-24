module Directory where

import System.Directory
import System.FilePath (pathSeparator)
import Data.Functor ((<&>))

data Directory = Relative FilePath | Absolute FilePath

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
