{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wno-name-shadowing #-}

module Document (Document (..), WordCountMap, Factory(..)) where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Factory
import Interview (Interview (..))
import Prelude hiding (words)

data Document = Document
  { title :: String,
    words :: [String],
    wordCounts :: WordCountMap
  }
  deriving (Show)

type WordCountMap = Map String Int

-- TODO: Use secure id
instance Eq Document where
  (==) first second = Document.title first == Document.title second

instance Ord Document where
  (<=) first second = Document.title first <= Document.title second

instance Factory String ([String] -> Document) where
  create title words =
    Document
      { title = title,
        words = words,
        wordCounts = countWords words Map.empty
      }

instance Factory Interview Document where
  create Interview {..} = create title (splitOn " " (filter isBad content))
    where
      isBad char = char `notElem` ".,!?\n()/:"

countWords :: [String] -> WordCountMap -> WordCountMap
countWords [] wordCountMap = wordCountMap
countWords (word : words) wordCountMap = case Map.lookup word wordCountMap of
  Just count -> recurse (count + 1)
  Nothing -> recurse 1
  where
    recurse count = countWords words $ Map.insert word count wordCountMap

-- TODO: Filter out not needed words
{-
createDocumentFrom :: Interview -> Document
createDocumentFrom interview = createDocument2 (filter filtered contents) (title interview)
  where
    filtered word =
      word /= "" && '#' `notElem` word && '\r' `notElem` word && isNothing (readInt word)
        && '\246' `notElem` word
        && '\252' `notElem` word
        && word /= "I1"
        && word /= "Also"
        && word /= "Aufnahme"
        && word /= "Beginn"
        && word /= "Dann"
        && word /= "Elena"
        && word /= "Bruder"
        && word /= "Felder"
        && word /= "Erika"
      where
        readInt :: String -> Maybe Int
        readInt = readMaybe

    contents = splitOn " " (filter isBad (drop 15 (take 300 (content interview))))
    isBad char = char `notElem` ".,!?\n()/:"
-}
