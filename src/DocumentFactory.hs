{-# LANGUAGE RecordWildCards #-}

module DocumentFactory
  ( Input (..),
    documentFactory,
    interviewBasedDocumentFactory,
    create
  )
where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Document
import Factory
import InterviewReader (Interview (..))
import Prelude hiding (words)

data Input = Input
  { title :: String,
    words :: [String]
  }

documentFactory :: Factory Input Document
documentFactory = Factory {create = create}
  where
    create Input {..} =
      Document
        { title = title,
          words = words,
          wordCounts = countWords words Map.empty
        }

interviewBasedDocumentFactory :: Factory Interview Document
interviewBasedDocumentFactory = Factory {create = createWithInterview}
  where
    createWithInterview Interview {..} =
      create
        documentFactory
        Input
          { title = title,
            words = splitOn " " (filter isBad content)
          }
      where
        isBad char = char `notElem` ".,!?\n()/:"

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

countWords :: [String] -> WordCountMap -> WordCountMap
countWords [] wordCountMap = wordCountMap
countWords (word : words) wordCountMap = case Map.lookup word wordCountMap of
  Just count -> recurse (count + 1)
  Nothing -> recurse 1
  where
    recurse count = countWords words $ Map.insert word count wordCountMap
