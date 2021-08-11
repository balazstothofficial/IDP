{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module DocumentFactory
  ( Input (..),
    create,
  )
where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Document
import InterviewReader (Interview (..))
import Prelude hiding (words)

class DocumentFactory a where
  create :: a -> Document

data Input = Input
  { title :: String,
    words :: [String]
  }

instance DocumentFactory [String] where
  create words = create (Input "" words)

instance DocumentFactory Input where
  create Input {..} =
    Document
      { title = title,
        words = words,
        wordCounts = countWords words Map.empty
      }

instance DocumentFactory Interview where
  create Interview {..} =
    create
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
