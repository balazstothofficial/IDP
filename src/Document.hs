{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wno-name-shadowing #-}

module Document (Document (..), WordCountMap, Factory (..)) where

import Data.Char (toLower)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Factory
import Interview (Interview (..))
import Text.Read (readMaybe)
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
  create Interview {..} =
    create title $
      filter filtered $
        fmap lowercase $
          splitOn " " $
            filter isBad content
    where
      isBad char = char `notElem` ".,!?\n\r()/:+-\""

      filtered word =
        word /= ""
          && '#' `notElem` word
          && '\NUL' `notElem` word
          && length word >= 3
          && isNothing (readInt word)
          && word `notElem` fillwords
        where
          readInt :: String -> Maybe Int
          readInt = readMaybe

countWords :: [String] -> WordCountMap -> WordCountMap
countWords [] wordCountMap = wordCountMap
countWords (word : words) wordCountMap = case Map.lookup word wordCountMap of
  Just count -> recurse (count + 1)
  Nothing -> recurse 1
  where
    recurse count = countWords words $ Map.insert word count wordCountMap

lowercase :: String -> String
lowercase = fmap toLower

fillwords :: [String]
fillwords =
  [ "ich",
    "ist",
    "sie",
    "das",
    "und",
    "auch",
    "dann",
    "die",
    "nicht",
    "dass",
    "oder",
    "also",
    "der",
    "sich",
    "man",
    "war",
    "noch",
    "jetzt",
    "wie",
    "der",
    "hat",
    "was",
    "dass",
    "war",
    "mit",
    "also",
    "den",
    "wie",
    "meine",
    "jetzt",
    "der",
    "haben",
    "man",
    "oder",
    "dass",
    "hat",
    "eine",
    "ein",
    "ihr",
    "also",
    "dass",
    "oder",
    "also",
    "der",
    "sich",
    "man",
    "war",
    "noch",
    "jetzt",
    "wie",
    "jetzt",
    "der",
    "haben",
    "man",
    "oder",
    "dass",
    "hat",
    "eine",
    "ihr",
    "also",
    "also",
    "dass",
    "aber",
    "eben",
    "oder",
    "hier",
    "der",
    "von",
    "halt",
    "gesagt",
    "schon",
    "als",
    "ein",
    "ganz",
    "weil",
    "wenn",
    "immer",
    "bin",
    "habe",
    "ein",
    "wir",
    "dem",
    "alles",
    "mich",
    "bei",
    "eigentlich",
    "auf",
    "ein",
    "wir",
    "habe",
    "sagen",
    "immer",
    "mal",
    "bin",
    "nur",
    "dem",
    "bei",
    "wenn",
    "mal",
    "ein",
    "immer",
    "mir",
    "sind",
    "ganz",
    "einfach",
    "sehr",
    "gut",
    "ein",
    "wir",
    "habe",
    "sagen",
    "immer",
    "mal",
    "bin",
    "nur",
    "dem",
    "bei",
    "gut",
    "kann",
    "mehr",
    "wenn",
    "mir",
    "nur",
    "geht",
    "mal",
    "gut",
    "kann",
    "mehr",
    "wenn",
    "\228hm",
    "wei\223",
    "aus",
    "genau",
    "zum",
    "gar",
    "einen",
    "f\252r",
    "hab",
    "wird",
    "\228hm",
    "doch",
    "wei\223",
    "einem",
    "diese",
    "sondern",
    "\228hm",
    "einer",
    "sein",
    "f\252r",
    "genau",
    "gibt",
    "f\252r",
    "\228hm",
    "muss",
    "dieses",
    "denn",
    "hatte",
    "wei\223",
    "vielleicht",
    "des",
    "genau",
    "sein",
    "kommt",
    "uns",
    "k\246nnen",
    "hatte",
    "meiner",
    "wohl",
    "sowas",
    "bekommen",
    "worden",
    "wurde",
    "durch",
    "mein",
    "w\228re",
    "machen",
    "mhm",
    "ihm",
    "macht",
    "gegangen",
    "hast",
    "wurde",
    "sage",
    "dazu",
    "mein",
    "w\228re",
    "ihre",
    "merkt",
    "sage",
    "w\252rde",
    "waren",
    "ins",
    "geben",
    "gemacht",
    "h\228tte",
    "diesen",
    "\252ber",
    "k\246nnte",
    "vom",
    "bis",
    "meinen",
    "gekommen",
    "unv",
    "zur",
    "wer",
    "meinen",
    "kam",
    "meinem",
    "damit",
    "sonst",
    "ging",
    "vor",
    "waren",
    "gemacht"
  ]
