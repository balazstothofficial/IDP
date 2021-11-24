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
      isBad char = char `notElem` ".,!?\n\r()/:+-\"][><'"

      filtered word =
        word /= ""
          && '#' `notElem` word
          && '\NUL' `notElem` word
          && length word >= 3
          && isNothing (readInt word)
          && word `notElem` stopWords
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

-- Own + https://countwordsfree.com/stopwords/german
-- TODO: Move to own file
-- TODO: Fix encoding issues!
stopWords :: [String]
stopWords =
  [ "ich",
    "ist",
    "\244\231\170",
    "w\252rd",
    "klar",
    "leu",
    "\244\231\215ja",
    "sie",
    "das",
    "und",
    "auch",
    "find",
    "dann",
    "finde",
    "eher",
    "ner",
    "nen",
    "nem",
    "\244\231\244",
    "gr\228fe",
    "glaub",
    "s\252\223",
    "gibt\244\231\255s",
    "gibt\244\231\246s",
    "w\228r",
    "grade",
    "w\252rd",
    "\244\231\215ich",
    "\244\231\215wie",
    "n\9516\9508",
    "sag",
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
    "k\226nnen",
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
    "gemacht",
    "a",
    "ab",
    "aber",
    "ach",
    "acht",
    "achte",
    "achten",
    "achter",
    "achtes",
    "ag",
    "alle",
    "allein",
    "allem",
    "allen",
    "aller",
    "allerdings",
    "alles",
    "allgemeinen",
    "als",
    "also",
    "am",
    "an",
    "andere",
    "anderen",
    "andern",
    "anders",
    "au",
    "auch",
    "auf",
    "aus",
    "ausser",
    "außer",
    "ausserdem",
    "außerdem",
    "b",
    "bald",
    "bei",
    "beide",
    "beiden",
    "beim",
    "beispiel",
    "bekannt",
    "bereits",
    "besonders",
    "besser",
    "besten",
    "bin",
    "bis",
    "bisschen",
    "irgendwie",
    "bisher",
    "bist",
    "c",
    "d",
    "da",
    "dabei",
    "dadurch",
    "dafür",
    "dagegen",
    "daher",
    "dahin",
    "dahinter",
    "damals",
    "damit",
    "danach",
    "daneben",
    "dank",
    "dann",
    "daran",
    "darauf",
    "daraus",
    "darf",
    "darfst",
    "darin",
    "darüber",
    "darum",
    "darunter",
    "das",
    "dasein",
    "daselbst",
    "dass",
    "daß",
    "dasselbe",
    "davon",
    "davor",
    "dazu",
    "dazwischen",
    "dein",
    "deine",
    "deinem",
    "deiner",
    "dem",
    "dementsprechend",
    "demgegenüber",
    "demgemäss",
    "demgemäß",
    "demselben",
    "demzufolge",
    "den",
    "denen",
    "denn",
    "denselben",
    "der",
    "deren",
    "derjenige",
    "derjenigen",
    "dermassen",
    "dermaßen",
    "derselbe",
    "derselben",
    "des",
    "deshalb",
    "desselben",
    "dessen",
    "deswegen",
    "d.h",
    "dich",
    "die",
    "diejenige",
    "diejenigen",
    "dies",
    "diese",
    "dieselbe",
    "dieselben",
    "diesem",
    "diesen",
    "dieser",
    "dieses",
    "dir",
    "doch",
    "dort",
    "drei",
    "drin",
    "dritte",
    "dritten",
    "dritter",
    "drittes",
    "du",
    "durch",
    "durchaus",
    "dürfen",
    "dürft",
    "durfte",
    "durften",
    "e",
    "eben",
    "ebenso",
    "ehrlich",
    "ei",
    "ei,",
    "eigen",
    "eigene",
    "eigenen",
    "eigener",
    "eigenes",
    "ein",
    "einander",
    "eine",
    "einem",
    "einen",
    "einer",
    "eines",
    "einige",
    "einigen",
    "einiger",
    "einiges",
    "einmal",
    "eins",
    "elf",
    "en",
    "ende",
    "endlich",
    "entweder",
    "er",
    "ernst",
    "erst",
    "erste",
    "ersten",
    "erster",
    "erstes",
    "es",
    "etwa",
    "etwas",
    "euch",
    "f",
    "früher",
    "fünf",
    "fünfte",
    "fünften",
    "fünfter",
    "fünftes",
    "für",
    "g",
    "gab",
    "ganz",
    "ganze",
    "ganzen",
    "ganzer",
    "ganzes",
    "gar",
    "gedurft",
    "gegen",
    "gegenüber",
    "gehabt",
    "gehen",
    "geht",
    "gekannt",
    "gekonnt",
    "gemacht",
    "gemocht",
    "gemusst",
    "genug",
    "gerade",
    "gern",
    "gesagt",
    "geschweige",
    "gewesen",
    "gewollt",
    "geworden",
    "gibt",
    "ging",
    "gleich",
    "gott",
    "gross",
    "groß",
    "grosse",
    "große",
    "grossen",
    "großen",
    "grosser",
    "großer",
    "grosses",
    "großes",
    "gut",
    "gute",
    "guter",
    "gutes",
    "h",
    "habe",
    "haben",
    "habt",
    "hast",
    "hat",
    "hatte",
    "hätte",
    "hatten",
    "hätten",
    "heisst",
    "her",
    "heute",
    "hier",
    "hin",
    "hinter",
    "hoch",
    "i",
    "ich",
    "ihm",
    "ihn",
    "ihnen",
    "ihr",
    "ihre",
    "ihrem",
    "ihren",
    "ihrer",
    "ihres",
    "im",
    "immer",
    "in",
    "indem",
    "infolgedessen",
    "ins",
    "irgend",
    "ist",
    "j",
    "ja",
    "jahr",
    "jahre",
    "jahren",
    "je",
    "jede",
    "jedem",
    "jeden",
    "jeder",
    "jedermann",
    "jedermanns",
    "jedoch",
    "jemand",
    "jemandem",
    "jemanden",
    "jene",
    "jenem",
    "jenen",
    "jener",
    "jenes",
    "jetzt",
    "k",
    "kam",
    "kann",
    "kannst",
    "kaum",
    "kein",
    "keine",
    "keinem",
    "keinen",
    "keiner",
    "kleine",
    "kleinen",
    "kleiner",
    "kleines",
    "kommen",
    "kommt",
    "können",
    "könnt",
    "konnte",
    "könnte",
    "konnten",
    "kurz",
    "l",
    "lang",
    "lange",
    "leicht",
    "leide",
    "lieber",
    "los",
    "m",
    "machen",
    "macht",
    "machte",
    "mag",
    "magst",
    "mahn",
    "man",
    "manche",
    "manchem",
    "manchen",
    "mancher",
    "manches",
    "mann",
    "mehr",
    "mein",
    "meine",
    "meinem",
    "meinen",
    "meiner",
    "meines",
    "mensch",
    "menschen",
    "mich",
    "mir",
    "mit",
    "mittel",
    "mochte",
    "möchte",
    "mochten",
    "mögen",
    "möglich",
    "mögt",
    "morgen",
    "muss",
    "muß",
    "müssen",
    "musst",
    "müsst",
    "musste",
    "mussten",
    "n",
    "na",
    "nach",
    "nachdem",
    "nahm",
    "natürlich",
    "nat\9500\9565rlich",
    "neben",
    "nein",
    "neue",
    "neuen",
    "neun",
    "neunte",
    "neunten",
    "neunter",
    "neuntes",
    "nicht",
    "nichts",
    "nie",
    "niemand",
    "niemandem",
    "niemanden",
    "noch",
    "nun",
    "nur",
    "o",
    "ob",
    "oben",
    "oder",
    "offen",
    "oft",
    "ohne",
    "ordnung",
    "p",
    "q",
    "r",
    "recht",
    "rechte",
    "rechten",
    "rechter",
    "rechtes",
    "richtig",
    "rund",
    "s",
    "sa",
    "sache",
    "sagt",
    "sagte",
    "sah",
    "satt",
    "schlecht",
    "Schluss",
    "schon",
    "sechs",
    "sechste",
    "sechsten",
    "sechster",
    "sechstes",
    "sehr",
    "sei",
    "seid",
    "seien",
    "sein",
    "seine",
    "seinem",
    "seinen",
    "seiner",
    "seines",
    "seit",
    "seitdem",
    "selbst",
    "sich",
    "sie",
    "sieben",
    "siebente",
    "siebenten",
    "siebenter",
    "siebentes",
    "sind",
    "so",
    "solang",
    "solche",
    "solchem",
    "solchen",
    "solcher",
    "solches",
    "soll",
    "sollen",
    "sollte",
    "sollten",
    "sondern",
    "sonst",
    "sowie",
    "später",
    "statt",
    "t",
    "tag",
    "tage",
    "tagen",
    "tat",
    "teil",
    "tel",
    "tritt",
    "trotzdem",
    "tun",
    "u",
    "über",
    "überhaupt",
    "übrigens",
    "uhr",
    "um",
    "und",
    "und?",
    "uns",
    "unser",
    "unsere",
    "unserer",
    "unter",
    "v",
    "vergangenen",
    "viel",
    "viele",
    "vielem",
    "vielen",
    "vielleicht",
    "vier",
    "vierte",
    "vierten",
    "vierter",
    "viertes",
    "vom",
    "von",
    "vor",
    "w",
    "wahr?",
    "während",
    "währenddem",
    "währenddessen",
    "wann",
    "war",
    "wäre",
    "waren",
    "wart",
    "warum",
    "was",
    "wegen",
    "weil",
    "weit",
    "weiter",
    "weitere",
    "weiteren",
    "weiteres",
    "welche",
    "welchem",
    "welchen",
    "welcher",
    "welches",
    "wem",
    "wen",
    "wenig",
    "wenige",
    "weniger",
    "weniges",
    "wenigstens",
    "wenn",
    "wer",
    "werde",
    "werden",
    "werdet",
    "wessen",
    "wie",
    "wieder",
    "will",
    "willst",
    "wir",
    "wird",
    "wirklich",
    "wirst",
    "wo",
    "wohl",
    "wollen",
    "wollt",
    "wollte",
    "wollten",
    "worden",
    "wurde",
    "würde",
    "wurden",
    "würden",
    "x",
    "y",
    "z",
    "z.b",
    "zehn",
    "zehnte",
    "zehnten",
    "zehnter",
    "zehntes",
    "zeit",
    "zu",
    "zuerst",
    "zugleich",
    "zum",
    "zunächst",
    "zur",
    "zurück",
    "zusammen",
    "zwanzig",
    "zwar",
    "zwei",
    "zweite",
    "zweiten",
    "zweiter",
    "zweites",
    "zwischen",
    "zwölf",
    "euer",
    "eure",
    "hattest",
    "hattet",
    "jedes",
    "mußt",
    "müßt",
    "sollst",
    "sollt",
    "soweit",
    "weshalb",
    "wieso",
    "woher",
    "wohin"
  ]
