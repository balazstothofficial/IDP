module Interview where

data Interview = Interview
  { title :: String,
    group :: String,
    content :: String
  }
  deriving (Eq)

-- Just for debugging purposes
instance Show Interview where
  show interview = "Interview: " ++ title interview
