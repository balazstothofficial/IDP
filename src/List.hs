module List where

-- TODO: Naming  
mapToStructure :: [a] -> [[b]] -> [[a]]
mapToStructure _ [] = []
mapToStructure as (b : bs) = firstAs : mapToStructure restAs bs
  where
    firstAs = take lengthOfB as
    restAs = drop lengthOfB as
    lengthOfB = length b
