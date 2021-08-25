{-# LANGUAGE MultiParamTypeClasses #-}

module Factory
  ( Factory (..),
  )
where

class Factory a b where
  create :: a -> b
