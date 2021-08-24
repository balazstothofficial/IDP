{-# LANGUAGE MultiParamTypeClasses #-}

module Factory
  ( Factory (..),
  )
where

newtype Factory a b = Factory {create :: a -> b}
