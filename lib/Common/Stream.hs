{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Stream where

import Protolude

class Stream s a | s -> a, a -> s where
  read :: s -> Maybe (a, s)

instance Stream [a] a where
  read (x:xs) = Just (x, xs)
  read [] = Nothing
