{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Stream where

import Protolude

class Stream s a | s -> a, a -> s where
  read :: s -> Maybe (a, s)
