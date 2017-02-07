module Utils where

import Protolude

unsafeFromRight :: Either b a -> a
unsafeFromRight (Right x) = x
unsafeFromRight _ = undefined
