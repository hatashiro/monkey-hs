module Utils where

import Protolude

unsafeFromRight :: Either b a -> a
unsafeFromRight (Right x) = x
unsafeFromRight _ = undefined

isLetter :: Char -> Bool
isLetter = flip elem $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']

isDigit :: Char -> Bool
isDigit = flip elem ['0' .. '9']

(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) = liftM2 (||)

(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)

(<<) :: Monad m => m a -> m b -> m a
ma << mb = ma >>= \a -> mb $> a
