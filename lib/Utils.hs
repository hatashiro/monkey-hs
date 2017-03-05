{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
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

type (~>) f1 f2 = forall a. f1 a -> f2 a

returnOrThrow :: (MonadError e m) => e -> Maybe ~> m
returnOrThrow e (Just a) = return a
returnOrThrow e Nothing = throwError e

at :: Integral n => [a] -> n -> Maybe a
at [] _ = Nothing
at (x:_) 0 = Just x
at (_:xs) n = at xs (n-1)
