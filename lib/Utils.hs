{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Utils where

import Protolude

unsafeFromRight :: Either b a -> a
unsafeFromRight (Right x) = x
unsafeFromRight _ = undefined

letter :: Char -> Bool
letter = flip elem $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']

digit :: Char -> Bool
digit = flip elem ['0' .. '9']

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
