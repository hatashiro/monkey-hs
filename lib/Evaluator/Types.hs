{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Evaluator.Types where

import Protolude
import Utils (type (~>))

import Control.Monad.Trans.Class (MonadTrans(..))

newtype EvalError = EvalError Text
                    deriving (Show, Eq, Typeable)

instance Exception EvalError

newtype EvalState = EvalState () -- FIXME

emptyState :: EvalState
emptyState = EvalState () -- FIXME

newtype EvaluatorT m a = EvaluatorT
  { runEvaluatorT :: StateT EvalState (ExceptT EvalError m) a }

instance Functor m => Functor (EvaluatorT m) where
  fmap f (EvaluatorT e) = EvaluatorT $ fmap f e

instance Monad m => Applicative (EvaluatorT m) where
  pure = EvaluatorT . pure
  EvaluatorT mf <*> EvaluatorT ma = EvaluatorT $ mf <*> ma

instance Monad m => Monad (EvaluatorT m) where
  EvaluatorT ma >>= f = EvaluatorT $ ma >>= runEvaluatorT . f

instance Monad m => MonadState EvalState (EvaluatorT m) where
  get = EvaluatorT get
  put = EvaluatorT . put

instance Monad m => MonadError EvalError (EvaluatorT m) where
  throwError = EvaluatorT . throwError
  EvaluatorT e `catchError` f = EvaluatorT $ e `catchError` (runEvaluatorT . f)

instance MonadTrans EvaluatorT where
  lift = EvaluatorT . lift . lift

type Evaluator = EvaluatorT Identity

execEvaluatorT :: Monad m => EvaluatorT m a -> m (Either EvalError a)
execEvaluatorT =
  fmap (second fst) . runExceptT . flip runStateT emptyState . runEvaluatorT

execEvaluator :: Evaluator ~> Either EvalError
execEvaluator = runIdentity . execEvaluatorT
