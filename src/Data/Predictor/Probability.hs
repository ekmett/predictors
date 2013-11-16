{-# LANGUAGE DeriveFunctor #-}
module Data.Predictor.Probability
  ( Pr(..)
  , binomial
  , collapse
  , (.*)
  , delay
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Map as M
import Numeric.Log as Log

data Pr a = Pr { runPr :: [(Log Double, Either a (Pr a))] }
  deriving Show

delay :: Pr a -> Pr a
delay p = Pr [(1,Right p)]

instance Functor Pr where
  fmap f = Pr . fmap (fmap (bimap f (fmap f))) . runPr

instance Applicative Pr where
  pure = return
  (<*>) = ap

instance Alternative Pr where
  empty = mzero
  (<|>) = mplus

instance Monad Pr where
  return a = Pr [(1,Left a)]
  Pr as >>= f = Pr $
    as >>= \(p,ea) -> case ea of
      Left a ->    [(p*q,bs) | (q, bs) <- runPr (f a)]
      Right as' -> [(p, Right (as' >>= f))]

instance MonadPlus Pr where
  mzero = Pr []
  mplus (Pr xs) (Pr ys) = Pr (xs ++ ys)

linear :: [(Log Double, Pr a)] -> Pr a
linear xs = Prelude.foldr (\(p,va) b -> p .* va <|> b) mzero xs

(.*) :: Log Double -> Pr a -> Pr a
p .* Pr xs = Pr [ (p*q, a) | (q,a) <- xs ]

binomial :: Log Double -> Pr Bool
binomial p = Pr [(p, Left True), (1-p, Left False)]
{-# INLINE binomial #-}

collapse :: Ord a => Pr a -> [(Log Double,a)]
collapse = fmap (first Log.sum . swap) . M.toList . go M.empty . runPr
  where
    go = Prelude.foldr $ \(p,ea) m -> case ea of
      Left a        -> m & at a . non [] %~ cons p
      Right (Pr bs) -> go m bs

-- * Utilities

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
