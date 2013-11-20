{-# LANGUAGE RankNTypes #-}
module Data.Predictor.Entropy
  ( entropy
  , entropyOf
  ) where

import Control.Lens
import Data.Foldable as F

-- | Shannon entropy in logBase 2
entropy :: Foldable f => f Double -> Double
entropy xs = negate (F.foldr (\x r -> xlog x + r) 0 xs)
{-# INLINE entropy #-}

entropyOf :: Fold s Double -> s -> Double
entropyOf l xs = negate (foldrOf l (\x r -> xlog x + r) 0 xs)
{-# INLINE entropyOf #-}

xlog :: Double -> Double
xlog x
  | x <= 1e-323 = 0 -- below this @'xlog' x = NaN@
  | otherwise   = x * logBase 2 x
{-# INLINE xlog #-}
