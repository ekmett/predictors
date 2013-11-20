module Data.Predictor.Softmax
  ( Softmax(..)
  , softmax
  ) where

import Data.Monoid
import Data.Foldable

data Softmax = Softmax {-# UNPACK #-} !Double {-# UNPACK #-} !Double

instance Monoid Softmax where
  mempty = Softmax 0 0
  mappend (Softmax p q) (Softmax p' q') = Softmax (p + p') (q + q')

soft :: Double -> Double -> Softmax
soft a x = Softmax (x*eax) eax where
  eax = exp (a*x)

runSoft :: Softmax -> Double
runSoft (Softmax a b) = a / b

softmax :: Foldable f => (e -> Double) -> Double -> f e -> Double
softmax f a = runSoft . foldMap (soft a . f)
