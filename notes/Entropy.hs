{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

import Control.Applicative
import Control.Arrow
import Control.Monad.Random
import Control.Lens
import Data.Foldable as F
import Data.Map as M hiding (foldr)
import Data.Vector as V hiding (foldr)
import Prelude hiding (foldr, sum)

instance Num a => Num (b -> a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  fromInteger = pure . fromInteger
  abs = fmap abs
  signum = fmap signum

-- | Shannon entropy in @logBase 2@
entropy :: Foldable f => f Double -> Double
entropy = negate . foldr (\x r -> xlog x + r) 0
{-# INLINE entropy #-}

xlog :: Double -> Double
xlog x
  | x <= 1e-323 = 0 -- below this @'xlog' x = NaN@
  | otherwise   = x * logBase 2 x
{-# INLINE xlog #-}

type Predictor a = [a] -> Map a Double

-- | Maximum likelihood predictor or density estimator
mlp :: Ord a => Predictor a
mlp s = case foldr go (0 :: Int,M.empty) s of
  (n, m) -> fmap (\k -> fromIntegral k / fromIntegral n) m
 where
  go a (n,m) = (n + 1, m & at a . non (0 :: Int) +~ 1)

-- | Entropy @H(a)@
h :: (Foldable f, Ord a) => (e -> a) -> f e -> Double
h f = entropy . mlp . fmap f . F.toList

-- | Conditional entropy
--
-- @
-- H(a|b) = H(a,b) - H(b)
-- @
--
-- @
-- cond a b = h (a &&& b) - h b
-- @
cond :: (Foldable f, Ord a, Ord b) => (e -> a) -> (e -> b) -> f e -> Double
cond a b = h (a &&& b) - h b

-- | Mutual information
-- @
-- I(a;b) = H(a) - H(a|b)
-- I(a;b) = H(b) - H(b|a)
-- I(a;b) = H(a) + H(b) - H(a,b)
-- I(a;b) = D(P(a,b) || P(a)P(b)) -- KL divergence
-- I(a;b) = D(P(a|b) || P(a))
-- @
--
-- vague(a;b) = D(P(a,b) || Q(a)Q(b))
-- self-loss(a;b) = D(P(a|b) || Q(a|b))
-- honest(a;b) = D(P(a,b) || Q(a)Q(b)) - D(P(a,b) || Q(a,b))
-- vague-honest-and-predictive(a;b) = D(P(b|a) || Q(b)) - D(P(b|a) || Q(b|a))
--
-- where Q is drawn from the bootstrap
--
-- @
-- mutual a b = h a - cond a b
-- @
mutual :: (Foldable f, Ord a, Ord b) => (e -> a) -> (e -> b) -> f e -> Double
mutual a b = h a - cond a b

bootstrap :: (Foldable f, MonadRandom m) => f a -> m (Vector a)
bootstrap as = fmap (backpermute vs) $ replicateM n $ getRandomR (0 :: Int,n-1)
  where
    vs = V.fromList (F.toList as)
    n = V.length vs

-- | 1 pass
{-
cond f g s = case F.foldr go (0 :: Int,M.empty) s of
  (n,m) -> F.sum $ fmap (step n) m
 where
  go ab (n,m) = (n + 1, m & at (f ab) . non (0 :: Int, M.empty) %~ \(n',m') -> (n' + 1, m' & at (g ab).non (0 :: Int) +~ 1))
  step n (k,m') = entropy (fmap (\j -> fromIntegral j / fromIntegral k) m') * fromIntegral k / fromIntegral n
-}
