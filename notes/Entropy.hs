{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

import Control.Applicative
import Control.Arrow
import Control.Monad as M
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

-- | Shannon entropy in @nats@
entropy :: Foldable f => f Double -> Double
entropy = negate . foldr (\x r -> xlog x + r) 0
{-# INLINE entropy #-}

xlog :: Double -> Double
xlog x
  | x <= 1e-323 = 0 -- below this @'xlog' x = NaN@
  | otherwise   = x * log x
{-# INLINE xlog #-}

-- TODO: add vagueness sources

-- | Maximum likelihood predictor or density estimator
mlp :: (Foldable f, Ord a) => (e -> a) -> f e -> Map a Double
mlp f s = case foldr go (0 :: Int,M.empty) s of
  (n, m) -> fmap (\k -> fromIntegral k / fromIntegral n) m
 where
  go e (n,m) = (n + 1, m & at (f e) . non (0 :: Int) +~ 1)

-- | Entropy @H(a)@
h :: (Foldable f, Ord a) => (e -> a) -> f e -> Double
h f = entropy . mlp f

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

divergence :: Foldable f => (e -> Double) -> (e -> Double) -> f e -> Double
divergence p q = foldr (\x r -> p x * (log (p x) - log (q x)) + r) 0

-- | vague kl-divergence comparing between the maximum likelihood predictor and a bootstrapped
-- maximum entropy model
vague :: (Foldable f, MonadRandom m, Ord a, Ord b) => (e -> a) -> (e -> b) -> f e -> m Double
vague f g s = do
  let pa  = mlp f s
      pb  = mlp g s
  s' <- bootstrap s
  let qab = mlp (f &&& g) s'
      stepq r (a,b) q = r + q * (log q - log (pa M.! a) - log (pb M.! b))
  return $ M.foldlWithKey' stepq 0 qab

-- | vague and "honest" kl-divergence comparing between the maximum likelihood predictor and a bootstrapped
-- maximum entropy model. Here we account for self-loss.
-- honest a b = D(P a b||P a * P b) - D(P a b || Q a b) + D(P a * P b||Q a * Q b)
honest :: (Foldable f, MonadRandom m, Ord a, Ord b) => (e -> a) -> (e -> b) -> f e -> m Double
honest f g s = do
  let pa  = mlp f s
      pb  = mlp g s
      pab = mlp (f &&& g) s
  s' <- bootstrap s
  let qab = mlp (f &&& g) s'
      qa  = mlp f s'
      qb  = mlp g s'
      stepp r (a,b)    p = r + p * (log p - log (pa M.! a) - log (pb M.! b))
      stepq r ab@(a,b) q = r + q * (log q - log (pab M.! ab)
                                  + log (pa M.! a) - log (qa M.! a)
                                  + log (pb M.! b) - log (qb M.! b))
      -- stepq ab    q r = q * (log q - log (pab M.! ab)) + r
  return $ M.foldlWithKey' stepp 0 pab - M.foldlWithKey' stepq 0 qab

bootstrap :: (Foldable f, MonadRandom m) => f a -> m (Vector a)
bootstrap as = liftM (backpermute vs) $ V.replicateM n $ getRandomR (0 :: Int,n-1)
  where
    vs = V.fromList (F.toList as)
    n = V.length vs
