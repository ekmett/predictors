{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
module Data.Predictor
  ( Occ(..)
  ) where

import GHC.Prim (Constraint)
import Data.Map as Map
import Data.Semigroup hiding (All)

type family All (p :: a -> Constraint) (as :: [a]) :: Constraint
type instance All p '[] = ()
type instance All p (a ': as) = (p a, All p as)

data Example :: [*] -> * where
  Done   :: Example '[]
  Has    :: a -> Example as -> Example (a ': as)
  Hasn't :: Example as -> Example (a ': as)

instance All Show as => Show (Example as) where
  showsPrec d Done = showString "Done"
  showsPrec d (Has a as) = showParen (d > 10) $
    showString "Has " . showsPrec 11 a . showChar ' ' . showsPrec 11 as
  showsPrec d (Hasn't as) = showParen (d > 10) $
    showString "Hasn't " . showsPrec 11 as

class Occurs t where
  occ :: t as -> Occ as

instance Occurs Example where
  occ Done = Nil 1
  occ (Has a as) = Occ 1 $ Map.singleton (Just a) (occ as)
  occ (Hasn't as) = Occ 1 $ Map.singleton Nothing (occ as)

data Occ :: [*] -> * where
  Nil :: Int -> Occ '[]
  Occ :: Int -> Map (Maybe a) (Occ as) -> Occ (a ': as)

instance Occurs Occ where
  occ = id

instance Monoid (Occ as) => Semigroup (Occ as) where
  (<>) = mappend

instance Monoid (Occ '[]) where
  mempty = Nil 0
  mappend (Nil n) (Nil m) = Nil (n + m)

instance (Ord a, Monoid (Occ as)) => Monoid (Occ (a ': as)) where
  mempty = Occ 0 Map.empty
  mappend (Occ n as) (Occ m bs) = Occ (n + m) (Map.unionWith mappend as bs)

instance All Show as => Show (Occ as) where
  showsPrec d (Nil n) = showParen (d > 10) $
    showString "Nil " . showsPrec 11 n
  showsPrec d (Occ n as) = showParen (d > 10) $
    showString "Occ " . showsPrec 11 n . showString " $ " . showsPrec 1 as

bob, nobody :: Example [String, Int]
bob = Has "Bob" (Has 60 Done)
nobody = Hasn't (Has 73 Done)
