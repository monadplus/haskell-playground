{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vector.Vector2 where

import Data.Finite
import Data.Singletons
import Data.Singletons.Decide
import qualified Data.Vector as V
import GHC.TypeLits.Singletons ()
import GHC.TypeNats

newtype Vec (n :: Nat) a = UnsafeMkVec {getVector :: V.Vector a}
  deriving (Show)

-- Explicit
mkVec_ :: Sing n -> V.Vector a -> Maybe (Vec n a)
mkVec_ s v
  | V.length v == l = Just (UnsafeMkVec v)
  | otherwise = Nothing
  where
    l = fromIntegral (fromSing s)

-- Implicit
mkVec :: forall n a. (KnownNat n) => V.Vector a -> Maybe (Vec n a)
mkVec v
  | V.length v == l = Just (UnsafeMkVec v)
  | otherwise = Nothing
  where
    l = fromIntegral (fromSing (sing :: Sing n))

replicate_ :: Sing n -> a -> Vec n a
replicate_ s x = UnsafeMkVec $ V.replicate l x
  where
    l = fromIntegral (fromSing s)

replicate :: (KnownNat n) => a -> Vec n a
replicate = replicate_ sing

withVec :: V.Vector a -> (forall n. Sing n -> Vec n a -> r) -> r
withVec v f = case toSing (fromIntegral (V.length v)) of
  SomeSing s -> f s (UnsafeMkVec v)

withVec' :: V.Vector a -> (forall n. Sing n -> Vec n a -> r) -> r
withVec' v0 f = withSomeSing (fromIntegral (V.length v0)) $ \s ->
  f s (UnsafeMkVec v0)

exactLength_ :: Sing m -> Sing n -> Vec n a -> Maybe (Vec m a)
exactLength_ sM sN v = case sM %~ sN of
  Proved Refl -> Just v
  Disproved _ -> Nothing

exactLength :: (KnownNat m, KnownNat n) => Vec n a -> Maybe (Vec m a)
exactLength = exactLength_ sing sing

-- One slight bit of friction comes when using libraries that work with KnownNat, like finite-typelits and the Finite type. But we can convert between the two using SNat or withKnownNat:
--
-- SNat :: KnownNat n => Sing n
-- withKnownNat :: Sing n -> (KnownNat n => r) -> r

generate_ :: Sing n -> (Finite n -> a) -> Vec n a
generate_ s f =
  withKnownNat s $
    UnsafeMkVec $
      V.generate l (f . fromIntegral)
  where
    l = fromIntegral (fromSing s)

-- alternatively, via pattern matching:
-- FIXME:
generate'_ :: Sing n -> (Finite n -> a) -> Vec n a
generate'_ s@SNat f = UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (fromSing s)

generate :: (KnownNat n) => (Finite n -> a) -> Vec n a
generate = generate_ sing
