{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Vector.Vector1 where

-----------------------------------

import Data.Finite
import Data.Proxy
import Data.Type.Equality
import qualified Data.Vector as V
import GHC.TypeNats
import Prelude hiding (replicate, (++))

-----------------------------------

newtype Vec (n :: Nat) a = UnsafeMkVec {getVector :: V.Vector a}
  deriving (Show)

mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v
  | V.length v == l = Just (UnsafeMkVec v)
  | otherwise = Nothing
  where
    l = fromIntegral (natVal (Proxy @n))

mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f v = UnsafeMkVec $ V.map f (getVector v)

instance Functor (Vec n) where
  fmap = mapVec

-- However, GHC treats (+) “opaquely” when using using it with polymorphic type variables. That means that n + (m + o) is seen as a completely different type to GHC than (n + m) + o – GHC doesn’t reduce +, and to it, they both just look like different trees. Remember that one is (+) n ((+) m o), and the other is (+) ((+) n m) o. Completely different structure!
--
-- The ghc-typelits-natnormalise package provides such a plugin. If we pass it as a flag to GHC (as -fplugin GHC.TypeLits.NatNormalise) or as a pragma: {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
UnsafeMkVec xs ++ UnsafeMkVec ys = UnsafeMkVec (xs V.++ ys)

zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec (UnsafeMkVec xs) (UnsafeMkVec ys) = UnsafeMkVec (V.zip xs ys)

takeVec :: forall n m a. KnownNat n => Vec (n + m) a -> Vec n a
takeVec (UnsafeMkVec xs) = UnsafeMkVec (V.take l xs)
  where
    l = fromIntegral (natVal (Proxy @n))

splitVec :: forall n m a. KnownNat n => Vec (n + m) a -> (Vec n a, Vec m a)
splitVec (UnsafeMkVec xs) = (UnsafeMkVec ys, UnsafeMkVec zs)
  where
    l = fromIntegral (natVal (Proxy @n))
    (ys, zs) = V.splitAt l xs

-- package `finite-typelits`
--
-- packFinite :: KnownNat n => Integer  -> Maybe (Finite n)
-- getFinite  ::               Finite n -> Integer

index :: Vec n a -> Finite n -> a
index v i = getVector v V.! fromIntegral (getFinite i)

replicate :: forall n a. KnownNat n => a -> Vec n a
replicate x = UnsafeMkVec $ V.replicate l x
  where
    l = fromIntegral (natVal (Proxy @n))

instance KnownNat n => Applicative (Vec n) where
  pure = replicate
  fs <*> xs = (\(f, x) -> f x) <$> zipVec fs xs

generate :: forall n a. KnownNat n => (Finite n -> a) -> Vec n a
generate f = UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (natVal (Proxy @n))

withVec :: V.Vector a -> (forall n. KnownNat n => Vec n a -> r) -> r
withVec v f = case someNatVal (fromIntegral (V.length v)) of
  SomeNat (Proxy :: Proxy m) -> f (UnsafeMkVec @m v)

-- | Gets the third element of a vector
getThird :: V.Vector a -> Maybe a
getThird v = withVec v $ \v' -> fmap (v' `index`) (packFinite 2)

vectorToVector :: V.Vector a -> V.Vector a
vectorToVector v = withVec v getVector

exactLength :: forall n m a. (KnownNat n, KnownNat m) => Vec n a -> Maybe (Vec m a)
exactLength v = case sameNat (Proxy @n) (Proxy @m) of
  Just Refl -> Just v
  Nothing -> Nothing

-- Beautiful
zipSame :: forall a b. V.Vector a -> V.Vector b -> Maybe (V.Vector (a, b))
zipSame v1 v2 = withVec v1 $ \(v1' :: Vec n a) ->
  withVec v2 $ \(v2' :: Vec m b) ->
    case exactLength v1' of
      Just v1Same ->
        Just $
          getVector
            (zipVec v1Same v2')
      Nothing -> Nothing
