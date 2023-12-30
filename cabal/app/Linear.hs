{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Linear where

import Data.Kind (Type)
import GHC.Exts (MultMul, Multiplicity (One))
import Prelude.Linear
import Prelude ()

id1 :: a %1 -> a
id1 x = x

id2 :: a %1 -> a
id2 x = x

id3 :: a %One -> a
id3 x = x

map2 :: forall (a :: Type) (b :: Type) (m :: Multiplicity). (a %m -> b) -> [a] %m -> [b]
map2 _ [] = []
map2 f (a : as) = f a : map2 f as

type (•) :: Multiplicity -> Multiplicity -> Multiplicity
type p • q = MultMul p q

-- (∘) :: forall a b c p q. (b %p-> c) %1 -> (a %q-> b) %p-> a %(p • q) -> c
-- (∘) g f x = g (f x)

-- (∘) :: (b %p-> c) %1 -> (a %1-> b) %1-> a %p -> c
-- (∘) g f x = g (f x)

(∘) :: (b %1 -> c) %1 -> (a %1 -> b) %1 -> a %1 -> c
(∘) g f x = g (f x)

-- fst' :: (a, b) %1-> a
-- fst' (a, _) = a

fst' :: (a, Ur b) %1 -> a
fst' (a, Ur _) = a

fst'' :: Consumable b => (a, b) %1 -> a
fst'' (a, b) = case consume b of
  () -> a

data T1 a b where
  MkT1 :: a %1 -> b -> T1 a b

f' :: T1 a b %1 -> a
f' (MkT1 a _) = a
