{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Linear where

import qualified Data.Array.Mutable.Linear as Array
import Data.Kind (Type)
import GHC.Exts (MultMul, Multiplicity (..))
import Prelude.Linear
import Prelude ()

map2 :: forall (a :: Type) (b :: Type) (m :: Multiplicity). (a %m -> b) -> [a] %m -> [b]
map2 _ [] = []
map2 f (a : as) = f a : map2 f as

type (•) :: Multiplicity -> Multiplicity -> Multiplicity
type p • q = MultMul p q

--
-- (∘) :: forall a b c p q. (b %p-> c) %1 -> (a %q-> b) %p-> a %(p • q) -> c
-- (∘) g f x = g (f x)
--
-- (∘) :: (b %p-> c) %1 -> (a %1-> b) %1-> a %p -> c
-- (∘) g f x = g (f x)
(∘) :: (b %1 -> c) %1 -> (a %1 -> b) %1 -> a %1 -> c
(∘) g f x = g (f x)

-- Write linear-socket
