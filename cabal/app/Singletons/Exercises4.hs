{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Singletons.Exercises4 where

-------------------------------------------------------------------

import Data.Kind
import Data.Ord.Singletons
import Data.Singletons
import Data.Singletons.Sigma
import Data.Singletons.TH
import GHC.TypeLits.Singletons
import Prelude.Singletons hiding (And, Foldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, Or, sFoldr)

-------------------------------------------------------------------

$( singletons
     [d|
       data DoorState = Opened | Closed | Locked
         deriving (Show, Eq, Ord)
       |]
 )

data Door :: DoorState -> Type where
  UnsafeMkDoor :: {doorMaterial :: String} -> Door s

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

$( singletons
     [d|
       mergeState :: DoorState -> DoorState -> DoorState
       mergeState = max
       |]
 )

mergeDoor :: Door s -> Door t -> Door (MergeState s t)
mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e

type SomeDoor = Sigma DoorState (TyCon1 Door)

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing ->
  dsSing :&: mkDoor dsSing mat

mergeSomeDoor :: SomeDoor -> SomeDoor -> SomeDoor
mergeSomeDoor (s :&: d) (t :&: e) =
  sMergeState s t :&: mergeDoor d e

data Hallway :: [DoorState] -> Type where
  HEnd :: Hallway '[]
  (:<#) :: Door s -> Hallway ss -> Hallway (s ': ss)

infixr 5 :<#

$( singletons
     [d|
       mergeStateList :: [DoorState] -> DoorState
       mergeStateList [] = Opened
       mergeStateList (s : ss) = s `mergeState` mergeStateList ss
       |]
 )

collapseHallway :: Hallway ss -> Door (MergeStateList ss)
collapseHallway HEnd = mkDoor SOpened "End of Hallway"
collapseHallway (d :<# ds) = d `mergeDoor` collapseHallway ds

type SomeHallway = Sigma [DoorState] (TyCon1 Hallway)

collapseSomeHallway :: SomeHallway -> SomeDoor
collapseSomeHallway (ss :&: d) = sMergeStateList ss :&: collapseHallway d

$( singletons
     [d|
       foldr :: (a -> b -> b) -> b -> [a] -> b
       foldr _ z [] = z
       foldr f z (x : xs) = f x (foldr f z xs)
       |]
 )

$( singletons
     [d|
       fold :: Monoid b => [b] -> b
       fold [] = mempty
       fold (x : xs) = x <> fold xs

       instance Semigroup DoorState where
         (<>) = mergeState

       instance Monoid DoorState where
         mempty = Opened
         mappend = (<>)
       |]
 )

collapseHallway' :: Hallway ss -> Door (Fold ss)
collapseHallway' HEnd = UnsafeMkDoor "End of Hallway"
collapseHallway' (d :<# ds) = d `mergeDoor` collapseHallway' ds

collapseSomeHallway' :: SomeHallway -> SomeDoor
collapseSomeHallway' (ss :&: d) = sFold ss :&: collapseHallway' d

collapseHallway'' :: Hallway ss -> Door (FoldrSym2 MergeStateSym0 'Opened @@ ss)
collapseHallway'' HEnd = UnsafeMkDoor "End of Hallway"
collapseHallway'' (d :<# ds) = d `mergeDoor` collapseHallway'' ds

collapseSomeHallway'' :: SomeHallway -> SomeDoor
collapseSomeHallway'' (ss :&: d) =
  -- sFoldr (sing @MergeStateSym0) SOpened ss :&: collapseHallway'' d
  sFoldr (singFun2 @MergeStateSym0 sMergeState) SOpened ss :&: collapseHallway'' d

-- Exercises

-- | Supplementary definitions
data Knockable :: DoorState -> Type where
  KnockClosed :: Knockable 'Closed
  KnockLocked :: Knockable 'Locked

$( singletons
     [d|
       data Pass = Obstruct | Allow
         deriving (Show, Eq, Ord)

       statePass :: DoorState -> Pass
       statePass Opened = Allow
       statePass Closed = Obstruct
       statePass Locked = Obstruct
       |]
 )

-- | 1. Implement mergedIsKnockable
mergedIsKnockable ::
  Knockable s ->
  Knockable t ->
  Knockable (MergeState s t)
mergedIsKnockable = \case
  KnockClosed -> \case
    KnockClosed -> KnockClosed
    KnockLocked -> KnockLocked
  KnockLocked -> \case
    KnockClosed -> KnockLocked
    KnockLocked -> KnockLocked

-- | 2. Implement appendHallways
--
-- Remember the important principle that your type family must mirror the implementation of the functions that use it.
appendHallways :: Hallway ss -> Hallway ts -> Hallway (ss ++ ts)
appendHallways HEnd hw2 = hw2
appendHallways (d :<# hw1) hw2 =
  d :<# (hw1 `appendHallways` hw2)

appendSomeHallways :: SomeHallway -> SomeHallway -> SomeHallway
appendSomeHallways (s1 :&: hw1) (s2 :&: hw2) =
  (s1 %++ s2) :&: (hw1 `appendHallways` hw2)

-- | 3.  Try directly defining the defunctionalization symbol `KnockableDoor :: DoorState ~> Type` (or use singletons to generate it for you — remember that singletons can also promote type families) so that `type SomeKnockableDoor = Sigma DoorState KnockableDoor` will contain a Door that must be knockable.
--
-- Try doing it for both (a) the “dependent proof” version (with the Knockable data type) and for (b) the type family version (with the StatePass type family).

-- (a)
data KnockableDoor :: DoorState ~> Type

type instance Apply KnockableDoor s = (Knockable s, Door s)

type SomeKnockableDoor = Sigma DoorState KnockableDoor

-- (b)
data KnockableDoor2 :: DoorState ~> Type

type instance Apply KnockableDoor2 s = (StatePass s :~: 'Obstruct, Door s)

type SomeKnockableDoor2 = Sigma DoorState KnockableDoor2

-- Using promoted type families
$( singletons
     [d|
       type family KnockableDoor3 (s :: DoorState) :: Type where
         KnockableDoor3 s = (Knockable s, Door s)
       |]
 )

type SomeKnockableDoor3 = Sigma DoorState KnockableDoor3Sym0

-- Using promoted types
$( singletons
     [d|
       type KnockableDoor4 s = (StatePass s :~: 'Obstruct, Door s)
       |]
 )

type SomeKnockableDoor4 = Sigma DoorState KnockableDoor4Sym0

-- | 4.

-- (*) is multiplication from the Data.Singletons.Prelude.Num module. (You must have the -XNoStarIsType extension on for this to work in GHC 8.6+)

data IsHalfOf :: Natural -> Natural ~> Type

type instance Apply (IsHalfOf n) m = n :~: (m * 2)

type IsEven n = Sigma Natural (IsHalfOf n)

-- Won't compile as expected
-- sevenIsEven :: IsEven 10
-- sevenIsEven = SNat @4 :&: Refl

tenIsEven :: IsEven 10
tenIsEven = SNat @5 :&: Refl @10

data IsHalfPlusOneOf :: Natural -> Natural ~> Type

type instance Apply (IsHalfPlusOneOf n) m = n :~: (m * 2 + 1)

type IsOdd n = Sigma Natural (IsHalfPlusOneOf n)

type IsOdd2 n = Sigma Natural ((IsHalfOf n) .@#@$$$ ((+@#@$$) 1))

-- Won't compile as expected
-- eightIsOdd :: IsOdd 8
-- eightIsOdd = SNat @4 :&: Refl @8

sevenIsOdd :: IsOdd 7
sevenIsOdd = SNat @3 :&: Refl @7

-- On a sad note, one exercise I’d like to be able to add is to ask you to write decision functions and proofs for IsEven and IsOdd. Unfortunately, Nat is not rich enough to support this out of the box without a lot of extra tooling!

-- | 5.

-- map :: (a -> b) -> [a] _> [b]
-- map f = foldr ((:) . f) []

-- Directly implement a type-level Map, with kind (j ~> k) -> [j] -> [k], in terms of Foldr:

-- type Map f xs = Foldr ???? ???? xs

-- Try to mirror the value-level definition, passing in (:) . f, and use the promoted version of (.) from the singletons library, in Data.Singletons.Prelude. You might find TyCon2 helpful!

type Map (f :: j ~> k) (xs :: [j]) = Foldr (TyCon2 (:) .@#@$$$ f) '[] xs

-- | 6. Implement mkSomeHallway

-- Recal:
--
-- type SomeDoor = Sigma DoorState (TyCon1 Door)
-- type SomeHallway = Sigma [DoorState] (TyCon1 Hallway)

mkSomeHallway :: [SomeDoor] -> SomeHallway
mkSomeHallway [] = SNil :&: HEnd
mkSomeHallway ((sds :&: d) : sdd) =
  case mkSomeHallway sdd of
    (ssd :&: hw) -> (sds `SCons` ssd) :&: (d :<# hw)
