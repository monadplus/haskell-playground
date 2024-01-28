{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Singletons.Singleton1 where

--  Singletons: indexed types that have exaclty one inhabitant and witness the index type variable at runtime

import Data.Kind
import Data.Singletons ()

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

-- data Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String }

-- data Door (s :: DoorState) :: Type where
--   UnsafeMkDoor :: { doorMaterial :: String } -> Door s

-- Indexed data type which is sometimes callled a "type family" in the
-- dependently typed programming world (do not confuse it with -XTypeFamilies)
data Door :: DoorState -> Type where
  UnsafeMkDoor :: {doorMaterial :: String} -> Door s

-- >>> let myDoor = UnsafeMkDoor @'Opened "Spruce"
-- >>> let myClosedDoor = closeDoor myDoor
-- >>> closeDoor myClosedDoor -- doesn't compile
closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor material) = UnsafeMkDoor material

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor material) = UnsafeMkDoor material

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor material) = UnsafeMkDoor material

-- It can be done with an ad-hoc typeclass
doorStatusAttempt :: Door s -> DoorState
doorStatusAttempt _ = undefined -- ?

-- How can you create a 'Door' with a given state that isn't known until runtime ?

-- The caller picks the polymorphic type, not the callee.
-- This is wrong!
-- >>> mkDoor Opened "Acacia" :: Door 'Closed -- BOGUS
mkDoor :: DoorState -> String -> Door s
mkDoor Opened = UnsafeMkDoor
mkDoor Closed = UnsafeMkDoor
mkDoor Locked = UnsafeMkDoor

-- The Fundamental Issue: type erasure
-- Types only exists at compile-time, they are erased at runtime.

--------------------------------------------------
-- The Singleton Pattern

-- A singleton is a type (of kind Type) that has exactly one inhabitant.
data SDoorState :: DoorState -> Type where
  SOpened :: SDoorState 'Opened
  SClosed :: SDoorState 'Closed
  SLocked :: SDoorState 'Locked

-- We can pattern-match on 'SDoorState s' to reveal what 's' is.
-- This is known as *dependently pattern match*.
--
-- If pattern match goes down to 'SOpened ->' then we known that s ~ 'Opened
--
-- SOpened is a *runtime witness** to s being 'Opened
lockAnyDoor :: SDoorState s -> (Door s -> Door 'Locked)
lockAnyDoor = \case
  SOpened -> lockDoor . closeDoor
  SClosed -> lockDoor -- Try replacing it with `id` or `closeDoor`
  SLocked -> id

-- Singletons give us _runtime values_ that can be used as _witnesses_ for types and type variables.
--
-- They bypass type erasure. Types themselves are directly erased, but we can
-- hold on to them using these runtime tokens when we need them.

--------------------------------------------------
-- Reflection

-- Process of turning a type variable (like 's') into a _dynamic runtime value_ is known as *reflection*.
-- We moved a value from the type level to the term level.

-- Both 's' are the same
-- doorStatus :: SDoorState s -> Door s -> DoorState
-- doorStatus SOpened _ = Opened
-- doorStatus SClosed _ = Closed
-- doorStatus SLocked _ = Locked

fromSDoorState :: SDoorState s -> DoorState
fromSDoorState SOpened = Opened
fromSDoorState SClosed = Closed
fromSDoorState SLocked = Locked

doorStatus :: SDoorState s -> Door s -> DoorState
doorStatus s _ = fromSDoorState s

--------------------------------------------------
-- Recovering Implicit Passing

-- In the previous, we are required to manually pass in our witness.
-- Implicit witness passing?

class SingDSI s where
  singDS :: SDoorState s

-- GHC checks this is correct.
instance SingDSI 'Opened where singDS = SOpened

instance SingDSI 'Closed where singDS = SClosed

instance SingDSI 'Locked where singDS = SLocked

lockAnyDoor_ :: SingDSI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor singDS

doorStatus_ :: SingDSI s => Door s -> DoorState
doorStatus_ = doorStatus singDS

-- The Same Power:  'SignDSI s =>' is essentially the same as passing in 'SDoorState s' explicitly.

-- Going from 'SingDSI s =>' to 'SDoorState s ->' is easy, just use `singDS`
-- Going from 'SDoorState s ->' to 'SingDSI s =>' is a little trickier:

withSingDSI :: SDoorState s -> (SingDSI s => r) -> r
withSingDSI sng x = case sng of
  SOpened -> x -- SOpened witness s ~ 'Opened and so GHC knowns that there is an instance of SingDSI for s.
  SClosed -> x
  SLocked -> x

lockAnyDoor__ :: SDoorState s -> Door s -> DoorState
lockAnyDoor__ sng door = withSingDSI sng (doorStatus_ door)

-- Fun with Witnesses

-- >>> mkDoor_ SOpened "Oak"
-- Door 'Opened
mkDoor_ :: SDoorState s -> String -> Door s
mkDoor_ _ = UnsafeMkDoor

-- Jump to Singleton2.hs
