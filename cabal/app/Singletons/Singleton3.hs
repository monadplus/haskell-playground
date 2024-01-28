{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Singletons.Singleton3 where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Prelude.Singletons

-------------------------------------------------------
-- Previously

$( singletons
     [d|
       data DoorState = Opened | Closed | Locked
         deriving (Show, Eq)
       |]
 )

data Door :: DoorState -> Type where
  UnsafeMkDoor :: {material :: String} -> Door s
  deriving (Show)

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

doorStatus :: Sing s -> Door s -> DoorState
doorStatus SOpened _ = Opened
doorStatus SClosed _ = Closed
doorStatus SLocked _ = Locked

lockAnyDoor :: Sing s -> (Door s -> Door 'Locked)
lockAnyDoor = \case
  SOpened -> lockDoor . closeDoor
  SClosed -> lockDoor
  SLocked -> id

doorStatus_ :: SingI s => Door s -> DoorState
doorStatus_ = doorStatus sing

lockAnyDoor_ :: SingI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor sing

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

-------------------------------------------------------
-- Ditching the Phantom

{-
What if we don't know `s` at compile-time?

Sometimes we don't care about the type of the door. For example,
when reading a `Door` data from a serialization format.

When we don't care about the type:

data SomeDoor :: Type where
  MkSomeDoor ::
    { someDoorState :: DoorState -- runtime value instead of a type parameter
    , someDoorMaterial :: String
    } -> SomeDoor

The main issue with this version is that you must reimplement all `Door` functions
-}

-------------------------------------------------------
-- The Existential Datatype

-- We can actually implement `SomeDoor` in terms of `Door` using *existential data types*:

-- data SomeDoor = forall s. MkSomeDoor (Sing s) (Door s)

-- GADT syntax
data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

-- The advantages of our new versions is that it is implemented in terms of `Door`.
-- So, we can reuse `Door` functions on `SomeDoor`.

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing

-- Porting some `Door` functions:

closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpenedDoor (MkSomeDoor s d) = case s of
  SOpened -> Just . fromDoor_ $ closeDoor d
  SClosed -> Nothing
  SLocked -> Nothing

lockAnySomeDoor :: SomeDoor -> SomeDoor
lockAnySomeDoor (MkSomeDoor s d) = fromDoor_ $ lockAnyDoor s d

{-

If MkSomeDoor did not have the Sing, it would be impossible to recover the
state of the door after type erasure.

data SomeDoor where
  MkSomeDoor :: Door s -> SomeDoor

closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpenedDoor (MkSomeDoor d) = ???

-}

-------------------------------------------------------
-- The Link

-- Theorem 1. Having an existentially quantified singleton is the same as having
--            a value of the corresponding type.
--
-- An example of this is the two implementations of SomeDoor - the one using
-- a value of DoorState and the one using an existential 'Sing s'

{- The singleton library gives us a direct existential wrapper

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

SomeSing SOpened :: SomeSing DoorState
SomeSing SClosed :: SomeSing DoorState
SomeSing SLocked :: SomeSing DoorState

-- Proof (1) for DoorState by isomorphism

fromSome :: SomeSing DoorState -> DoorState
fromSome (SomeSing sng) = case sng of
  SOpened -> Opened
  SClosed -> Closed
  SLocked -> Locked

toSome :: DoorState -> SomeSing DoorState
toSome = \case
  Opened -> SomeSing SOpened
  Closed -> SomeSing SClosed
  Locked -> SomeSing SLocked
-}

-------------------------------------------------------
-- Some Lingo

{-
In dependently typed programming, we call `SomeDoor` a *dependent sum*.
Because you can imagine it basically as a sume type:

data SomeDoor = SDOpened (Door 'Opened)
              | SDClosed (Door 'Closed)
              | SDLocked (Door 'Locked)

You might also see `SomeDoor` called a *dependent pair* - it's a "tuple" where
the type of the second item (our Door s) is determined by the value of the first
item (our Sing s)

-}

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor = \case
  Opened -> fromDoor_ . mkDoor SOpened
  Closed -> fromDoor_ . mkDoor SClosed
  Locked -> fromDoor_ . mkDoor SLocked

-- With `mkSomeDoor` we can truly pass in a `DoorState` that we generate at runtime.

-------------------------------------------------------
-- The Existential Type

-- An _existentially quantified type_ is one that is hidden to the
-- consumer, but directly chosen by the producer.

-- In contrast, a _universally quatified type_ the type is directly chosen by the consumer.

-- Another way to express an existentially quantified type is using a CPS-style encoding.
-- CPS doesn't require creating an intermediate helper data type.

withDoor :: DoorState -> String -> (forall s. Sing s -> Door s -> r) -> r
withDoor s m k = case s of
  Opened -> k SOpened (mkDoor SOpened m)
  Closed -> k SClosed (mkDoor SClosed m)
  Locked -> k SLocked (mkDoor SLocked m)

-------------------------------------------------------
-- Reification

-- Opposite of reflection.

-- Lift a runtime value to the type level.

-- The singletons library automatically
-- generates functions to directly reify `DoorState` values:

{-
-- Demote k is the type corresponding to the kind.
-- For example:  Demote Bool = Bool
--
-- reifies the runtime value DoorState as an existentially quantified data type.

SomeSing :: forall {k} {a :: k}. Sing a -> SomeSing k

toSing :: Demote k  -> SomeSing k
toSing :: DoorState -> SomeSing DoorState

-- Same as toSing but in CPS-style with no intermediate wrapping.
withSomeSing :: SingKind k => forall k r. Demote k  -> (forall (a :: k). Sing a -> r) -> r
withSomeSing ::                           DoorState -> (forall s. SDoorState s -> r) -> r
-}

-- We can use toSing and withSomeSing to rewrite the following:

mkSomeDoor' :: DoorState -> String -> SomeDoor
mkSomeDoor' ds = case toSing ds of
  SomeSing s -> fromDoor s . mkDoor s

-- pattern FromSing :: SingKind k => forall (a :: k). Sing a -> Demote k        (read from right to left)

mkSomeDoor'' :: DoorState -> String -> SomeDoor
mkSomeDoor'' (FromSing s) = fromDoor s . mkDoor s

withDoor' :: DoorState -> String -> (forall s. Sing s -> Door s -> r) -> r
withDoor' ds m k = withSomeSing ds $ \s -> k s (mkDoor s m)

-------------------------------------------------------
-- Zooming Out

-- Sing s = "a runtime witness for s"

-- class SingI s = implicitly pass Sing s
--
-- `SingI s ->`                 `SingI s =>`
--             <<< `sing`
--             >>> `withSingI`

-- Singletons offers:
--  * pattern Sing :: forall k (a :: k). () => SingI a => Sing a

doorStatus_' :: SingI s => Door s -> DoorState
doorStatus_' = doorStatus Sing

fromDoor' :: Sing s -> Door s -> SomeDoor
fromDoor' Sing door = fromDoor_ door

-------------------------------------------------------
-- Reflection and Reification

{-

           --- reification -->
term-level                      type-level
           <-- reflection  ---

They are the gateways from the unsafe world to the safe world.

In haskell, there is no unification between:
 * The type DoorState and its values
 * The kind DoorState and its types
-}

{- Generalized reflection and reification process

class SingKind k where          -- `k` is a kind
    -- | Associate a kind k with its reflected type
    type Demote k = (r :: Type) -- GHC doesn't actually link the type and its promoted kind

    -- | Reflect a singleton to its term-level value
    fromSing :: Sing (a :: k) -> Demote k

    -- | Reify a term-level value to the type level, as an existentially quantified singleton
    toSing :: Demote k -> SomeSing k

instance SingKind DoorState where       -- the *kind* DoorState
    type Demote DoorState = DoorState   -- the *type* DoorState

    fromSing
        :: Sing (s :: DoorState)        -- the *kind* DoorState
        -> DoorState                    -- the *type* DoorState
    fromSing = \case
        SOpened -> Opened
        SClosed -> Closed
        SLocked -> Locked

    toSing
        :: DoorState                    -- the *type* DoorState
        -> SomeSing DoorState           -- the *kind* DoorState
    toSing = \case
        Opened -> SomeSing SOpened
        Closed -> SomeSing SClosed
        Locked -> SomeSing SLocked

-----------------------------------------

SFalse :: Sing 'False
STrue  :: Sing 'True

instance SingKind Bool where    -- the *kind* Bool
    type Demote Bool = Bool     -- the *type* Bool

    fromSing
        :: Sing (b :: Bool)        -- the *kind* Bool
        -> Bool                    -- the *type* Bool
    fromSing = \case
        SFalse -> False
        STrue  -> True

    toSing
        :: Bool                    -- the *type* Bool
        -> SomeSing Bool           -- the *kind* Bool
    toSing = \case
        False -> SomeSing SFalse
        True  -> SomeSing STrue

------------------------------------------

-- Maybe singletons have two constructors:
data SMaybe :: Maybe k -> Type where
    SNothing :: SMaybe 'Nothing
    SJust    :: Sing x -> SMaybe ('Just x)

-- The syntax for declaring an instance for the kind-indexed type family
type instance Sing = SMaybe

instance SingKind k => SingKind (Maybe k) where     -- the *kind* Maybe
    type Demote (Maybe k) = Maybe (Demote k)        -- the *type* Maybe

    fromSing
        :: Sing (m :: Maybe k)        -- the *kind* Maybe
        -> Maybe (Demote k)           -- the *type* Maybe
    fromSing = \case
        SNothing -> Nothing
        SJust sx -> Just (fromSing sx)

    toSing
        :: Maybe (Demote k)             -- the *type* Maybe
        -> SomeSing (Maybe k)           -- the *kind* Maybe
    toSing = \case
        Nothing -> SomeSing SNothing
        Just x  -> case toSing x of
          SomeSing sx -> SomeSing (SJust sx)
-}

-- Jump to Singleton4.hs
