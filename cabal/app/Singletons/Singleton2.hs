{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Singletons.Singleton2 where

import Data.Singletons ()
import Data.Singletons.TH
import Prelude.Singletons -- SCons/SNil

--------------------------------------------------
-- The Singletons Library

$( singletons
     [d|
       data DoorState = Opened | Closed | Locked
         deriving (Show, Eq)
       |]
 )

-- When the top type-level declaration name already exists:
--
--   genSingletons [ ''DoorState ]
--
-- https://hackage.haskell.org/package/singletons-2.7/docs/Data-Singletons-TH.html#v:genSingletons

{- This generates:

data SDoorState :: DoorState -> Type where
    SOpened :: Sing 'Opened
    SClosed :: Sing 'Closed
    SLocked :: Sing 'Locked
type instance Sing = SDoorState

Sing is a poly-kinded type constructor (a "data family"):
  type family Sing :: k -> Type

`Sing 'Opened ~ SOpened` because `type instance Sing = SDoorState :: DoorState -> Type`.
If I give you ('Opened :: DoorState), you return (SDoorState 'Opened :: Type) which is only inhabitat  by SOpened

Examples:

 * STrue :: Sing 'True
 * SJust SOpopened :: Sing ('Just 'Opened)
 * SOpened :: Sing' 'Opened

It also generates us instances for SingI, a poly-kinded typeclass:
  class SingI k where
    sing :: Sing k

instance Sing SDoorState
instance SingI 'Opened where sing = SOpened
instance SingI 'Closed where sing = SClosed
instance SingI 'Locked where sing = SLocked

SingI ~ generalized SingDSI for all kinds

-- >>> sing @'True
-- STrue :: Sing 'True
-- >>> sing :: Sing ('Just 'Opened)
-- SJust SOpened
-- >>> sing :: Sing 10
-- SNat @10

We also have withSingI (equivalent to withSingDSI):

withSingI :: Sing s -> (SingI s => r) -> r

-- Example

data Option a = Some a | None

type SOption :: Option a -> Type
data SOption maybe where
  SNone :: SOption ('None :: Option a)
  SSome :: Sing a -> SOption ('Some a)

type instance Sing = SOption

instance SingI 'None where sing = SNone
instance SingI (a :: k) => SingI ('Some a) where sing = SSome sing
-}

{- Extra Goodies

Recall that DoorState has four different things associated with it:

  1. The type DoorState whose term constructors Opened, Closed and Locked
  2. The kind DoorState whose types constructors 'Opened, 'Closed and 'Locked
  3. The singletons for 'Opened, 'Closed and 'Locked
                        SOpened, SClosed and SLocked
  4. The SignI instances for 'Opened, 'Closed and 'Locked

With real dependent types, these are unified into the same thing.
But for now, we have to convert between them.

-- From singleton to term-level values
fromSing :: Sing (s :: DoorState) -> DoorState

>>> fromSing SOpened
Opened
>>> fromSing (sing @'True)
True

class SingKind k where
  type Demote k = (r :: Type) | r -> k
  fromSing :: Sing (a :: k) -> Demote k        -- reflection
  toSing :: Demote k -> SomeSing k             -- reification

class SingKind DoorState where
  type Demote DoorState = DoorState -- One is the kind and the other the type
  fromSing :: Sing DoorState -> DoorState
-}

-- Jump to Exercises1.hs
