{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Singletons.Singleton5 where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Prelude.Singletons

---------------------------------------------
-- Recap

$( singletons
     [d|
       data DoorState = Opened | Closed | Locked
         deriving (Show, Eq)
       |]
 )

data Door :: DoorState -> Type where
  UnsafeMkDoor :: {doorMaterial :: String} -> Door s
  deriving (Show)

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor = \case
  Opened -> fromDoor_ . mkDoor SOpened
  Closed -> fromDoor_ . mkDoor SClosed
  Locked -> fromDoor_ . mkDoor SLocked

---------------------------------------------

-- mergeDoor :: Door s -> Door t -> Door ????
-- mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e

{-

The following definition generates:

 * type family MergeState (s :: DoorState) (s1 :: DoorState) :: DoorState
 * singleton function sMergeState :: Sing (s :: DoorState) -> Sing (s1 :: DoorState) -> Sing (MergeState s s1)
 * more (explained later)
-}
$( singletons
     [d|
       mergeState :: DoorState -> DoorState -> DoorState
       mergeState Opened d = d
       mergeState Closed Opened = Closed
       mergeState Closed Closed = Closed
       mergeState Closed Locked = Locked
       mergeState Locked _ = Locked
       |]
 )

mergeDoor :: Door s -> Door t -> Door (MergeState s t)
mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e

mergeSomeDoor :: SomeDoor -> SomeDoor -> SomeDoor
mergeSomeDoor (MkSomeDoor s1 d1) (MkSomeDoor s2 d2) =
  MkSomeDoor (sMergeState s1 s2) (mergeDoor d1 d2)

-----------------------------------------------------------------

-- | A hallway is either an empty stretch with no door, or two hallways linked by a door
data Hallway :: [DoorState] -> Type where
  HEnd :: Hallway '[]
  (:<#) :: Door s -> Hallway ss -> Hallway (s ': ss)

infixr 5 :<#

-- collapseHallway :: Hallway ss -> Door ?????
-- collapseHallway (d :<# hallway) = d `mergeDoor` collapseHallway hallway

-- Generates:

-- * type family MergeStateList

-- * singleton function sMergeStateList

$( singletons
     [d|
       mergeStateList :: [DoorState] -> DoorState
       mergeStateList [] = Opened
       mergeStateList (s : ss) = s `mergeState` mergeStateList ss
       |]
 )

collapseHallway :: Hallway ss -> Door (MergeStateList ss)
collapseHallway HEnd = mkDoor SOpened "End of Hallway"
collapseHallway (d :<# hallway) = d `mergeDoor` collapseHallway hallway

----------------------------------------------
-- Abstraction ?

mergeStateList' :: [DoorState] -> DoorState
mergeStateList' = foldr mergeState Opened

$( singletons
     [d|
       type family Foldr2 (f :: j -> k -> k) (z :: k) (xs :: [j]) :: k where
         Foldr2 _f z '[] = z
         Foldr2 f z (x ': xs) = f x (Foldr2 f z xs)
       |]
 )

-- >>> type MergeStateList2 ss = Foldr2 MergeState 'Opened ss
-- Won't compile.
-- The problem is that haskell's type families must be saturated and `MergeState` is not.

----------------------------------------------
-- Defunctionalization

{-

The singleton library has two parts:

  - First part: linking lifted DataKinds types with runtime values to allow us to manipulate types at runtime as first-class values.

  - Second part: a  system for effective functional programming at the type-level.

\**Defunctionalization**: higher-order functions into first-order functions (early 70's).

  1.- Instead of working with functions, work with symbols representing functions.
  2.- Build your final functions and values by composing and combining these symbols.
  3.- At the end of it all, have a single Apply function interpret all of your symbols and produce the value you want.

In singletons these symbols are implemented as “dummy” empty data constructors, and Apply is a type family.

-}

----------------------------------------------------------
-- Build our own defunctionalization system from scratch

{-

data TyFun a b

type a ~> b = TyFun a b -> Type
infixr 0 ~>

type family Apply (f :: a ~> b) (x :: a) :: b

type f @@ a = Apply f a
infixl 9 @@

-- >>> kind! Id
-- Id :: TyFun a a -> Type
data Id :: a ~> a
type instance Apply Id x = x

-- >>> :kind! Apply Id 'True
-- 'True

data Not :: Bool ~> Bool
type instance Apply Not 'True = 'False
type instance Apply Not 'False = 'True

-- >>> :kind! Not @@ 'True
-- 'False

-}

---------------------------------------------------------

{-

\$(singletons [d|
    not :: Bool -> Bool
    not False = True
    not True  = False
  |])

Generates:

type family Not (x :: Bool) :: Bool where
  Not 'False = 'True
  Not 'True  = 'False

sNot :: Sing x -> Sing (Not x)
sNot SFalse = STrue
sNot STrue  = SFalse

data NotSym0 :: Bool ~> Bool
type instance Apply NotSym0 x = Not x
type NotSym1 x = Not x

-}

-- The Sym0 suffix is a naming convention, and the 0 stands for “expects 0 arguments”.
-- Similarly for NotSym1 – the 1 stands for “expects 1 argument”.

{- Two-Argument Function

\$(singletons [d|
    and :: Bool -> (Bool -> Bool)
    and False _ = False
    and True  x = x
  |])

Generates:

type family And (x :: Bool) (y :: Bool) :: Bool where
    And 'False x = 'False
    And 'True  x = x

sAnd :: Sing x -> Sing y -> Sing (And x y)
sAnd SFalse x = SFalse
sAnd STrue  x = x

data AndSym0 :: Bool ~> (Bool ~> Bool)
type instance Apply AndSym0 x = AndSym1 x

data AndSym1 :: Bool -> (Bool ~> Bool)
-- Same as data AndSym1 (x :: Bool) :: Bool ~> Bool
type instance Apply (AndSym1 x) y = And x y

type AndSym2 x y = And x y

>>> :kind! AndSym0 @@ 'False
AndSym1 'False

>>> :kind! AndSym1 'False @@ 'True
'False

NOTE the AndSym0 from Data.Singletons.Prelude is a bit different since it is the generalization from Foldable.
-}

-----------------------------------------------------
-- Symbols for Type constructors

-- Turn any type constructor into a defunctionlization symbol.
-- `singletons` library defines: TyCon, TyCon1, TyCon2 ...

data TyCon1' :: (j -> k) -> (j ~> k)

type instance Apply (TyCon1' t) a = t a

-- Alternatively:
-- data TyCon1' (t :: j -> k) :: j ~> k

-- >>> :kind! TyCon1' Maybe @@ Int
-- Maybe Int
--
-- >>> :kind! TyCon1' 'Right @@ 'False
-- 'Right 'False

----------------------------------------------------
-- Higher-Order

-- `singletons` library provides these definitions

-- Note, avoiding clashes with Apostrophe '
$( singletons
     [d|
       type family Foldr'' (f :: j ~> k ~> k) (z :: k) (xs :: [j]) :: k where
         Foldr'' _f z '[] = z
         Foldr'' f z (x ': xs) = (f @@ x) @@ Foldr'' f z xs
       |]
 )

{- This was previously generated

data MergeStateSym0 :: DoorState ~> DoorState ~> DoorState
type instance Apply MergeStateSym0 s = MergeStateSym1 s

data MergeStateSym1 :: DoorState -> DoorState ~> DoorState
type instance Apply (MergeStateSym1 s) t = MergeState s t

type MergeStateSym2 s t = MergeState s t
-}

type MergeStateList' ss = Foldr'' MergeStateSym0 'Opened ss

-- >>> :kind! MergeStateList' '[ 'Closed, 'Opened, 'Locked ]
-- LockedSym0

collapseHallway' :: Hallway ss -> Door (MergeStateList' ss)
collapseHallway' HEnd = mkDoor SOpened "End of Hallway"
collapseHallway' (d :<# hallway) = d `mergeDoor` (collapseHallway' hallway)

-- TODO
--
-- type MergeStateList ss = Foldr MergeStateSym0 'Opened ss
--
-- We can't use in this example the Foldr from `singletons` because of this
--   https://github.com/goldfirere/singletons/issues/339
--
-- In general, the issue is that we should only expect type families to work
-- with singletons if the definition of the type family perfectly matches
-- the structure of how we implement our value-level functions like collapseHallway

----------------------------------------------------------------
-- Singletons to make things nicer

{-

You can let `singletons` library handles everything.
All the defunctionlization symbols are generated for you.

\$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq, Ord)

  mergeState :: DoorState -> DoorState -> DoorState
  mergeState = max

  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr _ z []     = z
  foldr f z (x:xs) = f x (foldr f z xs)

  mergeStateList :: [DoorState] -> DoorState
  mergeStateList = foldr mergeState Opened
  |])
-}

{- On operators

(++) will generate:

  - (++@#@$) be the completely unapplied defunctionalization symbol
  - (++@#@$$) be the type constructor that expects one argument before returning a defunctionalization symbol
  - (++@#@$$$) be the type constructor that takes two arguments before returning a defunctionalization symbol, etc.
-}

-- Another helpful thing that singleton does:

$( singletons
     [d|
       type family MyTypeFamily (b :: Bool) :: Type where
         MyTypeFamily 'False = Int
         MyTypeFamily 'True = String
       |]
 )

-- generates

{-
data MyTypeFamilySym0 :: Bool ~> Type
type instance Apply MyTypeFamilySym0 b = MyTypeFamily b

type MyTypeFamilySym1 b = MyTypeFamily b
-}

$( singletons
     [d|
       type MyTypeSynonym a = (a, [a])
       |]
 )

-- generates

{-
data MyTypeSynonymSym0 :: Type ~> Type
type instance Apply MyTypeSynonym a = MyTypeSynonym a

type MyTypeSynonymSym1 a = MyTypeSynonym a
-}

----------------------------------------------------------------------------
-- Promoted Typeclasses

-- Note, requires PSemigroup and SSemigroup from Singletons.Prelude

$( singletons
     [d|
       instance Semigroup DoorState where
         (<>) = mergeState

       instance Monoid DoorState where
         mempty = Opened
       |]
 )

$( singletons
     [d|
       fold' :: Monoid b => [b] -> b
       fold' [] = mempty
       fold' (x : xs) = x <> fold' xs
       |]
 )

collapseHallway2 :: Hallway ss -> Door (Fold' ss)
collapseHallway2 HEnd = UnsafeMkDoor "End of Hallway"
collapseHallway2 (d :<# ds) = d `mergeDoor` collapseHallway2 ds

-- Again,  we need to use implement Fold' instead of using Data.Singletons.Prelude.Fold
-- because of https://github.com/goldfirere/singletons/issues/339

-----------------------------------------------------------------------
-- Thoughts on Symbols

{-

At the end of the day, you can compare defunctionalization as turning “functions” into just constructors you can match on,
just like any other data or type constructor. That’s because they are just type constructors!

invertOperation :: (Double -> Dobule -> Double) -> (Double -> Double -> Double)
invertOperation (+) = (-)

You can’t quite match on the equality of functions to some list of patterns.
But, what you can do is create constructors representing your functions, and match on those.

This essentially fixes the “type lambda problem” of type inference and typeclass resolution.
You can’t match on arbitrary lambdas, but you can match on dummy constructors representing type functions.

Check out the defunctionalization symbols for Foldr:

data FoldrSym0 :: (j ~> k ~> k) ~> k ~> [j] ~> k
type instance Apply FoldrSym0 f = FoldrSym1 f

data FoldrSym1 :: (j ~> k ~> k) -> k ~> [j] ~> k
type instance Apply (FoldrSym1 f) z = FoldrSym2 f z

data FoldrSym2 :: (j ~> k ~> k) -> k -> [j] ~> k
type instance Apply (FoldrSym2 f z) xs = Foldr f z xs

type FoldrSym3 f z xs = Foldr f z xs

You can think of FoldrSym1 and FoldrSym2 as defunctionalization symbol constructors – they’re combinators
that take in defunctionalization symbols (like MergeStateSym0) and return new ones.

We can actually use these to define our MergeStateList defunctionalization symbols, since defunctionalization symbols are first-class:

type MergeStateListSym0 = FoldrSym2 MergeStateSym0 'Opened

collapseHallway :: Hallway ss -> Door (MergeStateListSym0 @@ ss)
-}

----------------------------------------------------------------------
-- Sigma

-- A **dependent pair** is a tuple where the type of the second field depends on the value of the first one.
--
-- This is basically what SomeDoor was.

-- How do we generalize the dependant pair ?

data Sigma k :: (k ~> Type) -> Type where
  (:&:) :: Sing x -> (f @@ x) -> Sigma k f

type Σ k = Sigma k

type SomeDoor' = Sigma DoorState (TyCon1 Door)

mkSomeDoor' :: DoorState -> String -> SomeDoor'
mkSomeDoor' ds mat = withSomeSing ds $ \dsSing ->
  dsSing :&: mkDoor dsSing mat

--------------------------------------------------------------
-- Singletons of Defunctionalization Symbols

type SomeHallway = Sigma [DoorState] (TyCon1 Hallway)

collapseSomeHallway :: SomeHallway -> SomeDoor'
collapseSomeHallway (ss :&: d) =
  sMergeStateList ss :&: collapseHallway d

-- But what if we didn’t write sMergeStateList, and we constructed our defunctionalization symbols from scratch?

collapseHallway'' ::
  Hallway ss ->
  Door (Foldr''Sym2 MergeStateSym0 'Opened @@ ss)
collapseHallway'' HEnd = UnsafeMkDoor "End of Hallway"
collapseHallway'' (d :<# ds) = d `mergeDoor` collapseHallway'' ds

-- collapseSomeHallway'' :: SomeHallway -> SomeDoor
-- collapseSomeHallway'' (ss :&: d) =
--   ??? ss :&: collapseHallway'' d

-- How do we turn ss into
--   FoldrSym2 MergeStateSym0 'Opened @@ ss

{- Recall

type family Foldr (f :: j ~> k ~> k) (z :: k) (xs :: [j]) :: k where
    Foldr f z '[]       = z
    Foldr f z (x ': xs) = (f @@ x) @@ Foldr f z xs

sFoldr
    :: Sing (f :: j ~> k ~> k)
    -> Sing (z :: k)
    -> Sing (xs :: [j])
    -> Sing (Foldr f z xs :: k)
sFoldr f z SNil           = z
sFoldr f z (x `SCons` xs) = (f @@ x) @@ sFoldr f z xs

collapseSomeHallway'' :: SomeHallway -> SomeDoor
collapseSomeHallway'' (ss :&: d) =
  sFoldr ??? :&: collapseHallway'' d

How do we get to `Sing MergeStateSym0` ?

singFun2 @MergeStateSym0 sMergeState :: Sing MergeStateSym0

The singletons library generates a SingI instance for MergeStateSym0, if you defined mergeState using the singletons template haskell:

sing @MergeStateSym0 :: Sing MergeStateSym0

collapseSomeHallway'' :: SomeHallway -> SomeDoor
collapseSomeHallway'' (ss :&: d) =
  --sFoldr (sing @MergeStateSym0) SOpened ss :&: collapseHallway'' d
  (sFoldr (singFun2 @MergeStateSym0 sMergeState) SOpened ss) :&: collapseHallway'' d

\^^^^^^^^^^ Not compiling because of https://github.com/goldfirere/singletons/issues/339.
-}

-- Jump to Exercises4.hs
