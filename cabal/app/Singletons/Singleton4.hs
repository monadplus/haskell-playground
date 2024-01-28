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

module Singletons.Singleton4 where

import Data.Kind
import Data.Ord.Singletons
import Data.Singletons
import Data.Singletons.TH
import Prelude.Singletons

$( singletons
     [d|
       data DoorState = Opened | Closed | Locked
         deriving (Show, Eq)
       |]
 )

data Door :: DoorState -> Type where
  UnsafeMkDoor :: {material :: String} -> Door s
  deriving (Show)

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: (SingI s) => Door s -> SomeDoor
fromDoor_ = fromDoor sing

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor = \case
  Opened -> fromDoor_ . mkDoor SOpened
  Closed -> fromDoor_ . mkDoor SClosed
  Locked -> fromDoor_ . mkDoor SLocked

---------------------------------------------
-- A need for more expressive restrictions

-- We can't knock on an opened door.. can we?
knockBad :: Door s -> IO ()
knockBad d = putStrLn $ "Knock knock on " ++ material d ++ " door!"

---------------------------------------------
-- Dependently Typed Proofs

-- Proofs (in the dependently typed/constructivist/Curry-Howard sense) are witness
-- to some type-level predicate or preposition.

-- A type-level predicate is (generally) a type constructor of kind `k -> Type`.
-- Given a type of kind `k`, if a value exists of that type (or it can be constructed), then the predicate is satisfied.
-- That value, if it exists, is called a _witness_ or a _proof_.

-- Predicate `Knockable :: DoorState -> Type` as a GADT that only has values
-- if given 'Closed and 'Locked, but not 'Opened:

data Knockable :: DoorState -> Type where
  KnockClosed :: Knockable 'Closed
  KnockLocked :: Knockable 'Locked

knock :: Knockable s -> Door s -> IO ()
knock _ d = putStrLn $ "Knock knock on " ++ material d ++ " door!"

-----------------------------------------
-- Let the compiler prove it for you

class Proved (p :: k -> Type) (a :: k) where
  auto :: p a

instance Proved Knockable 'Closed where
  auto = KnockClosed

instance Proved Knockable 'Locked where
  auto = KnockLocked

-- >>> knock auto (mkDoor SClosed "Acacia")
-- Knock knock on Acacia door!
--
-- >>> knock auto (mkDoor SOpened "Jungle")
-- Compile-time error

-- Such a typeclass exists in libraries like:
-- http://hackage.haskell.org/package/decidable

-- In dependently typed languages like Idris,
-- auto is actually a built-in language keyword
-- that does this automatically!

-----------------------------------------
-- Decidable Predicates

-- The previous only works if you know what 's' is at compile-time
-- What if you are retrieving 's' at runtime (like SomeDoor or withSomeSing) ?

-- We say that a predicate is _decidable_ if, for any input type, we can
-- say whether or not the predicate is satisfiable.

{-

-- Is there a proof for the given predicate ? (value level)
decidePred :: Sing x -> Decision (<Predicate> x)

data Decision a = Proved a
                | Disproved (Refuted a) -- Includes a proof when the predicate is not true

data Void

-- | 'a' cannot exist. Commonly also called `Not`
type Refuted a = a -> Void
-}

-- Refuted represents the fact that 'a' is impossible to construct.

-- If a possible function `a -> Void` exists, it necessarily means that a value
-- of type 'a' cannot exist.

-- Knockable is a decidable predicate!

-- Notice we can't give a Proved :: a -> Decision a
-- because `Knockable 'Opened` does not exists.
isKnockable :: Sing s -> Decision (Knockable s)
isKnockable = \case
  SOpened -> Disproved $ disproveOpened -- Disproved $ \case {}
  SClosed -> Proved KnockClosed
  SLocked -> Proved KnockLocked

disproveOpened :: Knockable 'Opened -> Void
disproveOpened k = case k of {}

-- Proof on runtime values:
knockSomeDoor :: SomeDoor -> IO ()
knockSomeDoor (MkSomeDoor s d) = case isKnockable s of
  Proved proof -> knock proof d
  Disproved _ -> putStrLn "No knocking allowed"

-----------------------------------------
-- Perpsective on Proofs

-- Compilers of languages that encourage heavy usage of
-- proofs (like Agda, Coq, Idris) actually implement something called
-- __proof erasure__. That is, in those languages, values like
-- KnockClosed and KnockLocked might never exist at runtime,
-- since they never actually do anything at runtime.
-- They only exist as ways to limit or enable specific programs
-- from compiling, and serve no purpose after compilation.

-----------------------------------------
-- The Role of Singletons

-- Proofs themselves might not play a role at run-time,
-- but generating/deciding them with types requires being able to
-- pattern match and work with types at run-time.
--
-- Because of this, singletons play an important practical
-- role in working with proofs in Haskell.

-- Data.Singletons.Decide module is dedicated to working with proofs and decisions

{- Propositonal equality

data (:~:) :: (a :: k) -> (b :: k) -> Type where
    Refl :: a :~: a

Knockable is a predicate that a given status is "knockable"

('Foo :~:) is a predicate that a given type is equal to 'Foo

It also offers the “kindclass” SDecide, which provides decision functions for the (a :~:) predicate:

class SDecide k where
    (%~) :: Sing (a :: k)
         -> Sing (b :: k)
         -> Decision (a :~: b)

For example, Bool is an instance of SDecide, so we have

(STrue %~) :: Sing b -> Decision ('True :~: b)

which is a decision function to check if b is equal to 'True.

See Exercises3.hs for examples.
-}

-----------------------------------------
-- Type Level Functions

$( singletons
     [d|
       data Pass = Obstruct | Allow
         deriving (Show, Eq, Ord)
       |]
 )

type family StatePass (s :: DoorState) :: Pass where
  StatePass 'Opened = 'Allow
  StatePass 'Closed = 'Obstruct
  StatePass 'Locked = 'Obstruct

knockP :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knockP d = putStrLn $ "Knock knock on " ++ material d ++ " door!"

-- What happens if we don't know 's' untile runtime?
-- How do we prove to the compiler that Passable s is 'Allow ?

-- Notice this definition is almost identical to the type family StatePass.
sStatePass :: Sing s -> Sing (StatePass s)
sStatePass = \case
  SOpened -> SAllow
  SClosed -> SObstruct
  SLocked -> SObstruct

knockSomeDoorP :: SomeDoor -> IO ()
knockSomeDoorP (MkSomeDoor s d) =
  case sStatePass s of
    SObstruct -> knockP d
    SAllow -> putStrLn "No knocking allowed!"

-----------------------------------------
-- Singletons Library to the Rescue

{-
This is where the singletons library comes in:

It provides us Template Haskell tools to automatically define type families
  and their associated singleton functions.

The following definition generates:

 * The type family StatePass (s :: DoorState) :: Pass, like we defined above
 * The singleton function sStatePass, with the type Sing s -> Sing (StatePass s), like we defined above.
-}

$( singletons
     [d|
       statePass' :: DoorState -> Pass
       statePass' Opened = Allow
       statePass' Closed = Obstruct
       statePass' Locked = Obstruct
       |]
 )

-----------------------------------------
-- A Comparison

{-
Remember that the problem of term-level functions was that they
are potentially “incorrect”, and not directly verifiable.

So, if you just lift your potentially incorrect term-level functions to the type level
what you get is potentially incorrect type-level functions! (same logic errors).

In contrast, if you use dependently typed proofs correctly, these proofs can compose,
 and GHC can check that these proofs compose correctly, or that the compositions
 of your proofs are also valid proofs. That’s because this is enforced at the structural level.
 GHC can’t do that directly with functions; it can’t check that the composition of
 functions gives correct answers.

These two approaches aren’t necessarily mutually exclusive, and you often might mix the two.
-}

-----------------------------------------
-- Singleton Library Functions

{-
You can find most of these in the Data.Singletons.Prelude module namespace.

So, with singletons, you get functions like:

fst :: (a, b) -> a
type family Fst (t :: (a, b)) :: a
sFst :: forall a b (t :: (a, b)). Sing t -> Sing (Fst t)

isLeft :: Either a b -> Bool
type family IsLeft (t :: Either a b) :: Bool
sIsLeft :: Sing t -> Sing (IsLeft t)

(++) :: [a] -> [a] -> [a]
type family (xs :: [a]) ++ (ys :: [a]) :: [a]
(%++) :: Sing xs -> Sing ys -> Sing (xs ++ ys)
-}

---------------------------------------------
-- Promoted Typeclasses

{-
But, how can we promote functions like (==) and max, which are typeclass-polymorphic?

The singletons library handles this by providing each of these in a separate typeclass.
Let’s look at the humble Eq typeclass as an example:

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

-- Promoted type family
class PEq a where
    type (x :: a) == (y :: a) :: Bool
    type (x :: a) /= (y :: a) :: Bool

-- Singleton functions
class SEq a where
    (%==) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x == y) -- Notice (==) is the associated type from PEq
    (%/=) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x /= y)
-}

-- You can promote your own custom typeclasses:

$( singletons
     [d|
       class MyClass a where
         myFunc :: a -> a
       |]
 )

{- Generates:

 1. The typeclass MyClass with method myFunc :: MyClass a => a -> a
 2. The promoted typeclass PMyClass with associated type MyFunc (x :: a) :: a
 3. The singletonized typeclass SMyClass with method sMyFunc :: Sing x -> Sing (MyFunc x)
-}

---------------------------------------------
-- Automatically Promoting Instances

{-

\$(singletons [d|
  data Pass = Obstruct | Allow

  instance Eq Pass where
      Obstruct == Obstruct = True
      Obstruct == Allow    = False
      Allow    == Obstruct = False
      Allow    == Allow    = True

      Obstruct /= Obstruct = True
      Obstruct /= Allow    = False
      Allow    /= Obstruct = False
      Allow    /= Allow    = True
  |])

But, you can also write:

\$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq, Ord)
  |])

>>> :kind! 'Obstruct == 'Obstruct
'True

>>> SAllow %== SObstruct
SFalse

>>> :kind! Show_ 'Obstruct
"Obstruct"

>>> sMax SObstruct SAllow
SAllow

-}

-- Jump to Exercises3.hs
