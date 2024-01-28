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
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Singletons.Exercises3 where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Data.Void
import Prelude.Singletons hiding (And, Or)

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

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing ->
  MkSomeDoor dsSing (mkDoor dsSing mat)

data Knockable :: DoorState -> Type where
  KnockClosed :: Knockable 'Closed
  KnockLocked :: Knockable 'Locked

knock :: Knockable s -> Door s -> IO ()
knock _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

class Proved p a where
  auto :: p a

instance Proved Knockable 'Closed where
  auto = KnockClosed

instance Proved Knockable 'Locked where
  auto = KnockLocked

isKnockable :: Sing s -> Decision (Knockable s)
isKnockable = \case
  SOpened -> Disproved $ \case {}
  SClosed -> Proved KnockClosed
  SLocked -> Proved KnockLocked

disproveOpened :: Knockable 'Opened -> Void
disproveOpened k = case k of {}

knockSomeDoor :: SomeDoor -> IO ()
knockSomeDoor (MkSomeDoor s d) = case isKnockable s of
  Proved k -> knock k d
  Disproved _ -> putStrLn "No knocking allowed!"

$( singletons
     [d|
       data Pass = Obstruct | Allow
         deriving (Show, Eq)

       statePass :: DoorState -> Pass
       statePass Opened = Allow
       statePass Closed = Obstruct
       statePass Locked = Obstruct
       |]
 )

knockP :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knockP d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

knockSomeDoorP :: SomeDoor -> IO ()
knockSomeDoorP (MkSomeDoor s d) = case sStatePass s of
  SObstruct -> knockP d
  SAllow -> putStrLn "No knocking allowed!"

-- Exercises

-- | 1.
--
-- | 1.1. It is a tautology. It can be proved for any input.
--
-- | 1.2. Corresponds to `const True`
--
-- | 1.3
decideDoorState :: Sing s -> Decision (SDoorState s)
decideDoorState = Proved

-- | 2. Double negation: not (not p) = p
refuteRefuteKnockable :: forall s. SingI s => Refuted (Refuted (Knockable s)) -> Knockable s
refuteRefuteKnockable doubleNeg =
  case isKnockable sing of
    Proved p -> p
    Disproved p -> absurd (doubleNeg p)

-- | 3

-- | 3.1 And predicate
data And :: (k -> Type) -> (k -> Type) -> k -> Type where
  And :: p a -> q a -> And p q a

-- | 3.2
data Or :: (k -> Type) -> (k -> Type) -> k -> Type where
  OrLeft :: p a -> Or p q a
  OrRight :: q a -> Or p q a

-- | 3.3
-- Notice how construction can't have errors.
decideAnd ::
  (forall. Sing s -> Decision (p s)) ->
  (forall. Sing s -> Decision (q s)) ->
  Sing s ->
  Decision (And p q s)
decideAnd pred1 pred2 s =
  case pred1 s of
    Proved p ->
      case pred2 s of
        Proved q -> Proved (And p q)
        Disproved k -> Disproved $ \(And _p q) -> k q
    Disproved k -> Disproved $ \(And p _q) -> k p

decideOr ::
  (forall. Sing s -> Decision (p s)) ->
  (forall. Sing s -> Decision (q s)) ->
  Sing s ->
  Decision (Or p q s)
decideOr pred1 pred2 s =
  case pred1 s of
    Proved p -> Proved (OrLeft p)
    Disproved k1 ->
      case pred2 s of
        Proved q -> Proved (OrRight q)
        Disproved k2 ->
          Disproved $ \case
            (OrLeft p) -> k1 p
            (OrRight q) -> k2 q

-- | 3.4 The implementations are unique
knockableNotOpened ::
  forall s.
  SingI s =>
  Refuted (And Knockable ((:~:) 'Opened) s) -- a -> Void
knockableNotOpened (And p q) =
  -- First we need a proof that s is not 'Opened.
  -- Then, we can proof that q does not exist.
  case p of
    KnockClosed -> case q of {}
    KnockLocked -> case q of {}

knockableOrOpened ::
  forall s.
  SingI s =>
  Or Knockable ((:~:) 'Opened) s
knockableOrOpened = case sing @s of
  SLocked -> OrLeft KnockLocked
  SClosed -> OrLeft KnockClosed
  SOpened -> OrRight Refl

-- | 4. Is `Refuted (s :~: Opened)` isomorphic to `Knockable s` ? Let's prove it.
knockedRefute ::
  forall s.
  SingI s =>
  Knockable s ->
  Refuted (s :~: 'Opened)
knockedRefute = \case
  KnockClosed -> \case {}
  KnockLocked -> \case {}

refuteKnocked ::
  forall s.
  SingI s =>
  Refuted (s :~: 'Opened) ->
  Knockable s
refuteKnocked refute = case sing @s of
  SOpened -> absurd (refute Refl)
  SLocked -> KnockLocked
  SClosed -> KnockClosed

knock' :: SingI s => Refuted (s :~: 'Opened) -> Door s -> IO ()
knock' p = knock (refuteKnocked p)

-- | 5.

-- Given this new version of knock, write the equivalent value-level function
knockRefl :: (StatePass s :~: 'Obstruct) -> Door s -> IO ()
knockRefl _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

-- Hint: DoorState has an instance of SDecide
knockSomeDoorRefl :: SomeDoor -> IO ()
knockSomeDoorRefl (MkSomeDoor s d) =
  case (sStatePass s) %~ SObstruct of
    Proved p -> knockRefl p d
    Disproved _ -> putStrLn "No knocking allowed!"

-- | 6.

-- Given
$( singletons
     [d|
       invertPass :: Pass -> Pass
       invertPass Obstruct = Allow
       invertPass Allow = Obstruct
       |]
 )

knockInv :: (InvertPass (StatePass s) ~ 'Allow) => Door s -> IO ()
knockInv d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

-- Implement this
knockSomeDoorInv :: SomeDoor -> IO ()
knockSomeDoorInv (MkSomeDoor s d) =
  case (sInvertPass (sStatePass s)) %~ SAllow of
    -- Notice you need to pattern on Refl to bring the equality into scope.
    Proved Refl -> knockInv d
    Disproved _ -> putStrLn "No knocking allowed!"

-- Another impl
knockSomeDoorInv' :: SomeDoor -> IO ()
knockSomeDoorInv' (MkSomeDoor s d) =
  case sInvertPass (sStatePass s) of
    SAllow -> knockInv d
    SObstruct -> putStrLn "No knocking allowed!"

-- | 7.
$( singletons
     [d|
       class Cycle a where
         next :: a -> a
         prev :: a -> a
       |]
 )

instance Cycle DoorState where
  next Opened = Closed
  next Closed = Locked
  next Locked = Opened

  prev Opened = Locked
  prev Closed = Opened
  prev Locked = Closed

instance PCycle DoorState where
  type Next 'Opened = 'Closed
  type Next 'Closed = 'Locked
  type Next 'Locked = 'Opened

  type Prev 'Opened = 'Locked
  type Prev 'Closed = 'Opened
  type Prev 'Locked = 'Closed

instance SCycle DoorState where
  sNext :: forall (x :: DoorState). Sing x -> Sing (Next x)
  sNext SOpened = SClosed
  sNext SClosed = SLocked
  sNext SLocked = SOpened

  sPrev :: forall (x :: DoorState). Sing x -> Sing (Prev x)
  sPrev SOpened = SLocked
  sPrev SClosed = SOpened
  sPrev SLocked = SClosed

-- Jump to Singleton5.hs
