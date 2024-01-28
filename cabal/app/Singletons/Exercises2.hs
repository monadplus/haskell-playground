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

module Singletons.Exercises2 where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Prelude.Singletons hiding (SList (..))

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

data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing

closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpenedDoor (MkSomeDoor s d) = case s of
  SOpened -> Just . fromDoor_ $ closeDoor d
  SClosed -> Nothing
  SLocked -> Nothing

lockAnySomeDoor :: SomeDoor -> SomeDoor
lockAnySomeDoor (MkSomeDoor s d) = fromDoor_ $ lockAnyDoor s d

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor (FromSing s) = fromDoor s . mkDoor s

withDoor :: DoorState -> String -> (forall s. Sing s -> Door s -> r) -> r
withDoor ds m k = withSomeSing ds $ \s -> k s (mkDoor s m)

-----------------------------------------------------
-- Exercises

-- 1.

-- data SomeDoor :: Type where
--   MkSomeDoor :: Sing s -> Door s -> SomeDoor

data OldSomeDoor :: Type where
  OldMkSomeDoor :: DoorState -> String -> OldSomeDoor

toOld :: SomeDoor -> OldSomeDoor
toOld (MkSomeDoor s d) = OldMkSomeDoor (fromSing s) (material d)

fromOld :: OldSomeDoor -> SomeDoor
fromOld (OldMkSomeDoor ds m) = mkSomeDoor ds m

-- 2.
unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor n (UnsafeMkDoor m)
  | n `mod` 2 == 1 = Just (UnsafeMkDoor m)
  | otherwise = Nothing

unlockSomeDoor :: Int -> Door 'Locked -> SomeDoor
unlockSomeDoor n d = case unlockDoor n d of
  Just d' -> fromDoor_ d'
  Nothing -> fromDoor_ d

-- 3.
openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor n = openAnyDoor_ sing
  where
    openAnyDoor_ :: Sing s -> Door s -> Maybe (Door 'Opened)
    openAnyDoor_ = \case
      SOpened -> Just
      SClosed -> Just . openDoor
      SLocked -> fmap openDoor . unlockDoor n

openAnySomeDoor :: Int -> SomeDoor -> SomeDoor
openAnySomeDoor n sdoor = case sdoor of
  MkSomeDoor s d ->
    withSingI s $ case openAnyDoor n d of
      Nothing -> sdoor
      Just d' -> fromDoor_ d'

-- 4.

data List a = Nil | Cons a (List a)

data SList :: List a -> Type where
  SNil :: SList 'Nil
  SCons :: Sing x -> SList xs -> SList ('Cons x xs)

type instance Sing = SList

instance SingKind a => SingKind (List a) where
  type Demote (List a) = List (Demote a)

  fromSing = \case
    SNil -> Nil
    SCons x xs -> Cons (fromSing x) (fromSing xs)

  toSing = \case
    Nil -> SomeSing SNil
    (Cons (FromSing sx) (FromSing sxs)) -> SomeSing (SCons sx sxs)

-- (Cons x xs) ->
-- withSomeSing x $ \ sx ->
-- withSomeSing xs $ \ sxs ->
-- SomeSing (SCons sx sxs)

-- Jump to Singleton4.hs
