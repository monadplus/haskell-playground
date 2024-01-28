{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Singletons.Exercises1 where

import Data.Kind
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
  UnsafeMkDoor :: {doorMaterial :: String} -> Door s
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

doorStatus_ :: (SingI s) => Door s -> DoorState
doorStatus_ = doorStatus sing

lockAnyDoor_ :: (SingI s) => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor sing

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

---------------------------------------------
-- Exercises

unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor n (UnsafeMkDoor m)
  | mod n 2 == 1 = Just (UnsafeMkDoor m)
  | otherwise = Nothing

-- >>> let myDoor = mkDoor (sing @'Locked) "cedar"
-- >>> openAnyDoor 2 myDoor
-- Nothing
-- >>> openAnyDoor 3 myDoor
-- Just ...
openAnyDoor :: forall s. (SingI s) => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor passwd door = case sing @s of
  SLocked -> openDoor <$> unlockDoor passwd door
  SOpened -> Just door
  SClosed -> Just (openDoor door)

-- Continue with Singleton3.hs

-- Ignore (this was written after finishing the chapters)
--
-- type SomeDoor = Sigma DoorState (TyCon1 Door)
--
-- openAnyDoor' :: Int -> SomeDoor -> SomeDoor
-- openAnyDoor' passwd (s :&: door) = case s of
--   SLocked -> case unlockDoor passwd door of
--     Nothing -> (s :&: door)
--     Just unlocked -> sing :&: (openDoor unlocked)
--   SOpened -> (s :&: door)
--   SClosed -> sing :&: (openDoor door)
