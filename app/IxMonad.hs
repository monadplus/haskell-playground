{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IxMonad where

import Control.Monad.Indexed
import Control.Monad.Indexed.Trans
import Control.Monad.Trans.Ix
import Data.Coerce
import Data.Kind (Type)
import Fcf hiding (type (+))
import GHC.TypeLits (Nat, type (+))
import Language.Haskell.DoNotation
import System.IO hiding (Handle, openFile)
import qualified System.IO as IO
import Prelude hiding (Monad (..), pure)

instance IxMonadTrans Ix where
  ilift = coerce

data LinearState = LinearState
  { linearNextKey :: Nat,
    linearOpenKeys :: [Nat]
  }

type Linear :: Type -> (Type -> Type) -> LinearState -> LinearState -> Type -> Type
newtype Linear s m i j a = Linear {unsafeRunLinear :: m a}
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad) via (Ix m)
  deriving (IxMonadTrans) via Ix

openFile ::
  FilePath ->
  IOMode ->
  Linear
    s
    IO
    ('LinearState next open)
    ('LinearState (next + 1) (next ': open))
    (Handle s next)
openFile = coerce IO.openFile

newtype Handle s key = Handle {unsafeGetHandle :: IO.Handle}

type IsOpen (key :: k) (ts :: [k]) =
  IsJust =<< Find (TyEq key) ts

type Close (key :: k) (ts :: [k]) =
  Filter (Not <=< TyEq key) ts

closeFile ::
  Eval (IsOpen key open) ~ 'True =>
  Handle s key ->
  Linear
    s
    IO
    ('LinearState next open)
    ('LinearState next (Eval (Close key open)))
    ()
closeFile = coerce IO.hClose

runLinear ::
  (forall s. Linear s m ('LinearState 0 '[]) ('LinearState n '[]) a) -> m a
runLinear = coerce

exampleOK :: IO ()
exampleOK = runLinear $ do
  h <- openFile "/etc/passwd" ReadMode
  ilift $ IO.hPutStr (coerce h) "\nroot:x:0:0::/root:/bin/bash"
  closeFile h

-- We forgot to close the file handle
--
-- exampleKO :: IO ()
-- exampleKO = runLinear $ do
--   h <- openFile "/etc/passwd" ReadMode
--   ilift $ IO.hPutStr (coerce h) "\nroot:x:0:0::/root:/bin/bash"
