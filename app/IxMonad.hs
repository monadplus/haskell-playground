{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module IxMonad where

import Control.Monad.Indexed
import Control.Monad.Trans.Ix
import Data.Coerce
import Fcf hiding (type (+))
import GHC.TypeLits (Nat, type (+))
import Language.Haskell.DoNotation
import System.IO hiding (Handle, openFile)
import qualified System.IO as IO
import Prelude hiding (Monad (..), pure)

data LinearState = LinearState
  { linearNextKey :: Nat,
    linearOpenKeys :: [Nat]
  }

newtype Linear s (i :: LinearState) (j :: LinearState) a = Linear {unsafeRunLinear :: IO a}
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad) via (Ix IO)

lift :: IO a -> Linear s i i a
lift = coerce

openFile ::
  FilePath ->
  IOMode ->
  Linear
    s
    ('LinearState next open)
    ('LinearState (next + 1) (next ': open))
    (Handle s next)
openFile = coerce IO.openFile

-- 's' is a rigid skolem type variable
-- and prevents leaking the value outside 'runLinear'
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
    ('LinearState next open)
    ('LinearState next (Eval (Close key open)))
    ()
closeFile = coerce IO.hClose

runLinear ::
  (forall s. Linear s ('LinearState 0 '[]) ('LinearState n '[]) a) -> IO a
runLinear = coerce

-- OK
main :: IO ()
main = runLinear $ do
  h <- openFile "/etc/passwd" ReadMode
  lift $ IO.hPutStr (coerce h) "\nroot:x:0:0::/root:/bin/bash"
  closeFile h

-- BOGUS: we forgot to close the file handle
--
-- main :: IO ()
-- main = runLinear $ do
--   h <- openFile "/etc/passwd" ReadMode
--   lift $ IO.hPutStr (coerce h) "\nroot:x:0:0::/root:/bin/bash"
