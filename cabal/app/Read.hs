{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-----------------------------------------------------------
-- Author: Tom Harding
--
-- When we want to use an externally-provided package (such as logging,
-- database connections, etc) in our work code, it might require some initial
-- config that we traditionally store in our environment.
--
-- We might even require something like a database connection or file handler
-- /throughout/ the lifetime of the code, in order to make queries or similar.
--
-- Inevitably, we end up repeating the same chunk of code (with a little help
-- from 'generic-lens'):
--
-- @
--   whatever = do
--     connection <- fmap (getTyped @Connection) ask
--     query connection "SELECT * FROM users WHERE name = ''; drop table users --' LIMIT 1"
--     ...
-- @
--
-- This is fine, but starts to make a mess when we have many instances of this,
-- or even multiple connections to track. So, like good programmers, we write
-- some application-specific helper functions:
--
-- @
--   query :: String -> IO ()
--   query body = do
--     connection <- fmap (getTyped @Connection) ask
--     query connection body
-- @
--
-- The @read@ function below is for exactly this: we pass to 'read' a
-- __list__ of the types we want to provide through 'MonadReader', along with
-- the function, and we get back the updated function. So, in this contrived
-- example:
--
-- @
--   query :: String -> IO ()
--   query = read @'[Connection] query'
--     where query' :: Connection -> String -> IO ()
-- @
--
-- The code below also doesn't care /where/ in the function we encounter the
-- types mentioned, or in what order! As long as the final result is wrapped in
-- some 'm' for which 'MonadReader r m' and 'HasType ThingThatIWant r' hold,
-- we're all good!

module Read where

import Control.Applicative (empty, (<|>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Generics.Product (HasType (..))
import Data.Kind (Type)
import Data.List (permutations, tails)
import Data.Semigroup (Sum (..))
import GHC.Float (double2Float)
import GHC.Generics (Generic (..))
import Prelude hiding (Read, read)

-- | The @read@ function takes a __type-applied__ list of the arguments to
-- supply from the 'MonadReader' instance, along with the function, and returns
-- the function with those arguments removed.
--
-- The functional dependencies are here to help with type inference: if I know
-- the function's type and the arguments for which I want to defer to
-- 'MonadReader', I can uniquely determine the resulting function. This is
-- lucky, because it otherwise wouldn't be clear to GHC what the result of this
-- function would be!
--
-- 'm' type variable can be removed and the example will still compile.
class
  Read (xs :: [Type]) (m :: Type -> Type) (i :: Type) (o :: Type)
    | i xs -> o,
      i -> m,
      o -> m
  where
  read :: i -> o

type family (x :: Type) `Elem` (xs :: [Type]) :: Bool where
  x `Elem` '[] = 'False
  x `Elem` (x ': xs) = 'True
  x `Elem` (y ': xs) = x `Elem` xs

instance
  (Read' (x `Elem` xs) xs m (x -> b) output) =>
  Read xs m (x -> b) output
  where
  read = read' @(x `Elem` xs) @xs @m

-- We have iterated over all `input` parameters.
-- Only remaining is the return `m x`, just output `m x`
instance
  {-# INCOHERENT #-}
  (input ~ m whatever, input ~ output) =>
  Read xs m input output
  where
  read = id

-- | This class is identical to 'Read', but for one extra parameter: a @flag@,
-- telling us whether the current parameter in question is one of the ones to
-- move to the @m@ via 'MonadReader'.
class
  Read' (flag :: Bool) (xs :: [Type]) (m :: Type -> Type) (i :: Type) (o :: Type)
    | i xs flag -> o,
      i -> m,
      o -> m
  where
  read' :: i -> o

-- Not in the reader, skip.
instance
  (input ~ (a -> b), output ~ (a -> c), Read xs m b c) =>
  Read' 'False xs m input output
  where
  read' a2b = \a -> read @xs @m (a2b a)

-- In reader, use 'Push' to push it into the 'MonadReader m'
instance
  (input ~ (a -> b), output ~ c, Read xs m b c, Push a b) =>
  Read' 'True xs m input output
  where
  read' = read @xs . push

-- It takes the first parameter of a function, and "removes" it by "pushing it into the @m@".
class Push (input :: Type) (output :: Type) where
  push :: (input -> output) -> output

-- | When we deal with a function like @input -> x -> output@, and we're trying
-- to push @input@, the result is @x -> output@. So, we 'flip' the arguments to
-- make @x -> input -> output@, and then 'push' @input -> output@, which gives
-- us our recursion.
instance (Push input output) => Push input (x -> output) where
  push f = \x -> push \i -> f i x

-- | When we reach the end, we'll have something like @input -> m output@, and
-- we'll want to land at @m output@. Now we know we have exactly one argument,
-- we can 'ask' for it, and pass it to the function.
instance
  {-# INCOHERENT #-}
  ( MonadReader r m,
    HasType input r,
    output ~ m something
  ) =>
  Push input output
  where
  push f = ask >>= f . getTyped

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

newtype Connection = Connection {unConnection :: String}
  deriving (Show)

data Logger = Logger
  deriving (Show)

data Env = Env
  { logger :: Logger,
    connection :: Connection
  }
  deriving (Show, Generic)

example :: (MonadReader Env m, MonadIO m) => Int -> m ()
example = read @'[Logger, Connection] go
  where
    go :: (MonadIO m) => Logger -> Int -> Connection -> m ()
    go _logger port conn = liftIO $ putStrLn $ show (unConnection conn) ++ ":" ++ show port

main :: IO ()
main = do
  let env = Env {connection = Connection "foo", logger = Logger}
  runReaderT (example 8080) env
