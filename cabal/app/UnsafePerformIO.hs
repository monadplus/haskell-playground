module UnsafePerformIO where

import Data.IORef
import GHC.IO

unsafeRef :: IORef [a]
unsafeRef = unsafePerformIO $ newIORef []
{-# NOINLINE unsafeRef #-}

safeRef :: IO (IORef [a])
safeRef = newIORef []

-- ok :: IO ()
-- ok = do
--   ref <- safeRef
--   writeIORef ref [42 :: Int]
--   bang <- readIORef ref
--   print (bang :: [Char])

ko :: IO ()
ko = do
  writeIORef unsafeRef [42 :: Int]
  bang <- readIORef unsafeRef
  print (bang :: [Char])
