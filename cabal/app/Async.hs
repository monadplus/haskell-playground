module Async where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

f :: Int -> TVar Int -> IO ()
f n tvar =
  replicateM_ n $
    atomically $
      modifyTVar' tvar (+ 1)

main :: IO ()
main = do
  tvar <- newTVarIO 0
  let times = 1000000
  replicateConcurrently_ 8 (f times tvar)
  print =<< readTVarIO tvar
