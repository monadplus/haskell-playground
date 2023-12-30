module Main where

import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Prelude hiding (print)

print1 :: HasCallStack => IO ()
print1 = print2 "print2 w/o HasCallStack"

print2 :: String -> IO ()
print2 = print3

print3 :: HasCallStack => String -> IO ()
print3 msg = do
  putStrLn msg
  putStrLn (prettyCallStack callStack)

main :: IO ()
main = print1
