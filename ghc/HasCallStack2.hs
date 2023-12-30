module Main where

import GHC.Stack (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import Prelude hiding (print)

print1 :: HasCallStack => IO ()
print1 = do
  print2 "all stack with HasCallStack"
  withFrozenCallStack (print2 "all stack with HasCallStack + withFrozenCallStack")

print2 :: HasCallStack => String -> IO ()
print2 = print3

print3 :: HasCallStack => String -> IO ()
print3 msg = do
  putStrLn msg
  putStrLn (prettyCallStack callStack)

main :: IO ()
main = print1
