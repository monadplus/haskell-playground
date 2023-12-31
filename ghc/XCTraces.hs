module Main where

import Control.Exception (Exception (..), SomeException (SomeException), catch, evaluate)

foo :: [a] -> a
foo xs =
  let xs' = id xs
   in bar xs'

bar :: [a] -> a
bar = head

-- `-xc`: print stack traces even if the exception is captured.
main :: IO ()
main = do
  print $ foo [1 .. 100]
  x <-
    evaluate (foo []) `catch` \(e :: SomeException) -> do
      return 0
  print @Int x
