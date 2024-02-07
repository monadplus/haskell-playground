-- Author: Mark Hopkins
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module AndPatterns where

pattern DivBy3 :: (Integral a) => a
pattern DivBy3 <- ((`mod` 3) -> 0)

pattern DivBy5 :: (Integral a) => a
pattern DivBy5 <- ((`mod` 5) -> 0)

pattern DivBy7 :: (Integral a) => a
pattern DivBy7 <- ((`mod` 7) -> 0)

-- From @x -> a@ to @(x,x) -> (a,b)@
--
-- You could manually do it this way
--
-- @
-- pattern MultipleOf3And5 :: (Integral a) => a
-- pattern MultipleOf3And5 <- (\x -> (x `mod` 3, x `mod` 5) -> (0, 0))
-- @
--
-- but it doesn't compose
pattern (:&:) :: b -> b -> b
pattern (:&:) i j <- (\x -> (x, x) -> (i, j))

fizzBuzzBop :: Int -> String
fizzBuzzBop = \case
  DivBy3 :&: DivBy5 :&: DivBy7 -> "fizzbuzzbop"
  DivBy3 :&: DivBy5 -> "fizzbuzz"
  DivBy3 :&: DivBy7 -> "fizzbop"
  DivBy5 :&: DivBy7 -> "buzzbop"
  DivBy3 -> "fizz"
  DivBy5 -> "buzz"
  DivBy7 -> "bop"
  i -> show i
