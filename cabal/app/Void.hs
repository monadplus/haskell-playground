{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Void where

import Data.Void

-- We still need to pattern-match against Left because
-- laziness allows that branch to still be valid even if Void is unhabited
-- i.e. it could still be populated by bottom values.
f :: Either Void Int -> Int
f (Right x) = x
f (Left void) = absurd void

-- But if we help the strictness analysis a bit
data Either' e a = Left' !e | Right' a

-- GHC can discard the branch
f' :: Either' Void Int -> Int
f' (Right' x) = x
