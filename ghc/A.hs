module A where

import Control.Lens (sumOf)

-- Case 1

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
{-# NOINLINE safeDiv #-}

divSquares :: Int -> Int -> Maybe Int
divSquares x y = (x * x) `safeDiv` (y * y)

{-
divSquares
  = \ (x_aBS :: Int) (y_aBT :: Int) ->
      case y_aBT of { I# x1_aWK ->
      safeDiv
        (case x_aBS of { I# x2_X1 -> I# (*# x2_X1 x2_X1) })
        (I# (*# x1_aWK x1_aWK))
      }

-- has been optimized into this

divSquares
  = \ (x_aBR :: Int) (y_aBS :: Int) ->
      case y_aBS of { I# x1_aWJ ->
      $wsafeDiv
        (case x_aBR of { I# x2_X2 -> I# (*# x2_X2 x2_X2) })
        (*# x1_aWJ x1_aWJ)
      }

-- thanks to the worker-wrapper transformation.

-- Note, both are compiled with the same optimization profile
-- the main difference is that the first one had the worker-wrapper-transformation off.

-- How is the optimized version better?
-- Note, the first version does 2 allocations
-- while the second version only does 1

-- What enables the optimizer to remove an allocation? Let's see.

-- Given the original divSquares

divSquares
  = \ (x_aBS :: Int) (y_aBT :: Int) ->
      case y_aBT of { I# x1_aWK ->
      safeDiv
        (case x_aBS of { I# x2_X1 -> I# (*# x2_X1 x2_X1) })
        (I# (*# x1_aWK x1_aWK))
      }

-- Inline safeDiv (the wrapper can always be inlined cross-module because they are stored in the interface file)

safeDiv
  = \ (ds_sX8 :: Int) (ds1_sX9 :: Int) ->
      case ds1_sX9 of { I# ww_sXb -> $wsafeDiv ds_sX8 ww_sXb }

-- to get

divSquares = \ (x_aBS :: Int) (y_aBT :: Int) ->
  case y_aBT of {
    I# x1_aWK ->
      case (I# (*# x1_aWK x1_aWK)) of {
        I# ww_sXb ->
          $wsafeDiv
            (case x_aBS of { I# x2_X1 -> I# (*# x2_X1 x2_X1) })
            ww_sXb
      }
  }

-- 'case (I# X_1) of { I# X_2 -> Y[X_2]}' can be simpliefied to 'Y[X_1]'

divSquares = \ (x_aBS :: Int) (y_aBT :: Int) ->
  case y_aBT of {
    I# x1_aWK ->
      $wsafeDiv
        (case x_aBS of { I# x2_X1 -> I# (*# x2_X1 x2_X1) })
        (*# x1_aWK x1_aWK)
  }

-- which matches the optimized version of 'divSquares'
-}

-- Case 2

data User = User
  { name :: String,
    age :: Int,
    isAdmin :: Bool
  }

isPrivileged :: User -> Bool
isPrivileged (User {age, isAdmin}) =
  isAdmin || age > 60
{-# NOINLINE isPrivileged #-}

displayUser :: User -> String
displayUser user
  | isPrivileged user = "+" ++ name user
  | otherwise = name user
