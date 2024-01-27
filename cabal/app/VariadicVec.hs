{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module VariadicVec where

import Data.Kind (Type)

type Vec :: [Type] -> Type
data Vec xs where
  VNil :: Vec '[]
  VCons :: x -> Vec xs -> Vec (x ': xs)

instance Show (Vec '[]) where show VNil = "VNil"

instance (Show (Vec xs), Show x) => Show (Vec (x ': xs)) where
  showsPrec p (VCons x xs) =
    showParen (p > 10) $
      showString "VCons " . showsPrec 11 x . showChar ' ' . showsPrec 11 xs

-- type family Rev (acc :: [a]) (xs :: [a]) :: [a] where
--   Rev acc '[] = acc
--   Rev acc (x ': xs) = Rev (x ': acc) xs
--
-- rev :: Vec acc -> Vec xs -> Vec (Rev acc xs)
-- rev acc VNil = acc
-- rev acc (VCons x xs) = rev (VCons x acc) xs
class Rev acc xs acc' where
  rev :: Vec acc -> Vec xs -> Vec acc'

instance (acc ~ acc') => Rev acc '[] acc' where
  rev acc VNil = acc

instance (Rev (x ': acc) xs acc') => Rev acc (x ': xs) acc' where
  rev acc (VCons x xs) = rev (VCons x acc) xs

class MkVec xs a where
  mkVec :: Vec xs -> a

instance (MkVec (x ': xs) a) => MkVec xs (x -> a) where
  mkVec acc x = mkVec (VCons x acc)

data End = End

instance {-# INCOHERENT #-} (a ~ Vec xs', Rev '[] xs xs') => MkVec xs (End -> a) where
  mkVec acc End = rev VNil acc

end :: End
end = End

example :: IO ()
example = do
  let vec = mkVec VNil True (1 :: Int) 'a' () end
  print vec
