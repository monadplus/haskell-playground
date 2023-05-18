{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module HasC where

import Data.Kind
import Data.Typeable (Typeable, cast)

type Has :: (Type -> Constraint) -> Type
data Has c where
  Has :: c t => t -> Has c

elimHas ::
  (forall a. c a => a -> r) ->
  Has c ->
  r
elimHas f (Has a) = f a

type HasShow = Has Show

type Dynamic = Has Typeable

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimHas cast

isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty = (== mempty)

class (Eq a, Monoid a) => EqMonoid a

instance (Eq a, Monoid a) => EqMonoid a

example :: IO ()
example = do
  let dyn = Has ([] @Int)
  print $ elimHas @EqMonoid isMempty dyn
