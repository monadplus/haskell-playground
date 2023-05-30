{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module DecideConstr where

-- Source: https://discourse.haskell.org/t/deciding-constraints-for-a-gadt-tag-datatypes/6239

import Data.Kind

type Dict :: Constraint -> Type
data Dict c where
  Dict :: a => Dict a

data TRep a where
  TInt :: TRep Int
  TList :: TRep a -> TRep [a]
  TFun :: TRep a -> TRep b -> TRep (a -> b)

-- decideEq :: TRep a -> Maybe (Dict (Eq a))
-- decideEq TInt = pure Dict
-- decideEq (TList a) = do
--   Dict <- decideEq a
--   pure Dict
-- decideEq (TFun _a _b) = Nothing

-- decideConstr :: forall c a. Typeable c => TRep a -> Maybe (Dict (c a))
decideConstr ::
  forall c a.
  () =>
  c Int =>
  (forall x. c x => c [x]) =>
  TRep a ->
  Maybe (Dict (c a))
decideConstr = \case
  TInt ->
    pure Dict
  TList rep -> do
    Dict <- decideConstr @c rep
    pure Dict
  TFun {} ->
    Nothing

decideEq :: TRep a -> Maybe (Dict (Eq a))
decideEq = decideConstr @Eq
