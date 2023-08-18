{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module DependentFieldsStruct where

{- Example showcasing dependent enum fields on a struct -}

import Data.Kind
import Data.Text

data Cat
  = A
  | B

type FT :: Cat -> Type
data FT c where
  AA :: FT 'A
  AB :: FT 'B
  BA :: FT 'B
  BB :: FT 'B

type SubCat :: Cat -> Type
data SubCat c where
  X :: Int -> Text -> SubCat 'A
  Y :: Double -> Double -> SubCat 'B

data Obj = forall c.
  Obj
  { ft :: FT c,
    iDontCare :: Int,
    subCat :: SubCat c
  }

obj :: Obj
obj =
  Obj
    { ft = AA,
      iDontCare = 0,
      subCat = X 0 "Hi"
    }

objIsConsistent :: Obj -> ()
objIsConsistent Obj {..} =
  case ft of
    AA ->
      case subCat of
        X _ _ -> ()
    AB ->
      case subCat of
        Y _ _ -> ()
    _ -> ()
