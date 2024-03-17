{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dynamic where

import Data.Dynamic -- uses Type.Reflection
import Data.Word
import Prelude hiding (any)

example :: Word16
example = maybe zero16 (`fromDyn` zero16) (dynApply widenDyn any)
  where
    any :: Dynamic
    any = toDyn (0 :: Word8)

    widen :: Word8 -> Word16
    widen = fromIntegral

    widenDyn :: Dynamic
    widenDyn = toDyn widen

    zero16 :: Word16
    zero16 = 0
