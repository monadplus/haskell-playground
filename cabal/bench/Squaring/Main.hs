module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Semigroup
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import Gauge.Main

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

replicate :: Int -> String -> String
replicate n = concat . Prelude.replicate n

replicate' :: Int -> String -> String
replicate' = Semigroup.mtimesDefault

replicate'' :: Int -> String -> String
replicate'' n str = go n str
  where
    -- This is much slower..
    -- go :: Int -> String -> String -> String
    -- go n str z
    --   | even n = go (n `quot` 2) (str <> str) z
    --   | n == 1 = str <> z
    --   | otherwise = go (n `quot` 2) (str <> str) (str <> z)
    go :: Int -> String -> String
    go n str
      | even n = go (n `quot` 2) (str <> str)
      | n == 1 = str
      | otherwise = str <> go (n `quot` 2) (str <> str)
    {-# INLINE go #-}

replicateEndo :: Int -> String -> String
replicateEndo n str =
  let f = Semigroup.mtimesDefault n (Endo (str ++))
   in appEndo f ""

replicateByteString :: Int -> String -> String
replicateByteString n str = LBSC.unpack $ Builder.toLazyByteString $ go n (Builder.stringUtf8 str)
  where
    go n bs
      | even n = go (n `quot` 2) (bs <> bs)
      | n == 1 = bs
      | otherwise = bs <> go (n `quot` 2) (bs <> bs)
    {-# INLINE go #-}

replicateText :: Int -> String -> String
replicateText n = Text.unpack . Text.replicate n . Text.pack

main :: IO ()
main = do
  let times = 10000
  defaultMain
    [ bench "replicate string naive" $ nf (Main.replicate times) "hello",
      bench "replicate string" $ nf (Main.replicate' times) "hello",
      bench "replicate manual string" $ nf (Main.replicate'' times) "hello",
      bench "replicate endo" $ nf (Main.replicateEndo times) "hello",
      bench "replicate bytestring" $ whnf (Main.replicateByteString times) "hello",
      bench "replicate text" $ nf (Main.replicateText times) "hello"
    ]
