-- https://github.com/haskellfoundation/hs-opt-handbook.github.io
{-# LANGUAGE BangPatterns #-}
-- Need to disable optimizations because GHC will recognize and perform
-- let-floating for us!
{-# OPTIONS_GHC -O0 -ddump-simpl -ddump-to-file -ddump-stg-final #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

import Control.Concurrent (threadDelay)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.List (foldl')
import Debug.Trace (traceMarker, traceMarkerIO)
import System.Random (mkStdGen)
import System.Random.Stateful (newIOGenM, uniformRM)

-- lazy_mean :: [Double] -> Double
-- lazy_mean xs = traceMarker "Begin: lazy_mean" $ s / fromIntegral ln
--   where
--     (s, ln) = foldl step (0, 0) xs
--     step (s, ln) a = (s + a, ln + 1)

stricter_mean :: [Double] -> Double
stricter_mean xs = (traceMarker "s" s) / fromIntegral (traceMarker "ln" ln)
  where (s, ln)        = foldl' step (0,0) xs
        step (!s, !ln) a = (s + a, ln + 1)

strict_mean :: [Double] -> Double
strict_mean xs = traceMarker "Begin: strict_mean" $ s / fromIntegral ln
  where
    (s, ln) = foldl' step (0, 0) xs
    step (!s, !ln) a = (s + a, ln + 1)

-- \$ cabal bench pointerChasing --benchmark-options='+RTS -hy -l-agu -i0.0001 -RTS' && eventlog2html pointerChasing.eventlog && firefox pointerChasing.eventlog.htm
main :: IO ()
main = do
  let wait = threadDelay 100000
  wait
  traceMarkerIO "Bench Initialization"
  !seed <- newIOGenM (mkStdGen 1729)
  let genValue = fmap force uniformRM (0,500000) seed >>= evaluate
  test_values <- replicateM 50000 genValue            >>= evaluate . force
  traceMarkerIO "End Bench Initialization"
  wait
  -- print $! lazy_mean test_values
  -- traceMarkerIO "End lazy_mean"
  -- wait
  print $! stricter_mean test_values
  traceMarkerIO "End stricter_mean"
  wait
  print $! strict_mean test_values
  traceMarkerIO "End strict_mean"
