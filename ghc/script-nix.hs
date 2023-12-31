#!/usr/bin/env nix-shell 
#!nix-shell -i runghc -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ mwc-random ])"
#!nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/d373d80b1207d52621961b16aa4a3438e4f98167.tar.gz
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.ST
import Data.Vector.Unboxed
import System.Random.MWC

main = do
  vs <- withSystemRandom $
    \(gen :: GenST s) -> uniformVector gen 20 :: ST s (Vector Int)
  print vs
