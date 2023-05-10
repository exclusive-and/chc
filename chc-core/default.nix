
# Package       : chc-core
# Description   : 
# Build         : GHC 9.4.2 using Cabal2Nix

{ nixpkgs ? import <nixpkgs> {} }:
    with nixpkgs.haskell.packages.ghc942;
    callCabal2nix "chc-core" ./. {}
