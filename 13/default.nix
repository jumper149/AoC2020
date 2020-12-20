{ stdenv, lib, makeWrapper, haskellPackages }:
haskellPackages.callCabal2nix "main" ./. {}
