with import <nixpkgs> {};
let
  main = haskellPackages.callCabal2nix "main" ./. {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      cabal-install
      ghcid
      haskellPackages.haskell-language-server
      haskellPackages.implicit-hie
      hlint
    ];
    inputsFrom = [
      main.env
    ];
  }
