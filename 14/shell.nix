with import <nixos> {};
pkgs.mkShell {
  buildInputs = [
    (callPackage (import ./idris2.nix) {})
  ];
}
