with import <nixos> {};
pkgs.mkShell {
  buildInputs = [
    pkgs.idris2
  ];
}
