with import <nixos> {};
pkgs.mkShell {
  buildInputs = with pkgs; [
    idris
  ];
}
