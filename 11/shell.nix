with import <nixos> {};
pkgs.mkShell {
  buildInputs = with pkgs.idrisPackages; [
    (with-packages [
      lightyear
    ])
  ];
}
