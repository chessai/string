with (import ./default.nix {});

hsPkgs.shellFor {
  packages = _: [ string ];

  withHoogle = false;

  buildInputs = with pkgs; [
    cabal-install
    cabal2nix

    hsPkgs.ghcid
  ];
}
