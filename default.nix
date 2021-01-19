{ pkgs ? import (builtins.fetchTarball {
    name = "nixos-stable-20.09";
    url = "https://github.com/nixos/nixpkgs/archive/3d8fa5f72158b866804ab30f42f6937bf25e420f.tar.gz";
    sha256 = "18l8pdwz5q1fhp6vya0xg2kppfn9hfl8qyjkffklwynkyj61v6ax";
  }) {
    config = {};
    overlays = [
      (self: super: {
        simdjson = self.callPackage ./simdjson.nix {};

        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc8102 = super.haskell.packages.ghc8102.override {
              overrides = hself: hsuper: with super.haskell.lib; rec {
                byteslice = hself.callCabal2nix "byteslice" (super.fetchFromGitHub {
                  owner = "andrewthad";
                  repo = "byteslice";
                  rev = "0.2.4.0";
                  sha256 = "1m1fsqj4cbfdbkjmqdspjadgm4fhdr3g6j7yfllm7v11mg5sw3ia";
                }) {};

                hedgehog-classes = unmarkBroken hsuper.hedgehog-classes;
              };
            };
          };
        };

      })
    ];
  }
}:

rec {
  inherit pkgs;
  hsPkgs = pkgs.haskell.packages.ghc8102;

  string = hsPkgs.callCabal2nix "string" ./. {
    inherit (pkgs) simdjson;
  };
}
