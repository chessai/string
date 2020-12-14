{ pkgs ? import <nixpkgs> {}
}:

rec {
  inherit pkgs;
  hsPkgs = pkgs.haskell.packages.ghc8102;

  simdjson = pkgs.callPackage ./simdjson.nix {};

  string = hsPkgs.callCabal2nix "string" ./. {
    inherit simdjson;
  };
}
