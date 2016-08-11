with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "cassava-streams";

  buildInputs = [
    # GHC:
    haskell.packages.lts-6_7.ghc

    # Non-Haskell Dependencies:
    zlib
  ];
}
