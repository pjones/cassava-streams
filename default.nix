{ sources ? import ./nix/sources.nix, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }, ghc ? "default" }:

nix-hs {
  cabal = ./cassava-streams.cabal;
  flags = [ "tutorial" ];
  compiler = ghc;
}
