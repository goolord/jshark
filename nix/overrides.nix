{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };
     
with pkgs.haskell.lib;

{
  jsedsl = (
    with rec {
      jsedslSource = pkgs.lib.cleanSource ../.;
      jsedslBasic  = self.callCabal2nix "jsedsl" jsedslSource { };
    };
    overrideCabal jsedslBasic (old: {
    })
  );
}
