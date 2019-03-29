{ compiler }:
with rec {
  fetchNixpkgs = import ./fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "382333b25e0d6fbac746d91224ed6b805b6407a0";
    sha256 = "11g2kaj087c4yaxaszqjksccfa9s5968yi0brd0jk1gw8lznj44q";
  };
};
import nixpkgs {
  config = {
    packageOverrides = super: let self = super.pkgs; in {
      haskellPackages = super.haskell.packages.${compiler}.override {
        overrides = import ./overrides.nix { pkgs = self; };
      };
    };
  };
  overlays = [ ];
}
