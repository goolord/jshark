{ compiler ? "ghc864"
, nixpkgs ? (import ./nix/nixpkgs.nix { inherit compiler; })
}:
with {
  drv = nixpkgs.haskellPackages.jsedsl;
};
drv
