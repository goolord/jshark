{ nixpkgs ? import ./nixpkgs.nix {} # nix package set we're using
, compiler ? "ghc863" # compiler we are using
, profiling ? false # Whether or not to enable library profiling
}:

with rec {
  overlay = import ./overlay.nix { inherit profiling; };

  pkgs = import nixpkgs {
    config = {
      allowUnfree = true; 
    };
    overlays = [ overlay ];
  };

  make = name: pkgs.haskell.packages.${compiler}.${name};

  jsedsl = make "jsedsl";
};

rec {
  inherit pkgs;
  inherit jsedsl;
}
