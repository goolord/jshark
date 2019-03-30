{ }:

let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    owner = "layer-3-communications";
    repo = "nixpkgs";
    rev = "711cfa0b6612ceb3d727fd14cc92324eb60461b6";
    sha256 = "0gz5ybq00nif578bbricb4d640m1szh06xwrdk7q97hw4fy9rkjh";
  };

in
  nixpkgs