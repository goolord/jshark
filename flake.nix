{
  description = "JShark: a typed subset of JavaScript";

  inputs = {
    # GHC 8.6 matches jshark's cabal bounds (base < 4.13). The previous
    # layer-3-communications/nixpkgs pin is no longer public.
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-20.03";
      flake = false;
    };
    quantification = {
      url = "github:andrewthad/quantification/aa6582f57fe2b68d8ba5d94325b53aca3e30ceea";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, quantification }:
    let
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f:
        builtins.listToAttrs (map (system: {
          name = system;
          value = f system;
        }) systems);

      overlay = { profiling ? false }: final: prev:
        let
          inherit (prev) lib;
          hlib = prev.haskell.lib;
          compose = f: g: x: f (g x);
          apply = lib.foldl' compose lib.id;
        in {
          haskell = prev.haskell // {
            packages = prev.haskell.packages // {
              ghc865 = prev.haskell.packages.ghc865.override {
                overrides = hself: hsuper: {
                  quantification =
                    hself.callCabal2nix "quantification" quantification { };

                  semirings = hsuper.semirings_0_3_1_1 or hsuper.semirings;

                  jshark = apply (
                    [ hlib.dontHaddock hlib.dontCheck ]
                    ++ (if profiling
                      then [ hlib.enableLibraryProfiling hlib.enableExecutableProfiling ]
                      else [ hlib.disableLibraryProfiling hlib.disableExecutableProfiling ])
                  ) (hself.callCabal2nix "jshark" self { });
                };
              };
            };
          };
        };

      pkgsFor = { system, profiling ? false }:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [ (overlay { inherit profiling; }) ];
        };

      hsFor = args: (pkgsFor args).haskell.packages.ghc865;
    in {
      overlays.default = overlay { };

      packages = forAllSystems (system: rec {
        default = jshark;
        jshark = (hsFor { inherit system; }).jshark;
        jshark-profiled = (hsFor { inherit system; profiling = true; }).jshark;
      });

      devShells = forAllSystems (system:
        let hs = hsFor { inherit system; };
        in {
          default = hs.shellFor {
            packages = p: [ p.jshark ];
            nativeBuildInputs = [ hs.cabal-install ];
          };
        });
    };
}
