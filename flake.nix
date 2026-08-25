{
  description = "JShark: a typed subset of JavaScript";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f:
        builtins.listToAttrs (map (system: {
          name = system;
          value = f system;
        }) systems);

      overlay = { profiling ? false }: final: prev:
        let
          hlib = prev.haskell.lib;
          compose = f: g: x: f (g x);
          apply = prev.lib.foldl' compose prev.lib.id;
        in {
          haskellPackages = (prev.haskell.packages.ghc914.extend (hself: hsuper: {
            jshark = apply (
              [ hlib.dontHaddock hlib.dontCheck ]
              ++ (if profiling
                then [ hlib.enableLibraryProfiling hlib.enableExecutableProfiling ]
                else [ hlib.disableLibraryProfiling hlib.disableExecutableProfiling ])
            ) (hself.callCabal2nix "jshark" self { });
          }));
        };

      pkgsFor = { system, profiling ? false }:
        import nixpkgs {
          inherit system;
          overlays = [ (overlay { inherit profiling; }) ];
        };
    in {
      overlays.default = overlay { };

      packages = forAllSystems (system: rec {
        default = jshark;
        jshark = (pkgsFor { inherit system; }).haskellPackages.jshark;
        jshark-profiled = (pkgsFor { inherit system; profiling = true; }).haskellPackages.jshark;
      });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor { inherit system; };
          llvm = pkgs.llvm_21;
        in {
          default = pkgs.haskellPackages.shellFor {
            packages = p: [ p.jshark ];
            nativeBuildInputs = with pkgs; [
              cabal-install
              esbuild
              bun
              fourmolu
              zig
              llvm
            ];
            shellHook = ''
              export PATH="${llvm}/bin:$PATH"
            '';
          };
        });
    };
}
