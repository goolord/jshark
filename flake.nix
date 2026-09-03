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
          tune = [ hlib.dontHaddock hlib.dontCheck ]
            ++ (if profiling
              then [ hlib.enableLibraryProfiling hlib.enableExecutableProfiling ]
              else [ hlib.disableLibraryProfiling hlib.disableExecutableProfiling ]);
        in {
          haskellPackages = (prev.haskell.packages.ghc914.extend (hself: hsuper:
            let
              mkPkg = name: dir:
                apply tune (hself.callCabal2nix name dir { });
            in {
              jshark = mkPkg "jshark" ./packages/jshark;
              jshark-lucid = mkPkg "jshark-lucid" ./packages/jshark-lucid;
              jshark-bindgen = mkPkg "jshark-bindgen" ./packages/jshark-bindgen;
              jshark-hotreload = mkPkg "jshark-hotreload" ./packages/jshark-hotreload;
              jshark-examples = mkPkg "jshark-examples" ./examples;
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
        jshark-lucid = (pkgsFor { inherit system; }).haskellPackages.jshark-lucid;
        jshark-bindgen = (pkgsFor { inherit system; }).haskellPackages.jshark-bindgen;
        jshark-hotreload = (pkgsFor { inherit system; }).haskellPackages.jshark-hotreload;
        jshark-examples = (pkgsFor { inherit system; }).haskellPackages.jshark-examples;
        jshark-profiled = (pkgsFor { inherit system; profiling = true; }).haskellPackages.jshark;
      });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor { inherit system; };
          llvm = pkgs.llvm_20;
        in {
          default = pkgs.haskellPackages.shellFor {
            packages = p: [
              p.jshark
              p.jshark-lucid
              p.jshark-bindgen
              p.jshark-hotreload
              p.jshark-examples
            ];
            nativeBuildInputs = with pkgs; [
              cabal-install
              esbuild
              bun
              biome
              fourmolu
              zig
              llvm
            ];
            shellHook = ''
              mkdir -p .nix-llvm-wrappers
              ln -sf ${llvm}/bin/opt .nix-llvm-wrappers/opt-20
              ln -sf ${llvm}/bin/llc .nix-llvm-wrappers/llc-20
              export PATH="$PWD/.nix-llvm-wrappers:${llvm}/bin:$PATH"
            '';
          };
        });
    };
}
