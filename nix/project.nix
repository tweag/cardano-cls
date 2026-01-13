pkgs:
let
  defaultCompiler = "ghc910";
  fourmoluVersion = "0.19.0.0";
  cabalGildVersion = "1.6.0.2";
in {
  inherit fourmoluVersion cabalGildVersion;

  cardanoCanonicalLedger = pkgs.haskell-nix.cabalProject' {
    src = ../.;

    name = "cardano-canonical-ledger";

    compiler-nix-name = pkgs.lib.mkDefault defaultCompiler;

    flake.variants = {
      "${defaultCompiler}" = { };
      "${defaultCompiler}-profiled".modules = [{
        enableProfiling = true;
        enableLibraryProfiling = true;
      }];
      "${defaultCompiler}-coverage".modules = [{
        packages.scls-format.components.library.doCoverage = true;
        packages.scls-core.components.library.doCoverage = true;
        packages.scls-cbor.components.library.doCoverage = true;
        packages.scls-cardano.components.library.doCoverage = true;
        packages.mempack-scls.components.library.doCoverage = true;
        packages.scls-util.components.library.doCoverage = true;
        packages.merkle-tree-incremental.components.library.doCoverage = true;
      }];
      ghc912.compiler-nix-name = "ghc912";
      ghc910-mempack-1 = {
        cabalProjectLocal = ''
          constraints: mempack >=0.1 && <0.2
        '';
        modules = [{
          packages.scls-format.configureFlags = [ "-f use-mempack-1" ];
          packages.scls-format.flags.use-mempack-1 = true;
        }];
      };
    };

    # Tools to include in the development shell
    shell.tools = {
      cabal = "3.16.0.0";
      haskell-language-server = "2.11.0.0";
      hlint = "3.10";
      fourmolu = fourmoluVersion;
      weeder = "2.10.0";
      cabal-gild = cabalGildVersion;
    };

    # Non-Haskell shell tools go here
    shell.buildInputs = let
      # Add this for editors which expect to use hls-wrapper
      hls-wrapper =
        pkgs.writeShellScriptBin "haskell-language-server-wrapper" ''
          exec haskell-language-server "$@"
        '';
      verifyScls = pkgs.callPackage ./verify-scls.nix { };
    in with pkgs; [ nixfmt-classic hls-wrapper ];
  };
}
