{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.git-hooks = {
    url = "github:cachix/git-hooks.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.treefmt-nix = {
    url = "github:numtide/treefmt-nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.cips = {
    url = "github:tweag/CIPs/cip-canonical";
    flake = false;
  };
  outputs =
    { self, nixpkgs, flake-utils, haskellNix, git-hooks, treefmt-nix, cips }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          overlays = [
            (import ./nix/pkgs {
              referenceSpecKsy = "${cips}/CIP-0165/format/format.ksy";
            })
            haskellNix.overlay
          ];
          inherit system;
          inherit (haskellNix) config;
        };

        inherit (pkgs) lib;

        supportedGhcVersions = [ "ghc910" "ghc912" ];

        referenceCDDLDir = pkgs.runCommand "reference-cddl" { } ''
          mkdir -p $out
          cp ${cips}/CIP-0165/namespaces/*.cddl $out/ || true
        '';

        project = import ./nix/project.nix {
          inherit pkgs supportedGhcVersions referenceCDDLDir;
        };

        inherit (project) cardanoCanonicalLedger;

        flake = cardanoCanonicalLedger.flake { };

        pre-commit-check = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            fourmolu.enable = true;
            nixfmt-classic.enable = true;
            cabal-gild.enable = true;
          };
          tools = {
            fourmolu =
              cardanoCanonicalLedger.tool "fourmolu" project.fourmoluVersion;
            cabal-gild =
              cardanoCanonicalLedger.tool "cabal-gild" project.cabalGildVersion;
          };
        };

        treefmtEval =
          treefmt-nix.lib.evalModule pkgs (import ./nix/treefmt.nix project);
      in lib.recursiveUpdate flake {
        project = cardanoCanonicalLedger;
        legacyPackages = { inherit cardanoCanonicalLedger pkgs; };

        checks = {
          formatting = treefmtEval.config.build.check self;
          validate-scls-test = pkgs.runCommand "validate-scls-test" {
            buildInputs = with pkgs; [
              verify-scls
              cardanoCanonicalLedger.hsPkgs.scls-util.components.exes.scls-util
            ];
          } ''
            scls-util debug generate 1.scls
            verify-scls 1.scls
            touch "$out"
          '';
          "scls-util:test:scls-util-test" =
            flake.checks."scls-util:test:scls-util-test".overrideAttrs (old: {
              preCheck = (old.preCheck or "") + ''
                export SCLS_UTIL_PATH=${cardanoCanonicalLedger.hsPkgs.scls-util.components.exes.scls-util}/bin/scls-util
              '';
            });
          "scls-cardano:test:scls-cardano-test" =
            flake.checks."scls-cardano:test:scls-cardano-test".overrideAttrs
            (old: {
              preCheck = (old.preCheck or "") + ''
                export REFERENCE_CDDL_DIR=${referenceCDDLDir}
              '';
            });
        };

        devShells = let
          mkDevShells = p: {
            # Shell with profiling enabled
            profiling = (p.appendModule {
              modules = [{ enableLibraryProfiling = true; }];
            }).shell;
            # Default shell with pre-commit hooks
            default = p.shell.overrideAttrs (old: {
              shellHook = old.shellHook + pre-commit-check.shellHook;
            });
          };
        in mkDevShells cardanoCanonicalLedger // lib.listToAttrs (map
          (compiler-nix-name:
            let
              p = cardanoCanonicalLedger.appendModule {
                inherit compiler-nix-name;
              };
            in {
              name = compiler-nix-name;
              value = p.shell // (mkDevShells p);
            }) supportedGhcVersions);

        formatter = treefmtEval.config.build.wrapper;
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters =
      [ "https://cache.iog.io" "https://tweag-cardano-cls.cachix.org" ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "tweag-cardano-cls.cachix.org-1:4/Ger2Oe/TpXbV4RY45mvuFt6t4RFMiJXi1y4/YugIU="
    ];
    allow-import-from-derivation = "true";
  };
}
