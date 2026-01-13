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
  outputs = { self, nixpkgs, flake-utils, haskellNix, git-hooks, treefmt-nix }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          overlays = [ haskellNix.overlay ];
          inherit system;
          inherit (haskellNix) config;
        };

        inherit (pkgs) lib;

        project = import ./nix/project.nix pkgs;

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
          formatting = treefmtEval.${pkgs.system}.config.build.check self;
        };

        devShells = let
          mkDevShells = p: {
            default = p.shell.overrideAttrs (old: {
              shellHook = old.shellHook + pre-commit-check.shellHook;

              buildInputs = old.buildInputs ++ pre-commit-check.enabledPackages;
            });
          };
        in mkDevShells cardanoCanonicalLedger // lib.mapAttrs
        (compiler-nix-name: _:
          let
            p = cardanoCanonicalLedger.appendModule {
              inherit compiler-nix-name;
            };
          in p.shell // (mkDevShells p)) pkgs.haskell-nix.compiler;

        formatter = treefmtEval.config.build.wrapper;
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
