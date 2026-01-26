{ writeShellApplication, jq }:
writeShellApplication {
  name = "update-reference";
  runtimeInputs = [ jq ];
  text = ''
    # update the cips input in flake.nix to the latest commit
    nix flake update cips

    # get the latest commit hash of the cips input
    LATEST_COMMIT=$(nix flake metadata --json | jq --raw-output .locks.nodes.cips.locked.rev)

    # update the haskell-ci.yml file
    CI_FILE=".github/workflows/haskell-ci.yml"
    sed -i "/repository: tweag\/CIPs/{n;s/ref: .*/ref: $LATEST_COMMIT/}" "$CI_FILE"
  '';
}
