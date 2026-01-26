{ referenceSpecKsy }:
# our packages overlay
final: prev: {
  verify-scls =
    final.callPackage ./verify-scls.nix { inherit referenceSpecKsy; };
  validate-scls = final.callPackage ./validate-scls.nix { };
}
