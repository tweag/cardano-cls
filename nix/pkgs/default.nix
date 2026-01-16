# our packages overlay
final: prev: {
  verify-scls = final.callPackage ./verify-scls.nix { };
  validate-scls = final.callPackage ./validate-scls.nix { };
}
