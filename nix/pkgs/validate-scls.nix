{ writeShellApplication, verify-scls }:
writeShellApplication {
  name = "validate-scls";
  runtimeInputs = [ verify-scls ];
  text = ''
    TEMP_DIR=$(mktemp -d)
    cabal run scls-util -- debug generate "$TEMP_DIR/1.scls"
    verify-scls "$TEMP_DIR/1.scls"
  '';
}
