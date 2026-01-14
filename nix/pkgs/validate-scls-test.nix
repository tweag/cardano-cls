{ writeShellApplication, verify-scls, scls-util-exe }:
writeShellApplication {
  name = "validate-scls-test";
  runtimeInputs = [ verify-scls scls-util-exe ];
  text = ''
    TEMP_DIR=$(mktemp -d) && scls-util debug generate "$TEMP_DIR/1.scls" && verify-scls "$TEMP_DIR/1.scls"
  '';
}
