{ fetchurl, python3Packages, kaitai-struct-compiler, python3 }:
python3.pkgs.buildPythonApplication {
  pname = "verify-scls";
  version = "0.0.1";
  doCheck = false;
  format = "setuptools";

  srcs = [
    (fetchurl {
      url =
        "https://raw.githubusercontent.com/tweag/CIPs/refs/heads/move-slot-no/CIP-0165/format/format.ksy";
      sha256 = "sha256-N8pSmcW4uJd/rq7ShsooQsaTTJIcpuUKZtQInVpbCjA=";
    })
    ../scripts/verify.py
  ];

  propagatedBuildInputs = with python3.pkgs; [ kaitaistruct ];

  build-system = with python3Packages; [ setuptools ];

  nativeBuildInputs = [ kaitai-struct-compiler ];

  unpackPhase = ''
        for _src in $srcs; do
          cp "$_src" $(stripHash "$_src")
        done
        cat > setup.py << EOF
    from setuptools import setup

    setup(
      name='verify-scls',
      version='0.1.0',
      install_requires=['kaitaistruct'],
      py_modules=[
        'verify',
        'scls_file',
      ],
      entry_points={
        'console_scripts': ['verify-scls=verify:main']
      },
    )
    EOF
        ${kaitai-struct-compiler}/bin/kaitai-struct-compiler -t python format.ksy
  '';
}
