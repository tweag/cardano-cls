#!/bin/sh
mkdir -p scripts/wrk
curl -fSL -o scripts/wrk/scls.ksy https://raw.githubusercontent.com/tweag/CIPs/refs/heads/cip-canonical/CIP-0165/format/format.ksy
cp ./scripts/verify.py ./scripts/wrk || exit 1
kaitai-struct-compiler --outdir scripts/wrk/ -t python scripts/wrk/scls.ksy || exit 1
cabal run scls-util:scls-util -- debug generate scripts/wrk/1.scls || exit 1
python3 ./scripts/wrk/verify.py scripts/wrk/1.scls