# Cardano Canonical Ledger

This repository contains the tools for keeping and working with canonical ledger state format.
The format that is used for common representation of the state that can be shared between all
node implementations.

## Overview

### Prerequisites

- Nix (with flakes support)

or

- Haskell (recommended version: GHC 9.6+)
- Cabal or Stack

## Documentation

See the [docs](./docs) directory for detailed documentation.

To generate cddl files use

```
cabal run gen-cddl -- target_directory
```


## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](./Contributing.md) for guidelines.
