# Contributing to Cardano Canonical Ledger State

Thank you for your interest in contributing! We welcome contributions from the community.

## Reporting Issues

- Use [GitHub Issues](../../issues) to report bugs or request features.

## Setting up the build tools

### Using nix

Make sure you have [Nix](https://nixos.org/download.html) installed with flakes support enabled.

You can enter a Nix shell with all dependencies by running:

``` sh
nix develop
```

This will set up the environment with the required GHC version and all necessary libraries and tools.

#### Multiple GHC versions and configurations

The Nix setup provides dev shells for various GHC versions and configurations (test coverage enabled, profiling enabled, ...). You can enter a shell with a specific GHC version by using:

``` sh
nix develop .#ghc984
```

Replace `ghc984` with the desired GHC version and configuration variant. Available options can be found in the flake configuration.

#### Discovering available outputs

To see all available outputs (dev shells, packages, checks, etc.), you should first update `nix/project.nix` to include `evalSystem = <YOUR_SYSTEM>;` in the `pkgs.haskell-nix.cabalProject'` call, for example:

```nix
    # ...
    cardanoCanonicalLedger = pkgs.haskell-nix.cabalProject' {
        evalSystem = "x86_64-linux";
        # ...
    };
    # ...
```

Then run:

```sh
nix flake show --allow-import-from-derivation
```

This command will display a tree structure of all available flake outputs, including:

- Development shells for different GHC versions
- Package components that can be built
- Checks and formatters

#### Nix cache (optional, but recommended)

To speed up the build process (avoid building GHC), you should use the IOHK Nix cache.

For NixOS, add the following to your `/etc/nixos/configuration.nix` or system flake configuration:

```nix
nix.settings.substituters = [
    # ... any pre-existing substituters ...
    "https://cache.iog.io"
];
nix.settings.trusted-public-keys = [
    # ... any pre-existing keys ...
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
];
```

If using `nix` on non-NixOS systems, add the following to your `/etc/nix/nix.conf`:

```conf
substituters = https://cache.iog.io <other substituters you might have>
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= <other keys you might have>
```

#### Using direnv (optional, but recommended)

If you have [direnv](https://direnv.net/)  and [nix-direnv](https://github.com/nix-community/nix-direnv) installed, you can set it up to automatically load the Nix environment when you enter the project directory.

1. Create a `.envrc` file in the project root with the following content:

   ```sh
   watch_file \
    nix/hix.nix
   use flake
   ```

2. Allow the `.envrc` file:

   ```sh
   direnv allow
   ```

3. Now, whenever you `cd` into the project directory, direnv will automatically load the Nix environment.

### Using cabal

In order to work with the project you need to install [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/) tools, we suggest installing them using [GHCup](https://www.haskell.org/ghcup/) project. For working with
this project you need to have GHC>=9.6 and cabal>=3.10

To install ghcup follow the instructions on site. After installing run

```sh
ghcup tui
```

And select recommended versions of GHC an cabal.

## Building the project

To build the project in the project directory run command:

``` sh
cabal build all
```

### Building with Nix

You can also build specific package components using Nix:

``` sh
nix build .#scls-format:lib:scls-format
```

This example builds the `scls-format` library component. You can replace the component specification with other available packages and components in the project.

You can also build these components for each of the project's supported GHC versions:

``` sh
nix build .#ghc98:scls-format:lib:scls-format
```

Replace `ghc98` with the desired GHC version to build components with that specific compiler version.

## Testing

When implementing new test suites, make sure to add them to the 'Run tests' step in `.github/workflows/nix.yaml`.

To run tests in the project directory run command:

``` sh
cabal test all
```

To test that generated file matches the kaitai specification run:

``` sh
nix develop -c validate-scls
```

It generate a scls file and validate file against
the specification.

Whenever the reference specification is updated, the flake input `cips` should be updated accordingly. This ensures that the latest specification is used for validation. You can run `nix develop -c update-reference` to update the `cips` input to its latest version, as well as the Haskell CI workflow.

## Generating documentation and setting up hoogle

To generate documentation run

``` sh
cabal haddock all
```

## Code style and pre-commit hooks

This project follows specific code style guidelines to maintain consistency across the codebase.

If using Nix, you can run the `nix fmt` tool to automatically format your code according to the project's style guidelines. This will run [`treefmt`](https://github.com/numtide/treefmt) configured to format Haskell and Nix files using the following tools:

- [`fourmolu`](https://github.com/fourmolu/fourmolu): Haskell code formatting
- [`nixfmt`](https://github.com/NixOS/nixfmt): Nix file formatting
- [`cabal-gild`](https://github.com/tfausak/cabal-gild): Cabal file formatting

This project also uses pre-commit hooks to ensure code quality and consistency. These hooks are automatically configured by the Nix shell environment. To use them, simply make your changes and commit as usual. The hooks will run automatically before each commit.

## How to Contribute

1. **Fork the repository** and create your branch from `main`.
1. **Clone your fork** and set up the project locally, see setting up section.
1. **Make your changes** with clear, descriptive commit messages.
1. **Open a Pull Request** describing your changes and referencing any related issues.

## Code of conduct

Project follows the same code conventions as cardano project [Contributor Covenant][cc-homepage].

## License

By contributing, you agree that your contributions will be licensed under the project's license.

---

Thank you for helping improve Cardano Canonical Ledger!

[cc-homepage]: https://www.contributor-covenant.org
