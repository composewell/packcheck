## Haskell Package Sanity Testing

Highly flexible and fully automated, CI or manual, Haskell package sanity
testing for package maintainers or end users.

## Usage

Options are passed via environment variables.

### Local Machine
```
$ cd package-dir
$ env BUILD=stack RESOLVER=lts-6 SDIST_OPTIONS="--pvp-bounds both" package-sanity.sh
$ env BUILD=cabal GHCVER=7.10.3 CABALVER=1.22.9.0 COVERALLS_OPTIONS="test1 test2" package-sanity.sh
```

### Travis CI

Link to script
Link to output

### Clean environment

It is recommended to use a clean environment so that any environment variables
are explicit. Use an explicitly set minimal `PATH` environment variable to find
the tools. For example:

```
$ cd package-dir
$ env -i PATH=/bin:/usr/bin BUILD=stack package-sanity.sh
```

## Features

* Works on *Linux* and *OSX*
* Works for *stack* as well as *cabal* builds
* Works on *local machine* as well as *travis CI* or other *continuous
  integration* systems
* Optionally, installs the tools as well (stack, cabal, ghc etc.)
* Tests the source dist instead of the repo so that there are no surprises later
* Test with stack even if you do not have a stack.yaml (it creates one automatically)
* Use stack installed GHC for cabal builds
* Optionally, generates and sends *coverage* information to coveralls.io

## Configurable options
```
cueball $ package-sanity.sh --help
The following environment variables can be passed.

BUILD                  : [stack | cabal] The only mandatory option
SDIST_OPTIONS          : Argument to stack sdist (e.g. pvp-bounds)
GHC_OPTIONS            : Specify GHC options to use
GHCVER                 : [a.b.c] GHC version (may not be enforced when using stack)
RESOLVER               : Stack resolver to use for stack or cabal builds
PATH                   : Use an explicitly set PATH for predictable builds

STACK_YAML             : Alternative stack config file to use
STACK_BUILD_OPTIONS    : Override the default stack build command options

CABALVER               : [a.b.c.d] Cabal version requested
USE_STACK_SDIST        : [y] For cabal builds, use stack sdist to create dist to test
CABAL_DESTRUCTIVE      : [y] Clobber cabal config, install bins, force install packages
CABAL_CONFIGURE_OPTIONS: Override the default cabal configure options

COVERALLS_OPTIONS      : [test suite names] Send coverage to coveralls.io
COVERAGE               : [y] Just generate coverage information

Example usage:
env BUILD=stack RESOLVER=lts-6 SDIST_OPTIONS="--pvp-bounds both" ./package-sanity.sh
```

## Special Notes

Hackage mirror: hackage.haskell.org:http://hackage.fpcomplete.com/

## How it works, read before using on a local system

### Tools install phase

When using stack build, if stack and cabal are not found in PATH, it
will install both in ~/.local/bin automatically without any explicit
permission. It will also install ghc via stack when using stack build
and you do not have a ghc in PATH or if it is not suitable for building
your package. When using a cabal build it initiates a cabal update.

For stack builds you can use GHCVER as well. This will mean a preference
will be given to the compiler in the PATH but if the compiler in the
PATH is not suitable for the given RESOLVER then stack may choose
another compiler if already installed by stack (it will not install
though when GHCVER is specified).

You can use stack installed GHC for cabal builds by specifying a
RESOLVER for BUILD type cabal. For cabal builds you cannot use RESOLVER
and GHCVER at the same time.

### Generate and unpack source distribution

It first creates a source dist using `stack sdist` or `cabal sdist`
command.  The source distribution is then unpacked and tested.  You can
use `stack sdist` with cabal build as well. That is useful when you want
to test wiht cabal too but want to use `--pvp-bounds` option from `stack
sdist`

The source dist is unpacked in `.sanity-test` directory in the package
directory.

### Dependency install phase

This pahse onwards it works on the source dist unpacked in the
`.sanity-test` directory.  First it installs all the dependencies using
`stack` or `cabal`. A `cabal sandbox` is used for cabal build.

### Build and test phase

In this phase, it builds the packages and runs the tests. Build and test
options are completely configurable via environment variables.

## Sample Travis CI Script
## Sample output
