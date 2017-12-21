## TL; DR

### What is it?

A high level universally portable super build script; a short and sweet command
to uniformly, consistently, comprehensively build and sanity test a Haskell
package across build tools (stack/cabal) and all platforms to ensure that all
is well with the package.  You do not need to be familiar with any of the build
tools to use it.

## What all does it do?

Build source, build benchmarks, build docs, run tests, create source
distribution, ***build from source distribution***, test install after build,
perform distribution checks, generate coverage report, optionally send coverage
report to coveralls.io. Everything can be controlled by the user.

### Where can I use it?

Everywhere, the same build steps can be consistently performed on or using:
* Linux/OSX/Windows
* Travis/Appveyor/Local Host
* stack/cabal

### What else can I use it for?

This package is a minimal yet complete "hello world" Haskell package with all
the metadata, tests/benchmarks, and travis/appveyor CI setup fully working,
ready to be shipped to hackage. It can be used as a starting point to develop a
new package. It can also be used by beginners to learn about haskell package
metadata and builds.

### How do I use it?

To use it for CI, simply copy the travis/appveyor config file
from this package to your package and that's it. It should work without
modification, edit to customize. For use on local host, copy the
`packcheck.sh` script as well. To run locally:

```
$ env BUILD=stack packcheck.sh
$ env BUILD=cabal packcheck.sh
```

## Salient Features

* ***runs everywhere, for all build types***
* ***specifically designed for use with CI*** or pre-release testing
* ***installs all the required tools automatically*** (including stack) or lets
you know what it needs so you can install/use your own. It never overwrites
an existing tool during install.
* ***tests the source distribution of the package*** so you can rest assured
that what you release on hackage is exactly what you tested. Also performs
distribution checks and whether the package installs successfully.
* ***reproduces a failed CI on local machine***.  You can just cut
and paste the same command on your local machine and run it there for easy
debugging.
* ***can send coverage information to coveralls.io*** with a simple option.
* ***tests everything needed to ship to hacakge***

## Usage Examples

You can run these commands on your local machine as well as inside a CI script.

Make sure you are in the package directory. You can try these commands in this
package itself:
```
$ cd packcheck
```

Stack build (installs stack automatically if not found, creates a `stack.yaml`
if not found):
```
$ env BUILD=stack RESOLVER=lts-6 ./packcheck.sh
```

Set pvp-bounds before the test:
```
$ env BUILD=stack RESOLVER=lts-6 SDIST_OPTIONS="--pvp-bounds both" ./packcheck.sh
```

Stack build with system installed GHC, when GHCVER is specified it looks for
the specified GHC version in PATH:
```
$ env BUILD=stack GHCVER=7.10.3 ./packcheck.sh
```

Stack build with a specific `stack.yaml` config file and specified build flags,
and requiring a specific cabal version:
```
$ env BUILD=stack RESOLVER=lts-7.24 STACK_YAML=stack-8.0.yaml STACK_BUILD_OPTIONS="--flag streamly:examples-sdl" CABALVER=1.24 ./packcheck.sh
```

Cabal build using stack installed ghc:
```
$ stack exec env BUILD=cabal RESOLVER=lts-6 ./packcheck.sh
```

Cabal build using system installed ghc and cabal on PATH:
```
$ env BUILD=cabal GHCVER=7.10.3 CABALVER=1.22.9.0 ./packcheck.sh
```

Send coverage info of the testsuites named `test1` and `test2` to coveralls.io
using `hpc-coveralls`.  Note that this currently works only with a cabal build:
```
$ env BUILD=cabal GHCVER=8.0.2 COVERALLS_OPTIONS="test1 test2" ./packcheck.sh
```

Use clean environment, makes sure there is no interference from existing
environment:
```
$ env -i PATH=/bin:/usr/bin BUILD=stack ./packcheck.sh
```

## Full Reference

Options marked `DESTRUCTIVE!` are fine in a CI environment. But on a
local machine sometimes it may not be desirable as it will change the
state of your global cabal config, so consider that before using these options.

By default it uses cabal sandbox builds. It creates any temporary files or
build artifacts inside `.packcheck` directory. You can remove that directory
after the build to release the space if needed. For full cleanup or build from
scratch you can also remove `.stack-work` and `.cabal-sandbox`.

stack is automatically installed and can be used to do cabal builds as well. If
you specify `BUILD=cabal` and `RESOLVER` at the same time then the cabal build
uses stack installed `cabal` and `ghc`, both are installed automatically when
needed.

For pure cabal builds i.e. when `BUILD=cabal` and `RESOLVER` is not specified,
`cabal` and `ghc` must be pre-installed.

```
cueball $ packcheck.sh --help
------------------------------------------
Example usage
------------------------------------------
env BUILD=stack RESOLVER=lts-9 ./packcheck.sh
env BUILD=cabal ./packcheck.sh
env BUILD=stack HLINT_OPTIONS=. ./packcheck.sh

------------------------------------------
Commonly used env variables
------------------------------------------
BUILD                   : [stack | cabal] The only mandatory option
RESOLVER                : Stack resolver to use for stack or cabal builds
GHCVER                  : [a.b.c] GHC version prefix (may not be enforced when using stack)
CABALVER                : [a.b.c.d] Cabal version prefix for cabal builds
GHC_OPTIONS             : Specify GHC options to use
SDIST_OPTIONS           : Arguments to stack sdist (e.g. --pvp-bounds)
TEST_INSTALL            : [y] DESTRUCTIVE! Install the package after building (force install with cabal)
DISABLE_BENCH           : [y] Do not build benchmarks, default is to build but not run
PATH                    : [path] Set PATH explicitly for predictable builds

------------------------------------------
Advanced stack build env variables
------------------------------------------
STACK_YAML              : Alternative stack config file to use
STACK_UPGRADE           : Upgrade stack to latest version
STACK_OPTIONS           : Provide additional stack global options (e.g. -v)
STACK_BUILD_OPTIONS     : Override the default stack build command options

------------------------------------------
Advanced cabal build env variables
------------------------------------------
CABAL_USE_STACK_SDIST   : [y] Use stack sdist (to use --pvp-bounds)
CABAL_CONFIGURE_OPTIONS : Override the default cabal configure options
CABAL_NO_SANDBOX        : [y] DESTRUCTIVE! Clobber (force install) global cabal ghc package db
CABAL_HACKAGE_MIRROR    : [y] DESTRUCTIVE! Specify an alternative mirror, will modify the cabal user config file.
CABAL_REINIT_CONFIG     : [y] DESTRUCTIVE! Remove old cabal config to avoid any config incompatibility issues
CABAL_CHECK_RELAX       : [y] Do not fail if cabal check fails on the package.

------------------------------------------
Coverage related env variables
------------------------------------------
COVERALLS_OPTIONS       : hpc-coveralls args and options, usually just test suite names
COVERAGE                : [y] Just generate coverage information

------------------------------------------
hlint related env variables
------------------------------------------
HLINT_OPTIONS           : hlint arguments and options, usually just '.'
HLINT                   : [y] Run hlint. Defaults to 'y' if HLINT_OPTIONS is set

------------------------------------------
Diagnostics
------------------------------------------
CHECK_ENV               : Treat unknown env variables as error, used with env -i
