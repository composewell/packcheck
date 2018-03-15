# packcheck

[![Hackage](https://img.shields.io/hackage/v/packcheck.svg?style=flat)](https://hackage.haskell.org/package/packcheck)
[![Build Status](https://travis-ci.org/harendra-kumar/packcheck.svg?branch=master)](https://travis-ci.org/harendra-kumar/packcheck)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/f7c0ncy84cxp8lbe?svg=true)](https://ci.appveyor.com/project/harendra-kumar/packcheck)
[![Coverage Status](https://coveralls.io/repos/harendra-kumar/packcheck/badge.svg?branch=master&service=github)](https://coveralls.io/github/harendra-kumar/packcheck?branch=master)

## TL; DR

* Just copy
[.travis.yml](https://github.com/harendra-kumar/packcheck/blob/master/.travis.yml)
and [appveyor.yml](https://github.com/harendra-kumar/packcheck/blob/master/appveyor.yml)
to your package repo and your package is CI ready.
* Copy
[packcheck.sh](https://github.com/harendra-kumar/packcheck/blob/master/packcheck.sh)
to your local machine and run it from your package directory (works on
Linux/OSX/Windows) to perform all the same tests that are done by CI, locally.

### What is it?

`packcheck` is a minimal yet complete "hello world" Haskell package with model
`travis` and `appveyor` config files that can be used unmodified in any Haskell
package. The CI configs can be modified **declaratively** to adapt to **any**
kind of build scenario you can imagine.

The package includes `packcheck.sh`, a high level universal super build script
to uniformly, consistently build and comprehensively sanity test a Haskell
package across build tools (stack/cabal) and across all platforms
(Linux/MacOS/Windows).  You do not need to be familiar with any of the build
tools to use it.

This is also a minimal yet complete model package (with tests, benchmarks,
Linux/MacOS/Windows CI already working) that can be used as a starting point to
develop a new package. Beginners can use it to learn about haskell package
metadata structure.

## What all does it do?

In a single invocation it performs a whole battery of tests:
build source, build benchmarks, build docs, run tests, create source
distribution, ***build from source distribution***, test install after build,
perform distribution checks, generate coverage report, optionally send coverage
report to coveralls.io. Everything can be controlled by the user.

### Where can I use it?

Everywhere. The same build steps can be consistently performed on or using:
* Linux/OSX/Windows
* Travis/Appveyor/Local Host
* stack/cabal

### How do I use it?

To use it for CI, simply copy the
[travis](https://github.com/harendra-kumar/packcheck/blob/master/.travis.yml),
[appveyor](https://github.com/harendra-kumar/packcheck/blob/master/appveyor.yml)
config files from this package to your package and that's it. It should work
without modification, of course you can edit them to customize. For use on
local host, just copy over the
[packcheck.sh](https://github.com/harendra-kumar/packcheck/blob/master/packcheck.sh)
script and put it in your `PATH`. Run the script from the package
directory of the package you want to build.

```
$ packcheck.sh stack
$ packcheck.sh cabal
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
$ ./packcheck.sh stack RESOLVER=lts-6
```

Set pvp-bounds before the test:
```
$ ./packcheck.sh stack RESOLVER=lts-6 SDIST_OPTIONS="--pvp-bounds both"
```

Stack build with system installed GHC, when GHCVER is specified it looks for
the specified GHC version in PATH:
```
$ ./packcheck.sh stack GHCVER=7.10.3
```

Stack build with a specific `stack.yaml` config file and specified build flags,
and requiring a specific cabal version:
```
$ ./packcheck.sh stack RESOLVER=lts-7.24 STACK_YAML=stack-8.0.yaml STACK_BUILD_OPTIONS="--flag streamly:examples-sdl" CABALVER=1.24
```

Cabal build using stack installed ghc:
```
$ stack exec ./packcheck.sh cabal RESOLVER=lts-6
```

Cabal build using system installed ghc and cabal on PATH:
```
$ ./packcheck.sh cabal GHCVER=7.10.3 CABALVER=1.22
```

Run hlint commands on the directories `src` and `test`:
```
$ ./packcheck.sh stack HLINT_COMMANDS="hlint lint src; hlint lint test"
```

Send coverage info of the testsuites named `test1` and `test2` to coveralls.io
using `hpc-coveralls`.  Note that this currently works only with a cabal build:
```
$ ./packcheck.sh cabal GHCVER=8.0.2 COVERALLS_OPTIONS="test1 test2"
```

## Diagnostics

There may be issues due to some environment variables unknowingly set or some
command line parameters or env variables being misspelled and therefore
silently ignored. To avoid any such issues the cleanest way to invoke the
script is to use a clean environment using `env -i` and `CHECK_ENV=y`
parameter. When this parameter is set unwanted/misspelled variables are
detected and reported.

```
$ env -i CHECK_ENV=y ./packcheck.sh stack
```

For performance diagnostics the script prints the time elapsed from the
beginning at each build step performed.

## Full Reference

Options marked `DESTRUCTIVE!` are fine in a CI environment. But on a
local machine sometimes it may not be desirable as it will change the
state of your global cabal config, so consider that before using these options.

By default cabal builds are done using sandboxes. It creates any temporary
files or build artifacts inside `.packcheck` directory. See the `clean` and
`cleanall` commands to release the temporary space.

`stack` is automatically installed and can be used to do cabal builds as well.
If you specify `BUILD=cabal` and `RESOLVER` at the same time then the cabal
build uses stack installed `cabal` and `ghc`, both are installed automatically
when needed.

For pure cabal builds i.e. when `BUILD=cabal` and `RESOLVER` is not specified,
`cabal` and `ghc` must be pre-installed on the system before building.

```
$ packcheck.sh --help

--------------------------------------------------
Usage
--------------------------------------------------
./packcheck.sh COMMAND [PARAMETER=VALUE ...]

For example:
./packcheck.sh stack RESOLVER=lts-10.0 GHC_OPTIONS="-O0 -Werror"

Control parameters can either be passed on command line or exported
as environment variables. Parameters marked DESTRUCTIVE may modify
your global user config or state.

--------------------------------------------------
Commands
--------------------------------------------------
stack                   : build using stack
cabal                   : build using cabal
clean                   : remove the .packcheck directory
cleanall                : remove .packcheck, .stack-work, .cabal-sandbox directories
help                    : show this help message

--------------------------------------------------
Commonly used parameters or env variables
--------------------------------------------------
RESOLVER                : Stack resolver to use for stack or cabal builds
GHCVER                  : [a.b.c] GHC version prefix (may not be enforced when using stack)
CABALVER                : [a.b.c.d] Cabal version (prefix) to use
STACKVER                : [a.b.c.d] Stack version (prefix) to use
GHC_OPTIONS             : Specify GHC options to use
SDIST_OPTIONS           : Arguments to stack/cabal sdist command (e.g. --pvp-bounds)
DISABLE_SDIST_BUILD     : [y] Do not build from source distribution
DISABLE_BENCH           : [y] Do not build benchmarks, default is to build but not run
PATH                    : [path] Set PATH explicitly for predictable builds
TEST_INSTALL            : [y] DESTRUCTIVE! Install the package after building (force install with cabal)

--------------------------------------------------
Advanced stack build parameters or env variables
--------------------------------------------------
STACK_YAML              : Alternative stack config, cannot be a path, just the file name
STACK_OPTIONS           : ADDITIONAL stack global options (e.g. -v) to append
STACK_BUILD_OPTIONS     : ADDITIONAL stack build command options to append
STACK_UPGRADE           : [y] DESTRUCTIVE! Upgrades stack to latest version

--------------------------------------------------
Advanced cabal build parameters or env variables
--------------------------------------------------
CABAL_USE_STACK_SDIST   : [y] Use stack sdist (to use --pvp-bounds)
CABAL_CONFIGURE_OPTIONS : ADDITIONAL default cabal configure options to append
CABAL_CHECK_RELAX       : [y] Do not fail if cabal check fails on the package.
CABAL_NO_SANDBOX        : [y] DESTRUCTIVE! Clobber (force install) global cabal ghc package db
CABAL_HACKAGE_MIRROR    : [y] DESTRUCTIVE! Specify an alternative mirror, will modify the cabal user config file.
CABAL_REINIT_CONFIG     : [y] DESTRUCTIVE! Remove old cabal config to avoid any config incompatibility issues

--------------------------------------------------
Coverage related parameters or env variables
--------------------------------------------------
COVERALLS_OPTIONS       : hpc-coveralls args and options, usually just test suite names
COVERAGE                : [y] Just generate coverage information

--------------------------------------------------
hlint related parameters or env variables
--------------------------------------------------
HLINT_COMMANDS          : hlint commands e.g.'hlint lint src; hlint lint test'

--------------------------------------------------
Diagnostics parameters or env variables
--------------------------------------------------
CHECK_ENV               : [y] Treat unknown env variables as error, used with env -i
BASE_TIME               : System time to be used as base for timeline reporting
