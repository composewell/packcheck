# packcheck

[![Hackage](https://img.shields.io/hackage/v/packcheck.svg?style=flat)](https://hackage.haskell.org/package/packcheck)
[![Build Status](https://travis-ci.org/composewell/packcheck.svg?branch=master)](https://travis-ci.org/composewell/packcheck)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/f7c0ncy84cxp8lbe?svg=true)](https://ci.appveyor.com/project/composewell/packcheck)
[![CircleCI](https://circleci.com/gh/composewell/packcheck/tree/master.svg?style=svg)](https://circleci.com/gh/composewell/packcheck/tree/master)
[![Coverage Status](https://coveralls.io/repos/composewell/packcheck/badge.svg?branch=master&service=github)](https://coveralls.io/github/composewell/packcheck?branch=master)

## Quick Start

### CI
To enable CI for your repo, just copy the relevant CI config file i.e.
[.travis.yml](https://github.com/composewell/packcheck/blob/master/.travis.yml),
[appveyor.yml](https://github.com/composewell/packcheck/blob/master/appveyor.yml),
or
[.circleci/config.yml](https://github.com/composewell/packcheck/blob/master/.circleci/config.yml)
to your package repo, add your repo to travis/appveyor/circleci and CI should just
work for most packages. Uncomment the relevant lines in the CI config files to enable CI for more
configs. Just modify some environment variables in the configs to control
everything about the build.

### Local Machine
For local use, copy
[packcheck.sh](https://github.com/composewell/packcheck/blob/master/packcheck.sh)
to your local machine (Linux/OSX/Windows), put it in your PATH, and run it
from your package directory. You can pass the same evironment variables that
are used in CI files to run the exact same tests locally. Usage is as simple
as:
```
$ packcheck.sh cabal-new
$ packcheck.sh cabal-new ENABLE_GHCJS=y
$ packcheck.sh cabal
$ packcheck.sh stack
```

### Some Key Points

* If a CI build fails just copy and paste the command printed in the log and
  the same build runs on the local machine so that you can debug quickly.
* To send coverage info to [coveralls.io](https://coveralls.io) just
  uncomment a line in your `.travis.yml`.
* If you are using `hvr-ghc` PPA, just use `TOOLS_DIR=/opt` or the path where
  it is installed, and you can use all the ghc/cabal versions available,
  automatically.
* Conveniently control all aspects of build through command line or environment
  variables, including tool options or whether
  to enable benchmarks, haddock, coverage, install test etc. It is a very
  powerful tool, can do whatever you can imagine, see full reference at the
  end.
* `packcheck` creates the source distribution and builds the package from the
  generated tarball to make sure that you build what you release and don't miss
  adding a file to the distribution.
* The most important part is that you can run exact same tests, in the same
  way, everywhere:

### Out of the box support

| Platforms     | Build Types     | CI Modes      |
|:-------------:|:---------------:|:-------------:|
| Linux         | stack           | Travis        |
| OSX           | cabal           | Appveyor      |
| Windows       | cabal new-build | CircleCI      |
|               |                 | Local Machine |

GHCJS builds are also supported. See
[.travis.yml](https://github.com/composewell/packcheck/blob/master/.travis.yml) for GHCJS CI build example.
The script can be easily adapted to any CI with a single line build command.

## What is it?

The package `packcheck` includes a script called `packcheck.sh`, it is a high
level universal super build script to uniformly, consistently build and
comprehensively sanity test a Haskell package across build tools (stack/cabal)
and across all platforms (Linux/MacOS/Windows).  You do not need to be familiar
with any of the build tools to use it.

To make sure that it works everywhere without installing anything it is
deliberately written in bash. Any of the parameters to control the builds can
either be passed on the script command line or as environment variables for
convenient use on CI systems.

`packcheck` is also a minimal yet complete "hello world" Haskell package with
model `travis` and `appveyor` config files that can be used unmodified in any
Haskell package. The CI configs can be modified **declaratively**, using
environment variables, to adapt to **any** kind of build scenario you can
imagine.

This model package has everything that a Haskell package usually has; including
tests, benchmarks and Linux/MacOS/Windows CI already working. It can be used as
a starting point to develop a new package. Beginners can use it to learn about
haskell package metadata structure.

## What all does it do?

An invocation of `packcheck.sh` performs a whole battery of tests. `packcheck` is
designed to be a simple to use tool for power users, you can control all
aspects of the build process the way you want, see the reference section below.

### Auto tool install and selection
* Picks up the right version of GHC automatically (based on the version
  sepcified via an environment variable) if multiple versions are
  available in the PATH or from hvr-ghc style ghc/cabal installation.
* When using stack builds, `stack` and `ghc` are installed automatically, if
  needed
* For stack builds, if the package being tested does not have a `stack.yaml` it
  can even create it automatically using `stack init`.
### Build
* build source
* build benchmarks
* build docs
### Test
* run tests
### Lint
* run `hlint`
### Coverage and Coveralls
* generate coverage report
* send coverage report to coveralls.io
### Create and Test Source Distribution
* create source distribution
* build from source distribution
* test installation after build
* perform distribution checks

## Usage Examples

You can run these commands on your local machine as well as inside a CI script.
You can try these commands in the `packcheck` package itself:
```
$ ./packcheck.sh cabal-new GHCVER=8.4.1
$ ./packcheck.sh cabal GHCVER=7.10.3 CABALVER=1.22
```

```
$ cd packcheck
$ ./packcheck.sh stack RESOLVER=lts-11
$ ./packcheck.sh stack GHCVER=8.2.2
$ ./packcheck.sh stack RESOLVER=lts-7.24 STACK_YAML=stack-8.0.yaml STACK_BUILD_OPTIONS="--flag streamly:examples-sdl" CABALVER=1.24
# You can also do a cabal build using stack installed ghc:
$ stack exec ./packcheck.sh cabal RESOLVER=lts-11
```

Run hlint commands on the directories `src` and `test`:
```
$ ./packcheck.sh stack HLINT_COMMANDS="hlint lint src; hlint lint test"
```

Send coverage info of the testsuites named `test1` and `test2` to coveralls.io
using `hpc-coveralls`.
```
$ ./packcheck.sh cabal GHCVER=8.0.2 COVERALLS_OPTIONS="test1 test2"
```

## Full Reference

NOTE: Any of the parameters described below can either be passed on command
line or as an environment variable. On a CI system you can just use a common
command and control the build behavior for different builds using environment
variables.

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
cabal-new               : build using cabal new-build
clean                   : remove the .packcheck directory
cleanall                : remove .packcheck, .stack-work, .cabal-sandbox directories
help                    : show this help message

--------------------------------------------------
Selecting tool versions
--------------------------------------------------
ENABLE_GHCJS            : [y] Use GHCJS instead of GHC to build
GHCVER                  : [a.b.c] GHC version prefix (may not be enforced when using stack)
CABALVER                : [a.b.c.d] Cabal version (prefix) to use
RESOLVER                : Stack resolver to use for stack builds or cabal builds using stack
STACKVER                : [a.b.c.d] Stack version (prefix) to use
STACK_UPGRADE           : [y] DESTRUCTIVE! Upgrades stack to latest version

--------------------------------------------------
Where to find the required tools
--------------------------------------------------
PATH                    : [path] Set PATH explicitly for predictable builds
TOOLS_DIR               : [dir] Find ghc|cabal by version as in TOOLS_DIR/ghc/8.4.1/bin

--------------------------------------------------
Specifying common tool options
--------------------------------------------------
GHC_OPTIONS             : Specify GHC options to use
SDIST_OPTIONS           : Arguments to stack/cabal sdist command
CABAL_REINIT_CONFIG     : [y] DESTRUCTIVE! Remove old config to avoid incompatibility issues

--------------------------------------------------
Specifying what to build
--------------------------------------------------
DISABLE_BENCH           : [y] Do not build benchmarks, default is to build but not run
DISABLE_TEST            : [y] Do not run tests, default is to run tests
DISABLE_DOCS            : [y] Do not build haddocks, default is to build docs
DISABLE_SDIST_BUILD     : [y] Do not build from source distribution
DISABLE_DIST_CHECKS     : [y] Do not perform source distribution checks
ENABLE_INSTALL          : [y] DESTRUCTIVE! Install the package after building

--------------------------------------------------
stack options
--------------------------------------------------
STACK_YAML              : Alternative stack config, cannot be a path, just the file name
STACK_OPTIONS           : ADDITIONAL stack global options (e.g. -v) to append
STACK_BUILD_OPTIONS     : ADDITIONAL stack build command options to append

--------------------------------------------------
cabal options
--------------------------------------------------
CABAL_NEWBUILD_OPTIONS  : ADDITIONAL cabal new-build options to append
CABAL_NEWBUILD_TARGETS  : cabal new-build targets, default is 'all'
CABAL_CONFIGURE_OPTIONS : ADDITIONAL cabal old style configure options to append
CABAL_CHECK_RELAX       : [y] Do not fail if cabal check fails on the package.
CABAL_NO_SANDBOX        : [y] DESTRUCTIVE! Clobber (force install) global cabal ghc package db
CABAL_HACKAGE_MIRROR    : [y] DESTRUCTIVE! Specify an alternative mirror, modifies the cabal config file.

--------------------------------------------------
Coverage options
--------------------------------------------------
COVERALLS_OPTIONS       : hpc-coveralls args and options, usually just test suite names
COVERAGE                : [y] Just generate coverage information

--------------------------------------------------
hlint options
--------------------------------------------------
HLINT_COMMANDS          : hlint commands e.g.'hlint lint src; hlint lint test'

--------------------------------------------------
Diagnostics options
--------------------------------------------------
CHECK_ENV               : [y] Treat unknown env variables as error, used with env -i
BASE_TIME               : System time to be used as base for timeline reporting
```

Options marked `DESTRUCTIVE!` are fine in a CI environment. But on a
local machine sometimes it may not be desirable as it will change the
state of your global cabal config, so consider that before using these options.

By default cabal builds are done using sandboxes. It creates any temporary
files or build artifacts inside `.packcheck` directory. See the `clean` and
`cleanall` commands to release the temporary space.

`stack` is automatically installed and can be used to do cabal builds as well.
If you specify `BUILD=cabal-new` and `RESOLVER` at the same time then the cabal
build uses stack installed `cabal` and `ghc`, both are installed automatically
when needed.

For pure cabal builds i.e. when `BUILD=cabal-new` and `RESOLVER` is not
specified, `cabal` and `ghc` must be pre-installed on the system before
building.

## Diagnostics

There may be issues due to some environment variables unknowingly set or some
command line parameters or env variables being misspelled and therefore
silently ignored. To avoid any such issues the robust way to invoke `packcheck`
is to use a clean environment using `env -i` and passing `CHECK_ENV=y`
parameter. When this parameter is set unwanted/misspelled variables are
detected and reported.

```
$ env -i CHECK_ENV=y ./packcheck.sh stack
```

For performance diagnostics `packcheck` prints the time elapsed from the
beginning at each build step performed.
