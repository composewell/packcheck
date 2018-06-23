# packcheck

[![Hackage](https://img.shields.io/hackage/v/packcheck.svg?style=flat)](https://hackage.haskell.org/package/packcheck)
[![Build Status](https://travis-ci.org/harendra-kumar/packcheck.svg?branch=master)](https://travis-ci.org/harendra-kumar/packcheck)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/f7c0ncy84cxp8lbe?svg=true)](https://ci.appveyor.com/project/harendra-kumar/packcheck)
[![Coverage Status](https://coveralls.io/repos/harendra-kumar/packcheck/badge.svg?branch=master&service=github)](https://coveralls.io/github/harendra-kumar/packcheck?branch=master)

## TL; DR

* For CI, just copy
  [.travis.yml](https://github.com/harendra-kumar/packcheck/blob/master/.travis.yml)
  and
  [appveyor.yml](https://github.com/harendra-kumar/packcheck/blob/master/appveyor.yml)
  files to your package repo, add your repo to travis/appveyor and CI will just
  work.
* For local use, copy
  [packcheck.sh](https://github.com/harendra-kumar/packcheck/blob/master/packcheck.sh)
  to your local machine (Linux/OSX/Windows), put it in your PATH, and run it
  from your package directory and watch all the CI tests being done locally.
  ```
  $ packcheck.sh stack
  $ packcheck.sh cabal
  $ packcheck.sh cabal-new
  ```
* If a CI build fails just copy and paste the command printed in the log and
  the same build runs on the local machine so that you can debug quickly.
* Want to send coverage info to coverall.io? Just uncomment a line in your
  `.travis.yml`.
* If you are using `hvr-ghc` PPA, just use `TOOLS_DIR=/opt` or the path where
  it is installed, and you can use all the ghc/cabal versions available,
  automatically.
* Conveniently control all aspects of build, including tool options or whether
  to enable benchmarks, haddock, coverage, install test etc. It is a very
  powerful tool, can do whatever you can imagine, see full reference at the
  end.
* Builds from the source distribution tar to make sure you build what you
  release and don't miss any file from the distribution.
* The most important part is that you run exact same tests, in the same way,
  everywhere:

| Platforms     | CI Modes      | Build Types     |
|:-------------:|:-------------:|:---------------:|
| Linux         | Travis        | stack           |
| OSX           | Appveyor      | cabal           |
| Windows       | Local Machine | cabal new-build |

## What is it?

The package `packcheck` is a minimal yet complete "hello world" Haskell package
with model `travis` and `appveyor` config files that can be used unmodified in
any Haskell package. The CI configs can be modified **declaratively** to adapt
to **any** kind of build scenario you can imagine.

The package includes a script called `packcheck.sh`, it is a high level
universal super build script to uniformly, consistently build and
comprehensively sanity test a Haskell package across build tools (stack/cabal)
and across all platforms (Linux/MacOS/Windows).  You do not need to be familiar
with any of the build tools to use it.

This is also a minimal yet complete model package (with tests, benchmarks,
Linux/MacOS/Windows CI already working) that can be used as a starting point to
develop a new package. Beginners can use it to learn about haskell package
metadata structure.

## What all does it do?

An invocation of packcheck performs a whole battery of tests:

* When using stack builds, stack and ghc are installed automatically, if needed
* for stack builds, if the package being tested does not have a `stack.yaml` it
  is created automatically using `stack init`.
* Picks up the right version of GHC automatically if multiple versions are
  available in the PATH or from hvr-ghc style ghc/cabal installation.
* build source
* build benchmarks
* build docs
* run tests
* create source distribution
* build from source distribution
* test installation after build
* perform distribution checks
* generate coverage report
* send coverage report to coveralls.io

## Usage Examples

You can run these commands on your local machine as well as inside a CI script.
You can try these commands in the `packcheck` package itself:
```
$ cd packcheck
$ ./packcheck.sh stack RESOLVER=lts-11
$ ./packcheck.sh stack GHCVER=8.2.2
$ ./packcheck.sh stack RESOLVER=lts-7.24 STACK_YAML=stack-8.0.yaml STACK_BUILD_OPTIONS="--flag streamly:examples-sdl" CABALVER=1.24
```

```
$ ./packcheck.sh cabal-new GHCVER=8.4.1
$ ./packcheck.sh cabal GHCVER=7.10.3 CABALVER=1.22
# You can also do a cabal build using stack installed ghc:
$ stack exec ./packcheck.sh cabal RESOLVER=lts-11
```

Run hlint commands on the directories `src` and `test`:
```
$ ./packcheck.sh stack HLINT_COMMANDS="hlint lint src; hlint lint test"
```

Send coverage info of the testsuites named `test1` and `test2` to coveralls.io
using `hpc-coveralls`.  Note that this currently works only with an old-style
cabal build:
```
$ ./packcheck.sh cabal GHCVER=8.0.2 COVERALLS_OPTIONS="test1 test2"
```

## Full Reference

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
