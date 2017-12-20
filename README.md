## Universal Haskell Package Build and Test

In any Haskell package on any machine (your local Linux/OSX/Windows or
Travis/AppVeyor) just type a simple CLI command and viola, it builds and
comprehensively tests the package automagically.  You can choose `stack` or
`cabal` build type and myriad other options whatever you like. Simply:

```
$ env BUILD=stack package-test.sh
$ env BUILD=cabal package-test.sh
```

If you are familiar with one build tool (stack vs cabal) but not the other,
this script can simply let you comprehensively build and test with that without
knowing anything about it.

Salient features:

* ***specifically designed for use with CI*** or pre-release testing, that's
why it takes options as environment variables.
* ***installs all the required tools automatically*** (including stack) or lets
you know what it needs so you can install/use your own. It never overwrites
an existing tool during install.
* ***tests the source distribution of the package*** so you can rest assured
that what you release on hackage is exactly what you tested. Also performs
distribution checks and whether the package installs successfully.
* ***tests if documents (haddock) are built successfully***.
* ***reproduces a failed CI on local machine***.  You can just cut
and paste the same command on your local machine and run it there for easy
debugging.
* ***can send coverage information to coveralls.io*** with a simple option.

## Usage Examples

You can run these commands on your local machine or they can be used in a CI
script as well.

Make sure you are in the package directory. You can try these commands in the
`package-test` package itself:
```
$ cd package-test
```

Stack build (installs stack automatically if not found):
```
$ env BUILD=stack RESOLVER=lts-6 ./package-test.sh
```

Set pvp-bounds before the test:
```
$ env BUILD=stack RESOLVER=lts-6 SDIST_OPTIONS="--pvp-bounds both" ./package-test.sh
```

Stack build with system installed GHC, when GHCVER is specified it looks for
the specified GHC version in PATH:
```
$ env BUILD=stack GHCVER=7.10.3 ./package-test.sh
```

Stack build with a specific `stack.yaml` config file and specified build flags,
and requiring a specific cabal version:
```
$ env BUILD=stack RESOLVER=lts-7.24 STACK_YAML=stack-8.0.yaml STACK_BUILD_OPTIONS="--flag streamly:examples-sdl" CABALVER=1.24 ./package-test.sh
```

Cabal build using stack installed ghc:
```
$ stack exec env BUILD=cabal RESOLVER=lts-6 ./package-test.sh
```

Cabal build using system installed ghc and cabal on PATH:
```
$ env BUILD=cabal GHCVER=7.10.3 CABALVER=1.22.9.0 ./package-test.sh
```

Send coverage info of the testsuites named `test1` and `test2` to coveralls.io
using `hpc-coveralls`.  Note that this currently works only with a cabal build:
```
$ env BUILD=cabal GHCVER=8.0.2 COVERALLS_OPTIONS="test1 test2" ./package-test.sh
```

Use clean environment, makes sure there is no interference from existing
environment:
```
$ env -i PATH=/bin:/usr/bin BUILD=stack ./package-test.sh
```

## Using with Travis or Appveyor CI
Just copy `.travis.yml` and `appveyor.yml` files from this package to your
package and you should be good to go unless your package has some special
requirements, external dependencies etc. If needed you can tweak the CI config
files to your taste.

You can also copy the `package-test.sh` and commit it to your repo. This will
allow you to conveniently run exactly the same build and test on your local
machine that you would run on a CI host. Of course, if you don't commit it to
your repo you you can also run it from somewhere else when needed.

## Full Reference

Options marked `DESTRUCTIVE!` are fine in a CI environment. But on a
local machine sometimes it may not be desirable as it will change the
state of your global cabal config, so consider that before using these options.

By default it uses cabal sandbox builds. It creates any temporary files or
build artifacts inside `.package-test` directory. You can remove that directory
after the build to release the space if needed.

stack is automatically installed and can be used to do cabal builds as well. If
you specify `BUILD=cabal` and `RESOLVER` at the same time then the cabal build
uses stack installed `cabal` and `ghc`, both are installed automatically when
needed.

For pure cabal builds i.e. when `BUILD=cabal` and `RESOLVER` is not specified,
`cabal` and `ghc` must be pre-installed.

```
cueball $ package-test.sh --help
------------------------------------------
Example usage
------------------------------------------
env BUILD=stack RESOLVER=lts-9 ./package-test.sh
env BUILD=cabal ./package-test.sh
env BUILD=stack HLINT_OPTIONS=. ./package-test.sh

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
