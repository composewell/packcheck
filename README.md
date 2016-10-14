## Haskell Package Pre-release or CI Testing

A convenient and versatile shell script to test your package:
* creates and then tests the **source distribution**
* easily test on Travis or Appveyor or any other CI
* or simply on your local machine
* can send coverage info to coveralls.io
* use cabal or stack for builds
* build on Linux, MacOS or Windows
* installs required tools automatically
* a lot more flexibility available

It first creates a source distribution of the package, unpacks it and then
tests rather than testing directly from the git repo to make sure that your
package is ready to upload on Hackage without any further testing.

## Usage

For convenience in CI use case, options are passed via environment variables.
### Local Machine

Make sure you are in the package directory:
```
$ cd package-dir
```

Stack build (installs stack automatically if not found):
```
$ env BUILD=stack RESOLVER=lts-6 package-test.sh
```

Cabal build using stack installed ghc:
```
$ env BUILD=cabal RESOLVER=lts-6 package-test.sh
```

Cabal build using system installed ghc and cabal on PATH:
```
$ env BUILD=cabal GHCVER=7.10.3 CABALVER=1.22.9.0 package-test.sh
```

### Travis or Appveyor CI

You can embed this script in a travis or appveyor YAML config and pass
options via environment variables. Here are some example CI scripts:

* [Travis script for xls
  package](https://github.com/harendra-kumar/xls/blob/master/.travis.yml)
* [Appveyor script for xls
  package](https://github.com/harendra-kumar/xls/blob/master/appveyor.yml)

### More Examples

Stack build with system installed GHC:
```
$ env BUILD=stack GHCVER=7.10.3 package-test.sh
```

Send coverage info to coveralls:
```
$ env BUILD=stack COVERALLS_OPTIONS="test1 test2" package-test.sh
```

Set pvp-bounds before the test:
```
$ env BUILD=stack RESOLVER=lts-6 SDIST_OPTIONS="--pvp-bounds both" package-test.sh
```

Use clean environment:
```
$ env -i PATH=/bin:/usr/bin BUILD=stack package-test.sh
```

## All Configurable Options

Options marked `DESTRUCTIVE!` are fine in a CI environment. But on a
local machine sometimes it may not be desirable as it will change the
state of your global config, so consider that before using these options.

```
cueball $ package-test.sh --help
------------------------------------------
Commonly used env variables
------------------------------------------
BUILD                   : [stack | cabal] The only mandatory option
RESOLVER                : Stack resolver to use for stack or cabal builds
GHCVER                  : [a.b.c] GHC version prefix (may not be enforced when using stack)
CABALVER                : [a.b.c.d] Cabal version prefix for cabal builds
GHC_OPTIONS             : Specify GHC options to use
SDIST_OPTIONS           : Argument to stack sdist (e.g. --pvp-bounds)
TEST_INSTALL            : [y] DESTRUCTIVE! Install the package after building (force install with cabal)
PATH                    : [path] Set PATH explicitly for predictable builds

------------------------------------------
Advanced stack build env variables
------------------------------------------
STACK_YAML              : Alternative stack config file to use
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
COVERALLS_OPTIONS       : [test suite names] Send coverage to coveralls.io
COVERAGE                : [y] Just generate coverage information

------------------------------------------
Diagnostics
------------------------------------------
CHECK_ENV               : Treat unknown env variables as error, used with env -i
```

## How it works

* Install stack if not found and stack build is specified
* Create source dist using `stack sdist` or `cabal sdist`
* Unpack the source dist in `.sanity-test` dir
* By default it uses a sandbox for cabal builds
* Build and test the unpacked sdist
* Perform distribution checks and test installation
