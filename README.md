# Packcheck

[![Hackage](https://img.shields.io/hackage/v/packcheck.svg?style=flat)](https://hackage.haskell.org/package/packcheck)
[![Gitter chat](https://badges.gitter.im/composewell/gitter.svg)](https://gitter.im/composewell/streamly)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/f7c0ncy84cxp8lbe?svg=true)](https://ci.appveyor.com/project/harendra-kumar/packcheck)
[![CircleCI](https://circleci.com/gh/composewell/packcheck/tree/master.svg?style=svg)](https://circleci.com/gh/composewell/packcheck/tree/master)

**Packcheck** is a simple tool that makes it easy to build a Haskell
package. With a single command, it will build your package, run tests,
benchmarks, and doctests, generate documentation, check the package,
validate source distributions, and even verify that everything builds
correctly from the distribution.

You don’t need to set up or install tools like GHC or Cabal yourself;
Packcheck handles finding or installing all the tools for you. The same
command works across Linux, macOS, Windows, and FreeBSD, so you get
consistent results locally and in CI without extra configuration.

## Quick Start

### Local Testing

Easiest way to try it out is to download the `packcheck.sh` script and run
it from within your Haskell package repo locally.

```bash
curl -sLO https://raw.githubusercontent.com/composewell/packcheck/master/packcheck.sh
bash packcheck.sh cabal
```

### Running in GitHub Actions

One of Packcheck's core strengths is parity. You can run the exact same
tests locally or in the CI.

The following `github.yml` uses the default GHC available on the runner
(typically the latest stable version).

```yaml
on: [push, pull_request]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: |
          curl -sLO https://raw.githubusercontent.com/composewell/packcheck/master/packcheck.sh
          bash packcheck.sh cabal
```

> **Tip:** Replace `master` with a specific commit hash for production
> environments to ensure build reproducibility.

### Testing Across a GHC Matrix

To verify your package against multiple GHC versions, use a strategy matrix:

```yaml
on: [push, pull_request]
jobs:
  build:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        ghc: ["9.14.1", "9.12.4", "9.10.3"]
    steps:
      - uses: actions/checkout@v4
      - run: |
          curl -sLO https://raw.githubusercontent.com/composewell/packcheck/master/packcheck.sh
          bash packcheck.sh cabal GHCVER=${{ matrix.ghc }}
```

### Fast CI Testing

Packcheck supports **Split Caching** which makes CI as fast as your
local testing. Packcheck can be invoked to install dependencies and
exit, then we save the cache and resume. After failures CI can resume
from building your package instead of installing dependencies, making it
fast and amenable to iterative development.

See the
[github CI template](https://github.com/composewell/packcheck/blob/master/.github/workflows/packcheck.yml) |
for an example use.

---

## OS and CI Support

Packcheck provides out-of-the-box configuration files for all major
OS and CI platforms. These templates include optimized, multi-stage caching to
ensure your CI runs are fast and efficient.  The templates are designed
to work immediately for most packages. For fine-grained control, simply
uncomment the labeled lines in the config files to toggle benchmarks,
Haddock generation, or coverage reports.

| CI System | Platforms | Configuration Template |
| :--- | :--- | :--- |
| **GitHub Actions** | Linux, macOS, Windows | [.github/workflows/packcheck.yml](https://github.com/composewell/packcheck/blob/master/.github/workflows/packcheck.yml) |
| **Cirrus CI** | FreeBSD | [.cirrus.yml](https://github.com/composewell/packcheck/blob/master/.cirrus.yml) |
| **Appveyor** | Windows | [appveyor.yml](https://github.com/composewell/packcheck/blob/master/appveyor.yml) |
| **CircleCI** | Linux | [.circleci/config.yml](https://github.com/composewell/packcheck/blob/master/.circleci/config.yml) |

**Build Tools:** Cabal, Stack  
**GHC Provisioning:** System PATH, `ghcup`, Stack-managed GHC

### Implementation
1. **Copy** the relevant configuration file to your repository.
2. **Enable** the project in your CI provider's dashboard.
3. **Customize** the environment variables in the YAML.

---

## Key Features

* **Identical Testing:** Run the exact same test suite locally that runs
on your CI—eliminating "it worked on my machine" issues.
* **Deep Diagnostics:** Precise error messages and detailed build
metadata (tool paths, versions, OS CPU, memory, disk space). Prints
timing for every step for build performance debugging.
* **Production-Ready Checks:**
    * **SDist Verification:** Builds from the generated tarball to
    ensure no files are missing from your release.
    * **Git Parity:** Automatically alerts you if files in your
    repository are missing from the source distribution.
* **Smart GHC Discovery:** Automatically resolves GHC versions from
PATH, `ghcup` (with auto-install), or Stack.
* **Zero-Footprint:** Non-destructive by default. It won’t alter your
global configs or upgrade tools unless explicitly requested.
* **Declarative Control:** Toggle benchmarks, docs, coverage, or `hlint`
via simple environment variables.
* **Docspec Integration:** Seamlessly run doctest-style snippets within
your source code using `cabal-docspec`. Toggle it on with a single
variable and optionally provide a custom binary URL for specific
environments.

---

## Usage Examples

Packcheck is driven by environment variables, making it highly
scriptable for both local use and CI.

```bash
# Test using a specific GHC version via ghcup
./packcheck.sh cabal GHCUP_VERSION=0.1.50.0 GHCVER=9.14.1

# Test using Stack with a specific resolver
./packcheck.sh stack RESOLVER=lts-21

# Run hlint on specific directories
./packcheck.sh hlint HLINT_TARGETS="src test"

# Run a cabal build using a stack-installed GHC
stack exec ./packcheck.sh cabal RESOLVER=lts-21
```

To verify the documentation snippets in your source files, enable the
`docspec` flag.

```bash
# Run all docspec snippets in the source
./packcheck.sh cabal \
  ENABLE_DOCSPEC="y" \
  DOCSPEC_URL="https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20250606/cabal-docspec-0.0.0.20250606-x86_64-linux.xz" \
  DOCSPEC_OPTIONS="--timeout 60"
```

> **Tip:** Instead of passing the options on the command line, you can also
> export them in your shell environment and just run `packcheck.sh
> cabal` after that.

---

## Pro-Tips & Advanced Logic

* **Reproducibility:** When `CHECK_ENV=y` is enabled, `packcheck`
ignores all environment variables and relies solely on command-line
inputs; it can also detect and report mistyped variable names. We
recommend using this mode by default, especially when troubleshooting
“ghost” issues caused by unintended environment variables or typos
(e.g., `GHCVWR` instead of `GHCVER`).
* **GHC Resolution:** If `GHCVER` is a prefix (e.g., `9.12`), Packcheck
finds the first matching binary in your `PATH`. Set `GHCVER=head` to
specifically target `ghc-head`.
* **Hybrid Workflows:** You can use Stack-installed GHCs for Cabal builds: 
    `stack exec ./packcheck.sh cabal RESOLVER=lts-21`
* **Template Repo:** This repository is a fully functional "Hello World"
Haskell package. You can clone it as a foundation for new projects to
get CI, testing, and benchmarking pre-configured.
* **Tooling Pass-through:** Use `HLINT_OPTIONS` and
`CABAL_BUILD_OPTIONS` to pass raw flags directly to the underlying
tools.
* **Split Caching Control:** Use `SKIP_POST_DEP` and `SKIP_PRE_DEP` in
your CI YAML to manually manage where caching starts and stops.

---

## Why Packcheck?

Most CI scripts are fragile collections of shell commands.
`packcheck.sh` is a high-level, universal "super-build" script written
in `bash` to ensure it works everywhere without dependencies. It
wraps the complexity of cross-platform Haskell tooling into a single,
consistent interface. Whether you are a beginner learning Haskell
metadata or a maintainer managing complex libraries, Packcheck ensures
your builds are reproducible, transparent, and fast.

## Full Reference

> **Note:** Any parameter can be passed either as a command-line
> argument or an environment variable. Command-line arguments are ideal
> for interactive local use, while environment variables are preferred
> for CI configurations.

```bash
# Show all available commands and options
./packcheck.sh --help
```

### Help Output Reference
```text
--------------------------------------------------
Usage
--------------------------------------------------

packcheck.sh COMMAND [PARAMETER=VALUE ...]

For example:
packcheck.sh cabal GHCVER=9.8.1
packcheck.sh stack RESOLVER=lts GHC_OPTIONS="-O0 -Werror"
packcheck.sh hlint

Ask questions: https://app.gitter.im/#/room/#composewell_streamly:gitter.im
Report issues: https://github.com/composewell/packcheck/issues/new

Control parameters can either be passed on command line or exported
as environment variables. Parameters marked DESTRUCTIVE may modify
your global user config or state.

Boolean parameters can be specified as
y|Y|yes|Yes|YES|true|True|TRUE|on|On|ON for an affirmative value and as
n|N|no|No|NO|false|False|FALSE|off|Off|OFF or empty for a negative value.

--------------------------------------------------
Commands and flags
--------------------------------------------------

cabal                   : build using cabal
stack                   : build using stack
hlint                   : run hlint
clean                   : remove the .packcheck directory
cleanall                : remove .packcheck, .stack-work directories
help | --help | -h      : show this help message
--version               : show packcheck version

--------------------------------------------------
Selecting tool versions
--------------------------------------------------

GHCUP_VERSION           : [a.b.c.d] or 'latest' to install at $HOME/.ghcup/bin/ghcup (for versions see https://downloads.haskell.org/~ghcup)
GHCVER                  : [a.b.c | head] GHC version prefix (may not be enforced when using stack)
CABALVER                : [a.b.c.d] Cabal version (prefix) to use
STACKVER                : [a.b.c.d] Stack version (prefix) to use
STACK_UPGRADE           : [y] DESTRUCTIVE! Upgrades stack to latest version
RESOLVER                : Stack resolver to use for stack builds or cabal builds using stack
HLINT_VERSION           : hlint version to install at $HOME/.local/bin/hlint (see [https://github.com/ndmitchell/hlint/releases](https://github.com/ndmitchell/hlint/releases))
DOCSPEC_URL             : cabal-docspec release URL to install at $HOME/.local/bin/cabal-docspec (see [https://github.com/phadej/cabal-extras/releases/](https://github.com/phadej/cabal-extras/releases/))

--------------------------------------------------
Where to find the required tools
--------------------------------------------------

PATH                    : [path] Set PATH explicitly for predictable builds
TOOLS_DIR               : [dir] Find ghc|cabal by version as in TOOLS_DIR/ghc/<version>/bin

--------------------------------------------------
Specifying common tool options
--------------------------------------------------

GHCUP_GHC_OPTIONS       : Used as in "ghcup install ghc <GHCUP_GHC_OPTIONS> <version>"
GHC_OPTIONS             : Specify GHC options to use
SDIST_OPTIONS           : Arguments to stack/cabal sdist command

--------------------------------------------------
Specifying what to build
--------------------------------------------------

DISABLE_BENCH           : [y] Do not build benchmarks, default is to build but not run
DISABLE_TEST            : [y] Do not run tests, default is to run tests
DISABLE_DOCS            : [y] Do not build haddocks, default is to build docs
ENABLE_DOCSPEC          : [y] Run cabal-docspec after the cabal build
DISABLE_SDIST_BUILD     : [y] Do not build from source distribution
DISABLE_SDIST_PROJECT_CHECK: [y] Ignore project file and continue
DISABLE_SDIST_GIT_CHECK : [y] Do not compare source distribution with git repo
DISABLE_DIST_CHECKS     : [y] Do not perform source distribution checks

--------------------------------------------------
Skipping parts of build (for split caching)
--------------------------------------------------

BUILD_DEPS_ONLY         : [y] Build dependencies and exit
BUILD_PACKAGE_ONLY      : [y] Skip tools/deps; build local package only

--------------------------------------------------
cabal options
--------------------------------------------------

CABAL_REINIT_CONFIG     : [y] DESTRUCTIVE! Remove old config to avoid incompatibility issues
CABAL_PROJECT           : Alternative cabal project file, path relative to project root
CABAL_BUILD_OPTIONS     : ADDITIONAL cabal build options to append to defaults
CABAL_BUILD_TARGETS     : cabal build targets, default is 'all'
CABAL_HADDOCK_TARGETS   : cabal haddock targets, default is '.'
CABAL_DISABLE_DEPS      : [y] Do not install deps, no cabal update, useful for nix
CABAL_TEST_OPTIONS      : ADDITIONAL cabal test options to append to defaults
CABAL_CHECK_RELAX       : [y] Do not return failure if 'cabal check' fails on the package.
CABAL_HACKAGE_MIRROR    : DESTRUCTIVE! Specify an alternative mirror, modifies the cabal config file.
CABAL_INDEX_TTL         : Do not attempt cabal update until it is stale by this many hours.
HADDOCK_OPTIONS         : ADDITIONAL haddock build options to append to defaults

--------------------------------------------------
stack options
--------------------------------------------------

STACK_YAML              : Alternative stack config file path relative to project root
STACK_OPTIONS           : ADDITIONAL stack global options (e.g. -v) to append
STACK_BUILD_OPTIONS     : ADDITIONAL stack build command options to append

--------------------------------------------------
hlint options
--------------------------------------------------

HLINT_OPTIONS           : hlint arguments e.g.'--datadir=.'
HLINT_TARGETS           : target directories to run hlint on e.g. 'src test'

--------------------------------------------------
Coverage options
--------------------------------------------------

COVERAGE                : [y] Just generate coverage information

--------------------------------------------------
Diagnostics options
--------------------------------------------------

CHECK_ENV               : [y] Treat unknown env variables as error, used with env -i
BASE_TIME               : System time to be used as base for timeline reporting
```

### Critical Behavioral Notes
* **SDist Parity:** By default, the build fails if the source
distribution (`.tar.gz`) does not match your git repository. This
ensures you only release what you have committed. You can bypass this
with `DISABLE_SDIST_GIT_CHECK=y`.
* **Destructive Options:** Parameters marked `DESTRUCTIVE!` (like
`STACK_UPGRADE` or `CABAL_REINIT_CONFIG`) are safe for CI but should
be used cautiously on local machines as they modify global user
configurations.
* **Sandboxing:** Cabal builds are performed in a sandbox. All temporary
artifacts are stored in the `.packcheck` directory to keep your project
root clean.

<!--
---

## Remote Testing (`packcheck-remote`)
You can run Packcheck on a remote repository without manually cloning
it. This is particularly useful for verifying Pull Requests locally
before merging.

```bash
# Clones the repo, merges a branch, and runs the build
./packcheck-remote.sh --force \
    --remote=[https://github.com/user/repo](https://github.com/user/repo) \
    --checkout=origin/master \
    --merge=origin/feature-branch \
    --directory=./temp_build \
    -- cabal GHCVER=9.12.1
```
-->
