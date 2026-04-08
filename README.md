# Packcheck

[![Hackage](https://img.shields.io/hackage/v/packcheck.svg?style=flat)](https://hackage.haskell.org/package/packcheck)
[![Gitter chat](https://badges.gitter.im/composewell/gitter.svg)](https://gitter.im/composewell/streamly)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/f7c0ncy84cxp8lbe?svg=true)](https://ci.appveyor.com/project/harendra-kumar/packcheck)
[![CircleCI](https://circleci.com/gh/composewell/packcheck/tree/master.svg?style=svg)](https://circleci.com/gh/composewell/packcheck/tree/master)

**Packcheck** is a universal Haskell build and CI script designed for
uniform, consistent testing across all platforms (Linux, macOS, Windows,
FreeBSD). It is the simplest way to build and test your Haskell packages
comprehensively, ensuring your local environment and CI pipeline run
identical checks with minimal configuration.

## Quick Start

### Minimal GitHub Actions Setup

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
        ghc: ["9.12.4", "9.14.1"]
    steps:
      - uses: actions/checkout@v4
      - run: |
          curl -sLO https://raw.githubusercontent.com/composewell/packcheck/master/packcheck.sh
          bash packcheck.sh cabal GHCVER=${{ matrix.ghc }}
```

### Consistent Local Testing

One of Packcheck's core strengths is parity. You can run the exact same
CI tests locally by executing the script directly:

```bash
./packcheck.sh cabal GHCVER=9.14.1
```

If you prefer to commit `packcheck.sh` directly to your repository root,
your GitHub workflow becomes even leaner:

```yaml
on: [push, pull_request]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: ./packcheck.sh cabal
```

---

## Environment & CI Support

Packcheck provides one-line, out-of-the-box configurations for all major
platforms. These templates include optimized, multi-stage caching to
ensure your CI runs are fast and efficient.  The templates are designed
to work immediately for most packages. For fine-grained control, simply
uncomment the labeled lines in the config files to toggle benchmarks,
Haddock generation, or coverage reports.


| CI System | Platforms | Configuration Template |
| :--- | :--- | :--- |
| **GitHub Actions** | Linux, macOS, Windows | [.github/workflows/packcheck.yml](https://github.com/composewell/packcheck/blob/master/.github/workflows/packcheck.yml) |
| **CircleCI** | Linux | [.circleci/config.yml](https://github.com/composewell/packcheck/blob/master/.circleci/config.yml) |
| **Appveyor** | Windows | [appveyor.yml](https://github.com/composewell/packcheck/blob/master/appveyor.yml) |
| **Cirrus CI** | FreeBSD | [.cirrus.yml](https://github.com/composewell/packcheck/blob/master/.cirrus.yml) |

**Build Tools:** Cabal, Stack  
**GHC Provisioning:** System PATH, `ghcup`, Stack-managed GHC

### Implementation
1. **Copy** the relevant configuration file to your repository.
2. **Enable** the project in your CI provider's dashboard.
3. **Customize** by modifying environment variables (e.g., `GHCVER`,
`CABALVER`) in the YAML. Simply uncomment labeled lines to toggle
benchmarks, Haddock generation, or coverage.

---

## Key Features

* **Identical Testing:** Run the exact same test suite locally that runs
on your CI—eliminating "it worked on my machine" issues.
* **Deep Diagnostics:** Precise error messages and detailed build
metadata (tool paths, versions, timing) for every step.
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

## Advanced CI & Caching

For complex projects, Packcheck supports **Split Caching**. The CI is
architected to save progress at logical boundaries:
1.  **Post-Tool Install:** Caches GHC/Cabal/Stack if the build fails early.
2.  **Post-Dependency:** Caches the build artifacts before running your
local tests.

This ensures that even after a failure, your next CI run starts from the
last successful compilation step, significantly reducing iteration time.

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
`docspec` flag. You can also disable the source distribution build to
speed up the check:

```bash
# Run all docspec snippets in the source
./packcheck.sh cabal \
  ENABLE_DOCSPEC="y" \
  DISABLE_SDIST_BUILD="y" \
  DOCSPEC_OPTIONS="--timeout 60" \
  DOCSPEC_URL="https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20250606/cabal-docspec-0.0.0.20250606-x86_64-linux.xz"
```

> **Tip:** Instead of passing the options on the command line, you can also
> export them in your shell environment and just run `packcheck.sh
> cabal` after that.

---

## Pro-Tips & Advanced Logic

* **GHC Resolution:** If `GHCVER` is a prefix (e.g., `9.12`), Packcheck
finds the first matching binary in your `PATH`. Set `GHCVER=head` to
specifically target `ghc-head`.
* **Hybrid Workflows:** You can use Stack-installed GHCs for Cabal builds: 
    `stack exec ./packcheck.sh cabal RESOLVER=lts-21`
* **Template Repo:** This repository is a fully functional "Hello World"
Haskell package. You can clone it as a foundation for new projects to
get CI, testing, and benchmarking pre-configured.
* **Tooling Pass-through:** Use `HLINT_OPTIONS` and
`STACK_BUILD_OPTIONS` to pass raw flags directly to the underlying
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

Ask questions: [https://app.gitter.im/#/room/#composewell_streamly:gitter.im](https://app.gitter.im/#/room/#composewell_streamly:gitter.im)
Report issues: [https://github.com/composewell/packcheck/issues/new](https://github.com/composewell/packcheck/issues/new)

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
--version                : show packcheck version

--------------------------------------------------
Selecting tool versions
--------------------------------------------------
GHCUP_VERSION           : [a.b.c.d] ghcup version to install at $HOME/.ghcup/bin/ghcup (see [https://downloads.haskell.org/~ghcup](https://downloads.haskell.org/~ghcup))
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
SKIP_PRE_DEP            : [y] Skip all the steps before deps, resume main build
SKIP_POST_DEP           : [y] Install dependencies only, skip building the package itself
DISABLE_DOCS            : [y] Do not build haddocks, default is to build docs
ENABLE_DOCSPEC          : [y] Run cabal-docspec after the cabal build
DISABLE_SDIST_BUILD     : [y] Do not build from source distribution
DISABLE_SDIST_PROJECT_CHECK: [y] Ignore project file and continue
DISABLE_SDIST_GIT_CHECK : [y] Do not compare source distribution with git repo
DISABLE_DIST_CHECKS     : [y] Do not perform source distribution checks

--------------------------------------------------
cabal options
--------------------------------------------------
CABAL_REINIT_CONFIG     : [y] DESTRUCTIVE! Remove old config to avoid incompatibility issues
CABAL_PROJECT           : Alternative cabal project file, path relative to project root
CABAL_BUILD_OPTIONS     : ADDITIONAL cabal build options to append to defaults
CABAL_TEST_OPTIONS      : ADDITIONAL cabal test options to append to defaults
CABAL_DISABLE_DEPS      : [y] Do not install dependencies, do not do cabal update
CABAL_BUILD_TARGETS     : cabal build targets, default is 'all'
CABAL_CHECK_RELAX       : [y] Do not fail if cabal check fails on the package.
CABAL_HACKAGE_MIRROR    : DESTRUCTIVE! Specify an alternative mirror, modifies the cabal config file.

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

---

## Advanced Workflows & Diagnostics

#### 1. Bulletproof Reproducibility (`packcheck-safe`)
If you are troubleshooting a "ghost" issue where a build fails due to a
hidden environment variable or a typo (e.g., typing `GHCVWR` instead of
`GHCVER`), use the safe wrapper:

```bash
# packcheck-safe runs in a clean environment and validates all parameter names
./packcheck-safe.sh cabal PATH=/bin:/usr/bin:/opt/ghc/bin GHCVER=9.14.1
```

#### 2. Remote Testing (`packcheck-remote`)
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

#### 3. Performance & Environment Debugging
* **Timeline Reporting:** Packcheck prints the elapsed time for every
build step, allowing you to identify bottlenecks in your CI pipeline.
* **Variable Validation:** Use `CHECK_ENV=y` to force Packcheck to error
out if it detects misspelled or unrecognized environment variables.
