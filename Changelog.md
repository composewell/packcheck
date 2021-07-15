## 0.6.0

### Enhancements

* CABAL_DISABLE_DEPS env var to disable dependencies install by cabal. This can
  be useful when we have dependencies already installed e.g. in a nix shell.
* Add support for github CI
* Add packcheck-remote.sh, a wrapper over packcheck that allows you
  to run packcheck on a remote repository by cloning it locally and
  optionally merging a branch into another branch (e.g. merging a PR
  branch into master).
* Several fixes to make distribution builds safer and with more checks
* Do a sanity check for the existence of files in .packcheck.ignore and
  .hlint.ignore

### Breaking Changes

* "packcheck cabal" now defaults to "packcheck cabal-v2"
* Support for `cabal-v1` is removed
  * CI now fails if `cabal-v1` is used as a command
  * `CABAL_CONFIGURE_OPTIONS` is removed
  * `CABAL_NO_SANDBOX` is removed
  * `packcheck cleanall` does not remove `.cabal-sandbox/` and
    `.cabal.sandbox.config` anymore
* Support for `cabal-new` is removed
  * CI now fails if `cabal-new` is used as a command
  * `CABAL_NEWBUILD_OPTIONS` is removed
  * `CABAL_NEWBUILD_TARGETS` is removed
* A new command `hlint` is introduced. The `hlint` build is only triggered by
  using this command.
* `ENABLE_INSTALL` option has been removed.

## 0.5.1

### Bug Fixes

* Fix breakage due to `DISABLE_SDIST_GIT_CHECK` option. Due to this bug,
  build was always failing by default and reported as success.

### Deprecations

* `HLINT_COMMANDS` is deprecated and replaced by
  `HLINT_OPTIONS`/`HLINT_TARGETS`

### Enhancements

* New `HLINT_OPTIONS`/`HLINT_TARGETS` env vars to specify hlint commands in
  a better way.

## 0.5.0

### Bug Fixes

* `packcheck.sh` script itself was missing from the package, added.

### Breaking Changes

* CI now fails if `DISABLE_SDIST_BUILD` is not set and the contents
  of the source distribution tar ball do not match the git repository
  contents. Either add any exceptions to `.packcheck.ignore` file or use
  `DISABLE_SDIST_GIT_CHECK=y` to disable this feature. Currently this check is
  done only if `git` and `tar` commands are available in the `PATH`.

### Deprecations

* `cabal-v1` command now shows a deprecation message and is removed from help.
  This command will be removed in future.
* `ENABLE_INSTALL` option now does nothing. This change is because of the new
  behavior in cabal-3. This option will be removed in future.

### Enhancements

* Added a feature to detect if any files in the git repo are missing from the
  source distribution tarball.
* Add `CABAL_PROJECT` environment variable to support specifying a cabal
  project file.

## 0.4.2

### Bug Fixes

* When building from source distribution, it would not build again unless
  cleaned with `packcheck clean` if a file in the source has changed.

### Deprecations

* Deprecate and replace the `cabal` command with `cabal-v1`, in future `cabal`
  will be used for `cabal-v2`.
* Deprecate and replace the `cabal-new` command with `cabal-v2`.
* Deprecate and rename `CABAL_NEWBUILD_OPTIONS` to `CABAL_BUILD_OPTIONS`
* Deprecate and rename `CABAL_NEWBUILD_TARGETS` to `CABAL_BUILD_TARGETS`
* Use STACK_BUILD_OPTIONS envvar in the dependency install phase as well
* Remove stack yaml creation using stack init/solver

### Enhancements

* Search for ghc among stack installed GHC binaries as well
* Add GHCJS support. Use ENABLE_GHCJS=y option.
* Add packcheck-safe.sh . The safe version does not trust or use any
  environment variables, all environment needs to be specified on the command
  line. It also catches any misspelled command line parameter names.
* Allow boolean parameters to be specified with a lenient syntax allowing
  values y|Y|yes|Yes|YES|true|True|TRUE|on|On|ON|n|N|no|No|NO|false|False|FALSE|off|Off|OFF

## 0.4.1

* Disable hpc-coveralls by default

## 0.4.0

* Add support for circle CI
* Add support for multi-package stack as well as cabal repos
* Add a version command
* Add CABAL_NEWBUILD_TARGETS envvar to build specific targets
* Add GHC 8.6.1 in build matrices

## 0.3.1

* Add a new environment var option DISABLE_DIST_CHECKS to disable source
  distribution checks. This can be used as a workaround for a bug in stack
  causing "stack sdist" to fail.
* For stack builds, use the same options (STACK_BUILD_OPTIONS) for install test
  as for build so that an extra rebuild does not occur during install.
* Workaround to avoid depending on `cabal info` command; in certain cases this
  command crashes cabal. See issue #13.

## 0.3.0

### Enhancements
* Add cabal new-build support. Use `packcheck.sh cabal-new` to use it.
* Add knobs to disable tests or doc builds (`DISABLE_TEST`, `DISABLE_DOCS`)
* Now you can specify multiple versions of GHC in PATH and packcheck
  automatically finds the right one based on GHCVER envvar.
* Add TOOLS_DIR option to specify hvr-ghc style installation of ghc and
  cabal. A correct version of GHC is automatically picked from this directory.
* GHCVER and CABALVER variables are now optional in travis config if you
  specify the cabal and ghc PPAs under apt sources.
* Run `autoreconf` if there is a `configure.ac` in the package dir

### Deprecations
* TEST_INSTALL option is deprecated, use ENABLE_INSTALL instead

## 0.2.0

### Breaking Changes
* Make `STACK_BUILD_OPTIONS` and `CABAL_CONFIGURE_OPTIONS` append to the
  existing build/configure options instead of overriding them.
* Do not enforce specific `stack` version in CI configs - this is done to avoid
  failures due to github API limits when upgrading or downgrading.

### Bug Fixes
* Avoid build failures in cases when `cabal-install` has to be installed and
  its dependencies may conflict with the current project dependencies.

### Enhancements
* Better documentation in travis and appveyor configs
* Reduce the number of builds in default config from 11 to 6

## 0.1.1

* _Enhancement_: Nix support; fix bash location to make it work on NixOS and
  potentially on other systems.

## 0.1.0

* Initial release
