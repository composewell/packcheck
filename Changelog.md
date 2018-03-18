## Unreleased

* _Enhancement:_ Add cabal new-build support. Use `packcheck.sh cabal-new` to
  use it.
* Run autoreconf if there is a configure.ac in the package dir

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
