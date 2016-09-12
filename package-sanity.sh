#!/bin/bash

# Works on Linux and OSX
# Installs stack if required
# On OSX - Installs ghc using stack and puts it on PATH
# Uses stack to create a dist of the pkg in CURRENT DIR
# Unpacks, builds and tests the distribution using stack or cabal

# Pass the following build params via environment variables
# BUILD=cabal | stack
# GHCVER=x.y.z (stack build will install ghc if this is not specified)

# Only for cabal builds
# CABALVER=x.y.z
# STACK_SDIST=y (optional)
#   always use stack for sdist. This will require stack to be installed even
#   for a cabal build.

# Only when using stack for sdist
# PVP_BOUNDS=

# Only for stack builds
# RESOLVER=<resolver> (optional)
# STACK_YAML=filename or path (will be created by stack init --solver if missing)
#
# For example:
# env BUILD=cabal GHCVER=7.8.4 CABALVER=1.24 STACK_SDIST=y
#     STACK_YAML=stack-7.8.yaml PVP_BOUNDS=both

# TODO
# COVERAGE=y Generate coverage
# COVERALLS=y Generate coverage report and send it to coveralls.io

# $1: varname
show_var() {
  echo "$1=$(eval \$$1)"
}

# $1: var name
# $2: default value
init_default() {
  test -n "$(eval \$$1)" || eval "export $1=$2"
}

# Set the defaults if no environment is passed to us
init_build_config() {
  init_default BUILD    stack
  init_default GHCVER   7.10.3
  init_default CABALVER 1.22

  STACKCMD="stack --no-terminal"

  case STACK_SDIST in
    y|Y|yes|YES|Yes)
      NEED_STACK_YAML=true
      SDIST_CMD="$STACKCMD sdist" ;;
    *) STACK_SDIST=
      if test "$BUILD" = stack
      then
         SDIST_CMD="$STACKCMD sdist"
      else
         SDIST_CMD="cabal sdist"
      fi ;;
  esac
}

show_build_config() {
  show_var BUILD
  show_var GHCVER
  test "$BUILD" = stack -o -n STACK_SDIST && show_var PVP_BOUNDS

  if test "$BUILD" = cabal
  then
    show_var CABALVER
    show_var STACK_SDIST
  else
    test -z "$CABALVER" || die "CABALVER is meaningful only for cabal build"
    test -z "$STACK_SDIST" || die "STACK_SDIST is meaningful only for cabal build"
  fi

  if test "$BUILD" = stack -o -n "$STACK_SDIST"
  then
    show_var RESOLVER
    show_var STACK_YAML
  else
    test -z "$RESOLVER" || die "RESOLVER is not meaningful in this config"
    test -z "$STACK_YAML" || die "STACK_YAML is not meaningful in this config"
  fi
}

require_file () {
  if test ! -f "$1"
  then
    echo "Required file [$1] does not exist."
    exit 1
  fi
}

ensure_stack_yaml() {
  if test "$BUILD" = stack -o -n "$NEED_STACK_YAML"
  then
    if test -n "$STACK_YAML"
      require_file $STACK_YAML
    elif test ! -e stack.yaml
      $STACKCMD init --solver
      require_file stack.yaml
    fi
  fi
  test -n "$STACK_YAML" || STACKCMD="$STACKCMD --stack-yaml $STACK_YAML"
}

ensure_ghc() {
}

# $1: message
function die () {
  >&2 echo -e $1
  exit 1
}

retry_cmd() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

fetch_stack_osx() {
  curl -skL https://www.stackage.org/stack/osx-x86_64 \
    | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin
}

fetch_stack_linux() {
  curl -sL https://www.stackage.org/stack/linux-x86_64 \
    | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
}

fetch_stack() {
  mkdir -p ~/.local/bin
  if [ `uname` = "Darwin" ]
  then
    retry_cmd fetch_stack_osx
    # Use stack installed ghc on OSX
    retry_cmd $STACKCMD setup
  else
    retry_cmd fetch_stack_linux
  fi
}

use_stack_paths() {
    STACKPATH=`$STACKCMD path --bin-path`
    if test -n "$STACKPATH"
    then
      export PATH=$STACKPATH
    fi
    unset STACKPATH
}

setup_path() {
  PATH=/opt/ghc/$GHCVER/bin:$PATH
  case "$BUILD" in
       stack) export PATH=$HOME/.local/bin:$PATH;;
       cabal) export PATH=$HOME/.cabal/bin:/opt/cabal/$CABALVER/bin:$PATH;;
  esac
  use_stack_paths
  echo "PATH is [$PATH]"
}

init_cabal_config() {
  mkdir -p $HOME/.cabal
  cat > $HOME/.cabal/config <<EOF
remote-repo: hackage.haskell.org:http://hackage.fpcomplete.com/
remote-repo-cache: $HOME/.cabal/packages
jobs: \$ncpus
EOF
}

require_cmd () {
  if test -z "$(type -t $1)"
  then
    echo "Required command [$1] not found in PATH [$PATH]."
    exit 1
  fi
}

# $1: tool name
# $2: expected version
check_version() {
  local real_ver=$($1 --numeric-version)
  test "$2" = "$real_ver" || die "Wrong $1 version [$real_ver] expected [$2]"
}

show_tools() {
  require_cmd stack && $STACKCMD --version
  require_cmd ghc && check_version ghc $GHCVER
             && echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
  require_cmd cabal && check_version cabal && cabal --version
}

get_pkg_name() {
  local name=$(echo *.cabal)
  name=${name%.cabal}
  test -f "${name}.cabal" || \
    die "Cannot determine package name. File [${name}.cabal] does not exist"
}

get_pkg_full_name() {
  local pkgname=$(get_pkg_name)
  local full_name=$(cabal info . | awk '{print $2;exit}')
  if test "${pkgname}${full_name#$pkgname}" != "${full_name}"
  then
    echo "Cabal file name does not match the package name."
    exit 1
  fi
}

# $1: package full name (name + ver)
create_and_unpack_pkg_dist() {
  echo "Creating and unpacking distribution of [$1]"

  local pkgtar=${1}.tar.gz
  local tarpath=$($STACKCMD --compiler=ghc-$GHCVER path --dist-dir)/${pkgtar}
  rm -f $tarpath
  $STACKCMD --compiler=ghc-$GHCVER sdist --pvp-bounds=both
  if test ! -f $tarpath
  then
    echo "stack sdist did not create [$tarpath]"
    exit 1
  fi
  tar xzvf $tarpath
}

# $1 package dir
install_deps() {
  echo "Installing dependencies for [$1]"
  cd $1
  case "$BUILD" in
    stack) $STACKCMD test --only-dependencies
    cabal)
      cabal --version;
      retry_cmd cabal update
      cabal install --only-dependencies \
                    --enable-tests \
                    --enable-benchmarks \
                    --force-reinstalls \
                    --ghc-options=-O0 \
                    --reorder-goals \
                    --max-backjumps=-1
  esac
}

# $1 package dir (name + ver)
# $2 package name
build_and_test() {
  echo "Building and testing [$1]"
  cd $1
  case "$BUILD" in
    stack)
        $STACKCMD test --haddock --no-haddock-deps --ghc-options="-Werror";;
    cabal)
      cabal configure -v2 \
                       --enable-tests \
                       --enable-benchmarks \
                       --ghc-options="-O0 -Werror"
      cabal build
      cabal test
      cabal check
      cabal sdist
      cabal copy
      (cd dist && cabal install --force-reinstalls "${1}.tar.gz")
      rm -f $HOME/.cabal/bin/$2 ;;
  esac
}

set -eux
set -o pipefail
unset CC

init_build_config
#fetch_stack
#init_cabal_config
setup_path
show_tools

PACKAGE_NAME=$(get_pkg_name)
PACKAGE_FULL_NAME=$(get_pkg_full_name)
create_and_unpack_pkg_dist $PACKAGE_FULL_NAME
install_deps $PACKAGE_FULL_NAME
build_and_test $PACKAGE_FULL_NAME $PACKAGE_NAME
