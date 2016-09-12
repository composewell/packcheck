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

# cabal build can use stack ghc if GHCVER is not specified
# Be interactive and warn about auto-install etc when run from a terminal

#------------------------------------------------------------------------------
# Utility functions
#------------------------------------------------------------------------------

# $1: varname
show_var() {
  echo "$1=$(eval \"echo \$$1\")"
}

# $1: varname
show_nonempty_var() {
  local var=$(eval "echo \$$1")
  if test -n "$var"
  then
    echo "$1=$var"
  fi
}

# $1: var name
# $2: default value
init_default() {
  local var=$(eval "echo \$$1")
  test -n "$var" || eval "export $1=$2"
}

require_file () {
  if test ! -f "$1"
  then
    echo "Required file [$1] does not exist."
    exit 1
  fi
}

# $1: message
die () {
  >&2 echo -e $1
  exit 1
}

retry_cmd() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

require_cmd () {
  if test -z "$(type -t $1)"
  then
    echo "Required command [$1] not found in PATH [$PATH]."
    exit 1
  fi
}

#------------------------------------------------------------------------------
# Build config show and determine
#------------------------------------------------------------------------------

# $1: msg
show_build_config() {
  echo "$1"
  show_nonempty_var BUILD
  show_nonempty_var GHCVER
  show_nonempty_var PVP_BOUNDS
  show_nonempty_var RESOLVER
  show_nonempty_var STACK_YAML
  show_nonempty_var CABALVER
  show_nonempty_var STACK_SDIST
  echo "----End build config----"
}

# Rationalise the STACK_SDIST global var
use_stack_sdist() {
  case STACK_SDIST in
    y|Y|yes|YES|Yes) echo true ;;
    *) : ;;
  esac
}

verify_build_config() {
  init_default BUILD stack

  if test "$BUILD" != cabal
  then
    test -z "$CABALVER" || die "CABALVER is meaningful only for cabal build"
    test -z "$STACK_SDIST" || die "STACK_SDIST is meaningful only for cabal build"
  fi

  if test "$BUILD" = cabal -a -z "$STACK_SDIST"
  then
    test -z "$RESOLVER" || die "RESOLVER is not meaningful in this config"
    test -z "$STACK_YAML" || die "STACK_YAML is not meaningful in this config"
  fi
}

get_sdist_cmd() {
  if test "$BUILD" = stack -o -n "$STACK_SDIST"
  then
     echo "$STACKCMD sdist"
  else
     echo "cabal sdist"
  fi
}

need_stack() {
  if test "$BUILD" = stack -o -n "$STACK_SDIST"
  then
    echo true
  fi
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

ensure_stack() {
  case STACK_SDIST in
    y|Y|yes|YES|Yes)
      NEED_STACK=true
      SDIST_CMD="$STACKCMD sdist" ;;
    *) STACK_SDIST=
      if test "$BUILD" = stack
      then
         SDIST_CMD="$STACKCMD sdist"
      else
         SDIST_CMD="cabal sdist"
      fi ;;
  esac

  if test "$BUILD" = stack -o -n "$NEED_STACK"
  then
    # User specified PATH takes precedence
    export PATH=$PATH:$HOME/.local/bin
    if test -z "$(type -t stack)"
    then
      fetch_stack
    fi
    require_cmd stack
    # stack upgrade
    STACKCMD="stack --no-terminal"
    echo "Using stack..."
    $STACKCMD --version
  fi
}

use_travis_paths() {
  test -n "$GHCVER" && PATH=/opt/ghc/$GHCVER/bin:$PATH
  if test "$BUILD" = "cabal" -a -n "$CABALVER"
  then
     export PATH=$HOME/.cabal/bin:/opt/cabal/$CABALVER/bin:$PATH
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

# $1: tool name
# $2: expected version
check_version() {
  local real_ver=$($1 --numeric-version)
  test "$2" = "$real_ver" || die "Wrong $1 version [$real_ver] expected [$2]"
}

ensure_ghc() {
  if test -n "$(need_stack)" -a -z "$GHCVER"
  then
    # Use stack supplied ghc
    use_stack_paths
  fi
  require_cmd ghc && \
    echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
  if test -n "$GHCVER" 
  then
    check_version ghc $GHCVER
  else
    GHCVER=$(ghc --numeric-version) || exit 1
  fi
}

init_cabal_config() {
  mkdir -p $HOME/.cabal
  cat > $HOME/.cabal/config <<EOF
remote-repo: hackage.haskell.org:http://hackage.fpcomplete.com/
remote-repo-cache: $HOME/.cabal/packages
jobs: \$ncpus
EOF
}

ensure_cabal() {
  require_cmd cabal && cabal --version
  test -n "$CABALVER" && check_version cabal $CABALVER
  init_cabal_config
}

ensure_stack_yaml() {
  if test -n "$STACK_YAML"
  then
    require_file $STACK_YAML
  elif test ! -e stack.yaml
  then
    # solver needs cabal
    #if test -z "$(type -t cabal)"
    #then
    # $STACKCMD install cabal-install
    #fi
    $STACKCMD init --solver
    require_file stack.yaml
  fi
  test -n "$STACK_YAML" && STACKCMD="$STACKCMD --stack-yaml $STACK_YAML"
  echo "Using stack command [$STACKCMD]"
}

get_pkg_name() {
  local name=$(echo *.cabal)
  test -f "$name" || die "One and only one .cabal file is required in the current directory."
  name=${name%.cabal}
  test -f "${name}.cabal" || \
    die "Cannot determine package name. File [${name}.cabal] does not exist"
}

get_pkg_full_name() {
  local pkgname
  local full_name
  pkgname=$(get_pkg_name) || exit 1
  full_name=$(cabal info . | awk '{print $2;exit}') || exit 1
  if test "${pkgname}${full_name#$pkgname}" != "${full_name}"
  then
    echo "Cabal file name does not match the package name."
    exit 1
  fi
  echo $full_name
}

# $1: package full name (name + ver)
create_and_unpack_pkg_dist() {
  echo "Creating source distribution tarball for [$1]"

  local pkgtar=${1}.tar.gz
  local tarpath
  tarpath=$($STACKCMD --compiler=ghc-$GHCVER path --dist-dir)/${pkgtar} || exit 1
  rm -f $tarpath
  $STACKCMD --compiler=ghc-$GHCVER sdist --pvp-bounds=both || exit 1
  if test ! -f $tarpath
  then
    echo "stack sdist did not create [$tarpath]"
    exit 1
  fi
  echo "Unpacking the source distribution tarball..."
  tar xzvf $tarpath || exit 1
}

install_deps() {
  echo "Installing dependencies..."
  case "$BUILD" in
    stack) $STACKCMD test --only-dependencies ;;
    cabal)
      cabal --version;
      retry_cmd cabal update
      cabal install --only-dependencies \
                    --enable-tests \
                    --enable-benchmarks \
                    --force-reinstalls \
                    --ghc-options=-O0 \
                    --reorder-goals \
                    --max-backjumps=-1 ;;
  esac
}

# $1 package dir (name + ver)
# $2 package name
build_and_test() {
  echo "Building and testing [$1]"
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

#set -eux
set -o pipefail
unset CC

# Show, process and verify the config
show_build_config "----Requested build config----"
# Anything other than "y/Y/yes/YES/Yes" is considered fals
# Set or unset the global var for easy tests later on
STACK_SDIST=$(use_stack_sdist)
verify_build_config

use_travis_paths
echo "PATH is [$PATH]"

# Install any tools needed
test -n "$(need_stack)" && ensure_stack
ensure_ghc
test "$BUILD" = "cabal" && ensure_cabal
show_build_config "----Using build config----"

PACKAGE_NAME=$(get_pkg_name) || exit 1
PACKAGE_FULL_NAME=$(get_pkg_full_name) || exit 1
test -n "$(need_stack)" && ensure_stack_yaml
SDIST_CMD=$(get_sdist_cmd)
echo "Using [$SDIST_CMD] for creating source distribution of [$PACKAGE_FULL_NAME]"
create_and_unpack_pkg_dist $PACKAGE_FULL_NAME
cd $PACKAGE_FULL_NAME
install_deps $PACKAGE_FULL_NAME
build_and_test $PACKAGE_FULL_NAME $PACKAGE_NAME
