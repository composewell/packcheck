#!/bin/bash

# Works on Linux and OSX
# Installs stack if required
# On OSX - Installs ghc using stack and puts it on PATH
# Uses stack to create a dist of the pkg in CURRENT DIR
# Unpacks, builds and tests the distribution using stack or cabal

# NOTE: When using stack build, if stack and cabal are not
# found in PATH, it will install both in ~/.local/bin automatically
# without any explicit permission. It will also install ghc via stack if
# you are not using a system ghc in PATH or if it is not suitable for
# building your package. When using a cabal build it will also do a cabal
# update.

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

# DESTRUCTIVE=y  for CI environments or if you can tolerate changes to your
# cabal config, bin and force installing the package being tested.
#
# For example:
# env BUILD=cabal GHCVER=7.8.4 CABALVER=1.24 STACK_SDIST=y
#     STACK_YAML=stack-7.8.yaml PVP_BOUNDS=both

# TODO
# COVERAGE=y Generate coverage
# COVERALLS=y Generate coverage report and send it to coveralls.io

# cabal build can use stack ghc if GHCVER is not specified

# ---------Skip to the end for main flow of script-----------

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
  else
    echo "Using [$1] at [$(which $1)]"
  fi
}

# $1: command
function run_verbose() {
  echo "$*"
  /bin/bash -c "$*"
}

function run_verbose_errexit() {
  run_verbose "$*" || die "Command [$*] failed. Exiting."
}

#------------------------------------------------------------------------------
# Build config show and determine
#------------------------------------------------------------------------------

# $1: varname
# $2: help text
help_envvar() {
  printf "%-15s: %s\n" "$1" "$2"
}

show_help() {
  echo "The following env variables can be passed. BUILD is mandatory."
  help_envvar BUILD "[stack | cabal]"
  help_envvar PVP_BOUNDS "Argument to stack --pvp-bounds to use for stack sdist"
  help_envvar RESOLVER "Resolver to use for stack commands"
  help_envvar STACK_YAML "Alternative stack config file to use"
  help_envvar STACK_SDIST "[y] For cabal builds, use stack sdist to create dist to test"

  help_envvar GHCVER "[a.b.c] GHC version requested"
  help_envvar CABALVER "[a.b.c.d] Cabal version requested"
  help_envvar DESTRUCTIVE "[y] Clobber cabal config, install bins, force install packages"

  echo
  echo "Example usage:"
  echo "env BUILD=stack RESOLVER=lts-6 PVP_BOUNDS=both $0"
  exit 1
}

required_envvar() {
  local var=$(eval "echo \$$1")
  test -n "$var" || show_help
}

# $1: envvar
check_boolean_var() {
  local var=$(eval "echo \$$1")
  if test -n "$var" -a "$var" != y
  then
    echo "Error: Boolean envvar [$1] can only be empty or 'y'"
    echo
    show_help
  fi
}

# $1: msg
show_build_config() {
  check_boolean_var STACK_SDIST
  check_boolean_var DESTRUCTIVE
  echo "$1"
  show_nonempty_var BUILD
  show_nonempty_var GHCVER
  show_nonempty_var PVP_BOUNDS
  show_nonempty_var RESOLVER
  show_nonempty_var STACK_YAML
  show_nonempty_var CABALVER
  show_nonempty_var STACK_SDIST
  show_nonempty_var DESTRUCTIVE
  show_nonempty_var PATH
  echo "----End build config----"
}

verify_build_config() {
  init_default BUILD stack

  if test "$BUILD" != cabal
  then
    test -z "$CABALVER" || die "Error: CABALVER is meaningful only for cabal build"
    test -z "$STACK_SDIST" || die "Error: STACK_SDIST is meaningful only for cabal build"
  fi

  if test "$BUILD" = cabal -a -z "$STACK_SDIST"
  then
    test -z "$RESOLVER" || die "Error: RESOLVER is not meaningful in this config"
    test -z "$STACK_YAML" || die "Error: STACK_YAML is not meaningful in this config"
  fi
}

#------------------------------------------------------------------------------
# Stack fetch and install etc.
#------------------------------------------------------------------------------

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
  # User specified PATH takes precedence
  export PATH=$PATH:$HOME/.local/bin

  if test -z "$(type -t stack)"
  then
    echo "Downloading stack..."
    fetch_stack
  fi
  require_cmd stack
  # stack upgrade
  STACKCMD="stack --no-terminal"
  $STACKCMD --version

  # We need cabal to retrieve the package version as well as for the solver
  # Do not consider the resolver to install this, install the latest
  if test -z "$(which cabal)"
  then
    echo "Installing cabal-install..."
    $STACKCMD install cabal-install
  fi

  if test -n "$RESOLVER"
  then
    STACKCMD="$STACKCMD --resolver $RESOLVER"
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

#------------------------------------------------------------------------------
# Ensure ghc, cabal are available and the right versions when requested
#------------------------------------------------------------------------------

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
    # If the user specified GHCVER then use it as system-ghc
    # Stack will still silently choose its own ghc if the ghc does not match
    # the snapshot.
    STACKCMD="$STACKCMD --system-ghc"
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
    # solver seems to be broken with latest cabal
    local SOLVER_CMD="$STACKCMD init --solver"
    echo "Trying [$SOLVER_CMD] to generate a stack.yaml"
    $SOLVER_CMD || die "Solver failed to generate a stack.yaml.\n\
Please provide a working stack.yaml or use cabal build."
    require_file stack.yaml
  fi
  test -n "$STACK_YAML" && STACKCMD="$STACKCMD --stack-yaml $STACK_YAML"
  echo "Using stack command [$STACKCMD]"
}

#------------------------------------------------------------------------------
# Create a dist, install deps and test
#------------------------------------------------------------------------------

get_pkg_name() {
  local name=$(echo *.cabal)
  test -f "$name" || die "One and only one .cabal file is required in the current directory."
  name=${name%.cabal}
  test -f "${name}.cabal" || \
    die "Cannot determine package name. File [${name}.cabal] does not exist"
  echo $name
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

remove_pkg_executables() {
  exes=$(cabal info . | awk '{if ($1 == "Executables:") { print $2; exit }}') || exit 1
  echo "Removing [$exes] from [$HOME/.cabal/bin]"
  for i in $exes
  do
    rm -f $HOME/.cabal/bin/$i
  done
}

cabal_configure() {
    cabal configure -v2 \
                     --enable-tests \
                     --enable-benchmarks \
                     --ghc-options="-O0 -Werror"
}

# $1: package full name (name + ver)
create_and_unpack_pkg_dist() {
  local pkgtar=${1}.tar.gz
  local pvp
  local SDIST_DIR
  local SDIST_CMD

  test -n "$PVP_BOUNDS" && pvp="--pvp-bounds $PVP_BOUNDS"

  if test "$BUILD" = stack
  then
    SDIST_CMD="$STACKCMD sdist $pvp"
    SDIST_DIR=$($STACKCMD path --dist-dir) || exit 1
  elif test -n "$STACK_SDIST"
  then
    SDIST_CMD="$STACKCMD --compiler=ghc-$GHCVER sdist $pvp"
    SDIST_DIR=$($STACKCMD --compiler=ghc-$GHCVER path --dist-dir) || exit 1
  else
    cabal_configure
    SDIST_CMD="cabal sdist"
    SDIST_DIR=dist
  fi

  local tarpath=${SDIST_DIR}/${pkgtar}
  rm -f $tarpath
  echo "Using [$SDIST_CMD] to create source distribution tarball for [$1]"
  run_verbose_errexit $SDIST_CMD
  if test ! -f $tarpath
  then
    echo "stack sdist did not create [$tarpath]"
    exit 1
  fi
  echo "Unpacking the source distribution tarball..."
  run_verbose_errexit tar xzvf $tarpath
}

install_deps() {
  echo "Installing dependencies..."
  case "$BUILD" in
    stack) run_verbose_errexit $STACKCMD test --only-dependencies ;;
    cabal)
      cabal --version;
      retry_cmd cabal update
      run_verbose_errexit cabal sandbox init
      run_verbose_errexit cabal install --only-dependencies \
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
      cabal_configure
      run_verbose_errexit cabal build
      run_verbose_errexit cabal test
      run_verbose_errexit cabal check
      run_verbose_errexit cabal sdist

      if test "$DESTRUCTIVE" = "y"
      then
        run_verbose_errexit cabal copy
        (cd dist && run_verbose_errexit cabal install --force-reinstalls "${1}.tar.gz")
        remove_pkg_executables
      fi ;;
  esac
}

#------------------------------------------------------------------------------
# Main flow of script starts here
#------------------------------------------------------------------------------

set -e
set -o pipefail
unset CC

test $# -eq 0 || show_help

# Require at least one param so that accidentally running the script does not
# create surprises.
required_envvar BUILD

# ---------Show, process and verify the config------------
show_build_config "----Requested build config----"
verify_build_config

# ---------Install any tools needed--------
require_cmd /bin/bash
test -n "$(need_stack)" && ensure_stack
ensure_ghc
test "$BUILD" = "cabal" && ensure_cabal
show_build_config "----Using build config----"

# ---------Create dist, unpack, install deps, test--------
PACKAGE_NAME=$(get_pkg_name) || exit 1
PACKAGE_FULL_NAME=$(get_pkg_full_name) || exit 1
test -n "$(need_stack)" && ensure_stack_yaml
create_and_unpack_pkg_dist $PACKAGE_FULL_NAME
cd $PACKAGE_FULL_NAME
install_deps $PACKAGE_FULL_NAME
build_and_test $PACKAGE_FULL_NAME $PACKAGE_NAME
