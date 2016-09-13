#!/bin/bash

#------------------------------------------------------------------------------
# Skip to the end for main flow of script
#------------------------------------------------------------------------------

# See the Readme.md file for details on how it works and the user guide.

#------------------------------------------------------------------------------
# TODO
#------------------------------------------------------------------------------

# FORCE_TOOL_INSTALL: an option to not use cached installs, force fresh
# installs of tools What if we are using a tool from system path -
# invert path?
# NO_TOOL_INSTALL: do not download/install any tools
# cabal build can use stack installed ghc if GHCVER is not specified

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
  test -n "$var" || eval "export $1=\"$2\""
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
  >&2 echo -e "Error: $1"
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

# $1: msg
show_step() {
  echo "------------------------------------------"
  echo "$1"
  echo "------------------------------------------"
}

#------------------------------------------------------------------------------
# Build config show and determine
#------------------------------------------------------------------------------

# $1: varname
# $2: help text
help_envvar() {
  printf "%-23s: %s\n" "$1" "$2"
}

show_help() {
  echo "The following environment variables can be passed."
  echo
  help_envvar BUILD "[stack | cabal] The only mandatory option"
  help_envvar SDIST_OPTIONS "Argument to stack sdist (e.g. pvp-bounds)"
  help_envvar GHC_OPTIONS "Specify GHC options to use"
  help_envvar GHCVER "[a.b.c] GHC version (may not be enforced when using stack)"
  echo
  help_envvar RESOLVER "Resolver to use for stack commands"
  help_envvar STACK_YAML "Alternative stack config file to use"
  help_envvar STACK_BUILD_OPTIONS "Override the default stack build command options"
  echo
  help_envvar CABALVER "[a.b.c.d] Cabal version requested"
  help_envvar USE_STACK_SDIST "[y] For cabal builds, use stack sdist to create dist to test"
  help_envvar DESTRUCTIVE "[y] Clobber cabal config, install bins, force install packages"
  help_envvar CABAL_CONFIGURE_OPTIONS "Override the default cabal configure options"
  echo
  help_envvar COVERALLS_OPTIONS "[test suite names] Send coverage to coveralls.io"
  help_envvar COVERAGE "[y] Just generate coverage information"
  echo
  echo "Example usage:"
  echo "env BUILD=stack RESOLVER=lts-6 SDIST_OPTIONS=\"--pvp-bounds both\" $0"
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

show_build_config() {
  check_boolean_var USE_STACK_SDIST
  check_boolean_var DESTRUCTIVE
  check_boolean_var COVERAGE

  show_nonempty_var BUILD
  show_nonempty_var GHC_OPTIONS
  show_nonempty_var SDIST_OPTIONS

  show_nonempty_var RESOLVER  # stack options
  show_nonempty_var STACK_YAML # stack options
  show_nonempty_var STACK_BUILD_OPTIONS

  show_nonempty_var GHCVER
  show_nonempty_var CABALVER
  show_nonempty_var USE_STACK_SDIST
  show_nonempty_var DESTRUCTIVE
  show_nonempty_var CABAL_CONFIGURE_OPTIONS

  show_nonempty_var COVERAGE
  show_nonempty_var COVERALLS
  show_nonempty_var PATH
}

need_stack() {
  if test "$BUILD" = stack -o -n "$USE_STACK_SDIST"
  then
    echo true
  fi
}

# $1: varname
cabal_only_var() {
  local var=$(eval "echo \$$1")
  test -z "$var" || die "[$1] is meaningful only for cabal build"
}

# $1: varname
stack_only_var() {
  local var=$(eval "echo \$$1")
  test -z "$var" || die "[$1] is meaningful only when stack is used"
}

verify_build_config() {
  test -n "$COVERALLS_OPTIONS" && COVERAGE=y

  if test "$BUILD" = stack
  then
    init_default STACK_BUILD_OPTIONS \
          "--test \
          --haddock --no-haddock-deps"
    STACK_BUILD_OPTIONS=$(cat << EOF
      $STACK_BUILD_OPTIONS
      $(test -n "${COVERAGE}" && echo --coverage)
      $(test -n "${GHC_OPTIONS}" && echo --ghc-options=\"$GHC_OPTIONS\")
EOF)
  else
    init_default CABAL_CONFIGURE_OPTIONS \
                 "-v2 \
                 --enable-tests \
                 --enable-benchmarks"
    CABAL_CONFIGURE_OPTIONS=$(cat << EOF
      $CABAL_CONFIGURE_OPTIONS
      $(test -n "$COVERAGE" && echo --enable-coverage)
      $(test -n "$GHC_OPTIONS" && echo --ghc-options=\"$GHC_OPTIONS\")
EOF)
  fi

  # These variables are now combined with other options so clear them
  COVERAGE=
  GHC_OPTIONS=

  test "$BUILD" = stack -o "$BUILD" = cabal || \
    die "build can only be 'stack' or 'cabal'"

  if test "$BUILD" != cabal
  then
    cabal_only_var CABALVER
    cabal_only_var USE_STACK_SDIST
    cabal_only_var CABAL_CONFIGURE_OPTIONS
    cabal_only_var DESTRUCTIVE
  fi

  if test -z "$(need_stack)"
  then
    stack_only_var RESOLVER
    stack_only_var STACK_YAML
  fi

  if test "$BUILD" != stack
  then
    stack_only_var STACK_BUILD_OPTIONS
  fi
}

#------------------------------------------------------------------------------
# Stack fetch and install etc.
#------------------------------------------------------------------------------

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
    run_verbose_errexit $STACKCMD install cabal-install
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
  local CABAL_CONFIG=$HOME/.cabal/config
  echo "Overwriting [$CABAL_CONFIG] to add hackage.fpcomplete.com"
  cat > $CABAL_CONFIG <<EOF
remote-repo: hackage.haskell.org:http://hackage.fpcomplete.com/
remote-repo-cache: $HOME/.cabal/packages
jobs: \$ncpus
EOF
}

ensure_cabal() {
  # If we have to install tools like hpc-coveralls
  # User specified PATH takes precedence
  export PATH=$PATH:$HOME/.local/bin

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
    echo "Trying to generate a stack.yaml"
    run_verbose $STACKCMD init --solver || die "Solver failed to generate a stack.yaml.\n\
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
  echo "Remove installed binaries"
  for i in $exes
  do
    run_verbose_errexit rm -f $HOME/.cabal/bin/$i
  done
}

cabal_configure() {
    run_verbose_errexit cabal configure $CABAL_CONFIGURE_OPTIONS
}

# $1: package full name (name + ver)
create_and_unpack_pkg_dist() {
  local pkgtar=${1}.tar.gz
  local opts
  local SDIST_DIR
  local SDIST_CMD

  test -n "$SDIST_OPTIONS" && opts="$SDIST_OPTIONS"

  if test "$BUILD" = stack
  then
    SDIST_CMD="$STACKCMD sdist $opts"
    SDIST_DIR=$($STACKCMD path --dist-dir) || exit 1
  elif test -n "$USE_STACK_SDIST"
  then
    SDIST_CMD="$STACKCMD --compiler=ghc-$GHCVER sdist $opts"
    SDIST_DIR=$($STACKCMD --compiler=ghc-$GHCVER path --dist-dir) || exit 1
  else
    cabal_configure
    SDIST_CMD="cabal sdist $opts"
    SDIST_DIR=dist
  fi

  local tarpath=${SDIST_DIR}/${pkgtar}
  rm -f $tarpath
  run_verbose_errexit $SDIST_CMD
  if test ! -f $tarpath
  then
    echo "stack sdist did not create [$tarpath]"
    exit 1
  fi
  run_verbose_errexit tar xzvf $tarpath
}

install_deps() {
  case "$BUILD" in
    stack) run_verbose_errexit $STACKCMD test --only-dependencies ;;
    cabal)
      cabal --version;
      retry_cmd cabal update
      run_verbose_errexit cabal sandbox init
      run_verbose_errexit cabal install --only-dependencies \
            --force-reinstalls \
            --reorder-goals \
            --max-backjumps=-1 ;;
  esac
}

# $1 package dir (name + ver)
# $2 package name
build_and_test() {
  case "$BUILD" in
    stack) run_verbose_errexit $STACKCMD build $STACK_BUILD_OPTIONS ;;
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

coveralls_io() {
  if test -z "$(which hpc-coveralls)"
  then
    if test "$BUILD" = stack
    then
      run_verbose_errexit stack install hpc-coveralls
    else
      run_verbose_errexit cabal install hpc-coveralls
    fi
  fi
  run_verbose_errexit hpc-coveralls $COVERALLS_OPTIONS
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
show_step "Requested build config"
show_build_config
verify_build_config

# ---------Install any tools needed--------
show_step "Install tools needed for build"
require_cmd /bin/bash
test -n "$(need_stack)" && ensure_stack
ensure_ghc
test "$BUILD" = "cabal" && ensure_cabal

show_step "Effective build config"
show_build_config

# ---------Create dist, unpack, install deps, test--------
show_step "Create source distribution and unpack it"
PACKAGE_NAME=$(get_pkg_name) || exit 1
PACKAGE_FULL_NAME=$(get_pkg_full_name) || exit 1
test -n "$(need_stack)" && ensure_stack_yaml
create_and_unpack_pkg_dist $PACKAGE_FULL_NAME

show_step "Install dependencies"
cd $PACKAGE_FULL_NAME
install_deps $PACKAGE_FULL_NAME

show_step "Build and test"
build_and_test $PACKAGE_FULL_NAME $PACKAGE_NAME

if test -n "$COVERALLS_OPTIONS"
then
    show_step "Send coverage info to coveralls.io"
    coveralls_io
fi
