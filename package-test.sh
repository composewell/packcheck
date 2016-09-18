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

# MINGW 'which' does not seem to work when there are spaces in the
# PATH.  Note that, type returns a cached path, so if something got
# deleted we might still be returning a stale value (we can use hash -r
# to clear the cache if needed).

which_cmd() {
  hash -r && type -P "$1"
}

require_cmd () {
  if test -z "$(which_cmd $1)"
  then
    echo "Required command [$1] not found in PATH [$PATH]."
    exit 1
  else
    echo "Using [$1] at [$(which_cmd $1)]"
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
  echo
  echo "------------------------------------------"
  echo "$1"
  echo "------------------------------------------"
}

# $1: file to ungztar
win_ungztar() {
  local output=$(basename ${1%.gz})
  run_verbose_errexit 7z e -y $1 && run_verbose_errexit 7z x -y $output
  run_verbose_errexit rm -f $output
}

set_os_specific_vars() {
  local os=$(uname)
  case "$os" in
    Darwin|Linux)
      OS_HAS_TOOLS=tar
      OS_UNGZTAR_CMD="run_verbose_errexit tar xzvf"
      OS_LOCAL_DIR=.local
      OS_CABAL_DIR=.cabal
      OS_APP_HOME=$HOME ;;
    MINGW*)
      require_cmd cygpath
      require_envvar APPDATA
      OS_HAS_TOOLS=cygpath
      OS_UNGZTAR_CMD=win_ungztar
      OS_LOCAL_DIR=local
      OS_CABAL_DIR=cabal
      OS_APP_HOME=`cygpath $APPDATA` ;;
    *) die "Unknown OS [$os]" ;;
  esac
}

#------------------------------------------------------------------------------
# Build config show and determine
#------------------------------------------------------------------------------

ENVVARS="\
  BUILD \
  RESOLVER \
  GHCVER \
  CABALVER \
  GHC_OPTIONS \
  SDIST_OPTIONS \
  PATH \
  STACK_YAML \
  STACK_BUILD_OPTIONS \
  CABAL_REINIT_CONFIG \
  CABAL_USE_STACK_SDIST \
  CABAL_CONFIGURE_OPTIONS \
  CABAL_NO_SANDBOX \
  CABAL_TEST_INSTALL \
  CABAL_HACKAGE_MIRROR \
  COVERAGE \
  COVERALLS_OPTIONS \
  CHECK_ENV \
"

# $1: varname
# $2: list of vars to find in
find_var() {
  for v in $2
  do
   test $v = "$1" && return 0
  done
  return 1
}

error_novar() {
  find_var "$1" "$ENVVARS" || die "Unknown evnironment variable [$1]"
}

error_clean_env() {
    echo "Error: Unknown evnironment variable [$1]."
    die "Please check spelling mistakes and use a clean environment (e.g. env -i) with CHECK_ENV."
}

ALLOW_ENVVARS="STACK_ROOT APPDATA PWD SHLVL _"

check_clean_env() {
  local vars=$(env | cut -f1 -d=)
  for i in $vars
  do
    find_var $i "$ENVVARS $ALLOW_ENVVARS" || error_clean_env "$i"
  done
}

# $1: varname
require_envvar() {
  local var=$(eval "echo \$$1")
  test -n "$var" || die "Environment variable [$1] must be set."
}

# $1: envvar
check_boolean_var() {
  error_novar "$1"
  local var=$(eval "echo \$$1")
  if test -n "$var" -a "$var" != y
  then
    echo "Error: Boolean envvar [$1] can only be empty or 'y'"
    exit 1
  fi
}

# $1: varname
# $2: help text
help_envvar() {
  error_novar $1
  printf "%-24s: %s\n" "$1" "$2"
}

show_help() {
  show_step "Commonly used env variables"
  help_envvar BUILD "[stack | cabal] The only mandatory option"
  help_envvar RESOLVER "Stack resolver to use for stack or cabal builds"
  help_envvar GHCVER "[a.b.c] GHC version prefix (may not be enforced when using stack)"
  help_envvar CABALVER "[a.b.c.d] Cabal version prefix for cabal builds"
  help_envvar GHC_OPTIONS "Specify GHC options to use"
  help_envvar SDIST_OPTIONS "Argument to stack sdist (e.g. --pvp-bounds)"
  help_envvar PATH "[path] Set PATH explicitly for predictable builds"

  show_step "Advanced stack build env variables"
  help_envvar STACK_YAML "Alternative stack config file to use"
  help_envvar STACK_BUILD_OPTIONS "Override the default stack build command options"

  show_step "Advanced cabal build env variables"
  help_envvar CABAL_USE_STACK_SDIST "[y] Use stack sdist (to use --pvp-bounds)"
  help_envvar CABAL_CONFIGURE_OPTIONS "Override the default cabal configure options"
  # All of the following are recommended for a CI environment, should we use a
  # CONTINUOUS_INTEGRATION env variables to set them as default automatically?
  # The sandbox mode is a bit expensive because a sandbox is used and
  # dependencies have to be installed twice in two separate sandboxes, once to
  # create and sdist and once to build the sdist. For a CI NO sandbox mode
  # makes more sense as long as multiple builds running simultaneously will not
  # try to install conflicting packages.
  help_envvar CABAL_NO_SANDBOX "[y] Clobber (force install) your global cabal ghc package db"
  help_envvar CABAL_TEST_INSTALL "[y] Force install the package after building"
  help_envvar CABAL_HACKAGE_MIRROR "[y] Specify an alternative mirror, will modify the cabal user config file."
  # XXX this is not really a cabal build specific var
  help_envvar CABAL_REINIT_CONFIG "[y] Remove old cabal config to avoid any config incompatibility issues"

  show_step "Coverage related env variables"
  help_envvar COVERALLS_OPTIONS "[test suite names] Send coverage to coveralls.io"
  help_envvar COVERAGE "[y] Just generate coverage information"

  show_step "Diagnostics"
  # To catch spelling mistakes in envvar names passed, otherwise they will be
  # silently ignored and we will be wondering why the script is not working.
  help_envvar CHECK_ENV "Treat unknown env variables as error, used with env -i"

  show_step "Example usage"
  echo "env BUILD=stack RESOLVER=lts-6 SDIST_OPTIONS=\"--pvp-bounds both\" $0"
  exit 1
}

show_build_config() {
  check_boolean_var CABAL_USE_STACK_SDIST
  check_boolean_var CABAL_REINIT_CONFIG
  check_boolean_var CABAL_NO_SANDBOX
  check_boolean_var CABAL_TEST_INSTALL
  check_boolean_var COVERAGE

  for i in $ENVVARS
  do
    show_nonempty_var $i
  done
}

show_build_env() {
  show_nonempty_var HOME
  show_nonempty_var APPDATA
  show_nonempty_var STACK_ROOT
}

need_stack() {
  if test "$BUILD" = stack -o -n "$RESOLVER" -o -n "$CABAL_USE_STACK_SDIST"
  then
    echo true
  fi
}

# $1: varname
cabal_only_var() {
  error_novar $1
  local var=$(eval "echo \$$1")
  test -z "$var" || die "[$1] is meaningful only for cabal build"
}

# $1: varname
stack_only_var() {
  error_novar $1
  local var=$(eval "echo \$$1")
  test -z "$var" || die "[$1] is meaningful only when stack is used"
}

verify_build_config() {
  test -n "$COVERALLS_OPTIONS" && COVERAGE=y

  if test "$BUILD" = stack
  then
    STACK_DEP_OPTIONS="--test --bench --only-dependencies --ghc-options=-O0"

    init_default STACK_BUILD_OPTIONS \
          "--test \
          --bench --no-run-benchmarks \
          --haddock --no-haddock-deps"

    STACK_BUILD_OPTIONS=$(cat << EOF
      $STACK_BUILD_OPTIONS
      $(test -n "${COVERAGE}" && echo --coverage)
      $(test -n "${GHC_OPTIONS}" && echo --ghc-options=\"$GHC_OPTIONS\")
EOF
)
  else
    CABAL_DEP_OPTIONS="--only-dependencies \
        --enable-tests --enable-benchmarks --force-reinstalls \
        --reorder-goals --max-backjumps=-1 --ghc-options=-O0"

    init_default CABAL_CONFIGURE_OPTIONS \
                 "-v2 \
                 --enable-tests \
                 --enable-benchmarks"
    CABAL_CONFIGURE_OPTIONS=$(cat << EOF
      $CABAL_CONFIGURE_OPTIONS
      $(test -n "$COVERAGE" && echo --enable-coverage)
      $(test -n "$GHC_OPTIONS" && echo --ghc-options=\"$GHC_OPTIONS\")
EOF
)
  fi

  # These variables are now combined with other options so clear them
  # so that we do not show them in the effective config
  COVERAGE=
  GHC_OPTIONS=

  test "$BUILD" = stack -o "$BUILD" = cabal || \
    die "build can only be 'stack' or 'cabal'"

  if test "$BUILD" != cabal
  then
    cabal_only_var CABALVER
    cabal_only_var CABAL_USE_STACK_SDIST
    cabal_only_var CABAL_CONFIGURE_OPTIONS
    cabal_only_var CABAL_NO_SANDBOX
    cabal_only_var CABAL_TEST_INSTALL
    cabal_only_var CABAL_HACKAGE_MIRROR
  else
    stack_only_var STACK_BUILD_OPTIONS
    if test -n "$GHCVER" -a -n "$RESOLVER"
    then
      die "GHCVER and RESOLVER cannot be used together in cabal build."
    fi
  fi

  if test -z "$(need_stack)"
  then
    stack_only_var STACK_YAML
  fi
}

#------------------------------------------------------------------------------
# Stack fetch and install etc.
#------------------------------------------------------------------------------

ensure_msys_tools() {
  if [[ `uname` = MINGW* ]]
  then
    # retry??
    for i in "$1"
    do
      if test -z "$(which_cmd $i)"
      then
        stack exec pacman -- -S --noconfirm $i
      fi
    done
  fi
}

fetch_stack_osx() {
  curl -sSkL https://www.stackage.org/stack/osx-x86_64 \
    | tar xz --strip-components=1 -C $1 --include '*/stack'
}

fetch_stack_linux() {
  curl -sSkL https://www.stackage.org/stack/linux-x86_64 \
    | tar xz --strip-components=1 -C $1 --wildcards '*/stack'
}

fetch_stack_windows() {
  curl -sSkL http://www.stackage.org/stack/windows-i386 \
    | 7z x -si stack.exe
}

# $1: directory to place stack executable in
fetch_stack() {
  mkdir -p $1
  local os=$(uname)
  case "$os" in
    Darwin) retry_cmd fetch_stack_osx $1 ;;
    Linux)  retry_cmd fetch_stack_linux $1 ;;
    MINGW*) retry_cmd fetch_stack_windows $1 ;;
    *) die "Unknown OS [$os]" ;;
  esac
}

# $1: directory to place stack executable in
ensure_stack() {
  # User specified PATH takes precedence
  export PATH=$PATH:$1

  if test -z "$(which_cmd stack)"
  then
    echo "Downloading stack to [$1]..."
    fetch_stack $1
  fi
  require_cmd stack
  # stack upgrade
  STACKCMD="stack --no-terminal"
  $STACKCMD --version

  if test -n "$RESOLVER"
  then
    STACKCMD="$STACKCMD --resolver $RESOLVER"
  fi

}

use_stack_paths() {
  # Need the bin path (not just compiler-path) on mingw to find gcc
  # some packages may have a configure script looking for gcc, so we need to
  # use bin path so that on windows we will find the stack installed mingw gcc
  local BINPATH=`$STACKCMD path --bin-path`
  if [[ `uname` = MINGW* ]]
  then
    # Need for 7z on windows
    local GHCPATHS=`$STACKCMD path --programs`
    # Convert the path to MINGW format from windows native format
    BINPATH=$(cygpath -u -p $BINPATH)
    GHCPATHS=$(cygpath -u -p $GHCPATHS)
    if test -n "$GHCPATHS"
    then
      export PATH=$GHCPATHS:$PATH
    fi
  fi
  if test -n "$BINPATH"
  then
    export PATH=$BINPATH:$PATH
  fi
}

#------------------------------------------------------------------------------
# Ensure ghc, cabal are available and the right versions when requested
#------------------------------------------------------------------------------

# $1: tool name (used only for ghc and cabal)
# $2: expected version
check_version() {
  local real_ver=$($1 --numeric-version)

  # Match that the expected version is a prefix of real
  # Do not check when the expected version is head
  test "${real_ver#$2}" != ${real_ver} -o $2 = head || \
    die "Wrong $1 version [$real_ver] expected [$2]"
}

ensure_ghc() {
  if test -n "$(need_stack)" -a -z "$GHCVER"
  then
    # Use stack supplied ghc
    retry_cmd $STACKCMD setup
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
  fi
  # Use the real version, the user might have specified a prefix
  GHCVER=$(ghc --numeric-version) || exit 1
}

# XXX/TODO this may not work for cabal 1.24 config
# $1: mirror URL
cabal_use_mirror() {
  local CABAL_CONFIG=${OS_APP_HOME}/${OS_CABAL_DIR}/config
  if test -f $CABAL_CONFIG
  then
    local inplace
    if [ `uname` = "Darwin" ]
    then
      inplace="-i orig"
    else
      inplace="--in-place"
    fi
    echo "Adding hackage mirror [$1] to [$CABAL_CONFIG]"
    sed $inplace -e "s%^remote-repo:.*%remote-repo: $1%" $CABAL_CONFIG
  else
    die "cabal config file [$CABAL_CONFIG] not found."
  fi
}

# $1: Directory to install cabal in
ensure_cabal() {
  # If we have to install tools like hpc-coveralls
  # User specified PATH takes precedence
  export PATH=$PATH:$1

  # We can only do this after ghc is installed.
  # We need cabal to retrieve the package version as well as for the solver
  # Also when we are using stack for cabal builds use stack installed cabal
  # We are assuming CI cache will be per resolver so we can cache the bin
  if test -z "$(which_cmd cabal)" -a -n "$(need_stack)"
  then
      run_verbose_errexit $STACKCMD install cabal-install
  fi

  require_cmd cabal
  run_verbose cabal --version
  test -n "$CABALVER" && check_version cabal $CABALVER
  # Set the real version of cabal
  CABALVER=$(cabal --numeric-version) || exit 1

  if test "$CABAL_REINIT_CONFIG" = y
  then
    local cfg="${OS_APP_HOME}/${OS_CABAL_DIR}/config"
    echo "Removing old cabal config [$cfg]"
    run_verbose_errexit rm -f "$cfg"
  fi
}

ensure_stack_yaml() {
  if test -n "$STACK_YAML"
  then
    require_file $STACK_YAML
  elif test ! -e stack.yaml
  then
    # solver seems to be broken with latest cabal
    echo "Trying to generate a stack.yaml"
    run_verbose $STACKCMD init --solver --ignore-subdirs || die "Solver failed to generate a stack.yaml.\n\
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
  full_name=$(cabal info . | awk '{ if ($1 == "*") {print $2; exit}}') || die "cabal info failed"
  if test "${pkgname}${full_name#$pkgname}" != "${full_name}"
  then
    die "Inconsistent package name [$pkgname] and package full name [$full_name]"
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

install_cabal_deps() {
  if test "$CABAL_NO_SANDBOX" != "y"
  then
    run_verbose_errexit cabal sandbox init
  fi
  run_verbose_errexit cabal install $CABAL_DEP_OPTIONS
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

  if test "$BUILD" = cabal
  then
    echo "cabal update"
    retry_cmd cabal update
  fi

  test -n "$SDIST_OPTIONS" && opts="$SDIST_OPTIONS"

  if test "$BUILD" = stack
  then
    SDIST_CMD="$STACKCMD sdist $opts"
    SDIST_DIR=$($STACKCMD path --dist-dir) || exit 1
  elif test -n "$CABAL_USE_STACK_SDIST"
  then
    SDIST_CMD="$STACKCMD --compiler=ghc-$GHCVER sdist $opts"
    SDIST_DIR=$($STACKCMD --compiler=ghc-$GHCVER path --dist-dir) || exit 1
  else
    # We need to configure to use sdist and we need to install
    # dependencies to configure. So to just create the sdist we will
    # have to go through the whole process once and then again after
    # unpacking the sdist and to build it. If we use a sandbox then
    # we actually have to install the dependencies twice.
    install_cabal_deps
    cabal_configure
    SDIST_CMD="cabal sdist $opts"
    SDIST_DIR=dist
  fi

  # stack commands return path in windows format
  [[ `uname` = MINGW* ]] && SDIST_DIR=`cygpath ${SDIST_DIR}`

  local tarpath=${SDIST_DIR}/${pkgtar}
  rm -f $tarpath
  run_verbose_errexit $SDIST_CMD
  if test ! -f $tarpath
  then
    echo "stack sdist did not create [$tarpath]"
    exit 1
  fi

  # Unpack the tar inside .sanity-test directory
  mkdir -p .sanity-test || exit 1
  echo "cd .sanity-test"
  cd .sanity-test || exit 1
  test "${tarpath:0:1}" == / || tarpath=../$tarpath
  $OS_UNGZTAR_CMD $tarpath
}

install_deps() {
  case "$BUILD" in
    stack) run_verbose_errexit $STACKCMD build $STACK_DEP_OPTIONS ;;
    cabal) install_cabal_deps ;;
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

      if test "$CABAL_TEST_INSTALL" = "y"
      then
        run_verbose_errexit cabal copy
        (cd dist && run_verbose_errexit cabal install --force-reinstalls "${1}.tar.gz")
        remove_pkg_executables
      fi ;;
  esac
}

coveralls_io() {
  if test -z "$(which_cmd hpc-coveralls)"
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

test -n "$CHECK_ENV" && check_boolean_var CHECK_ENV
test -n "$CHECK_ENV" && check_clean_env

test $# -eq 0 || show_help

# Require at least one param so that accidentally running the script does not
# create surprises.
require_envvar BUILD

echo
bash --version

# ---------Show, process and verify the config------------
show_step "Requested build config and environment"
show_build_config
echo
show_build_env

# Determine home independent of the environment
export HOME=$(echo ~)
set_os_specific_vars # depends on HOME

TOOLS="awk cat curl cut env mkdir printf rm sleep which $OS_HAS_TOOLS"

show_step "Check basic tools"
require_cmd /bin/bash
for i in $TOOLS; do require_cmd $i; done

verify_build_config

# ---------Install any tools needed--------
show_step "Install tools needed for build"

# if we are running from a stack environment remove GHC_PACKAGE_PATH so that
# cabal does not complain
unset GHC_PACKAGE_PATH

test -n "$(need_stack)" && ensure_stack ${OS_APP_HOME}/${OS_LOCAL_DIR}/bin
# The tar installed by pacman does not seem to work. Maybe we need to have it
# packed with msys itself.
# ensure_msys_tools "tar" && require_cmd tar

ensure_ghc
ensure_cabal ${OS_APP_HOME}/${OS_LOCAL_DIR}/bin

# use the stack installed 7z instead. depends on ensure ghc where we setup
# stack paths.
[[ `uname` = MINGW* ]] && require_cmd 7z

show_step "Effective build config"
show_build_config

# ---------Create dist, unpack, install deps, test--------
show_step "Create source distribution and unpack it"
PACKAGE_NAME=$(get_pkg_name) || die "PACKAGE_NAME"
echo "Package name: [$PACKAGE_NAME]"

PACKAGE_FULL_NAME=$(get_pkg_full_name) || die "PACKAGE_FULL_NAME"
echo "Package name and version: [$PACKAGE_FULL_NAME]"

# The cabal info command run as part of package name determination above would
# have already created the cabal config, just change the mirror if needed.
test -n "$CABAL_HACKAGE_MIRROR" && cabal_use_mirror $CABAL_HACKAGE_MIRROR

test -n "$(need_stack)" && ensure_stack_yaml
create_and_unpack_pkg_dist $PACKAGE_FULL_NAME

# Note the above functions leaves us in the .sanity-test dir
cd $PACKAGE_FULL_NAME
show_step "Package info [sdist $SDIST_OPTIONS]"
run_verbose cabal info .

show_step "Install dependencies"
install_deps $PACKAGE_FULL_NAME

show_step "Build and test"
build_and_test $PACKAGE_FULL_NAME $PACKAGE_NAME

if test -n "$COVERALLS_OPTIONS"
then
    show_step "Send coverage info to coveralls.io"
    coveralls_io
fi
