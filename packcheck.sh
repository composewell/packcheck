#!/usr/bin/env bash

#------------------------------------------------------------------------------
# Skip to the end for main flow of script
#------------------------------------------------------------------------------

# See the Readme.md file for details on how it works and for the user guide.

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

PACKCHECK_VERSION=0.7.1

show_version() {
  echo "packcheck version $PACKCHECK_VERSION"
}

# $1: varname
show_var() {
  echo "$1=$(eval \"echo \$$1\")"
}

# $1: varname
show_nonempty_var() {
  local var=$(eval "echo \$$1")
  if test -n "$var"
  then
    printf "%q\n" "$1=$var"
  fi
}

# $1: var name
# $2: default value
init_default() {
  local var=$(eval "echo \$$1")
  test -n "$var" || eval "export $1=\"$2\""
}

# Rewrite deprecated environment variables
rewrite_deprecated() {
  local oldvar=$1
  local oldval=$(eval "echo \$$1")
  local newvar=$2

  if test -n "$oldval"
  then
      echo "DEPRECATED [$oldvar] please use [$newvar] instead"
      eval "export $newvar=$oldval"
      unset $oldvar
  fi
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
# PATH.  Note that "type" returns a cached path, so if something got
# deleted we might still be returning a stale value (we can use hash -r
# to clear the cache if needed).

which_cmd() {
  hash -r && type -P "$1" || true
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
  # XXX redirecting to stderr leads to misaligned output because stdout and
  # stdin are printed at different times.
  echo "$*" 1>&2
  bash -c "$*"
}

function run_verbose_errexit_with() {
    local errScript="$1";
    shift
    if ! run_verbose "$*";
    then
        eval "$errScript"
        die "Command [$*] failed. Exiting."
    fi
}

function run_verbose_errexit() {
  run_verbose "$*" || die "Command [$*] failed. Exiting."
}

# $1: msg
show_step1() {
  echo
  echo "--------------------------------------------------"
  echo "$1"
  echo "--------------------------------------------------"
}

# $1: msg
show_step() {
  local reltime
  local disptime
  reltime=$(get_rel_time)
  if test -n "$reltime"
  then
    disptime="[$reltime sec]"
  fi
  show_step1 "$disptime $1"
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
    Darwin|Linux|FreeBSD)
      OS_HAS_TOOLS=tar
      OS_UNGZTAR_CMD="run_verbose_errexit tar xmzvf"
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

show_machine_info() {
  local os=$(uname)
  case "$os" in
    Linux)
      #echo "OS: $os"
      run_verbose lsb_release -a || true
      echo
      run_verbose uname -a || true

      show_step "CPU"
      echo "lscpu"
      lscpu | grep "^Archi\|^CPU\|^Bogo\|^Hyper\|^Virtualiz"

      show_step "Memory"
      run_verbose free -h || true

      show_step "Container/cgroup information"
      # See https://stackoverflow.com/questions/20010199/determining-if-a-process-runs-inside-lxc-docker
      # For comprehensive detection see container-detect.conf in ubuntu
      #if test -f /.dockerenv
      #then
      #  echo "Running inside Docker (found /.dockerenv)";
      #fi
      #run_verbose head -n 1 /proc/1/cgroup
      sudo -n cat /proc/1/environ | tr '\0' '\n' | grep "^container=" || true
      run_verbose cat /sys/fs/cgroup/cpu/cpu.cfs_period_us || true
      run_verbose cat /sys/fs/cgroup/cpu/cpu.cfs_quota_us || true
      run_verbose cat /sys/fs/cgroup/memory/memory.limit_in_bytes || true

      show_step "Filesystems"
      run_verbose mount || true

      show_step "Disk Usage"
      run_verbose df -T || true ;;

    FreeBSD)
      echo "OS: FreeBSD"

      show_step "Filesystems"
      run_verbose mount || true

      show_step "Disk Usage"
      run_verbose df || true ;;

    Darwin)
      echo "OS: MacOS"

      show_step "Filesystems"
      run_verbose mount || true

      show_step "Disk Usage"
      run_verbose df -Y || true ;;

    MINGW*)
      echo "OS: Windows (MINGW)" ;;
    *) die "OS: Unknown OS [$os]" ;;
  esac
}

#------------------------------------------------------------------------------
# Build config show and determine
#------------------------------------------------------------------------------

SAFE_ENVVARS="\
  RESOLVER \
  ENABLE_GHCJS \
  GHCUP_VERSION \
  GHCVER \
  CABALVER \
  GHC_OPTIONS \
  GHCUP_GHC_OPTIONS \
  SDIST_OPTIONS \
  DISABLE_SDIST_BUILD \
  DISABLE_SDIST_PROJECT_CHECK \
  DISABLE_SDIST_GIT_CHECK \
  DISABLE_BENCH \
  DISABLE_TEST \
  DISABLE_DOCS \
  ENABLE_DOCSPEC \
  DISABLE_DIST_CHECKS \
  PATH \
  TOOLS_DIR \
  STACKVER \
  STACK_YAML \
  STACK_OPTIONS \
  STACK_BUILD_OPTIONS \
  CABAL_PROJECT \
  CABAL_CHECK_RELAX \
  CABAL_USE_STACK_SDIST \
  CABAL_BUILD_OPTIONS \
  CABAL_TEST_OPTIONS \
  CABAL_DISABLE_DEPS \
  CABAL_BUILD_TARGETS \
  COVERAGE \
  COVERALLS_OPTIONS \
  HLINT_VERSION \
  HLINT_BUILD \
  HLINT_COMMANDS \
  HLINT_OPTIONS \
  HLINT_TARGETS \
  DOCSPEC_URL \
  DOCSPEC_OPTIONS \
  CHECK_ENV \
  LANG \
  LC_ALL \
  BASE_TIME \
"

UNSAFE_ENVVARS="\
  ENABLE_INSTALL \
  STACK_UPGRADE \
  CABAL_REINIT_CONFIG \
  CABAL_HACKAGE_MIRROR \
"

ENVVARS="$SAFE_ENVVARS $UNSAFE_ENVVARS"

# $1: varname
# $2: list of vars to find in
find_var() {
  for v in $2
  do
   test $v != "$1" || return 0
  done
  return 1
}

error_novar() {
  find_var "$1" "$ENVVARS" || die "Unknown parameter or environment variable [$1]\nTry --help for supported parameters"
}

error_clean_env() {
    echo "Error: environment variable [$1] is set."
    echo "Error: No environment variables except [$ALLOW_ENVVARS] are allowed to be set when using CHECK_ENV=y"
    die "Please use a clean environment (e.g. env -i) with CHECK_ENV."
}

error_clean_param() {
    die "Unknown parameter [$1] specified on command line.\nTry --help for supported parameters"
}

ALLOW_ENVVARS="CHECK_ENV STACK_ROOT APPDATA PWD SHLVL _"

check_clean_env() {
  local vars=$(env | cut -f1 -d=)
  for i in $vars
  do
    find_var $i "$ALLOW_ENVVARS" || error_clean_env "$i"
  done
}

# $1: varname
require_envvar() {
  local var=$(eval "echo \$$1")
  test -n "$var" || die "Parameter or environment variable [$1] must be set. Try --help for usage."
}

# $1: envvar name
check_boolean_var() {
  error_novar "$1"
  local var=$(eval "echo \$$1")

  if test -n "$var"
  then
    case "$var" in
    y|Y|yes|Yes|YES|true|True|TRUE|on|On|ON) export $1=y ;;
    n|N|no|No|NO|false|False|FALSE|off|Off|OFF) export $1= ;;
    *) die "Boolean parameter or environment variable [$1=$var] can only be set to either an empty value or one of the following boolean values: y|Y|yes|Yes|YES|true|True|TRUE|on|On|ON|n|N|no|No|NO|false|False|FALSE|off|Off|OFF"
    esac
  fi
}

# $1: cmdname
# $2: help text
help_cmd() {
  printf "%-24s: %s\n" "$1" "$2"
}

# $1: varname
# $2: help text
help_envvar() {
  error_novar $1
  printf "%-24s: %s\n" "$1" "$2"
}

short_help() {
  local script=$(basename $0)
  echo "$script COMMAND [PARAMETER=VALUE ...]"
  echo
  echo "For example:"
  echo "$script cabal GHCVER=9.8.1"
  echo "$script stack RESOLVER=lts GHC_OPTIONS=\"-O0 -Werror\""
  echo "$script hlint"
  echo
  echo "Ask questions: https://app.gitter.im/#/room/#composewell_streamly:gitter.im"
  echo "Report issues: https://github.com/composewell/packcheck/issues/new"
}

# Please add the boolean options to check_boolean_var as well
show_help() {
  show_step1 "Usage"
  short_help

  echo
  echo "Control parameters can either be passed on command line or exported"
  echo "as environment variables. Parameters marked DESTRUCTIVE may modify"
  echo "your global user config or state."
  echo
  echo "Boolean parameters can be specified as "
  echo "y|Y|yes|Yes|YES|true|True|TRUE|on|On|ON for an affirmative value and as"
  echo "n|N|no|No|NO|false|False|FALSE|off|Off|OFF or empty for a negative value."

  show_step1 "Commands and flags"
  #help_cmd cabal-v2 "build using cabal v2-build"
  help_cmd cabal "build using cabal"
  #help_cmd cabal-new "Deprecated alias to cabal-v2"
  #help_cmd cabal-v1 "Deprecated: build using cabal v1-build"
  help_cmd stack "build using stack"
  help_cmd hlint "run hlint"
  # TODO add hlint as a tool
  help_cmd clean "remove the .packcheck directory"
  help_cmd cleanall "remove .packcheck, .stack-work directories"
  help_cmd "help | --help | -h" "show this help message"
  help_cmd "--version" "show packcheck version"

  show_step1 "Selecting tool versions"
  # untested/unsupported
  #help_envvar ENABLE_GHCJS "[y] Use GHCJS instead of GHC to build"
  help_envvar GHCUP_VERSION "[a.b.c.d] ghcup version to install at $GHCUP_PATH (see $GHCUP_URL_PREFIX)"
  help_envvar GHCVER "[a.b.c | head] GHC version prefix (may not be enforced when using stack)"
  help_envvar CABALVER "[a.b.c.d] Cabal version (prefix) to use"
  help_envvar STACKVER "[a.b.c.d] Stack version (prefix) to use"
  help_envvar STACK_UPGRADE "[y] DESTRUCTIVE! Upgrades stack to latest version"
  help_envvar RESOLVER "Stack resolver to use for stack builds or cabal builds using stack"
  help_envvar HLINT_VERSION "hlint version to install at $HLINT_PATH (see $HLINT_URL_PREFIX)"
  help_envvar DOCSPEC_URL "cabal-docspec release URL to install at $DOCSPEC_PATH (see $DOCSPEC_URL_PREFIX)"

  show_step1 "Where to find the required tools"
  help_envvar PATH "[path] Set PATH explicitly for predictable builds"
  # Untested/unsupported
  #help_envvar TOOLS_DIR "[dir] Find ghc|cabal by version as in TOOLS_DIR/ghc/<version>/bin"

  show_step1 "Specifying common tool options"
  # TODO
  # help_envvar TOOL_OPTIONS "Specify the tool specific (stack or cabal) options to use."
  # help_envvar BUILD_OPTIONS "Specify the tool specific build (stack build or cabal new-build) options to use."
  help_envvar GHCUP_GHC_OPTIONS "Used as in \"ghcup install ghc <GHCUP_GHC_OPTIONS> <version>\""
  help_envvar GHC_OPTIONS "Specify GHC options to use"
  help_envvar SDIST_OPTIONS "Arguments to stack/cabal sdist command"

  show_step1 "Specifying what to build"
  help_envvar DISABLE_BENCH "[y] Do not build benchmarks, default is to build but not run"
  help_envvar DISABLE_TEST "[y] Do not run tests, default is to run tests"
  help_envvar DISABLE_DOCS "[y] Do not build haddocks, default is to build docs"
  help_envvar ENABLE_DOCSPEC "[y] Run cabal-docspec after the cabal build"
  help_envvar DISABLE_SDIST_BUILD "[y] Do not build from source distribution"
  help_envvar DISABLE_SDIST_PROJECT_CHECK "[y] Ignore project file and continue"
  help_envvar DISABLE_SDIST_GIT_CHECK "[y] Do not compare source distribution with git repo"
  help_envvar DISABLE_DIST_CHECKS "[y] Do not perform source distribution checks"
  #help_envvar ENABLE_INSTALL "[y] DESTRUCTIVE! Install the package after building"

  show_step1 "cabal options"
  # XXX this applies to both stack and cabal builds
  help_envvar CABAL_REINIT_CONFIG "[y] DESTRUCTIVE! Remove old config to avoid incompatibility issues"
  help_envvar CABAL_PROJECT "Alternative cabal project file, path relative to project root"
  #help_envvar CABAL_USE_STACK_SDIST "[y] Use stack sdist (to use --pvp-bounds)"
  help_envvar CABAL_BUILD_OPTIONS "ADDITIONAL cabal build options to append to defaults"
  help_envvar CABAL_TEST_OPTIONS "ADDITIONAL cabal test options to append to defaults"
  help_envvar CABAL_DISABLE_DEPS "[y] Do not install dependencies, do not do cabal update"
  help_envvar CABAL_BUILD_TARGETS "cabal build targets, default is 'all'"
  help_envvar CABAL_CHECK_RELAX "[y] Do not fail if cabal check fails on the package."
  help_envvar CABAL_HACKAGE_MIRROR "DESTRUCTIVE! Specify an alternative mirror, modifies the cabal config file."

  show_step1 "stack options"
  help_envvar STACK_YAML "Alternative stack config file path relative to project root"
  help_envvar STACK_OPTIONS "ADDITIONAL stack global options (e.g. -v) to append"
  help_envvar STACK_BUILD_OPTIONS "ADDITIONAL stack build command options to append"

  show_step1 "hlint options"
  #help_envvar HLINT_COMMANDS "hlint commands e.g.'hlint lint src; hlint lint test'"
  # XXX this is broken
  #help_envvar HLINT_BUILD "Build latest hlint from hackage source"
  help_envvar HLINT_OPTIONS "hlint arguments e.g.'--datadir=. lint'"
  help_envvar HLINT_TARGETS "target directories to run hlint on e.g. 'src test'"

  show_step1 "Coverage options"
  # Untested/unsupported should be removed
  #help_envvar COVERALLS_OPTIONS "hpc-coveralls args and options, usually just test suite names"
  help_envvar COVERAGE "[y] Just generate coverage information"

  show_step1 "Diagnostics options"
  # To catch spelling mistakes in envvar names passed, otherwise they will be
  # silently ignored and we will be wondering why the script is not working.
  help_envvar CHECK_ENV "[y] Treat unknown env variables as error, used with env -i"
  help_envvar BASE_TIME "System time to be used as base for timeline reporting"
}

check_all_boolean_vars () {
  check_boolean_var STACK_UPGRADE
  check_boolean_var DISABLE_SDIST_BUILD
  check_boolean_var DISABLE_DIST_CHECKS
  check_boolean_var DISABLE_SDIST_PROJECT_CHECK
  check_boolean_var DISABLE_SDIST_GIT_CHECK
  check_boolean_var CABAL_USE_STACK_SDIST
  check_boolean_var CABAL_REINIT_CONFIG
  check_boolean_var CABAL_CHECK_RELAX
  check_boolean_var CABAL_DISABLE_DEPS
  if test -n "$TEST_INSTALL"
  then
    echo "WARNING! TEST_INSTALL is deprecated. Please use ENABLE_INSTALL instead"
    ENABLE_INSTALL="$TEST_INSTALL"
    unset TEST_INSTALL
  fi
  check_boolean_var ENABLE_GHCJS
  check_boolean_var ENABLE_INSTALL
  if test -n "$ENABLE_INSTALL"
  then
    echo "WARNING! ENABLE_INSTALL is deprecated and will be removed in future"
    echo "WARNING! ENABLE_INSTALL is a no op, it will do nothing"
    unset ENABLE_INSTALL
  fi
  check_boolean_var DISABLE_BENCH
  check_boolean_var DISABLE_TEST
  check_boolean_var DISABLE_DOCS
  check_boolean_var ENABLE_DOCSPEC
  check_boolean_var COVERAGE
  check_boolean_var CHECK_ENV
}

show_build_command() {
  check_all_boolean_vars

  echo "You can use the following command to reproduce this build:"
  echo
  echo -n "$0 $BUILD "
  for i in $SAFE_ENVVARS
  do
    local val="$(show_nonempty_var $i)"
    test -z "$val" || echo -n "$val "
  done
  echo

  local unsafe
  for i in $UNSAFE_ENVVARS
  do
    if test -n "$(show_nonempty_var $i)"
    then
      unsafe=y
    fi
  done

  if test -n "$unsafe"
  then
    echo
    echo "The above command has omitted the following unsafe options."
    echo "If you know what you are doing, you can also add these to the"
    echo "above command to reproduce this build more faithfully."
    echo
    echo "Unsafe options may modify your config and "
    echo "are usually meant to be used in a CI setup:"
    for i in $UNSAFE_ENVVARS
    do
      show_nonempty_var $i
    done
    echo
  fi
}

show_build_config() {
  check_all_boolean_vars
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

# $1: be verbose about why we need cabal
dont_need_cabal() {
  if test "$BUILD" = "cabal-v2"
  then
    test -z "$1" || echo "Need cabal-install because 'BUILD=$BUILD'"
    return 1
  fi

  if test -z "$DISABLE_SDIST_BUILD"
  then
    test -z "$1" || echo "Need cabal-install because 'DISABLE_SDIST_BUILD=$DISABLE_SDIST_BUILD'"
    return 1
  fi

  return 0
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
  test -z "$COVERALLS_OPTIONS" || COVERAGE=y
  if test -n "$ENABLE_GHCJS"
  then
    test "$BUILD" = "cabal-v2" || die "ENABLE_GHCJS works only with build type 'cabal-v2'"
    COMPILER=ghcjs
    GHCJS_FLAG=--ghcjs
  else
    if test "$GHCVER" = "head"
    then
      COMPILER=ghc-head
    else
      COMPILER=ghc
    fi
  fi

  if test "$BUILD" = stack
  then
    STACK_DEP_OPTIONS="$STACK_BUILD_OPTIONS --only-dependencies"
    test -n "$DISABLE_TEST" || STACK_DEP_OPTIONS="$STACK_DEP_OPTIONS --test"
    test -n "$DISABLE_BENCH" || STACK_DEP_OPTIONS="$STACK_DEP_OPTIONS --bench"

    STACK_BUILD_OPTIONS_ORIG=$STACK_BUILD_OPTIONS
    STACK_BUILD_OPTIONS=$(cat << EOF
      $(test -n "$DISABLE_DOCS" || echo "--haddock --no-haddock-deps")
      $(test -n "$DISABLE_TEST" || echo "--test")
      $(test -n "$DISABLE_BENCH" || echo "--bench --no-run-benchmarks")
      $(test -z "${COVERAGE}" || echo --coverage)
      $(test -z "${GHC_OPTIONS}" || echo --ghc-options=\"$GHC_OPTIONS\")
      $STACK_BUILD_OPTIONS
EOF
)
  elif test "$BUILD" = "cabal-v2"
  then
    test -n "$CABAL_BUILD_TARGETS" || CABAL_BUILD_TARGETS=all

    CABAL_BUILD_OPTIONS=$(cat << EOF
      $(test -n "$DISABLE_TEST" || echo "--enable-tests")
      $(test -n "$DISABLE_BENCH" || echo "--enable-benchmarks")
      $(test -z "${GHC_OPTIONS}" || echo --ghc-options=\"$GHC_OPTIONS\")
      $CABAL_BUILD_OPTIONS
EOF
)
    CABAL_DEP_OPTIONS="$CABAL_BUILD_OPTIONS --only-dependencies --reorder-goals --max-backjumps=-1"

    test -z "${COVERAGE}" || \
      CABAL_BUILD_OPTIONS="$CABAL_BUILD_OPTIONS --enable-coverage"
  else
      die "Bug: Unknown build type [$BUILD]"
  fi

  # These variables are now combined with other options so clear them
  # so that we do not show them in the effective config
  COVERAGE=
  GHC_OPTIONS=

  test "$BUILD" = stack -o \
       "$BUILD" = "cabal-v2" || \
    die "build [$BUILD] can only be 'stack', or 'cabal-v2'"

  if test -n "$CHECK_ENV"
  then
    if test "$BUILD" != "cabal-v2"
    then
      cabal_only_var CABAL_PROJECT
      cabal_only_var CABALVER
      cabal_only_var CABAL_USE_STACK_SDIST
      cabal_only_var CABAL_CHECK_RELAX
      cabal_only_var CABAL_HACKAGE_MIRROR

      cabal_only_var CABAL_BUILD_OPTIONS
      cabal_only_var CABAL_TEST_OPTIONS
      cabal_only_var CABAL_DISABLE_DEPS
      cabal_only_var CABAL_BUILD_TARGETS
    fi

    if test -z "$(need_stack)"
    then
      stack_only_var STACK_YAML
      stack_only_var STACK_UPGRADE
      stack_only_var STACK_OPTIONS
      stack_only_var STACK_BUILD_OPTIONS
    fi
  fi
}

#------------------------------------------------------------------------------
# Stack fetch and install etc.
#------------------------------------------------------------------------------

#ensure_msys_tools() {
#  if [[ `uname` = MINGW* ]]
#  then
#    # retry??
#    for i in "$1"
#    do
#      if test -z "$(which_cmd $i)"
#      then
#        stack exec pacman -- -S --noconfirm $i
#      fi
#    done
#  fi
#}

fetch_stack_osx() {
  curl --fail -sSkL https://www.stackage.org/stack/osx-x86_64 \
    | tar xz --strip-components=1 -C $1 --include '*/stack'
}

fetch_stack_linux() {
  curl --fail -sSkL https://www.stackage.org/stack/linux-x86_64 \
    | tar xz --strip-components=1 -C $1 --wildcards '*/stack'
}

fetch_stack_windows() {
  curl --fail -sSkL http://www.stackage.org/stack/windows-i386 \
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
  if test -z "$(which_cmd stack)"
  then
    echo "Downloading stack to [$1]..."
    fetch_stack $1
  fi
  require_cmd stack

  # Empty STACK_YAML variable does not go well
  if test -z "$STACK_YAML"
  then
    unset STACK_YAML
  fi
  if test -n "$STACK_UPGRADE"
  then
    echo "Upgrading stack to the required version"
    if test -n "$STACKVER"
    then
      local curver=$(stack --numeric-version)
      if test "${curver#$STACKVER}" = ${curver}
      then
        run_verbose stack --no-terminal upgrade --binary-only --binary-version $STACKVER
      fi
    else
      run_verbose stack --no-terminal upgrade --binary-only || fetch_stack $1
    fi
  fi

  test -z "$STACKVER" || check_version_die stack $STACKVER
  # Set the real version of stack
  STACKVER=$(stack --numeric-version) || exit 1

  STACKCMD="stack --no-terminal $STACK_OPTIONS"
  STACKCMD_TOOL_INSTALL="$STACKCMD"
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
  run_verbose_errexit $STACKCMD path --bin-path
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
      echo "Adding [$GHCPATHS] in front of PATH..."
      export PATH=$GHCPATHS:$PATH
    fi
  fi
  if test -n "$BINPATH"
  then
    echo "Adding [$BINPATH] in front of PATH..."
    export PATH=$BINPATH:$PATH
  fi
}

#------------------------------------------------------------------------------
# Ensure ghc, cabal are available and the right versions when requested
#------------------------------------------------------------------------------

# $1: version to compare
# $2: version to compare with
# true (returns 0) if $1 <= $2
verlte () {
  [ "$1" = "$(echo -e "$1\n$2" | sort -V | head -n1)" ]
}

# $1: tool name (used only for ghc, cabal and stack)
# $2: expected version
check_version() {
  local real_ver=$($1 --numeric-version)

  # Match that the expected version is a prefix of the real version.
  # Do not check when the expected version is head.
  if test "${real_ver#$2}" != "${real_ver}" -o "$2" = head
  then
    return 0
  else
    echo "Found $1 version [$real_ver] expecting [$2]"
    return 1
  fi
}

# $1: tool name (used only for ghc, cabal and stack)
# $2: expected version
check_version_die() {
  check_version $1 $2 || \
    die "Wrong $1 version [$($1 --numeric-version)] expected [$2]"
}

# Remove a path from the PATH envvar
# $1: path component to remove
function path_remove {
  # Delete path by parts so we can never accidentally remove sub paths
  PATH=${PATH//":$1:"/":"} # delete any instances in the middle
  PATH=${PATH/#"$1:"/} # delete any instance at the beginning
  PATH=${PATH/%":$1"/} # delete any instance at the end
}

# Find ghc/cabal having the given version prefix in PATH or in a version
# suffixed directory at TOOLS_DIR, and check if it matches the requested
# version. If we found the requested binary we make sure that it is in the
# PATH.
#
# $1: binary name (e.g. ghc or cabal)
# $2: binary version prefix (e.g. 8 or 8.0 or 8.0.1)
find_binary () {
  local binary
  local path=$PATH
  local removed_path

  echo "Looking for binary [$1] in PATH..."
  binary=$(which_cmd $1)
  while test -n "$binary"
  do
    if test -z "$2" || check_version $binary $2
    then
      PATH=$PATH$removed_path
      echo "Found. PATH set to [$PATH]..."
      return 0
    else
      # remove it from the path and search again
      echo "Mismatching $1 version found at [$(dirname $binary)], removing it from PATH and trying again"
      removed_path=$removed_path:$(dirname $binary)
      path_remove $(dirname $binary)
      binary="$(which_cmd $1)"
    fi
  done
  echo "Not found."

  # If we did not find the binary, restore the PATH for better error reporting
  PATH=$path

  if test -n "$TOOLS_DIR"
  then
    echo "Looking for binary [$1] in [$TOOLS_DIR]..."
    # Find if we have a binary in TOOLS_DIR
    local dir
    if test -n "$2"
    then
      dir=$(echo ${TOOLS_DIR}/$1/$2*/ | tr ' ' '\n' | sort | tail -1)
    else
      dir=$(echo ${TOOLS_DIR}/$1/[0-9]*/ | tr ' ' '\n' | sort | tail -1)
      test -x "${dir}/bin/$1" || dir="${TOOLS_DIR}/$1/head"
    fi

    if test -x "${dir}/bin/$1"
    then
      if test -z "$2" || check_version "${dir}/bin/$1" $2
      then
        if [[ $dir != /* ]]
        then
          dir=`pwd`/$dir
        fi
        PATH=$dir/bin:$PATH
        echo "Found. PATH set to [$PATH]..."
        export PATH
        return 0
      fi
    fi
    echo "Not found."
  fi

  STACK_ROOT_PATH="~/.stack"
  if test -n "$STACK_ROOT"
  then
    STACK_ROOT_PATH=$STACK_ROOT
  fi
  # XXX what if there are multiple arch dirs in programs?
  if test -d $STACK_ROOT_PATH -a "$1" = "ghc"
  then
    echo "Looking for binary [$1] in [$STACK_ROOT_PATH]..."
    # XXX We should pick the highest version by default
    local dir=$(echo $STACK_ROOT_PATH/programs/*/${1}-${2}*/)
    if test -x "${dir}/bin/$1"
    then
      if test -z "$2" || check_version "${dir}/bin/$1" $2
      then
        if [[ $dir != /* ]]
        then
          dir=`pwd`/$dir
        fi
        PATH=$dir/bin:$PATH
        echo "Found. PATH set to [$PATH]..."
        export PATH
        return 0
      fi
    fi
    echo "Not found."
  fi
  return 1
}

ghcup_install() {
  local tool=$1
  local tool_ver=$2
  local GHCUP_ARCH
  local ghcup_path

  # XXX add only if not already on PATH
  PATH=$GHCUP_BIN:$PATH
  echo "Added $GHCUP_BIN to PATH [$PATH]"
  ghcup_path=$(which_cmd ghcup)

  if test -n "$ghcup_path"
  then
    echo "Using existing $ghcup_path in PATH"
  else
    # User can either add it in the PATH or we can use the full path of the
    # tool either as found in PATH or use GHCUP_PATH directly. We should
    # probably fix each tool's location as found and use that rather
    # than using it from PATH.
    if test -e "$GHCUP_PATH"
    then
      die "$GHCUP_PATH already exists, not overwriting."
    fi

    # Determine GHCUP_ARCH
    os=$(uname -s -m)
    case "$os" in
      "Linux x86_64") GHCUP_ARCH="x86_64-linux" ;;
      "FreeBSD x86_64" | "FreeBSD amd64") GHCUP_ARCH=x86_64-portbld-freebsd ;;
      "Darwin x86_64") GHCUP_ARCH="x86_64-apple-darwin" ;;
      "Darwin arm64") GHCUP_ARCH="aarch64-apple-darwin" ;;
      *) echo "Unknown OS/Arch: $os"; exit 1;;
    esac

    # Check available versions here: https://downloads.haskell.org/~ghcup/
    URL="$GHCUP_URL_PREFIX/$GHCUP_VERSION/${GHCUP_ARCH}-ghcup-$GHCUP_VERSION"
    echo "Downloading $URL to $GHCUP_PATH"
    # XXX Download to a temp location and move when successful?
    #TEMP=$(mktemp -d .ghcup-XXXXXX)
    #cleanup(){
    #    rm -r $TEMP
    #}
    #trap cleanup EXIT
    mkdir -p $(dirname $GHCUP_PATH)
    retry_cmd curl --fail --progress-bar --location -o $GHCUP_PATH $URL
    if test $? -ne 0
    then
      rm -f $GHCUP_PATH
      echo "Failed to download ghcup"
      exit 1
    fi
    chmod +x $GHCUP_PATH
    # Avoid changing the user's setup
    #$GHCUP_PATH set $tool $tool_ver
  fi

  if test "$tool" = "ghc"
  then
    run_verbose_errexit_with "cat /usr/local/.ghcup/logs/*" \
        ghcup install ghc $GHCUP_GHC_OPTIONS $tool_ver
  else
    run_verbose_errexit ghcup install $tool $tool_ver
  fi
}

ensure_default_ghc() {
  local ghc
  ghc="$(which_cmd ghc)"
  if test -z "$ghc" -a -n "$GHCUP_VERSION"
  then
    echo "No default ghc found in PATH. Setting it using ghcup"
    ghcup set ghc $GHCVER
  fi
}

ensure_ghc() {
  local found
  local compiler
  # If there is a ghc in PATH then use that otherwise install it using
  # ghcup or stack
  find_binary "$COMPILER" "$GHCVER"
  found=$?
  if test "$found" -ne 0
  then
    if test -n "$GHCVER"
    then
      find_binary "$COMPILER-$GHCVER" "$GHCVER"
      found=$?
      if test "$found" -eq 0
      then
        COMPILER="$COMPILER-$GHCVER"
      fi
    fi

    if test "$found" -ne 0
    then
      if test -n "$(need_stack)"
      then
        # Use stack supplied ghc
        echo "$STACKCMD setup"
        retry_cmd $STACKCMD setup || die "stack setup falied"
        use_stack_paths
        echo
      else
        ghcup_install ghc $GHCVER
        if test -n "$GHCVER" -a "$COMPILER" != "ghc-head"
        then
          COMPILER="$COMPILER-$GHCVER"
        fi
      fi
    fi
  fi

  compiler="$(which_cmd $COMPILER)"
  if test -z "$compiler"
  then
    local msg
    msg="$COMPILER $GHCVER not found in PATH [$PATH]"
    if test -n "$TOOLS_DIR"
    then
      msg="$msg or in $TOOLS_DIR/$COMPILER/$GHCVER*/bin"
    fi
    die "$msg"
  fi

  if test -n "$GHCVER"
  then
    check_version_die $COMPILER $GHCVER
  fi

  if test -n "$GHCVER" -a -n "$(need_stack)"
  then
    echo "GHCVER is specified, using '$STACKCMD --system-ghc'."
    echo "Clear GHCVER if you want to use stack supplied ghc."
    # If the user specified GHCVER then use it as system-ghc
    # Stack will still silently choose its own ghc if the ghc does not match
    # the snapshot.
    STACKCMD="$STACKCMD --system-ghc"
  fi

  if test -n "$(need_stack)"
  then
    compiler=$($STACKCMD path --compiler-exe) || exit 1
  fi

  export COMPILER_EXE_PATH="$compiler"
  echo "Using $COMPILER at $compiler"
  echo "$($compiler --version) [$($compiler --print-project-git-commit-id 2> /dev/null || echo '?')]"
  # Use the real version, the user might have specified a version prefix in
  # GHCVER
  GHCVER=$($compiler --numeric-version) || exit 1

  # cabal info command requires "ghc" to be in PATH
  if test -z "$DISABLE_SDIST_BUILD"
  then
    ensure_default_ghc
  fi

  if test -n "$ENABLE_GHCJS"
  then
    local tmpdir
    local shebang
    local nodepath
    local output

    # XXX the temp file may not get removed on crash/interrupt
    tmpdir=`mktemp -d 2>/dev/null || mktemp -d -t 'packcheck'`
    echo "main=putStrLn \"hello\"" > $tmpdir/test.hs
    $compiler -build-runner -o ${tmpdir}/run $tmpdir/test.hs
    shebang=$(head -1 ${tmpdir}/run)
    nodepath=${shebang:2}
    echo "$compiler uses node at [${nodepath}] for executing js output"
    if test -x $nodepath
    then
      echo "$nodepath is version [$($nodepath --version)]" || die "Cannot determine node version"
    else
      rm -rf $tmpdir
      die "[$nodepath] not found or not executable"
    fi
    echo "Trying to run a test executable produced by [$compiler]..."
    output=$(${tmpdir}/run)
    rm -rf $tmpdir

    if test "$output" != "hello"
    then
      die "[$compiler] cannot produce executables runnable by [$nodepath]"
    else
      echo "[$compiler] produces executables runnable by [$nodepath]"
    fi
  fi
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

# We need to ignore the project's stack.yaml when installing
# cabal-install, otherwise it may fail due to dependency conflicts.
#
# On CI machines if our repo is cloned in the top dir then there is no way to
# teach stack to ignore the project's stack.yaml. We cannot change our
# directory to go above it. Because we have a project stack.yaml, stack does
# not even create a global stack.yaml. So we work it around by creating a new
# package and installing cabal via that.
#
# $1 tool name
stack_install_tool () {
    rm -rf .packcheck/tool-install
    mkdir -p .packcheck/tool-install || exit 1
    cd .packcheck/tool-install || exit 1

    local res
    # Where possible we want to use the same resolver for build as well as
    # tools to share the snapshot when building.
    # But nightlies may not have cabal-install all the time
    if [[ "$RESOLVER" != "" && "$RESOLVER" != nightly* ]]
    then
      res="--resolver $RESOLVER"
    fi

    if test ! -e stack.yaml
    then
      run_verbose_errexit $STACKCMD_TOOL_INSTALL $res \
        new --bare tool-install
    fi
    run_verbose_errexit $STACKCMD_TOOL_INSTALL $res install $1
    cd ../..
}

# $1: Directory to install cabal in
# We are not using the param as currently it is always the dir where stack
# installs binaries.
ensure_cabal() {
  # We can only do this after ghc is installed.
  # We need cabal to retrieve the package version
  # We are assuming CI cache will be per resolver so we can cache the bin

  find_binary $CABAL_BINARY_NAME "$CABALVER"
  found=$?
  if test "$found" -ne 0
  then
    if test -n "$CABALVER"
    then
      find_binary "$CABAL_BINARY_NAME-$CABALVER" "$CABALVER"
      found=$?
      if test "$found" -eq 0
      then
        CABAL_BINARY_NAME="$CABAL_BINARY_NAME-$CABALVER"
      fi
    fi

    if test "$found" -ne 0
    then
      if test -n "$(need_stack)"
      then
        stack_install_tool cabal-install
      elif test -n "$GHCUP_VERSION"
      then
        ghcup_install cabal $CABALVER
        if test -n "$CABALVER"
        then
          CABAL_BINARY_NAME="$CABAL_BINARY_NAME-$CABALVER"
        fi
      fi
    fi
  fi

  require_cmd $CABAL_BINARY_NAME
  $CABAL_BINARY_NAME --version
  test -z "$CABALVER" || check_version_die $CABAL_BINARY_NAME $CABALVER
  # Set the real version of cabal
  CABALVER=$($CABAL_BINARY_NAME --numeric-version) || exit 1

  MIN_CABALVER="1.24.0.0"
  verlte $MIN_CABALVER $CABALVER || \
      die "Cabal version should at least be $MIN_CABALVER"
}

# Find a file in the parent/ancestor directories
find_in_parents () {
  local curdir="$1" # must be absolute path without ..
  local file="$2"

  if test -e "$curdir/$file"
  then echo "$curdir/$file"
  elif test "$curdir" != "/"
  then find_in_parents "$(dirname "$curdir")" "$file"
  fi
}

ensure_cabal_project() {
  CABALCMD=$CABAL_BINARY_NAME

  if test -n "$CABAL_PROJECT"
  then
    require_file "$CABAL_PROJECT"
    echo "Using user specified cabal project file at [$CABAL_PROJECT]"
    if test -n "$DISABLE_SDIST_BUILD"
    then # regular build
      SDIST_CABALCMD="$CABALCMD --project-file=$CABAL_PROJECT"
    else # sdist build
      # We run the command from .packcheck/<package> dir after unpacking
      # sdist
      SDIST_CABALCMD="$CABALCMD --project-file=../../$CABAL_PROJECT"
      echo "WARNING!! You should not test a distribution build with a cabal" \
           "project file. It may not be reproducible."
    fi
    CABALCMD="$CABALCMD --project-file=$CABAL_PROJECT"
  else
    SDIST_CABALCMD="$CABALCMD"
    local implicit_proj_file
    implicit_proj_file=$(find_in_parents "$(pwd)" cabal.project)
    if test -n "$implicit_proj_file"
    then
      echo "Implicit cabal project file found at [$implicit_proj_file]"
    fi
  fi
  echo "Using cabal command [$CABALCMD]"
  echo "Using sdist cabal command [$SDIST_CABALCMD]"
}

ensure_stack_yaml() {
  if test -n "$STACK_YAML"
  then
    require_file $STACK_YAML
    echo "Using user specified stack.yaml file at [$STACK_YAML]"
    if test -n "$DISABLE_SDIST_BUILD"
    then # regular build
      SDIST_STACKCMD="$STACKCMD --stack-yaml $STACK_YAML"
    else # sdist build
      # We run the stack command from .packcheck/<package> dir after unpacking
      # sdist
      SDIST_STACKCMD="$STACKCMD --stack-yaml ../../$STACK_YAML"
      echo "WARNING!! You should not test a distribution build with a" \
           "stack.yaml file. It may not be reproducible."
    fi
    STACKCMD="$STACKCMD --stack-yaml $STACK_YAML"
  else
    SDIST_STACKCMD="$STACKCMD"
    local implicit_stack_yaml
    implicit_stack_yaml=$(find_in_parents "$(pwd)" stack.yaml)
    if test -n "$implicit_stack_yaml"
    then
      echo "Implicit stack.yaml file found at [$implicit_stack_yaml]"
    else
      if test -n "$DISABLE_SDIST_BUILD"
      then # regular build
        run_verbose_errexit $STACKCMD init
      fi
    fi
  fi
  echo "Using stack command [$STACKCMD]"
  echo "Using sdist stack command [$SDIST_STACKCMD]"
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
  full_name=$($CABAL_BINARY_NAME info . | awk '{ if ($1 == "*") {print $2; exit}}') || true
  if test -z "$full_name"
  then
    if test -z "$DISABLE_SDIST_BUILD"
    then
      die "'cabal info' command failed to determine package name.\nPlease use 'DISABLE_SDIST_BUILD=y' to avoid this issue."
    fi
  else
    if test "${pkgname}${full_name#$pkgname}" != "${full_name}"
    then
      die "Inconsistent package name [$pkgname] and package full name [$full_name]"
    fi
  fi

  echo $full_name
}

# XXX use MULTI_PACKAGE_PROJECT as a CLI option, instead of trying to determine
# it here.
determine_build_type() {
  MULTI_PACKAGE_PROJECT=false
  echo "Looking for cabal or stack build files in the current directory..."
  local name=$(echo *.cabal)
  if test "$name" = "*.cabal"
  then
    if test -f "package.yaml" -a -n "$STACKCMD"
    then
        echo "No cabal file found but a package.yaml file found"
        echo "Generating cabal file from package.yaml"
        # Generate cabal file from package.yaml
        run_verbose "$STACKCMD query > /dev/null 2>&1"
    else
      if test $BUILD = "stack" -a -f "stack.yaml"
      then
        echo "No cabal file found but a stack.yaml file found, assuming a multipackage project."
        echo "Setting DISABLE_SDIST_BUILD=y and DISABLE_DIST_CHECKS=y"
        MULTI_PACKAGE_PROJECT=true
        DISABLE_SDIST_BUILD=y
        DISABLE_DIST_CHECKS=y
      else
        if test $BUILD = "cabal-v2" -a -f "cabal.project"
        then
          echo "No cabal file found but a cabal.project file found, assuming a multipackage project."
          echo "Setting DISABLE_SDIST_BUILD=y and DISABLE_DIST_CHECKS=y"
          MULTI_PACKAGE_PROJECT=true
          DISABLE_SDIST_BUILD=y
          DISABLE_DIST_CHECKS=y
        else
          echo "No valid build config file cabal/cabal.project/package.yaml/stack.yaml found."
          die "Make sure you are using BUILD=cabal-v2 if you are using a cabal.project file"
        fi
      fi
    fi
  else
    if test ! -f "$name"
    then
      echo "Either there are multiple cabal files in the directory"
      echo "or the cabal file is not a regular file."
      die "cabal files: $name"
    else
      echo "Found [$name]"
    fi
  fi
}

ensure_cabal_config() {
  # When cabal versions change across builds on a CI host its safer to remove
  # the old config so that the build does not error out.
  local cfg="${OS_APP_HOME}/${OS_CABAL_DIR}/config"
  if test "$CABAL_REINIT_CONFIG" = y
  then
    echo "Removing old cabal config [$cfg]"
    run_verbose_errexit rm -f "$cfg"
  fi

  # this generates it in ~/.config which creates issues for cabal-docspec and
  # some other issues.
  if test ! -e $cfg
  then
    run_verbose $CABAL_BINARY_NAME user-config init || true
    if test ! -f $cfg
    then
      if test -f ${OS_APP_HOME}/.config/cabal/config
      then
        mkdir -p $(dirname $cfg)
        run_verbose_errexit ln -s ${OS_APP_HOME}/.config/cabal/config $cfg
      fi
    fi
  fi

  if test "$BUILD" = "cabal-v2"
  then
    if test -n "$CABAL_HACKAGE_MIRROR"
    then
      cabal_use_mirror $CABAL_HACKAGE_MIRROR
    fi

    if test -z "$CABAL_DISABLE_DEPS"
    then
      echo
      echo "cabal v2-update"
      if test -n "$CABAL_PROJECT"
      then
        retry_cmd $CABAL_BINARY_NAME v2-update --project-file "$CABAL_PROJECT"
      else
        retry_cmd $CABAL_BINARY_NAME v2-update
      fi
    fi
  fi
}

# $1: dir where they are installed
remove_pkg_executables() {
  test -n "$(which_cmd $CABAL_BINARY_NAME)" || return 0

  exes=$($CABAL_BINARY_NAME info . | awk '{if ($1 == "Executables:") { print $2; exit }}') || exit 1
  echo "Remove installed binaries"
  for i in $exes
  do
    # The executables may be under a cabal flag and may not have been installed
    # unless we used that flag. So just use "rm -f" to remove silently.
    run_verbose_errexit rm -f "$1"/"$i"
  done
}

sdist_remove_project_file () {
    if test -e "$1"
    then
      echo "WARNING! Avoid including $1 in a distribution"
      test -n "$DISABLE_SDIST_PROJECT_CHECK" || \
        die "Use DISABLE_SDIST_PROJECT_CHECK=y to allow this"
      run_verbose rm -f "$1"
    fi
}

# $1: package full name (name + ver)
create_and_unpack_pkg_dist() {
  local pkgtar=${1}.tar.gz
  local opts
  local SDIST_DIR
  local SDIST_CMD

  test -z "$SDIST_OPTIONS" || opts="$SDIST_OPTIONS"

  if test "$BUILD" = stack
  then
    # When custom setup is used we need to configure before we can use sdist.
    if test -z "$STACK_YAML"
    then
      test -e stack.yaml || run_verbose_errexit $STACKCMD init
    fi
    run_verbose_errexit $STACKCMD build --only-configure
    SDIST_CMD="$STACKCMD sdist $opts"
    SDIST_DIR=$($STACKCMD path --dist-dir) || exit 1
  elif test -n "$CABAL_USE_STACK_SDIST"
  then
    # When custom setup is used we need to configure before we can use sdist.
    run_verbose_errexit $STACKCMD --compiler=$COMPILER-$GHCVER build --only-configure
    SDIST_CMD="$STACKCMD --compiler=$COMPILER-$GHCVER sdist $opts"
    SDIST_DIR=$($STACKCMD --compiler=$COMPILER-$GHCVER path --dist-dir) || exit 1
  else
    # XXX We need to configure to use sdist and we need to install
    # dependencies to configure. So to just create the sdist we will
    # have to go through the whole process once and then again after
    # unpacking the sdist and to build it.
    SDIST_CMD="$CABALCMD v2-sdist $opts"
    SDIST_DIR=dist-newstyle/sdist
  fi

  # stack commands return path in windows format
  [[ `uname` != MINGW* ]] || SDIST_DIR=`cygpath ${SDIST_DIR}`

  local tarpath=${SDIST_DIR}/${pkgtar}
  rm -f $tarpath
  echo
  run_verbose_errexit $SDIST_CMD
  if test ! -f $tarpath
  then
    echo "$SDIST_CMD did not create [$tarpath]"
    exit 1
  fi

  # Unpack the tar inside .packcheck directory
  mkdir -p .packcheck || exit 1

  # XXX Move the git repo comparison code to a separate function
  local gitcmd="$(which_cmd git)"
  test -n "$gitcmd" || echo "WARNING! git command not found skipping source distribution comparison with git repo"

  if test -d ".git" -a -n "$gitcmd" -a -n "$(which_cmd tar)"
  then
    echo ".git directory found, assuming git repo"
    echo "Comparing distribution contents against git ls-files..."
    tar -ztf $tarpath \
      | sed -e "s%^$PACKAGE_FULL_NAME/%%" \
      | grep -v '/$' \
      | grep -v '^$' \
      > .packcheck/tar-ztf.txt
    if test -f .packcheck.ignore
    then
      local pi_files_exist=""
      local pi_files_n_exist=""
      while read p; do
          if [ ! -f "$p" -a ! -d "$p" ]
          then
              pi_files_n_exist="$p\n$pi_files_n_exist"
          fi
          x=`git ls-files $p`
          pi_files_exist="$x\n$pi_files_exist"
      done <.packcheck.ignore
      if test -n "$pi_files_n_exist"
      then
          echo "WARNING: The following files or directories don't exist but are mentioned in \
your .packcheck.ignore file."
          printf "$pi_files_n_exist"
      fi
      local sane_ignore_file=".packcheck/sane-ignore"
      printf "$pi_files_exist" > "$sane_ignore_file"
      cat "$sane_ignore_file" .packcheck/tar-ztf.txt \
        | sort | grep -v '^$' > .packcheck/tar-ztf1.txt
    else
      cat .packcheck/tar-ztf.txt \
        | sort | grep -v '^$' > .packcheck/tar-ztf1.txt
    fi
    git ls-files | sort | grep -v '^$' > .packcheck/git-ls-files.txt
    local diff_res_file=".packcheck/tar-git-diff.txt"
    local tar_minus_git
    local git_minus_tar
    diff -B --suppress-common-lines .packcheck/tar-ztf1.txt .packcheck/git-ls-files.txt > "$diff_res_file" ||
      { echo "WARNING! Source distribution tar and git repo contents differ."
        tar_minus_git="$(awk '$0~/^< .+$/' "$diff_res_file")"
        git_minus_tar="$(awk '$0~/^> .+$/' "$diff_res_file")"
        if test -n "$tar_minus_git"
        then
            echo
            echo "The following files exist in your source distribution but \
are not committed to the git repository."
            echo "$tar_minus_git"
            echo "Please consider committing them to the git repository."
            echo "Or clean the git workspace of unwanted files before creating \
the source distribution."
            echo "Wildcards in 'extra-sources-files' section and \
'extra-doc-files' section of the cabal file may pickup any files lying around."
        fi
        if test -n "$git_minus_tar"
        then
            echo
            echo "The following files are committed to the git repository \
but do not exist in the source distribution."
            echo "$git_minus_tar"
            echo "Please consider adding them to your cabal file under \
'extra-source-files' or 'extra-doc-files'."
            echo "If you do not want to add them in the source distribution \
then add them to .packcheck.ignore file at the root of the git repository."
        fi
        if test -z "$DISABLE_SDIST_GIT_CHECK"
        then
          echo
          echo "Exiting. Use DISABLE_SDIST_GIT_CHECK=y to disable this check."
          exit 1
        fi
      }

      rm -f .packcheck/tar-ztf.txt \
        .packcheck/tar-ztf1.txt \
        .packcheck/git-ls-files.txt \
        "$diff_res_file" \
        "$sane_ignore_file"
  fi

  echo
  echo "cd .packcheck"
  cd .packcheck || exit 1
  test "${tarpath:0:1}" == / || tarpath=../$tarpath
  $OS_UNGZTAR_CMD $tarpath
  echo

  # NOTE: We are entering the sdist directory inside .packcheck now. The
  # original tree is intact. When we remove the cabal.project or stack.yaml
  # files here, it is removed from the sdist copy not from the original tree.
  # However, the CABAL_PROJECT and STACK_YAML variables are pointing to a path
  # relative to the original tree root.
  cd ${1}
  if test "$BUILD" = stack
  then
    if test -z "$STACK_YAML"
    then
      sdist_remove_project_file stack.yaml
      run_verbose_errexit $SDIST_STACKCMD init
    fi
  else
    if test -z "$CABAL_PROJECT"
    then
      # Is there a way in cabal to ignore project file?
      # XXX how does cabal build dependencies with project files?
      sdist_remove_project_file cabal.project
      sdist_remove_project_file cabal.project.local
      sdist_remove_project_file cabal.project.freeze

      # We want to ensure that our build is not affected by the
      # implicit cabal.project file. This is likely because the
      # .packcheck directory is created inside the project repo which
      # may have a cabal.project file.
      local implicit_proj_file
      implicit_proj_file=$(find_in_parents "$(pwd)" cabal.project)

      if test -n "$implicit_proj_file"
      then
        echo "WARNING! Implicit cabal.project found at [$implicit_proj_file]"
        echo "Creating an explicit cabal.project file to ensure that we" \
             "do not use the implicit one for a distribution build"
        echo "packages: ." > cabal.project
        run_verbose cat cabal.project
        echo
      fi
    fi
  fi

  show_step "Package info [sdist $SDIST_OPTIONS]"
  run_verbose $CABAL_BINARY_NAME info . || true

  if test -f "./configure.ac"
  then
    show_step "Run autoreconf"
    run_verbose_errexit autoreconf -i
  fi
}

install_deps() {
  case "$BUILD" in
    stack) run_verbose_errexit $SDIST_STACKCMD build $STACK_DEP_OPTIONS ;;
    cabal-v2)
      if test -z "$CABAL_DISABLE_DEPS"
      then
        run_verbose_errexit $SDIST_CABALCMD v2-build \
          --with-compiler "$COMPILER_EXE_PATH" \
          $GHCJS_FLAG $CABAL_DEP_OPTIONS $CABAL_BUILD_TARGETS
      else
        echo "Skipping. CABAL_DISABLE_DEPS is on."
      fi ;;
  esac
}

build_and_test() {
  case "$BUILD" in
    stack) run_verbose_errexit $SDIST_STACKCMD build $STACK_BUILD_OPTIONS ;;
    cabal-v2)
      run_verbose_errexit $SDIST_CABALCMD v2-build \
        --with-compiler "$COMPILER_EXE_PATH" \
        $GHCJS_FLAG $CABAL_BUILD_OPTIONS $CABAL_BUILD_TARGETS
      echo
      test -n "$DISABLE_DOCS" || \
        run_verbose_errexit $SDIST_CABALCMD v2-haddock \
          --with-compiler "$COMPILER_EXE_PATH" \
          $GHCJS_FLAG $CABAL_BUILD_OPTIONS $CABAL_BUILD_TARGETS
      if test -z "$DISABLE_TEST"
      then
        local version
        local SHOW_DETAILS
        version=$($CABAL_BINARY_NAME --numeric-version|cut -f1,2 -d'.'|sed s/\\.//g)
        if test $version -ge 25
        then
          SHOW_DETAILS="--test-show-details=streaming"
        fi
        echo
        run_verbose_errexit $SDIST_CABALCMD v2-test \
          --with-compiler "$COMPILER_EXE_PATH" \
          $SHOW_DETAILS $GHCJS_FLAG $CABAL_BUILD_OPTIONS $CABAL_TEST_OPTIONS $CABAL_BUILD_TARGETS
      fi ;;
  esac
}

dist_checks() {
  case "$BUILD" in
    stack) run_verbose $SDIST_STACKCMD sdist $SDIST_OPTIONS ;;
    cabal-v2)
      echo
      if test -n "$CABAL_CHECK_RELAX"
      then
        run_verbose $CABAL_BINARY_NAME check || true
      else
        run_verbose $CABAL_BINARY_NAME check || \
          die "Use CABAL_CHECK_RELAX=y to ignore this error"
      fi
      run_verbose $SDIST_CABALCMD v2-sdist $CABAL_BUILD_TARGETS $SDIST_OPTIONS
      ;;
  esac
}

# XXX The downloading code is common among different tools (e.g. ghcup).
# we can write a common function to do it.
#
# hlint install from Neil Mitchell's github repo, script taken from
# https://raw.githubusercontent.com/ndmitchell/neil/master/misc/run.sh
install_hlint() {
  show_step "Installing hlint version $HLINT_VERSION"

  case "$(uname)" in
      "Darwin")
          OS=osx;;
      MINGW*|MSYS*)
          OS=windows;;
      Linux)
          OS=linux;;
      *) die "install_hlint: unknown os";;
  esac

  if [ "$OS" = "windows" ]; then
      EXT=.zip
      ESCEXT=\.zip
  else
      EXT=.tar.gz
      ESCEXT=\.tar\.gz
  fi

  local PACKAGE=hlint
  VERSION=$HLINT_VERSION

  URL="$HLINT_URL_PREFIX/download/v$VERSION/hlint-$VERSION-x86_64-$OS$EXT"
  TEMP=$(mktemp -d .$PACKAGE-XXXXXX)

  cleanup(){
      rm -r $TEMP
  }
  trap cleanup EXIT

  echo $URL
  retry_cmd curl --fail --progress-bar --location -o$TEMP/$PACKAGE$EXT $URL
  if test "$?" -ne 0
  then
    die "Failed to download $URL"
  fi

  if test -e "$HLINT_PATH"
  then
    die "$HLINT_PATH already exists, not overwriting."
  fi

  mkdir -p ${LOCAL_BIN}
  if [ "$OS" = "windows" ]; then
      7z x -y $TEMP/$PACKAGE$EXT -o${TEMP} hlint-$VERSION/hlint.exe > /dev/null
      mv ${TEMP}/hlint-${VERSION}/hlint.exe ${LOCAL_BIN}
  elif [ "$OS" = "osx" ]; then
      tar -xzvf $TEMP/$PACKAGE$EXT -C${TEMP} --include '*/hlint'
      mv ${TEMP}/hlint-${VERSION}/hlint ${LOCAL_BIN}
  else
      tar -xzvf $TEMP/$PACKAGE$EXT -C${TEMP} --wildcards '*/hlint'
      mv ${TEMP}/hlint-${VERSION}/hlint ${LOCAL_BIN}
  fi
  chmod +x $HLINT_PATH
}

build_hlint() {
  show_step "Installing hlint..."
  if test -n "$(need_stack)"
  then
    ensure_stack ${LOCAL_BIN}
    ensure_ghc
    stack_install_tool hlint
  else
    ensure_cabal ${LOCAL_BIN}
    ensure_cabal_config
    ensure_ghc
    case "$BUILD" in
      cabal-v2) run_verbose_errexit $CABALCMD v2-install hlint ;;
      *) echo "Bug: unknown build type: $BUILD" ;;
    esac
  fi
}

run_hlint() {
  show_step "Running hlint ..."

  # Old method
  if test -n "$HLINT_COMMANDS"
  then
    echo "DEPRECATED env var HLINT_COMMANDS! Please use HLINT_OPTIONS and HLINT_TARGETS instead."
    run_verbose_errexit "$HLINT_COMMANDS"
  elif test -f .hlint.ignore
  then
    # Check if all files mentioned in .hlint.ignore exist.
    local hi_files_exist=""
    local hi_files_n_exist=""
    while read p; do
        if test -f "$p"
        then
            hi_files_exist="$p\n$hi_files_exist"
        else
            if test -n "$p"
            then
                hi_files_n_exist="$p\n$hi_files_n_exist"
            fi
        fi
    done <.hlint.ignore
    if test -n "$hi_files_n_exist"
    then
        echo "WARNING: The following files don't exist but are mentioned in \
your .hlint.ignore file."
        printf "$hi_files_n_exist"
    fi

    local files
    local found
    local i
    local target
    for target in $HLINT_TARGETS
    do
      files=$(find $target -name "*.hs")
      for i in $files
      do
        # XXX ignore whitespace at the end
        found=$(grep "^$i$" .hlint.ignore) || true
        if test -z "$found"
        then
          run_verbose_errexit "hlint $HLINT_OPTIONS $i"
        fi
      done
    done
  else
    local target
    for target in $HLINT_TARGETS
    do
      run_verbose_errexit "hlint $HLINT_OPTIONS $target"
    done
  fi
}

install_docspec() {
  show_step "Installing docspec from $DOCSPEC_URL"
  case "$(uname)" in
      Linux)
          OS=linux;;
      *) die "install_docspec: unknown or unsupported os";;
  esac

  TEMP=$(mktemp -d docspec-XXXXXX)
  cleanup(){
      rm -r $TEMP
  }
  trap cleanup EXIT

  # docspec does not have consistent release naming, therefore, use URL
  # directly instead of version.
  #URL="$DOCSPEC_URL_PREFIX/download/cabal-docspec-$DOCSPEC_VERSION/cabal-docspec-$DOCSPEC_VERSION-x86_64-$OS.xz"
  #echo "Downloading $URL ..."
  retry_cmd curl --fail --progress-bar --location -o$TEMP/cabal-docspec.xz $DOCSPEC_URL
  if test "$?" -ne 0
  then
    rm -f $TEMP/cabal-docspec.xz
    die "Failed to download $DOCSPEC_URL"
  fi

  if test -e "$DOCSPEC_PATH"
  then
    die "$DOCSPEC_PATH already exists, not overwriting."
  else
    mkdir -p $(dirname ${DOCSPEC_PATH})
    xz -d < $TEMP/cabal-docspec.xz > $DOCSPEC_PATH
    rm -f $TEMP/cabal-docspec.xz
    chmod +x $DOCSPEC_PATH
  fi
}

# We run it only after a stack or cabal build so we are sure that stack or
# cabal are already installed.
coveralls_io() {
  if test -z "$(which_cmd hpc-coveralls)"
  then
    if test "$BUILD" = stack
    then
      stack_install_tool hpc-coveralls
    else
      case "$BUILD" in
        cabal-v2) run_verbose_errexit $CABALCMD v2-install hpc-coveralls ;;
        *) echo "Bug: unknown build type: $BUILD" ;;
      esac
    fi
  fi
  run_verbose_errexit hpc-coveralls $COVERALLS_OPTIONS
}

determine_package_full_name() {
  PACKAGE_FULL_NAME=$(get_pkg_full_name) || die "PACKAGE_FULL_NAME"
  echo "Package name and version: [$PACKAGE_FULL_NAME]"
}

# stack or cabal build (i.e. not hlint)
build_compile () {
  # ---------Install any tools needed--------
  show_step "Check and install build tools"

  test -z "$(need_stack)" \
    || ensure_stack ${LOCAL_BIN} \
    && echo
  # The tar installed by pacman does not seem to work. Maybe we need to have it
  # packed with msys itself.
  # ensure_msys_tools "tar" && require_cmd tar

  # This may use STACKCMD so happens after stack install
  determine_build_type && echo
  ensure_ghc && echo
  dont_need_cabal 'verbose' || ensure_cabal ${LOCAL_BIN}

  # use the stack installed 7z instead. depends on ensure ghc where we setup
  # stack paths.
  [[ `uname` != MINGW* ]] || require_cmd 7z

  show_step "Effective build config"
  show_build_config

  # ---------Create dist, unpack, install deps, test--------
  show_step "Build tools: package level and global configuration"
  dont_need_cabal || ensure_cabal_project
  dont_need_cabal || ensure_cabal_config
  if test -z "$DISABLE_SDIST_BUILD"
  then
    determine_package_full_name
  fi
  test -z "$(need_stack)" || ensure_stack_yaml

  # Note: from here on we should be using SDIST_CABALCMD or SDIST_STACKCMD to
  # invoke cabal or stack unless we want to use the command without
  # cabal-project or stack.yaml argument in which case CABALCMD or STACKCMD
  # should be used.
  if test -z "$DISABLE_SDIST_BUILD"
  then
    if test -f "./configure.ac"
    then
      echo "Package contains a 'configure.ac' file."
      echo "Checking for availability of 'autoreconf' command to regenerate "
      echo "the configure script before testing. Use DISABLE_SDIST_BUILD "
      echo "option if you do not have 'autoreconf' available"
      require_cmd autoreconf
    fi
    # Note this function leaves us in the package dir unpacked from sdist
    show_step "Prepare to build from source distribution"
    create_and_unpack_pkg_dist $PACKAGE_FULL_NAME
  fi

  show_step "Install package dependencies"
  install_deps

  show_step "Build and test"
  build_and_test

  if test -n "$COVERALLS_OPTIONS"
  then
      show_step "Send coverage info to coveralls.io"
      coveralls_io
  fi

  if test -z "$DISABLE_DIST_CHECKS"
  then
    show_step "Package distribution checks"
    dist_checks || die "Use DISABLE_DIST_CHECKS=y to disable this check"
  fi
}

eval_env() {
  while test -n "$1"
  do
    case "$1" in
      (*=*)
        key=${1%%=*}
        val=${1#*=}
        find_var $key "$ENVVARS $ALLOW_ENVVARS" || error_clean_param "$key"
        eval "$key=\"$val\""
        ;;
      (*)
        die "Expecting key=value pair, got [$1]"
        ;;
    esac
    shift
  done
}

# $1: prompt
get_confirmation()
{
  echo -n "$1"
  read -p "(y/n) :" ANSWER
  echo

  if test $ANSWER != "y"
  then
    echo "Aborted."
    exit
  fi
}

get_sys_time() {
  local os=$(uname)
  case "$os" in
    # Do not use floating point values so that we can avoid using bc for
    # computations.
    #Linux | MINGW*) date +%s.%N ;;
    Linux | FreeBSD | MINGW*) date +%s ;;
    Darwin) date +%s ;;
    *) die "Unknown OS [$os]" ;;
  esac
}

get_rel_time() {
  echo $((`get_sys_time` - ${BASE_TIME}))
}

#------------------------------------------------------------------------------
# Main flow of script starts here
#------------------------------------------------------------------------------

# This leads to unexpected and undebuggable behavior especially when shell
# functions are returning non-zero exit codes.
#set -e
set -o pipefail
test -n "$BASE_TIME" || BASE_TIME=$(get_sys_time)

test -n "$1" \
    || { short_help; echo -e "\nTry --help for detailed help"; exit 1; }

#------------------------------------------------------------------------------

# Need these to produce help
# Determine home independent of the environment
export HOME=$(echo ~)
set_os_specific_vars # depends on HOME

LOCAL_BIN=$OS_APP_HOME/$OS_LOCAL_DIR/bin
HLINT_PATH="${LOCAL_BIN}/hlint"
HLINT_URL_PREFIX="https://github.com/ndmitchell/hlint/releases"

DOCSPEC_PATH="${LOCAL_BIN}/cabal-docspec"
DOCSPEC_URL_PREFIX="https://github.com/phadej/cabal-extras/releases/"

# XXX On windows this should be ghcup instead of .ghcup? See
# set_os_specific_vars
GHCUP_BIN="${OS_APP_HOME}/.ghcup/bin"
GHCUP_PATH="${GHCUP_BIN}/ghcup"
GHCUP_URL_PREFIX="https://downloads.haskell.org/~ghcup"

#------------------------------------------------------------------------------

case $1 in
  cabal) shift; eval_env "$@"; BUILD=cabal-v2;;
  cabal-v1) die "cabal-v1 is not supported, please use cabal-v2 instead";;
  cabal-new) die "cabal-new is not supported, please use cabal-v2 instead";;
  cabal-v2) shift; eval_env "$@"; BUILD=cabal-v2;;
  stack) shift; eval_env "$@"; BUILD=stack;;
  hlint) shift; eval_env "$@"; BUILD=hlint;;
  clean) rm -rf .packcheck; exit;;
  cleanall)
    rm -rf .packcheck .stack-work
    exit;;
  -h | --help | help) show_help; exit;;
  --version) show_version; exit;;
  *) echo -e "Error: First argument must be a command\n"
    short_help; exit 1 ;;
esac

test -z "$CHECK_ENV" || check_boolean_var CHECK_ENV
test -z "$CHECK_ENV" || check_clean_env

echo
bash --version

show_step "Build command"
show_build_command

TOOLS="awk cat curl cut date env head mkdir printf rm sleep tr which sort \
$OS_HAS_TOOLS"

show_step "Check basic tools"
require_cmd bash
for i in $TOOLS; do require_cmd $i; done

show_step "Build host machine information"
show_machine_info

# ---------Show, process and verify the config------------
show_step "Build environment"
show_build_env

# Set path for installed utilities, e.g. stack, cabal, hpc-coveralls
echo
echo "Original PATH is [$PATH]..."
if test "$BUILD" = "cabal-v2"
then
    export PATH=$OS_APP_HOME/$OS_CABAL_DIR/bin:$PATH
fi
export PATH=$LOCAL_BIN:$PATH
echo
echo "Added $LOCAL_BIN to PATH [$PATH]"

# if we are running from a stack environment remove GHC_PACKAGE_PATH so that
# cabal does not complain
# XXX this should be done from outside via env
unset GHC_PACKAGE_PATH
# stack does not work well with empty STACK_YAML env var
test -n "$STACK_YAML" || unset STACK_YAML

CABAL_BINARY_NAME=cabal
if test "$BUILD" = "hlint"
then
    hlint_path=$(which_cmd hlint)
    if test -n "$hlint_path"
    then
      echo
      echo "WARNING! Using hlint in PATH at $hlint_path"
    elif test -n "$HLINT_VERSION"
    then
      install_hlint
    elif test -n "$HLINT_BUILD"
    then
      # XXX This is broken as it expects BUILD to be either stack or cabal.
      # Also, we need to initialize COMPILER variable before this which is
      # done in verify_build_config happening after this.
      build_hlint
    else
        echo "hlint not found."
        die "Use HLINT_VERSION option to install."
    fi
    run_verbose_errexit hlint --version
    run_hlint
else
    verify_build_config
    build_compile
    if test -n "$ENABLE_DOCSPEC"
    then
      if test $BUILD = "cabal-v2"
      then
          docspec_path=$(which_cmd cabal-docspec)
          if test -n "$docspec_path"
          then
            echo "WARNING! Using cabal-docspec in PATH at $docspec_path"
          elif test -n "$DOCSPEC_URL"
          then
            install_docspec
          else
            echo "cabal-docspec not found."
            die "Use DOCSPEC_URL option to install."
          fi
      fi
      ensure_ghc
      run_verbose_errexit cabal-docspec --version
      run_verbose_errexit cabal-docspec $DOCSPEC_OPTIONS \
          --with-compiler "$COMPILER_EXE_PATH"
    fi
fi

show_step "Done"
