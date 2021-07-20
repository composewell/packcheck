#!/usr/bin/env bash

PACKCHECK_DIR="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
PACKCHECK_EXE="$PACKCHECK_DIR/packcheck.sh"
PWD="$(pwd)"
DEFAULT_DIRECTORY_NAME="packcheck-remote-work"
DEFAULT_DIRECTORY="$PWD/$DEFAULT_DIRECTORY_NAME"

# $1: msg
show_step() {
  echo
  echo "--------------------------------------------------"
  echo "$1"
  echo "--------------------------------------------------"
}

which_cmd() {
  hash -r && type -P "$1" || true
}

# $1: executable (eg. git)
require_cmd () {
    local cmd_path
    cmd_path=$(which_cmd "$1")
    if test -z "$cmd_path"
    then
        echo "Required command [$1] not found in PATH [$PATH]."
        exit 1
    else
        echo "Using [$1] at [$cmd_path]"
    fi
}

# $1: command
function run_verbose() {
    echo "$1"
    bash -c "$1"
}

# $1: Remote repository
# $2: Revision to checkout
# $3: Revision to merge into $2
# $4: Directory to clone into
# $5: Force mode
# $6: Packcheck cli options
try_git_clone_and_merge() {

    # Check for prerequisites
    require_cmd git

    local remote="$1"
    local rev="$2"
    local merge="$3"
    local dir="$4"
    local force_mode="$5"
    local packcheck_cli_opts="$6"

    if test -z "$dir"
    then
        dir="$DEFAULT_DIRECTORY"
        echo "No directory specified to clone to."
        echo "Using $dir"
    fi

    if test -z "$remote"
    then
        echo "No remote repository provided."
        echo "Skipping cloning."
        return
    fi

    if test -z "$rev"
    then
        echo "No revision given."
        echo "Defaulting to 'origin/master'."
        rev="origin/master"
    fi

    if test -d "$dir"
    then
        echo "$dir already exists"

        if [ "$force_mode" == "true" ];
        then
            echo "Forcing the deletion of the directory."
            echo "Removing $dir"
            rm -rf "$dir" || exit 1
        else
            echo "Set the script to force mode to force the deletion."
            return
        fi
    fi

    mkdir -p "$dir" || exit 1

    run_verbose "git clone $remote $dir" || exit 1

    cd "$dir" || exit 1
    run_verbose "git branch $rev" || exit 1

    if test -n "$merge"
    then
        # This will fail if there are any merge conflicts
        run_verbose "git merge -X theirs $merge --no-edit --commit" || exit 1
    fi

    show_step "Running packcheck"
    # Run packcheck here
    run_verbose "$PACKCHECK_EXE $packcheck_cli_opts"
}

# Arguments for the command line
FORCE="false"
REMOTE=""
CHECKOUT=""
MERGE=""
DIRECTORY=""
PACKCHECK_CLI_OPTS_ARR=()
PACKCHECK_CLI_OPTS=""

function run_help() {
    local script
    script=$(basename "$0")

    echo
    echo "USAGE: $script --remote=url"
    echo "               [--force]"
    echo "               [--checkout=base_branch] [--merge=merge_branch]"
    echo "               [--directory=path]"
    echo "               -- [...packcheck_arguments]"
    echo
    echo "-f (or) --force: Puts the script in force mode. In this mode, the \
script forcefully replaces existing work created by packcheck and friends."
    echo "-h (or) --help:  Print help."
    echo "--remote:        Repository to clone."
    echo "--checkout:      Revision to checkout. Defaults to 'origin/master'."
    echo "--merge:         Revision to merge in the checked out branch."
    echo "--directory:     Directory to clone the repository into."
    echo "                 Defaults to '$DEFAULT_DIRECTORY_NAME' under the present working directory."
    echo
    echo "All the arguments after '--' are passed to packcheck"
    echo

    echo
    echo "EXAMPLE:"
    echo
    echo "$script --force \\"
    echo "        --remote=https://github.com/user/repo \\"
    echo "        --checkout=origin/master \\"
    echo "        --merge=origin/branch \\"
    echo "        --directory=./repo.packcheck \\"
    echo "        -- cabal-v2 GHCVER=8.8.3"
    echo
}

# Program entry point
for i in "$@"
do
    case $i in
        -f|--force)
            FORCE="true"
            shift
            ;;
        -h|--help)
            run_help
            exit 0
            ;;
        --remote=*)
            REMOTE="${i#*=}"
            shift
            ;;
        --checkout=*)
            CHECKOUT="${i#*=}"
            shift
            ;;
        --merge=*)
            MERGE="${i#*=}"
            shift
            ;;
        --directory=*)
            DIRECTORY="${i#*=}"
            shift
            ;;
        --)
            shift
            PACKCHECK_CLI_OPTS_ARR=("$@")
            break
            ;;
        *)
            echo "Unknown argument to packcheck-remote"
            run_help
            exit 1
            ;;
    esac
done

if test -z "$REMOTE"
then
    run_help
    exit 1
fi

for i in "${PACKCHECK_CLI_OPTS_ARR[@]}"
do
    case $i in
        *=*)
            key=${1%%=*}
            val=${1#*=}
            PACKCHECK_CLI_OPTS="$PACKCHECK_CLI_OPTS $key=\"$val\""
            shift
            ;;
        *)
            PACKCHECK_CLI_OPTS="$PACKCHECK_CLI_OPTS $i"
            shift
            ;;
    esac
done

try_git_clone_and_merge "$REMOTE" "$CHECKOUT" "$MERGE" "$DIRECTORY" "$FORCE" "$PACKCHECK_CLI_OPTS"
