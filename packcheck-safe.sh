#!/usr/bin/env bash

PACKCHECK_DIR=$(dirname "$0")
echo "Running ${PACKCHECK_DIR}/packcheck.sh with clean environment and CHECK_ENV on..."
echo "No environment variables are honored, you have to specifiy ALL the"
echo "parameters explicitly on the command line, including PATH."
echo

PACKCHECK_CLI_OPTS_ARR=("$@")
PACKCHECK_CLI_OPTS=""

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

eval "/usr/bin/env -i CHECK_ENV=y $PACKCHECK_DIR/packcheck.sh $PACKCHECK_CLI_OPTS"
