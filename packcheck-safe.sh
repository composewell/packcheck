#!/usr/bin/env bash

PACKCHECK_DIR=$(dirname "$0")
echo "Running ${PACKCHECK_DIR}/packcheck.sh with clean environment and CHECK_ENV on..."
echo "No environment variables are honored, you have to specifiy ALL the"
echo "parameters explicitly on the command line, including PATH."
echo

# Collect options into a string safely
PACKCHECK_CLI_OPTS=""
for i in "$@"
do
    case $i in
        *=*)
            # Split key and value from the current argument 'i'
            key=${i%%=*}
            val=${i#*=}
            # Re-wrap in quotes to handle spaces (e.g., PATH or DOCSPEC_OPTIONS)
            PACKCHECK_CLI_OPTS="$PACKCHECK_CLI_OPTS $key=\"$val\""
            ;;
        *)
            PACKCHECK_CLI_OPTS="$PACKCHECK_CLI_OPTS \"$i\""
            ;;
    esac
done

eval "/usr/bin/env -i CHECK_ENV=y \"$PACKCHECK_DIR/packcheck.sh\" $PACKCHECK_CLI_OPTS"
