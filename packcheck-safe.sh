#!/usr/bin/env bash

PACKCHECK_DIR=$(dirname $0)
echo "Running ${PACKCHECK_DIR}/packcheck.sh with clean environment and CHECK_ENV on..."
echo "No environment variables are honored, you have to specifiy ALL the"
echo "parameters explicitly on the command line, including PATH."
echo

/usr/bin/env -i CHECK_ENV=y $(dirname $0)/packcheck.sh $*
