#!/bin/sh
# usage: census.sh BIN [OUT]
exec python3 "$(dirname "$0")/census.py" "$@"
