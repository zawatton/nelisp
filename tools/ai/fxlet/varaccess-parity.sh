#!/bin/sh
# Run the varaccess lane's variable-semantics corpus (tools/ai/varaccess/, in tree since v35) on BIN.
cd "$(dirname "$0")" || exit 1
BIN=$1
case "$BIN" in /*) ;; *) BIN=$(cd ../../.. && realpath "$BIN") ;; esac
exec sh ../varaccess/parity.sh "$BIN"
