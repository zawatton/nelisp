#!/usr/bin/env bash
# Run tools/ai/rawptr-gc-probe.el on a standalone binary.
# Exit 0: nothing freed; 1: an object reachable only from raw memory was
# clobbered.  NATIVE_LOAD=FILE loads another nelisp-native-load.el (for
# the negative control: the file as of origin/main).
root=$(cd "$(dirname "$0")/../.." && pwd)
bin=${1:-$root/target/nelisp}
tmp=$(mktemp -d) || exit 3
trap 'rm -rf "$tmp"' EXIT
{ echo "(load \"${NATIVE_LOAD:-$root/lisp/nelisp-native-load.el}\")"
  echo "(load \"$root/tools/ai/rawptr-gc-probe.el\")"; } > "$tmp/run.el"
(ulimit -v 8388608; cd "$tmp" && timeout 300 "$bin" --load "$tmp/run.el")
