#!/usr/bin/env bash
# Run tools/ai/gc-outslot-probe.el.  Exit 0: no corruption in the
# return gap; 1: corruption; 2: the positive control saw none (blind).
root=$(cd "$(dirname "$0")/../.." && pwd)
bin=${1:-$root/target/nelisp}
tmp=$(mktemp -d) || exit 3
trap 'rm -rf "$tmp"' EXIT
{ echo "(load \"$root/lisp/nelisp-native-load.el\")"
  echo "(load \"$root/tools/ai/gc-outslot-probe.el\")"; } > "$tmp/run.el"
(ulimit -v 8388608; cd "$tmp" && timeout 300 "$bin" --load "$tmp/run.el")
