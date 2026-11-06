#!/usr/bin/env bash
# Run tools/ai/gc-visibility-matrix.el on a standalone binary.
root=$(cd "$(dirname "$0")/../.." && pwd)
bin=${1:-$root/target/nelisp}
tmp=$(mktemp -d) || exit 3
trap 'rm -rf "$tmp"' EXIT
{ echo "(load \"$root/lisp/nelisp-native-load.el\")"
  echo "(load \"$root/tools/ai/gc-visibility-matrix.el\")"; } > "$tmp/run.el"
(ulimit -v 8388608; cd "$tmp" && timeout 300 "$bin" --load "$tmp/run.el")
