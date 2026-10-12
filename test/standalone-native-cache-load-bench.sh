#!/usr/bin/env bash
# Usage: test/standalone-native-cache-load-bench.sh READER [LIMIT]
# Byte-compiles the fixture with host Emacs, compiles it into a fresh
# native cache with READER, then times loading it in a new READER process
# (see test/standalone-native-cache-load-bench.el).
set -u
reader=$1; limit=${2:-}
root=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-ncl.XXXXXX") || exit 1
trap 'rm -rf "$work"' EXIT
cp "$root/test/fixtures/native-cache-load-fns.el" "$work/" || exit 1
emacs -Q --batch -f batch-byte-compile "$work/native-cache-load-fns.el" >/dev/null 2>&1 || exit 1
mkdir -m 700 "$work/cache" || exit 1
cd "$root" || exit 1
NCL_MODE=compile NCL_CACHE="$work/cache" NCL_ELC="$work/native-cache-load-fns.elc" \
  "$reader" -L lisp -L src -L scripts --load test/standalone-native-cache-load-bench.el || exit 1
NCL_MODE=load NCL_CACHE="$work/cache" NCL_ELC="$work/native-cache-load-fns.elc" NCL_LIMIT="$limit" \
  "$reader" -L lisp -L src -L scripts --load test/standalone-native-cache-load-bench.el
