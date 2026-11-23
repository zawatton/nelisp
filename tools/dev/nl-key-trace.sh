#!/bin/sh
# Records the build-script functions that run after the key for the first MAX units.
# usage: nl-key-trace.sh TREE OUTDIR MAX   (writes OUTDIR/trace.txt; env -i, own empty cache)
tree=$1; out=$2; max=$3; S=$(cd "$(dirname "$0")" && pwd)
rm -rf "$out"; mkdir -p "$out" "$out/cache" "$out/home" "$out/tmp"
cd "$tree" || exit 2
ulimit -v 8388608
exec timeout 600 taskset -c 9,25 env -i PATH=/usr/local/bin:/usr/bin:/bin HOME="$out/home" LANG=C.UTF-8 LC_ALL=C.UTF-8 TZ=UTC TMPDIR="$out/tmp" \
  NELISP_REPRODUCIBLE_BUILD=1 NELISP_VERSION=nl-repro NELISP_SOURCE_REVISION=0000000000000000000000000000000000000000 \
  NELISP_STANDALONE_UNIT_CACHE_DIR="$out/cache" NELISP_STANDALONE_UNIT_CACHE_PRUNE=1 NELISP_STANDALONE_JOBS=1 \
  KT_OUT="$out/trace.txt" KT_MAX="$max" \
  /usr/local/bin/emacs-31.1 --batch -Q -L lisp -L src -L scripts --eval '(setq load-prefer-newer t)' \
  -l nelisp-standalone-build -l "$S/nl-key-trace.el" -f nelisp-standalone-build-reader </dev/null >"$out/run.log" 2>&1
