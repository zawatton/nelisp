#!/bin/sh
# Records the first MAX units (dump, state, compiled-unit sha) for A/B comparison of two trees under one env.
# usage: nl-unit-probe.sh TREE OUTDIR MAX
tree=$1; out=$2; max=$3; S=$(cd "$(dirname "$0")" && pwd)
rm -rf "$out"; mkdir -p "$out" "$out/cache" "$out/home" "$out/tmp"
cd "$tree" || exit 2
ulimit -v 8388608
exec timeout 1500 env -i PATH=/usr/local/bin:/usr/bin:/bin HOME="$out/home" LANG=C.UTF-8 LC_ALL=C.UTF-8 TZ=UTC TMPDIR="$out/tmp" \
  NELISP_REPRODUCIBLE_BUILD=1 NELISP_VERSION=nl-repro NELISP_SOURCE_REVISION=0000000000000000000000000000000000000000 \
  NELISP_STANDALONE_UNIT_CACHE_DIR="$out/cache" NELISP_STANDALONE_UNIT_CACHE_PRUNE=1 NELISP_STANDALONE_JOBS=1 \
  NELISP_STANDALONE_UNIT_SOURCE_DUMP="$out/dump" C6_OUT="$out/probe.txt" C6_MAX="$max" \
  /usr/local/bin/emacs-31.1 --batch -Q -L lisp -L src -L scripts --eval '(setq load-prefer-newer t)' \
  -l nelisp-standalone-build -l "$S/nl-unit-probe.el" -f nelisp-standalone-build-reader </dev/null >"$out/run.log" 2>&1
