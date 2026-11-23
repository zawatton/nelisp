#!/bin/sh
# usage: nl-chunk-probe.sh TREE OUTDIR K CHUNK   -- compile one applyfn chunk alone, record wall time.
tree=$1; out=$2; k=$3; c=$4; S=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$out/cache.$c" "$out/home" "$out/tmp"
cd "$tree" || exit 2
ulimit -v 8388608
exec taskset -c 9,25 timeout 1500 env -i PATH=/usr/local/bin:/usr/bin:/bin HOME="$out/home" LANG=C.UTF-8 LC_ALL=C.UTF-8 TZ=UTC TMPDIR="$out/tmp" \
  NELISP_REPRODUCIBLE_BUILD=1 NELISP_VERSION=nl-repro NELISP_SOURCE_REVISION=0000000000000000000000000000000000000000 \
  NELISP_STANDALONE_UNIT_CACHE_DIR="$out/cache.$c" NELISP_STANDALONE_JOBS=1 \
  NELISP_STANDALONE_APPLYFN_CHUNKS="$k" CHUNK="$c" CHUNK_OUT="$out/chunks.txt" \
  /usr/local/bin/emacs-31.1 --batch -Q -L lisp -L src -L scripts --eval '(setq load-prefer-newer t)' \
  -l "$S/nl-chunk-probe.el" </dev/null >"$out/chunk$c.log" 2>&1
