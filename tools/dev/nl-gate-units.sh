#!/bin/sh
# Compile (or read from CACHE) the first MAX units of the real reader build and
# record "KIND NAME KEY PAYLOAD-SHA256 CLOSURE" rows in OUTDIR/units.tsv.
# usage: nl-gate-units.sh TREE OUTDIR MAX [CACHE]   env: extra NAME=VALUE pairs in GU_ENV
# Same scrubbed env as nl-unit-probe.sh.  Stops after MAX rows; never links or bakes.
tree=$1; out=$2; max=$3; cache=${4:-$out/cache}; S=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$out" "$cache" "$out/home" "$out/tmp"; rm -f "$out/units.tsv"
cd "$tree" || exit 2
ulimit -v 8388608
# shellcheck disable=SC2086
exec timeout "${GU_TIMEOUT:-900}" taskset -c 9,25 env -i PATH=/usr/local/bin:/usr/bin:/bin HOME="$out/home" LANG=C.UTF-8 LC_ALL=C.UTF-8 TZ=UTC TMPDIR="$out/tmp" \
  NELISP_REPRODUCIBLE_BUILD=1 NELISP_VERSION=nl-repro NELISP_SOURCE_REVISION=0000000000000000000000000000000000000000 \
  NELISP_STANDALONE_UNIT_CACHE_DIR="$cache" NELISP_STANDALONE_UNIT_CACHE_PRUNE=0 NELISP_STANDALONE_UNIT_CACHE_NOPRUNE=1 NELISP_STANDALONE_JOBS=1 \
  NELISP_STANDALONE_PRELUDE_DISK_CACHE=0 ${GU_ENV:-} \
  GU_OUT="$out/units.tsv" GU_MAX="$max" \
  /usr/local/bin/emacs-31.1 --batch -Q -L lisp -L src -L scripts --eval '(setq load-prefer-newer t)' \
  -l nelisp-standalone-build -l "$S/nl-gate-units.el" -f nelisp-standalone-build-reader </dev/null >"$out/run.log" 2>&1
