#!/usr/bin/env bash
# Multi-process parallel build of the standalone NeLisp eval binary.
#
# Spawns N `emacs --batch' workers, each compiling a round-robin slice of the
# cacheable units into target/standalone-units/ (per-unit .unit files -> no
# write contention), then runs the serial link step (all units cached).
#
# Usage: tools/build-standalone-parallel.sh [NJOBS]   (default: nproc)
#
# NOTE (measured): for the current 37-unit set this is SLOWER than the serial
# `make standalone-eval' -- per-unit compilation is ~0.4s total while each
# worker pays ~4s of emacs startup + module load.  The build is startup-bound,
# not compute-bound.  This script pays off only once per-unit compilation
# dominates startup (many more / much heavier units).  See README / the
# nelisp-standalone-compile-chunk docstring.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
JOBS="${1:-$(nproc 2>/dev/null || echo 4)}"
EMACS="${EMACS:-emacs}"
cd "$ROOT"

echo "[parallel] compiling units with ${JOBS} worker(s)..."
pids=()
for i in $(seq 0 $((JOBS - 1))); do
  NELISP_CHUNK_IDX="$i" NELISP_CHUNK_N="$JOBS" \
    "$EMACS" --batch -Q -L lisp -L src -L scripts \
      --eval '(setq load-prefer-newer t)' \
      -l nelisp-standalone-build -f nelisp-standalone-compile-chunk &
  pids+=("$!")
done

fail=0
for p in "${pids[@]}"; do wait "$p" || fail=1; done
if [ "$fail" != 0 ]; then
  echo "[parallel] a worker failed" >&2
  exit 1
fi

echo "[parallel] linking (serial)..."
"$EMACS" --batch -Q -L lisp -L src -L scripts \
  --eval '(setq load-prefer-newer t)' \
  -l nelisp-standalone-build -f nelisp-standalone-build
