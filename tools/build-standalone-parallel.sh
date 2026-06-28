#!/usr/bin/env bash
# Multi-process parallel build of the standalone NeLisp eval binary.
#
# Spawns N `emacs --batch' workers, each compiling a round-robin slice of the
# cacheable units into target/standalone-units/ (per-unit .unit files -> no
# write contention), then runs the serial link step (all units cached).
#
# Usage:
#   tools/build-standalone-parallel.sh [NJOBS]
#   tools/build-standalone-parallel.sh --jobs 4 --target macos-aarch64 --clean
#
# TARGET defaults to $NELISP_STANDALONE_TARGET when set, otherwise
# macos-aarch64 on Darwin and linux-x86_64 elsewhere.
# Windows PowerShell counterpart: tools/build-standalone-parallel.ps1
#
# NOTE (measured): for the current 37-unit set this is SLOWER than the serial
# `make standalone-eval' -- per-unit compilation is ~0.4s total while each
# worker pays ~4s of emacs startup + module load.  The build is startup-bound,
# not compute-bound.  This script pays off only once per-unit compilation
# dominates startup (many more / much heavier units).  See README / the
# nelisp-standalone-compile-chunk docstring.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
EMACS="${EMACS:-emacs}"
TARGET="${NELISP_STANDALONE_TARGET:-}"
CLEAN=0
COMPILE_ONLY=0

normalize_target() {
  case "$1" in
    macos-arm64|darwin-arm64|darwin-aarch64) echo "macos-aarch64" ;;
    macos-x64|macos-amd64|darwin-x86_64|darwin-amd64) echo "macos-x86_64" ;;
    linux-arm64) echo "linux-aarch64" ;;
    windows-arm64|win-arm64) echo "windows-aarch64" ;;
    *) echo "$1" ;;
  esac
}

default_jobs() {
  if command -v nproc >/dev/null 2>&1; then
    nproc
  elif [ "$(uname -s)" = "Darwin" ]; then
    sysctl -n hw.ncpu 2>/dev/null || echo 4
  else
    echo 4
  fi
}

if [ -z "$TARGET" ]; then
  if [ "$(uname -s)" = "Darwin" ]; then
    case "$(uname -m)" in
      arm64) TARGET="macos-aarch64" ;;
      x86_64) TARGET="macos-x86_64" ;;
      *) TARGET="macos-aarch64" ;;
    esac
  else
    TARGET="linux-x86_64"
  fi
fi

JOBS="$(default_jobs)"

while [ "$#" -gt 0 ]; do
  case "$1" in
    --jobs|-j) JOBS="$2"; shift 2 ;;
    --target) TARGET="$2"; shift 2 ;;
    --emacs) EMACS="$2"; shift 2 ;;
    --clean) CLEAN=1; shift ;;
    --compile-only) COMPILE_ONLY=1; shift ;;
    -h|--help)
      echo "usage: $0 [NJOBS] [--jobs N] [--target linux-x86_64|linux-aarch64|macos-aarch64|macos-x86_64] [--clean] [--compile-only]"
      exit 0
      ;;
    [0-9]*) JOBS="$1"; shift ;;
    *) echo "usage: $0 [NJOBS] [--jobs N] [--target linux-x86_64|linux-aarch64|macos-aarch64|macos-x86_64] [--clean] [--compile-only]" >&2; exit 2 ;;
  esac
done
TARGET="$(normalize_target "$TARGET")"

case "$JOBS" in
  ''|*[!0-9]*)
    echo "[parallel] FAIL: jobs must be a positive integer: $JOBS" >&2
    exit 2
    ;;
esac

if [ "$JOBS" -le 0 ]; then
  echo "[parallel] FAIL: jobs must be positive: $JOBS" >&2
  exit 2
fi

cd "$ROOT"

if [ "$CLEAN" -eq 1 ]; then
  rm -rf "target/standalone-units/$TARGET"
  if [ "$TARGET" = "windows-x86_64" ]; then
    rm -rf target/standalone-units/windows-x86_64-arena-*
  fi
fi

echo "[parallel] compiling units with ${JOBS} worker(s)..."
echo "[parallel] target ${TARGET}"
pids=()
for i in $(seq 0 $((JOBS - 1))); do
  NELISP_STANDALONE_TARGET="$TARGET" NELISP_CHUNK_IDX="$i" NELISP_CHUNK_N="$JOBS" \
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

if [ "$COMPILE_ONLY" -eq 1 ]; then
  echo "[parallel] PASS: standalone parallel compile completed"
  exit 0
fi

echo "[parallel] linking (serial)..."
NELISP_STANDALONE_TARGET="$TARGET" "$EMACS" --batch -Q -L lisp -L src -L scripts \
  --eval '(setq load-prefer-newer t)' \
  -l nelisp-standalone-build -f nelisp-standalone-build
echo "[parallel] PASS: standalone parallel build completed"
