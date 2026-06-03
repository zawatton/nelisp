#!/usr/bin/env bash
# Cross-PC verification script for NeLisp — Linux / macOS
# Usage: bash scripts/verify-cross-platform.sh [--parallel-jobs N] [--skip-native-smokes] [--include-tarball]
# Expected: last line = "=== Cross-platform verify PASS ==="
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"
EMACS="${EMACS:-emacs}"
PARALLEL_JOBS=0
SKIP_NATIVE_SMOKES=0
INCLUDE_TARBALL=0

default_parallel_jobs() {
  local cpus=2
  if command -v nproc >/dev/null 2>&1; then
    cpus="$(nproc)"
  elif [ "$(uname -s)" = "Darwin" ]; then
    cpus="$(sysctl -n hw.ncpu 2>/dev/null || echo 2)"
  fi
  if [ "$cpus" -lt 2 ]; then
    echo 1
  else
    echo 2
  fi
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --emacs) EMACS="$2"; shift 2 ;;
    --parallel-jobs|--jobs|-j) PARALLEL_JOBS="$2"; shift 2 ;;
    --skip-native-smokes) SKIP_NATIVE_SMOKES=1; shift ;;
    --include-tarball) INCLUDE_TARBALL=1; shift ;;
    -h|--help)
      echo "usage: $0 [--emacs EMACS] [--parallel-jobs N] [--skip-native-smokes] [--include-tarball]"
      exit 0
      ;;
    *) echo "usage: $0 [--emacs EMACS] [--parallel-jobs N] [--skip-native-smokes] [--include-tarball]" >&2; exit 2 ;;
  esac
done

case "$PARALLEL_JOBS" in
  ''|*[!0-9]*)
    echo "verify-cross-platform: --parallel-jobs must be a non-negative integer: $PARALLEL_JOBS" >&2
    exit 2
    ;;
esac

if [ "$PARALLEL_JOBS" -le 0 ]; then
  PARALLEL_JOBS="$(default_parallel_jobs)"
fi

echo "--- Platform info ---"
uname -a
"$EMACS" --version | head -1

if [ "$(uname -s)" = "Darwin" ]; then
  echo ""
  echo "--- make compile (byte-compile elisp) ---"
  make EMACS="$EMACS" compile 2>&1 | tail -5

  if [ "$SKIP_NATIVE_SMOKES" -eq 0 ]; then
    echo ""
    echo "--- macOS arm64 Mach-O self-host smoke ---"
    tools/macos-selfhost-test.sh --emacs "$EMACS"

    echo ""
    echo "--- macOS OS compatibility ERT smoke ---"
    tools/macos-os-compat-test.sh --emacs "$EMACS"
  fi

  echo ""
  echo "--- macOS standalone parallel build (zero-Rust) ---"
  tools/build-standalone-parallel.sh --jobs "$PARALLEL_JOBS" --target macos-aarch64 --emacs "$EMACS"

  echo ""
  echo "--- macOS standalone eval native smoke ---"
  tools/macos-standalone-eval-test.sh --emacs "$EMACS"

  echo ""
  echo "--- macOS standalone cache identity smoke ---"
  tools/macos-standalone-cache-identity-test.sh --emacs "$EMACS"

  echo ""
  echo "--- macOS standalone reader native smoke ---"
  tools/macos-standalone-reader-test.sh --emacs "$EMACS"

  if [ "$INCLUDE_TARBALL" -eq 1 ]; then
    echo ""
    echo "--- macOS standalone tarball smoke ---"
    tools/build-standalone-tarball.sh stage-d-v3.0 macos-aarch64
    tools/verify-standalone-tarball.sh stage-d-v3.0 macos-aarch64
  fi

  echo ""
  echo "=== Cross-platform verify PASS ==="
  exit 0
fi

echo ""
echo "--- make compile (byte-compile elisp) ---"
make EMACS="$EMACS" compile 2>&1 | tail -5

if [ "$SKIP_NATIVE_SMOKES" -eq 0 ]; then
  echo ""
  echo "--- Linux OS compatibility ERT smoke ---"
  tools/linux-os-compat-test.sh --emacs "$EMACS"

  echo ""
  echo "--- Linux x86_64 ELF self-host smoke ---"
  tools/selfhost-test.sh --emacs "$EMACS"
fi

echo ""
echo "--- Linux standalone parallel build (zero-Rust) ---"
tools/build-standalone-parallel.sh --jobs "$PARALLEL_JOBS" --target linux-x86_64 --emacs "$EMACS"

echo ""
echo "--- Linux standalone eval native smoke ---"
tools/linux-standalone-eval-test.sh --emacs "$EMACS"

echo ""
echo "--- Linux standalone cache identity smoke ---"
tools/linux-standalone-cache-identity-test.sh --emacs "$EMACS"

echo ""
echo "--- Linux standalone reader native smoke ---"
tools/linux-standalone-reader-test.sh --emacs "$EMACS"

if [ "$INCLUDE_TARBALL" -eq 1 ]; then
  echo ""
  echo "--- Linux standalone tarball smoke ---"
  tools/build-standalone-tarball.sh stage-d-v3.0 linux-x86_64
  tools/verify-standalone-tarball.sh stage-d-v3.0 linux-x86_64
fi

echo ""
echo "=== Cross-platform verify PASS ==="
