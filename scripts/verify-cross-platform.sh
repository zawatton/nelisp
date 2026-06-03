#!/usr/bin/env bash
# Cross-PC verification script for NeLisp — Linux / macOS
# Usage: bash scripts/verify-cross-platform.sh
# Expected: last line = "=== Cross-platform verify PASS ==="
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

echo "--- Platform info ---"
uname -a
emacs --version | head -1

if [ "$(uname -s)" = "Darwin" ]; then
  echo ""
  echo "--- make compile (byte-compile elisp) ---"
  make compile 2>&1 | tail -5

  echo ""
  echo "--- macOS arm64 Mach-O self-host smoke ---"
  tools/macos-selfhost-test.sh

  echo ""
  echo "--- macOS standalone eval native smoke ---"
  tools/macos-standalone-eval-test.sh

  echo ""
  echo "--- macOS standalone reader native smoke ---"
  tools/macos-standalone-reader-test.sh

  echo ""
  echo "=== Cross-platform verify PASS ==="
  exit 0
fi

echo ""
echo "--- make compile (byte-compile elisp) ---"
make compile 2>&1 | tail -5

echo ""
echo "--- make standalone-reader-test (zero-Rust standalone gate) ---"
make standalone-reader-test

echo ""
echo "=== Cross-platform verify PASS ==="
