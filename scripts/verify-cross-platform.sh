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

echo ""
echo "--- make compile (byte-compile elisp) ---"
make compile 2>&1 | tail -5

echo ""
echo "--- make standalone-reader-test (zero-Rust standalone gate) ---"
make standalone-reader-test

echo ""
echo "=== Cross-platform verify PASS ==="
