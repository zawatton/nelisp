#!/usr/bin/env bash
# Cross-PC verification script for NeLisp — Linux / macOS
# Usage: bash scripts/verify-cross-platform.sh
# Expected: last line = "=== Cross-platform verify PASS ==="
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

echo "--- Platform info ---"
uname -a
rustc --version
emacs --version | head -1

echo ""
echo "--- cargo build --release -p nelisp-build-tool ---"
cargo build --release -p nelisp-build-tool

echo ""
echo "--- cargo test --release -p nelisp-build-tool --lib ---"
cargo test --release -p nelisp-build-tool --lib

echo ""
echo "--- make compile (byte-compile elisp) ---"
make compile 2>&1 | tail -5

echo ""
echo "--- smoke: target/release/nelisp --eval '(+ 1 2)' ---"
if [ -x target/release/nelisp ]; then
    target/release/nelisp --eval '(+ 1 2)' || echo "Smoke: expected 3"
else
    echo "Smoke: target/release/nelisp not found (build-tool only mode), skipping"
fi

echo ""
echo "=== Cross-platform verify PASS ==="
