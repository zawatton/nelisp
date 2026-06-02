#!/usr/bin/env bash
# verify-quick.sh — zero-Rust fast iteration verify
#
# Usage:
#   scripts/verify-quick.sh
#
# Run order:
#   1. make standalone-reader-test  (build + exercise the pure-elisp standalone gate)
#
# 設計: Rust binary (target/release/nelisp) は削除済み。
# standalone-reader が唯一の実行可能ゲート。

set -e

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

T0=$(date +%s.%N)
make standalone-reader-test
T1=$(date +%s.%N)
python3 -c "print(f'  standalone-reader-test: {$T1-$T0:.2f}s')"

echo "[verify-quick] DONE"
