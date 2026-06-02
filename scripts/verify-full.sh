#!/usr/bin/env bash
# verify-full.sh — zero-Rust pre-commit full verify
#
# Usage:
#   scripts/verify-full.sh             # 全 verify、commit 前 1 回
#
# Run order:
#   1. make standalone-reader-test    (build + reader gate)
#   2. make standalone-selfhost-test  (self-host correctness gate)
#
# 注意: Rust binary (target/release/nelisp) は削除済み。
# cargo build / cargo test / build-tool/ integration tests は廃止。
# 全ゲートは pure-elisp standalone path に移行。

set -e

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

echo "[verify-full] $(date +%H:%M:%S) start"

# 1. standalone reader gate
T0=$(date +%s.%N)
make standalone-reader-test
T1=$(date +%s.%N)
python3 -c "print(f'  standalone-reader-test: {$T1-$T0:.2f}s')"

# 2. standalone self-host gate
T0=$(date +%s.%N)
make standalone-selfhost-test
T1=$(date +%s.%N)
python3 -c "print(f'  standalone-selfhost-test: {$T1-$T0:.2f}s')"

echo "[verify-full] $(date +%H:%M:%S) DONE"
