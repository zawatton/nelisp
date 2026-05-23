#!/usr/bin/env bash
# verify-iter.sh — Wave 9+ ultra-fast iteration sanity (~1-2s)
#
# Usage:
#   scripts/verify-iter.sh            # cargo check のみ (= ~1s)
#
# 用途: 「この edit syntax / type が通るか」だけ確認。
# fix の試行錯誤中、各 edit 毎に呼んで構文 sanity 取る。
# perf verify は verify-quick.sh、commit gate は verify-full.sh。

set -e
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

T0=$(date +%s.%N)
cargo check --release --bin nelisp 2>&1 | tail -2
T1=$(date +%s.%N)
python3 -c "print(f'  check: {$T1-$T0:.2f}s')"
