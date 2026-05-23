#!/usr/bin/env bash
# verify-quick.sh — Wave 9+ fast iteration verify (~15s total)
#
# Usage:
#   scripts/verify-quick.sh                          # = build + probe-fast のみ (= ~10s)
#   scripts/verify-quick.sh mirror                   # + 最初の mirror テスト 1 つ (= ~15s)
#   scripts/verify-quick.sh frame_stack_find_in_frame # + 完全一致 1 test (~10s)
#
# Run order:
#   1. cargo build --release --bin nelisp   (~3s incremental)
#   2. probe-fast (lambda N=5)               (~6s baseline)
#   3. (optional) 1 integration test         (~5s)
#
# 設計: 各 integration test ~5s overhead × 12 tests = 60s で iteration 死亡。
# pattern が指定されたら **最初の match 1 個** だけ実行 (= sanity 用)。
# 全 match 実行は `cargo test --workspace --release` (verify-full.sh) 行き。

set -e

PATTERN="${1:-}"

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

# 1. build
T0=$(date +%s.%N)
cargo build --release --bin nelisp 2>&1 | tail -2
T1=$(date +%s.%N)
python3 -c "print(f'  build: {$T1-$T0:.2f}s')"

# 2. probe-fast
T0=$(date +%s.%N)
NELISP_BENCH_N=5 ./target/release/nelisp --batch \
  -l "$REPO_ROOT/test/wave-7-probes/probe-fast.el" >/dev/null 2>&1
T1=$(date +%s.%N)
python3 -c "print(f'  probe-fast: {$T1-$T0:.2f}s')"

# 3. (optional) 1 matching test
if [ -n "$PATTERN" ]; then
  MATCH=$(ls "$REPO_ROOT/build-tool/tests/" | grep -E "$PATTERN" | head -1 || true)
  if [ -z "$MATCH" ]; then
    echo "  (no test matched pattern '$PATTERN', skip)"
  else
    name="${MATCH%.rs}"
    T0=$(date +%s.%N)
    cargo test --release --test "$name" 2>&1 | tail -3 | grep "test result" || true
    T1=$(date +%s.%N)
    python3 -c "print(f'  $name: {$T1-$T0:.2f}s')"
  fi
fi

echo "[verify-quick] DONE"
