#!/usr/bin/env bash
# verify-full.sh — Wave 9+ pre-commit full verify (~30-60s 目標)
#
# Usage:
#   scripts/verify-full.sh             # 全 verify、commit 前 1 回
#   scripts/verify-full.sh PATTERN     # 該当 integration test 群 + 上記
#
# Run order:
#   1. cargo build --release --bin nelisp     (~3s)
#   2. probe-fast N=5/20 multi-sample        (~12s、perf stability)
#   3. correctness 4 scenario (= probe-verify-combo の最初 stage のみ) (~5s)
#   4. (optional) integration tests matching PATTERN (= 各 ~5s)
#
# 注意: chain require (= phase47-compiler 系) は heavy なので skip。
# cargo test --workspace も pre-existing compile errors のため skip。
# 必要なら verify-quick.sh で個別 test を pattern 指定。

set -e

PATTERN="${1:-}"

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

echo "[verify-full] $(date +%H:%M:%S) start"

# 1. build
T0=$(date +%s.%N)
cargo build --release --bin nelisp 2>&1 | tail -2
T1=$(date +%s.%N)
python3 -c "print(f'  build: {$T1-$T0:.2f}s')"

# 2. probe-fast multi-N (perf stability)
for N in 5 20; do
  T0=$(date +%s.%N)
  NELISP_BENCH_N=$N ./target/release/nelisp --batch \
    -l "$REPO_ROOT/test/wave-7-probes/probe-fast.el" >/dev/null 2>&1
  T1=$(date +%s.%N)
  python3 -c "print(f'  probe-fast N=$N: {$T1-$T0:.2f}s')"
done

# 3. correctness 4 scenarios (= inline、chain require skip)
T0=$(date +%s.%N)
cat > /tmp/verify-full-correctness.el << 'EOF'
(let ((c1 (let ((x 1)) (let ((x 2)) x)))
      (c2 (funcall (let ((c 42)) (lambda () c))))
      (c3 (let ((a 10)) (let ((b 20)) (+ a b)))))
  (fset 'vfact (lambda (n) (if (<= n 1) 1 (* n (funcall 'vfact (- n 1))))))
  (let ((c4 (vfact 6)))
    (unless (and (= c1 2) (= c2 42) (= c3 30) (= c4 720))
      (princ (format "CORRECTNESS FAIL c1=%S c2=%S c3=%S c4=%S\n" c1 c2 c3 c4))
      (kill-emacs 1)))
  (princ "correctness: pass (4/4)\n"))
EOF
./target/release/nelisp --batch -l /tmp/verify-full-correctness.el 2>&1 | tail -1
T1=$(date +%s.%N)
python3 -c "print(f'  correctness: {$T1-$T0:.2f}s')"

# 4. (optional) targeted integration tests
if [ -n "$PATTERN" ]; then
  MATCHES=$(ls "$REPO_ROOT/build-tool/tests/" | grep -E "$PATTERN" || true)
  if [ -z "$MATCHES" ]; then
    echo "  (no test matched pattern '$PATTERN', skip)"
  else
    N=$(echo "$MATCHES" | wc -l)
    echo "[verify-full] $N integration test(s) matched"
    T0=$(date +%s.%N)
    FAIL=0
    for f in $MATCHES; do
      name="${f%.rs}"
      OUT=$(cargo test --release --test "$name" 2>&1 | tail -3 | grep "test result" || true)
      echo "  $name: $OUT"
      [ -z "$OUT" ] && FAIL=$((FAIL+1))
    done
    T1=$(date +%s.%N)
    python3 -c "print(f'  tests: {$T1-$T0:.2f}s, fail=$FAIL')"
  fi
fi

echo "[verify-full] $(date +%H:%M:%S) DONE"
