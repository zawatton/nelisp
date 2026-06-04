#!/usr/bin/env bash
# chunk-growth-test.sh — Doc 140 chunked-arena GROWTH pressure test.
#
# The chunk-aware allocator (`nl_alloc_bytes' -> `nl_chunk_alloc_new' ->
# `nl_os_alloc_chunk') is supposed to grow the arena by adding chunks once
# the fixed first chunk is full, instead of depending on an ever-larger
# fixed reservation.  This test PROVES that at runtime: it builds the
# standalone reader with a deliberately tiny first chunk (8 MiB) — smaller
# than the ~9 MiB boot footprint — so boot allocation overflows it and MUST
# grow a second chunk.  It then asserts:
#   * (nelisp--arena-stats) reports chunk-count > 1 (growth happened), and
#   * a value that escapes a `let' body is still correct after growth.
#
# The first-chunk size is overridden via NELISP_LINUX_ARENA_SIZE (bytes).
# Exit 0 on PASS.  Run via `make standalone-chunk-growth-test'.

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"
EMACS="${EMACS:-emacs}"
TINY=$((8 * 1024 * 1024))   # 8 MiB first chunk; boot needs ~9 MiB -> growth

build_reader() {  # $1 = NELISP_LINUX_ARENA_SIZE (empty = default 256 MiB)
  # The first-chunk size is baked into compiled units, and the env var is
  # not a source change, so the unit cache must be dropped to re-bake it.
  rm -rf target/standalone-units >/dev/null 2>&1 || true
  NELISP_LINUX_ARENA_SIZE="${1:-}" "$EMACS" --batch -Q -L lisp -L src -L scripts \
    --eval '(setq load-prefer-newer t)' \
    -l nelisp-standalone-build -f nelisp-standalone-build-reader >/dev/null 2>&1
}

echo "[chunk-growth] building standalone reader with an 8 MiB first chunk ..."
build_reader "$TINY"

printf '(nelisp--arena-stats)\n' > /tmp/nl-chunk-growth-stats.el
STATS="$(./target/nelisp --load /tmp/nl-chunk-growth-stats.el 2>&1 | head -1)"
# tuple: (base size bump used live next free coll reuse CHUNK-COUNT reserved used)
CHUNK_COUNT="$(printf '%s' "$STATS" | sed -E 's/[()]//g' | awk '{print $10}')"
RESERVED="$(printf '%s' "$STATS" | sed -E 's/[()]//g' | awk '{print $11}')"

printf '(car (let ((p (cons 7 8))) p))\n' > /tmp/nl-chunk-growth-eval.el
ESCAPE="$(./target/nelisp --load /tmp/nl-chunk-growth-eval.el 2>&1 | head -1)"

echo "[chunk-growth] arena-stats: $STATS"
echo "[chunk-growth] chunk-count=$CHUNK_COUNT  reserved-bytes=$RESERVED  let-escape=$ESCAPE"

rc=0
if [ "${CHUNK_COUNT:-0}" -gt 1 ]; then
  echo "[chunk-growth] PASS: arena grew to $CHUNK_COUNT chunks under boot pressure (no fixed-size dependency)"
else
  echo "[chunk-growth] FAIL: chunk-count=${CHUNK_COUNT:-?} (expected > 1 = growth)"; rc=1
fi
if [ "$ESCAPE" = "7" ]; then
  echo "[chunk-growth] PASS: value escaping a let body is correct across chunk growth"
else
  echo "[chunk-growth] FAIL: let-escape = '$ESCAPE' (expected 7)"; rc=1
fi

echo "[chunk-growth] restoring the default (256 MiB) reader binary ..."
build_reader ""

if [ "$rc" = "0" ]; then echo "[chunk-growth] RESULT: PASS"; else echo "[chunk-growth] RESULT: FAIL"; fi
exit "$rc"
