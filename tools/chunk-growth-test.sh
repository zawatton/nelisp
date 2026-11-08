#!/usr/bin/env bash
# chunk-growth-test.sh — Doc 140 chunked-arena GROWTH pressure test.
#
# The chunk-aware allocator (`nl_alloc_bytes' -> `nl_chunk_alloc_new' ->
# `nl_os_alloc_chunk') is supposed to grow the arena by adding chunks once
# the fixed first chunk is full, instead of depending on an ever-larger
# fixed reservation.  This test PROVES that at runtime: it builds the
# standalone reader with a deliberately tiny first chunk (8 MiB), then
# retains about 160 MB of vectors and asserts:
#   * (nelisp--arena-stats) reports chunk-count > 1 AFTER that retained
#     allocation (growth happened), and
#   * a value that escapes a `let' body is still correct.
#
# Why the probe allocates first: the cold image restored at boot (about 28 MB)
# makes chunk 0 grow by itself to about 95 MiB, so an 8 MiB first chunk no
# longer overflows at idle and chunk-count stays 1 until the program allocates.
#
# The first-chunk size is overridden via NELISP_LINUX_ARENA_SIZE (bytes).  The
# variant is built into its OWN output (target/nelisp-chunk8m) and its OWN unit
# cache (target/standalone-units/arena8m): the arena size is baked into the
# compiled units but is not part of the unit content key, so the variant must
# never share the default cache, and it must never overwrite target/nelisp.
# Nothing here touches the default binary or its unit cache.
# Exit 0 on PASS.  Run via `make standalone-chunk-growth-test'.

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"
EMACS="${EMACS:-emacs}"
TINY=$((8 * 1024 * 1024))   # 8 MiB first chunk; boot needs ~9 MiB -> growth

# Needs a host that can run the configured target; the build script's own
# predicate decides, not a uname table here.  Same convention as
# tools/selfhost-test.sh.  2026-08-23 Windows inventory: this test built a
# linux-x86_64 target/nelisp and then hit `Exec format error' trying to run
# it, instead of reporting a reasoned skip.
"$EMACS" --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
  >/dev/null 2>&1
host_rc=$?
case "$host_rc" in
  0) ;;
  3)
    printf 'GATE-SKIP target %s cannot run on host %s/%s\n' \
      "${NELISP_STANDALONE_TARGET:-linux-x86_64}" "$(uname -s)" "$(uname -m)"
    echo "[chunk-growth] SKIP: target cannot run on this host"
    exit 0
    ;;
  *)
    echo "[chunk-growth] FAIL: cannot ask host runnability (rc=$host_rc)" >&2
    exit 1
    ;;
esac

# The variant lives next to the default build but never replaces it.  The unit
# cache is nested under the default cache root (CI caches target/standalone-units
# as a whole, so the variant stays warm); the default build reads and prunes
# only its own partition directory, never this subdirectory.
VARIANT_OUT="$REPO_ROOT/target/nelisp-chunk8m"
VARIANT_CACHE="$REPO_ROOT/target/standalone-units/arena8m"
TARGET_NAME="${NELISP_STANDALONE_TARGET:-linux-x86_64}"
BUILD_LOG="$REPO_ROOT/target/chunk-growth-build.log"
mkdir -p "$REPO_ROOT/target"

build_variant() {
  # Calls the build Emacs directly with the variant's output path and
  # private unit cache, so the default target/nelisp and the shared cache
  # partition are never written.
  NELISP_LINUX_ARENA_SIZE="$TINY" \
  NELISP_STANDALONE_TARGET="$TARGET_NAME" \
  NELISP_STANDALONE_READER_OUTPUT="$VARIANT_OUT" \
  NELISP_STANDALONE_UNIT_CACHE_DIR="$VARIANT_CACHE" \
    "${NELISP_BUILD_EMACS:-$EMACS}" --batch -Q -L lisp -L src -L scripts \
      --eval '(setq load-prefer-newer t)' \
      -l nelisp-standalone-build -f nelisp-standalone-build-reader \
      >"$BUILD_LOG" 2>&1
}

echo "[chunk-growth] building standalone reader with an 8 MiB first chunk ..."
if ! build_variant; then
  echo "[chunk-growth] last 40 lines of $BUILD_LOG:"
  tail -n 40 "$BUILD_LOG" 2>/dev/null
  echo "[chunk-growth] FAIL: variant build failed"
  echo "GATE-COUNT checked=1 findings=1"
  echo "[chunk-growth] RESULT: FAIL"
  exit 1
fi

WORK="$(mktemp -d "${TMPDIR:-/tmp}/nl-chunk-growth.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

# Hold 200 vectors of 100000 elements (about 160 MB) so the first chunk must grow.
cat > "$WORK/stats.el" <<'EOF'
(let ((i 0) (acc nil)) (while (< i 200) (push (make-vector 100000 i) acc) (setq i (1+ i))) (nelisp--arena-stats))
EOF
STATS="$("$VARIANT_OUT" --load "$WORK/stats.el" 2>&1 | head -1)"
# tuple: (base size bump used live next free coll reuse CHUNK-COUNT reserved used)
CHUNK_COUNT="$(printf '%s' "$STATS" | sed -E 's/[()]//g' | awk '{print $10}')"
# A crash or error text puts a non-number here; treat it as no growth.
case "$CHUNK_COUNT" in ''|*[!0-9]*) CHUNK_COUNT_N=0 ;; *) CHUNK_COUNT_N=$CHUNK_COUNT ;; esac
RESERVED="$(printf '%s' "$STATS" | sed -E 's/[()]//g' | awk '{print $11}')"

printf '(car (let ((p (cons 7 8))) p))\n' > "$WORK/eval.el"
ESCAPE="$("$VARIANT_OUT" --load "$WORK/eval.el" 2>&1 | head -1)"

echo "[chunk-growth] arena-stats: $STATS"
echo "[chunk-growth] chunk-count=$CHUNK_COUNT  reserved-bytes=$RESERVED  let-escape=$ESCAPE"

rc=0
if [ "$CHUNK_COUNT_N" -gt 1 ]; then
  echo "[chunk-growth] PASS: arena grew to $CHUNK_COUNT chunks after a retained allocation (no fixed-size dependency)"
else
  echo "[chunk-growth] FAIL: chunk-count=${CHUNK_COUNT:-?} (expected > 1 = growth)"; rc=1
fi
if [ "$ESCAPE" = "7" ]; then
  echo "[chunk-growth] PASS: value escaping a let body is correct across chunk growth"
else
  echo "[chunk-growth] FAIL: let-escape = '$ESCAPE' (expected 7)"; rc=1
fi

if [ "$rc" = "0" ]; then
  echo "GATE-COUNT checked=1 findings=0"
  echo "[chunk-growth] RESULT: PASS"
else
  echo "GATE-COUNT checked=1 findings=1"
  echo "[chunk-growth] RESULT: FAIL"
fi
exit "$rc"
