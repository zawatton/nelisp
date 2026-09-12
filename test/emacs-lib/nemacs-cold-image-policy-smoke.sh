#!/bin/sh
# nemacs-cold-image-policy-smoke.sh --- the cold image is used, not built, by default
#
# Measured 2026-09-12 on this tree: starting WITH a cold image takes 1.2 s and
# starting without one takes ~20 s, but BUILDING one takes ~52 min and writes
# 549 MB, and the image is keyed on the bundle -- so a single edit under src/
# throws it away.  The launcher used to build a missing image on the critical
# path of an ordinary start, which is a ~52 min stall to save ~19 s per start
# until the next edit.  It also ran that build as a foreground child, so a
# `timeout'-killed launcher returned while the build kept going: one such
# orphan was observed at 100% CPU for 55 minutes, holding the cache lock the
# whole time, which made every other start wait out the lock timeout.
#
# Three checks, in the order of the three decisions:
#   1. an existing image is still used, and is fast
#   2. a missing image does NOT trigger a build, and says how to ask for one
#   3. a build that IS asked for dies with the launcher
#
# Check 3 is the one that needs a real build; it starts one and kills it after
# the builder appears, so the smoke costs seconds rather than an hour.

set -eu

smoke_script_dir=$(cd "$(dirname "$0")" && pwd)
smoke_root=$(cd "$smoke_script_dir/.." && pwd)
cd "$smoke_root"

NELISP_HOME=${NELISP_HOME:-$smoke_root/vendor/nelisp}
NEMACS_NELISP=${NEMACS_NELISP:-$NELISP_HOME/target/nelisp}
export NELISP_HOME NEMACS_NELISP

if [ ! -x "$NEMACS_NELISP" ]; then
    echo "nemacs-cold-image-policy-smoke: GATE-SKIP no nelisp binary at $NEMACS_NELISP"
    exit 0
fi

smoke_dir=$(mktemp -d "${TMPDIR:-/tmp}/nemacs-cold-policy.XXXXXX")
cleanup_smoke() {
    rm -rf "$smoke_dir"
}
trap cleanup_smoke EXIT

# --- 2. a missing image must not start a 52-minute build -------------------
# Run this first: it is the check that fails loudly if the default flips back.
NEMACS_COLD_CACHE_ROOT="$smoke_dir/miss" \
    ./bin/nemacs --driver=nelisp --batch --no-banner \
    --eval '(princ "MISS-OK\n")' > "$smoke_dir/miss.out" 2>&1 || true

if ! grep -q 'MISS-OK' "$smoke_dir/miss.out"; then
    echo 'nemacs-cold-image-policy-smoke: a cache miss did not reach the eval' >&2
    cat "$smoke_dir/miss.out" >&2
    exit 1
fi
if ! grep -q 'starting without one' "$smoke_dir/miss.out"; then
    echo 'nemacs-cold-image-policy-smoke: a cache miss did not say what it did' >&2
    cat "$smoke_dir/miss.out" >&2
    exit 1
fi
if ! grep -q 'NEMACS_COLD_BUILD=1' "$smoke_dir/miss.out"; then
    echo 'nemacs-cold-image-policy-smoke: a cache miss did not say how to build one' >&2
    cat "$smoke_dir/miss.out" >&2
    exit 1
fi
if [ -n "$(find "$smoke_dir/miss" -name '*.nlri' 2>/dev/null)" ]; then
    echo 'nemacs-cold-image-policy-smoke: a cache miss BUILT an image; the default flipped' >&2
    exit 1
fi

# --- 1. an existing image is still used ------------------------------------
# Only meaningful when this machine already has one for the current bundle;
# building one here would cost the 52 minutes this policy exists to avoid.
smoke_default_root="${XDG_CACHE_HOME:-$HOME/.cache}/nemacs"
smoke_existing=$(find "$smoke_default_root" -name 'nemacs-bootstrap.flat.nlri' 2>/dev/null | head -1 || true)
if [ -n "$smoke_existing" ]; then
    smoke_start=$(date +%s)
    ./bin/nemacs --driver=nelisp --batch --no-banner \
        --eval '(princ "WARM-OK\n")' > "$smoke_dir/warm.out" 2>&1 || true
    smoke_elapsed=$(( $(date +%s) - smoke_start ))
    if ! grep -q 'WARM-OK' "$smoke_dir/warm.out"; then
        echo 'nemacs-cold-image-policy-smoke: the warm path did not reach the eval' >&2
        cat "$smoke_dir/warm.out" >&2
        exit 1
    fi
    # A hit is ~1 s and a miss is ~20 s; 10 s separates them without pinning a
    # number this smoke would have to chase.
    if grep -q 'flat-image-cache=hit' "$smoke_dir/warm.out" && [ "$smoke_elapsed" -gt 10 ]; then
        echo "nemacs-cold-image-policy-smoke: cache hit but startup took ${smoke_elapsed}s" >&2
        exit 1
    fi
    printf 'warm start: %ss\n' "$smoke_elapsed"
else
    printf 'warm start: skipped (no image for this bundle on this machine)\n'
fi

# --- 3. an opted-in build dies with its launcher ---------------------------
NEMACS_COLD_CACHE_ROOT="$smoke_dir/build" NEMACS_COLD_BUILD=1 \
    ./bin/nemacs --driver=nelisp --batch --no-banner \
    --eval '(princ "BUILD-OK\n")' > "$smoke_dir/build.out" 2>&1 &
smoke_launcher=$!

smoke_builder=""
smoke_waited=0
while [ "$smoke_waited" -lt 90 ]; do
    smoke_builder=$(pgrep -P "$smoke_launcher" -f 'target/nelisp' 2>/dev/null | head -1 || true)
    [ -n "$smoke_builder" ] && break
    sleep 1
    smoke_waited=$((smoke_waited + 1))
done

if [ -z "$smoke_builder" ]; then
    kill -TERM "$smoke_launcher" 2>/dev/null || true
    wait "$smoke_launcher" 2>/dev/null || true
    echo 'nemacs-cold-image-policy-smoke: no builder appeared under NEMACS_COLD_BUILD=1' >&2
    cat "$smoke_dir/build.out" >&2
    exit 1
fi

kill -TERM "$smoke_launcher" 2>/dev/null || true
wait "$smoke_launcher" 2>/dev/null || true
sleep 3

if kill -0 "$smoke_builder" 2>/dev/null; then
    kill -KILL "$smoke_builder" 2>/dev/null || true
    echo "nemacs-cold-image-policy-smoke: builder $smoke_builder outlived its launcher" >&2
    exit 1
fi

# The lock must not be left behind either: the orphan held it for 55 minutes.
if [ -n "$(find "$smoke_dir/build" -name '*.lock' -type d 2>/dev/null)" ]; then
    echo 'nemacs-cold-image-policy-smoke: the cache lock outlived the launcher' >&2
    exit 1
fi

printf 'nemacs-cold-image-policy-smoke: ok\n'
