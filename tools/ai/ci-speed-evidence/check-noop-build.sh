#!/usr/bin/env bash
# S1.4: on a really built target/nelisp, a no-change `make standalone-reader'
# must finish in under 5 s, print the `up to date' line, and leave the binary
# byte-identical.  Exits non-zero WITHOUT building when no fresh stamp exists,
# so this check can never turn into a real build by accident.
cd "$(dirname "$0")/../../.." || exit 2

LIMIT_SECONDS=${NOOP_BUILD_LIMIT_SECONDS:-5}

if command -v sha256sum >/dev/null 2>&1; then
  sum() { sha256sum "$1" | cut -d' ' -f1; }
else
  sum() { shasum -a 256 "$1" | cut -d' ' -f1; }
fi

# The exact command line the recipe would run, from a dry run.
# The recipe spans several backslash-continued lines; join them (sh -c reads
# the backslash-newlines as continuations).
recipe=$(make -n standalone-reader 2>/dev/null | awk '/nelisp-standalone-reader-build.sh/ {on=1} on {print} on && !/\\$/ {exit}') && [ -n "$recipe" ] || {
  echo "FAIL: cannot read the standalone-reader recipe from make -n" >&2; exit 1; }
status_cmd=${recipe/nelisp-standalone-reader-build.sh/nelisp-standalone-reader-build.sh --status}
status_out=$(sh -c "$status_cmd" 2>&1)
status_rc=$?
if [ "$status_rc" -ne 0 ]; then
  echo "FAIL: no fresh stamp ($status_out); build first with make standalone-reader" >&2
  exit 1
fi

bin=
for cand in target/nelisp target/nelisp.exe; do
  [ -f "$cand.build-stamp" ] && [ -f "$cand" ] && { bin=$cand; break; }
done
[ -n "$bin" ] || { echo "FAIL: stamp reported fresh but no stamped binary found" >&2; exit 1; }

before=$(sum "$bin")
log=$(mktemp "${TMPDIR:-/tmp}/noop-build.XXXXXX")
t0=$(python3 -c 'import time; print(time.time())')
make standalone-reader > "$log" 2>&1
rc=$?
t1=$(python3 -c 'import time; print(time.time())')
elapsed=$(python3 -c "print(round($t1 - $t0, 2))")
after=$(sum "$bin")

fail=0
[ "$rc" -eq 0 ] || { echo "FAIL: make standalone-reader exited $rc" >&2; fail=1; }
grep -q '^standalone-reader: up to date' "$log" || { echo "FAIL: no 'up to date' line" >&2; fail=1; }
grep -q 'rebuilding' "$log" && { echo "FAIL: a rebuild happened" >&2; fail=1; }
python3 -c "import sys; sys.exit(0 if $elapsed < $LIMIT_SECONDS else 1)" \
  || { echo "FAIL: took ${elapsed}s, limit ${LIMIT_SECONDS}s" >&2; fail=1; }
[ "$before" = "$after" ] || { echo "FAIL: binary changed ($before -> $after)" >&2; fail=1; }
if [ "$fail" -ne 0 ]; then tail -5 "$log" >&2; rm -f "$log"; exit 1; fi
echo "OK: no-change make standalone-reader took ${elapsed}s; $(grep -m1 '^standalone-reader: up to date' "$log"); binary sha256 ${before:0:16} unchanged"
rm -f "$log"
