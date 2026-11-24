#!/usr/bin/env bash
# S3a-2 end-to-end: dynamic-align extern calls must be thread-safe.
# Builds threads.exe (N threads x many VirtualAlloc/WriteFile/VirtualFree
# calls, see dynalign-threads-sample.el) with the default frame-local rsp save
# and runs it under wine: it must exit 42.  Then builds the same program with
# the legacy global rsp slot (NELISP_AOT_DYNALIGN_GLOBAL_SLOT=1, negative
# control) and requires that run to FAIL (crash, hang or wrong exit code).
# Env: WINEPREFIX or NELISP_COM_DLL_WINEPREFIX (default: temp dir),
#      NELISP_DYNALIGN_CONTROL_TRIES (default 3).
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/dynalign-threads.XXXXXX")" || exit 1
PFX_TMP=""
if [ -n "${WINEPREFIX:-}" ]; then :
elif [ -n "${NELISP_COM_DLL_WINEPREFIX:-}" ]; then WINEPREFIX="$NELISP_COM_DLL_WINEPREFIX"
else PFX_TMP="$(mktemp -d "${TMPDIR:-/tmp}/dynalign-wineprefix.XXXXXX")"; WINEPREFIX="$PFX_TMP"; fi
export WINEPREFIX WINEDEBUG=-all WINEARCH=win64
cleanup() {
  timeout 60 wineserver -k >/dev/null 2>&1
  rm -rf "$WORK"
  [ -n "$PFX_TMP" ] && rm -rf "$PFX_TMP"
}
trap cleanup EXIT
fail() { echo "FAIL: $*"; exit 1; }
build() { # build DIR [env assignment]
  (ulimit -v 8388608; env $2 timeout 300 emacs -Q --batch -L "$ROOT/lisp" -L "$HERE" \
     -l dynalign-threads-sample.el --eval "(dynalign-threads-build \"$1\")") \
    >"$1.log" 2>&1 || { tail -5 "$1.log" | cut -c1-300; fail "build $1"; }
}
run() { (cd "$1" && timeout 150 wine threads.exe </dev/null >/dev/null 2>&1); }

build "$WORK/new" "NELISP_AOT_DYNALIGN_GLOBAL_SLOT=0"
run "$WORK/new"; rc=$?
[ "$rc" = 42 ] || fail "frame-local emission: threads.exe exit=$rc (expected 42)"
echo "PASS: frame-local rsp save, threads.exe => 42"

build "$WORK/old" "NELISP_AOT_DYNALIGN_GLOBAL_SLOT=1"
tries="${NELISP_DYNALIGN_CONTROL_TRIES:-3}"
failed=""
for i in $(seq 1 "$tries"); do
  run "$WORK/old"; rc=$?
  echo "control (global slot) run $i: exit=$rc"
  if [ "$rc" != 42 ]; then failed=1; break; fi
done
[ -n "$failed" ] || fail "negative control: global-slot build passed $tries runs (test is not sensitive)"
echo "PASS: negative control (global slot) fails as expected (exit=$rc)"
exit 0
