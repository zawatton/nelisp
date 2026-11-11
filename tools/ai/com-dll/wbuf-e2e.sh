#!/usr/bin/env bash
# S3a-1 end-to-end: build wbuf.dll (wbuf library, struct accessors, try macro,
# :fields / :extra-iids class) and wbuf-client.exe with nelisp, run under
# wine.  The client LoadLibrary()s the DLL, calls WbufSelfTest and exits 42
# when every check passes (100 + check number otherwise).
# Env: NELISP_WBUF_BAD=1 (negative control, see wbuf-sample.el: the self test
#      corrupts a wbuf length, so the run must FAIL),
#      WINEPREFIX or NELISP_COM_DLL_WINEPREFIX (default: temp dir).
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/wbuf-e2e.XXXXXX")" || exit 1
PFX_TMP=""
if [ -n "${WINEPREFIX:-}" ]; then :
elif [ -n "${NELISP_COM_DLL_WINEPREFIX:-}" ]; then WINEPREFIX="$NELISP_COM_DLL_WINEPREFIX"
else PFX_TMP="$(mktemp -d "${TMPDIR:-/tmp}/wbuf-wineprefix.XXXXXX")"; WINEPREFIX="$PFX_TMP"; fi
export WINEPREFIX WINEDEBUG=-all WINEARCH=win64
cleanup() {
  timeout 60 wineserver -k >/dev/null 2>&1
  rm -rf "$WORK"
  [ -n "$PFX_TMP" ] && rm -rf "$PFX_TMP"
}
trap cleanup EXIT
fail() { echo "FAIL: $*"; exit 1; }

(ulimit -v 8388608; timeout 300 emacs -Q --batch -L "$ROOT/lisp" -L "$HERE" \
   -l wbuf-sample.el --eval "(wbuf-sample-build-all \"$WORK\")") \
  >"$WORK/build.log" 2>&1 || { tail -5 "$WORK/build.log" | cut -c1-300; fail "build"; }

(cd "$WORK" && timeout 120 wine wbuf-client.exe </dev/null >/dev/null 2>&1); rc=$?
[ "$rc" = 42 ] || fail "wbuf-client exit=$rc (expected 42; 100+N = check N failed)"
echo "PASS: wbuf, struct accessors, try, :fields, :extra-iids => 42"
exit 0
