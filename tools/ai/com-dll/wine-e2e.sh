#!/usr/bin/env bash
# S2 end-to-end: build the COM sample DLL + client with nelisp and run them
# under wine.  Sequence: client (unregistered) -> 184, regsvr32 -> ok,
# registry key present, client -> 42, bad-IID client -> 102, regsvr32 /u,
# client -> 184.
# Env: NELISP_COM_BAD_IID / NELISP_COM_DROP_EXPORT / NELISP_COM_WRONG_SLOT /
#      NELISP_COM_NO_RELOC (negative-control hooks, see com-sample.el),
#      WINEPREFIX or NELISP_COM_DLL_WINEPREFIX (default: temp dir).
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/com-dll-e2e.XXXXXX")" || exit 1
PFX_TMP=""
if [ -n "${WINEPREFIX:-}" ]; then :
elif [ -n "${NELISP_COM_DLL_WINEPREFIX:-}" ]; then WINEPREFIX="$NELISP_COM_DLL_WINEPREFIX"
else PFX_TMP="$(mktemp -d "${TMPDIR:-/tmp}/com-dll-wineprefix.XXXXXX")"; WINEPREFIX="$PFX_TMP"; fi
export WINEPREFIX WINEDEBUG=-all WINEARCH=win64
CLSID='{7a1c0de0-5f3a-4a6e-9d11-0123456789ab}'
# Remove the sample class registration so a shared prefix starts and ends
# clean even when an earlier run (e.g. a negative control) stopped midway.
unregister_class() {
  local k
  for k in "HKLM\\Software\\Classes\\CLSID\\$CLSID" "HKCU\\Software\\Classes\\CLSID\\$CLSID"; do
    timeout 120 wine reg delete "$k" /f </dev/null >/dev/null 2>&1
  done
}
cleanup() {
  unregister_class
  timeout 60 wineserver -k >/dev/null 2>&1
  rm -rf "$WORK"
  [ -n "$PFX_TMP" ] && rm -rf "$PFX_TMP"
}
trap cleanup EXIT
unregister_class
fail() { echo "FAIL: $*"; exit 1; }
w() { (cd "$WORK" && timeout 120 wine "$@" </dev/null >/dev/null 2>&1); }

(ulimit -v 8388608; timeout 300 emacs -Q --batch -L "$ROOT/lisp" -L "$HERE" \
   -l com-sample.el --eval "(nelisp-com-sample-build-all \"$WORK\")") \
  >"$WORK/build.log" 2>&1 || { tail -5 "$WORK/build.log"; fail "build"; }

w client.exe; rc=$?
[ "$rc" = 184 ] || fail "unregistered client exit=$rc (expected 184 class not registered)"
w regsvr32 /s com.dll; rc=$?
[ "$rc" = 0 ] || fail "regsvr32 exit=$rc"
(cd "$WORK" && timeout 120 wine reg query \
   "HKLM\\Software\\Classes\\CLSID\\$CLSID\\InprocServer32" </dev/null 2>/dev/null) \
  | grep -qi 'com\.dll' || fail "InprocServer32 value missing after regsvr32"
w client.exe; rc=$?
[ "$rc" = 42 ] || fail "registered client exit=$rc (expected 42)"
w client-badiid.exe; rc=$?
[ "$rc" = 102 ] || fail "bad-IID client exit=$rc (expected 102 E_NOINTERFACE)"
w regsvr32 /u /s com.dll; rc=$?
[ "$rc" = 0 ] || fail "regsvr32 /u exit=$rc"
w client.exe; rc=$?
[ "$rc" = 184 ] || fail "client after unregister exit=$rc (expected 184)"
echo "PASS: 184 -> regsvr32 ok -> 42 (bad IID 102) -> regsvr32 /u ok -> 184"
exit 0
