#!/usr/bin/env bash
# S3a-1 registration test under wine: the default (HKCU) build of the sample
# COM server must register under HKCU\Software\Classes\CLSID\{...} (and
# nowhere in HKLM); the HKCR build (NELISP_COM_HIVE=hkcr, needed by wine) must
# register under HKLM\Software\Classes (wine's HKCR backing store) and be
# creatable.  Each build is unregistered with regsvr32 /u and the key must
# disappear.  wine ignores HKCU class registrations for CoCreateInstance, so
# the HKCU build's client is expected to get 184 (REGDB_E_CLASSNOTREG).
# The default value of InprocServer32 must be stored with its exact UTF-16
# size, 2 * (characters + 1) bytes; regsize.exe reads the value back and
# compares the registry size with the string length (a fixed 520-byte write
# fails it).  A third build uses :register-forms / :unregister-forms hooks
# (NELISP_COM_HOOKS): its extra key must be written, removed again by
# regsvr32 /u, and rolled back when the last hook fails (regsvr32 must fail).
# Env: NELISP_REG_BAD=1 (negative control: query the wrong hive for the HKCU
#      build, so the run must FAIL),
#      WINEPREFIX or NELISP_COM_DLL_WINEPREFIX (default: temp dir).
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/com-reg-e2e.XXXXXX")" || exit 1
PFX_TMP=""
if [ -n "${WINEPREFIX:-}" ]; then :
elif [ -n "${NELISP_COM_DLL_WINEPREFIX:-}" ]; then WINEPREFIX="$NELISP_COM_DLL_WINEPREFIX"
else PFX_TMP="$(mktemp -d "${TMPDIR:-/tmp}/com-reg-wineprefix.XXXXXX")"; WINEPREFIX="$PFX_TMP"; fi
export WINEPREFIX WINEDEBUG=-all WINEARCH=win64
CLSID='{7a1c0de0-5f3a-4a6e-9d11-0123456789ab}'
HOOKKEY='Software\Classes\NelispComHookTest'
unregister_class() {
  local k
  timeout 120 wine reg delete "HKLM\\$HOOKKEY" /f </dev/null >/dev/null 2>&1
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
w() { (cd "$1" && shift && timeout 120 wine "$@" </dev/null >/dev/null 2>&1); }
q() { timeout 120 wine reg query "$1\\Software\\Classes\\CLSID\\$CLSID\\InprocServer32" </dev/null 2>/dev/null | grep -qi 'com\.dll'; }
build() { # dir hive-env
  mkdir -p "$1"
  ( if [ -n "$2" ]; then export NELISP_COM_HIVE="$2"; else unset NELISP_COM_HIVE; fi
     if [ -n "${3:-}" ]; then export NELISP_COM_HOOKS="$3"; else unset NELISP_COM_HOOKS; fi
     ulimit -v 8388608; timeout 300 emacs -Q --batch -L "$ROOT/lisp" -L "$HERE" \
     -l com-sample.el --eval "(nelisp-com-sample-build-all \"$1\")") \
    >"$1/build.log" 2>&1 || { tail -5 "$1/build.log" | cut -c1-300; fail "build $2"; }
}

# The default build (NELISP_COM_HIVE unset) must be the HKCU one.
build "$WORK/hkcu" ""
build "$WORK/hkcr" hkcr

# --- HKCU (default) build
w "$WORK/hkcu" regsvr32 /s com.dll; rc=$?
[ "$rc" = 0 ] || fail "hkcu regsvr32 exit=$rc"
CHECK_HIVE=HKCU; [ "${NELISP_REG_BAD:-}" = 1 ] && CHECK_HIVE=HKLM
q "$CHECK_HIVE" || fail "InprocServer32 missing under $CHECK_HIVE after the HKCU-default build registered"
q HKLM && fail "HKCU build wrote HKLM\\Software\\Classes"
w "$WORK/hkcu" regsize.exe; rc=$?
[ "$rc" = 42 ] || fail "HKCU build: stored default value size is not 2*(len+1) (regsize exit=$rc)"
w "$WORK/hkcu" client.exe; rc=$?
[ "$rc" = 184 ] || fail "HKCU build: client exit=$rc (wine ignores HKCU classes, expected 184)"
w "$WORK/hkcu" regsvr32 /u /s com.dll; rc=$?
[ "$rc" = 0 ] || fail "hkcu regsvr32 /u exit=$rc"
q HKCU && fail "HKCU key still present after regsvr32 /u"

# --- HKCR switch build
w "$WORK/hkcr" regsvr32 /s com.dll; rc=$?
[ "$rc" = 0 ] || fail "hkcr regsvr32 exit=$rc"
q HKLM || fail "InprocServer32 missing under HKLM after the HKCR build registered"
q HKCU && fail "HKCR build wrote HKCU\\Software\\Classes"
w "$WORK/hkcr" regsize.exe; rc=$?
[ "$rc" = 42 ] || fail "HKCR build: stored default value size is not 2*(len+1) (regsize exit=$rc)"
w "$WORK/hkcr" client.exe; rc=$?
[ "$rc" = 42 ] || fail "HKCR build: client exit=$rc (expected 42)"
w "$WORK/hkcr" regsvr32 /u /s com.dll; rc=$?
[ "$rc" = 0 ] || fail "hkcr regsvr32 /u exit=$rc"
q HKLM && fail "HKLM key still present after regsvr32 /u"

# --- custom registration hooks (HKCR hive build, which wine can resolve)
hq() { timeout 120 wine reg query "HKLM\\$HOOKKEY" "$@" </dev/null 2>/dev/null; }
build "$WORK/hook" hkcr 1
w "$WORK/hook" regsvr32 /s com.dll; rc=$?
[ "$rc" = 0 ] || fail "hook regsvr32 exit=$rc"
hq /v Name | grep -q 'hookval' || fail "hook: REG_SZ value missing"
hq /v Count | grep -qi 'REG_DWORD.*0x7' || fail "hook: DWORD value missing"
hq /ve | grep -qi 'com\.dll' || fail "hook: module path default value missing"
w "$WORK/hook" regsize.exe; rc=$?
[ "$rc" = 42 ] || fail "hook build: default value size wrong (regsize exit=$rc)"
w "$WORK/hook" regsvr32 /u /s com.dll; rc=$?
[ "$rc" = 0 ] || fail "hook regsvr32 /u exit=$rc"
hq >/dev/null && fail "hook key still present after regsvr32 /u"
q HKLM && fail "hook CLSID key still present after regsvr32 /u"
build "$WORK/hookfail" hkcr fail
w "$WORK/hookfail" regsvr32 /s com.dll; rc=$?
[ "$rc" != 0 ] || fail "failing hook: regsvr32 succeeded"
hq >/dev/null && fail "failing hook: hook key not rolled back"
q HKLM && fail "failing hook: CLSID key not rolled back"
echo "PASS: default build -> HKCU (client 184), NELISP_COM_HIVE=hkcr build -> HKLM/HKCR (client 42), both unregister cleanly, exact default-value size, hooks + rollback"
exit 0
