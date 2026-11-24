#!/usr/bin/env bash
# S1 end-to-end: build the sample DLL + loader EXE with nelisp and run them
# under wine.  Usage: wine-e2e.sh [--force-rebase]
# Env: NELISP_PE_DLL_DROP_EXPORT / NELISP_PE_DLL_NO_RELOC (negative controls),
#      WINEPREFIX (default: private prefix under the scratchpad or TMPDIR).
set -u
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
REBASE=0
[ "${1:-}" = "--force-rebase" ] && REBASE=1
WORK="$(mktemp -d "${TMPDIR:-/tmp}/pe-dll-e2e.XXXXXX")" || exit 1
trap 'rm -rf "$WORK"' EXIT
export WINEPREFIX="${WINEPREFIX:-${NELISP_PE_DLL_WINEPREFIX:-${TMPDIR:-/tmp}/pe-dll-wineprefix}}"
export WINEDEBUG=-all WINEARCH=win64
(ulimit -v 8388608; timeout 300 emacs -Q --batch -L "$ROOT/lisp" \
   -l "$HERE/pe-dll-sample.el" \
   --eval "(nelisp-pe-dll-sample-build-all \"$WORK\")") || { echo "FAIL: build"; exit 1; }
EXE=loader.exe
[ "$REBASE" = 1 ] && EXE=loader-rebase.exe
(cd "$WORK" && timeout 120 wine "$EXE" </dev/null >/dev/null 2>&1)
rc=$?
if [ "$rc" = 42 ]; then
  echo "PASS: $EXE exit=42 (add2(40,2)==42, answer()==42, via_ptr()==42)$([ $REBASE = 1 ] && echo ', DLL rebased away from preferred base')"
  exit 0
fi
echo "FAIL: $EXE exit=$rc (expected 42; 10=VirtualAlloc 11=LoadLibrary 12=not rebased 13-15=GetProcAddress add2/answer/via_ptr 16/17=wrong result)"
exit 1
