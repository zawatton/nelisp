#!/bin/sh
# fxadopt-check.sh BINARY MODE
#   MODE vm-off : print the corpus transcript with the fx VM disabled
#   MODE vm-on  : print it with every corpus function forced onto the VM
#   MODE same   : fail unless the two transcripts are identical
#   MODE admitted : fail unless every function of the admitted families (all but the
#                   declined ones listed below) holds a compiled fx record
# The corpus is test/nelisp-fx-adopt-corpus.el; run from the repository root.
set -u
BIN=${1:?binary}; MODE=${2:?mode}
CORPUS=test/nelisp-fx-adopt-corpus.el
DECLINED="fxc-lam-let fxc-lam-letstar fxc-lam-mut fxc-lam-mut-outer fxc-lam-mapc-push fxc-lam-special fxc-lam-shadow fxc-lam-two-let fxc-lam-two-let2 fxc-lam-rebind fxc-lam-rebind2 fxc-lam-cc fxc-undef4"
ulimit -v 8388608
run() { # $1 = off|on
  if [ "$1" = off ]; then NELISP_FX_DISABLE=1 FXC_PRINT=1 timeout 200 "$BIN" --load $CORPUS 2>&1
  elif [ -n "${FXC_REC:-}" ]; then FXC_PRINT=1 FXC_FORCE=1 FXC_REC=1 timeout 200 "$BIN" --load $CORPUS 2>&1
  else FXC_PRINT=1 FXC_FORCE=1 timeout 200 "$BIN" --load $CORPUS 2>&1; fi
}
case $MODE in
 vm-off) run off ;;
 vm-on) run on ;;
 same) run off | grep -v '^t$\|^nil$' > "${TMPDIR:-/tmp}/fxa-off.$$"; run on | grep -v '^t$\|^nil$' > "${TMPDIR:-/tmp}/fxa-on.$$"
       test -s "${TMPDIR:-/tmp}/fxa-off.$$" && cmp -s "${TMPDIR:-/tmp}/fxa-off.$$" "${TMPDIR:-/tmp}/fxa-on.$$"; rc=$?; rm -f "${TMPDIR:-/tmp}/fxa-off.$$" "${TMPDIR:-/tmp}/fxa-on.$$"; exit $rc ;;
 admitted) FXC_REC=1 run on | grep '^FXREC' > "${TMPDIR:-/tmp}/fxa-rec.$$"
       test -s "${TMPDIR:-/tmp}/fxa-rec.$$" || exit 1
       bad=$(awk -v d=" $DECLINED " '$3!="fx" && index(d," "$2" ")==0 {print $2}' "${TMPDIR:-/tmp}/fxa-rec.$$"); rm -f "${TMPDIR:-/tmp}/fxa-rec.$$"
       test -z "$bad" || { echo "not compiled: $bad"; exit 1; } ;;
 *) echo "unknown mode" >&2; exit 2 ;;
esac
