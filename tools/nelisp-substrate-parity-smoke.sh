#!/usr/bin/env bash
# Substrate-parity smoke: one probe corpus, every NeLisp entry point, diffed
# against the bare-file baseline and (its shared part) against host Emacs.
#
# This script is the thin, host-aware half: the GATE-SKIP predicate (same
# one every other standalone gate uses) and the build.  All of the actual
# corpus/diff/ledger work is in tools/nelisp-substrate-parity.el, which
# prints its own GATE-COUNT line -- see that file's header for the design.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

EMACS="${EMACS:-emacs}"
BUILD=1

while [ "$#" -gt 0 ]; do
  case "$1" in
    --no-build) BUILD=0; shift ;;
    --emacs) EMACS="$2"; shift 2 ;;
    *)
      echo "usage: $0 [--no-build] [--emacs EMACS]" >&2
      exit 2
      ;;
  esac
done

# Three outcomes, not two, same as `nelisp-performance-gate': the target
# cannot run on this host, or the predicate itself could not be asked.
# Folding those together is how a broken Emacs invocation starts reading
# as a reasoned skip.
set +e
"$EMACS" --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
  >/dev/null 2>&1
host_rc=$?
set -e
case "$host_rc" in
  0) ;;
  3)
    printf 'GATE-SKIP target %s cannot run on host %s/%s\n' \
      "${NELISP_STANDALONE_TARGET:-linux-x86_64}" "$(uname -s)" "$(uname -m)"
    exit 0
    ;;
  *)
    echo "substrate_parity_gate_fail reason=cannot-ask-host-runnability rc=$host_rc" >&2
    exit 1
    ;;
esac

if [ "$BUILD" -eq 1 ]; then
  make standalone-reader
fi

EXE="$REPO_ROOT/target/nelisp"
if [ ! -x "$EXE" ]; then
  echo "substrate_parity_gate_fail reason=missing-executable path=$EXE" >&2
  exit 1
fi

EMACS="$EMACS" exec "$EMACS" --batch -Q -L lisp -L src -L scripts \
  -l tools/nelisp-substrate-parity.el
