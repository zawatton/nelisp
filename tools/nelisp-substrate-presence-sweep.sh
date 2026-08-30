#!/usr/bin/env bash
# Substrate presence sweep: one `fboundp' probe per name in the
# definable-name surface (~354 names: `scripts/nelisp-stdlib-prelude.el's
# top-level functions union `nelisp--primitive-symbols' in
# `src/nelisp-eval.el', see tools/nelisp-substrate-presence-gen.el), diffed
# across every NeLisp entry point and, for shared names, against host
# Emacs -- same machinery as `substrate-parity-smoke', a second corpus and
# a second ledger (Task A/B).
#
# Its own gate/report rather than folded into `nelisp-ai.sh extras':
# measured at ~2400 process launches versus the 36-form corpus's ~250, it
# does not fit that tier's per-gate budget (see tools/ai/gates.expected's
# substrate-presence-sweep entry for the measurement).
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

# Three outcomes, not two, same as `substrate-parity-smoke': the target
# cannot run on this host, or the predicate itself could not be asked.
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
    echo "substrate_presence_sweep_fail reason=cannot-ask-host-runnability rc=$host_rc" >&2
    exit 1
    ;;
esac

if [ "$BUILD" -eq 1 ]; then
  make standalone-reader
fi

EXE="$REPO_ROOT/target/nelisp"
if [ ! -x "$EXE" ]; then
  echo "substrate_presence_sweep_fail reason=missing-executable path=$EXE" >&2
  exit 1
fi

EMACS="$EMACS" NELISP_SUBSTRATE_PRESENCE=1 exec "$EMACS" --batch -Q -L lisp -L src -L scripts \
  -l tools/nelisp-substrate-parity.el
