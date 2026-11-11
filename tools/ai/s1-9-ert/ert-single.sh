#!/usr/bin/env bash
# Run ERT test files in ONE host Emacs, the way `nelisp-ai.sh test' ran them
# before e3470413f (one Emacs, loaded through nelisp-ert-collection-load-env).
# The per-file runner hides any crash that needs state left by an earlier
# file; this run is how S1 of tools/ai/s1-9-ert-progress.org finds them.
#
# Usage: ert-single.sh [FILE...]   (default: the full test_files glob)
# NELISP_GATE_SELECTOR narrows the tests as in tools/ai/nelisp-ert-gate.el.
# ERT_SINGLE_STRICT=1 also requires every selected test to pass.
#
# Exit 0 iff Emacs ran to its end: it exited without a signal (code < 128)
# and printed the ERT summary.  Test failures do not count here; they are
# the ert-full gate's business.
set -u
root=$(git rev-parse --show-toplevel) || exit 2
cd "$root" || exit 2
if [ "$#" -gt 0 ]; then
  files="$*"
else
  files=$(ls test/nelisp*-test.el packages/*/test/nelisp*-test.el \
            packages/*/test/nl-*-test.el 2>/dev/null \
            | grep -v 'nelisp-worker-soak-test\.el$')
fi
# The load path of tools/run-tests-parallel.sh (LOAD_DIRS), in its order.
lp="-L lisp -L src -L test -L bench"
for d in packages/*/src packages/*/test; do [ -d "$d" ] && lp="$lp -L $d"; done
lp="$lp -L scripts -L tools"
out=target/s1-9-ert
mkdir -p "$out"
# ERT_SINGLE_LOG lets a short run (a ledger cmd) keep its log apart from
# the full run's.
log=${ERT_SINGLE_LOG:-$out/ert-single.log}
# shellcheck disable=SC2086
NELISP_GATE_NAME=ert-single NELISP_GATE_COMMAND="tools/ai/s1-9-ert/ert-single.sh" \
NELISP_ERT_FILES="$files" \
  "${EMACS:-emacs}" --batch -Q -l json $lp \
    --eval '(setq load-prefer-newer t)' \
    -l ert -l tools/ai/nelisp-ert-collection.el \
    -f nelisp-ert-collection-load-env \
    --eval '(nelisp-ert-collection-finish)' \
    -l tools/ai/nelisp-ert-gate.el -f nelisp-ert-gate-run > "$log" 2>&1
rc=$?
summary=$(grep -E '^Ran [0-9]+ tests' "$log" | tail -1)
last=$(grep -E '^ +(passed|FAILED|skipped) +[0-9]+/' "$log" | tail -1 | sed 's/^ *//')
echo "ert-single: emacs exit $rc; ${summary:-no ERT summary}; last: ${last:-none} (log $log)"
[ "$rc" -lt 128 ] && [ -n "$summary" ] || exit 1
if [ "${ERT_SINGLE_STRICT:-0}" = 1 ]; then
  case "$summary" in *", 0 unexpected"*) ;; *) exit 1 ;; esac
fi
exit 0
