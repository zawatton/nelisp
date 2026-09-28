#!/usr/bin/env bash
# Compare standalone max/min with GNU Emacs on exact mixed integer/float cases.
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
EMACS_BIN=${EMACS:-emacs}
NELISP_BIN=${NELISP_BIN:-$ROOT/target/nelisp}

if [[ ! -x $NELISP_BIN ]]; then
  echo "missing standalone executable: $NELISP_BIN" >&2
  exit 2
fi

form='(let ((nan -0.0e+NaN) (pos-inf 1.0e+INF) (neg-inf -1.0e+INF))
  (list
   (list (max 9007199254740993 9007199254740992.0)
         (type-of (max 9007199254740993 9007199254740992.0)))
   (list (min 9007199254740993 9007199254740992.0)
         (type-of (min 9007199254740993 9007199254740992.0)))
   (list (max -9007199254740993 -9007199254740992.0)
         (type-of (max -9007199254740993 -9007199254740992.0)))
   (list (min -9007199254740993 -9007199254740992.0)
         (type-of (min -9007199254740993 -9007199254740992.0)))
   (max 9223372036854775809 9223372036854775808.0)
   (min 9223372036854775809 9223372036854775808.0)
   (max -9223372036854775809 -9223372036854775808.0)
   (min -9223372036854775809 -9223372036854775808.0)
   (max 9223372036854775808 9223372036854775808.0)
   (min 9223372036854775808 9223372036854775808.0)
   (max 9007199254740993 9007199254740994.0)
   (min 9007199254740993 9007199254740994.0)
   (max 1.0 nan) (min 1.0 nan) (max nan 1.0) (min nan 1.0)
   (max 9223372036854775809 pos-inf)
   (min -9223372036854775809 neg-inf)))'

cd "$ROOT"
expected=$("$EMACS_BIN" --batch -Q --eval "(prin1 $form)")
source_result=$(NELISP_PROBE_FORM="$form" "$EMACS_BIN" --batch -Q \
  -l test/nelisp-maxmin-source-probe.el)
actual=$("$NELISP_BIN" --eval "$form")
if [[ $source_result != "$expected" || $actual != "$expected" ]]; then
  printf 'max/min mismatch\nHost:       %s\nSource:     %s\nStandalone: %s\n' \
    "$expected" "$source_result" "$actual" >&2
  exit 1
fi
printf 'max/min mixed-number parity PASS (Host/source/standalone)\n%s\n' "$actual"
