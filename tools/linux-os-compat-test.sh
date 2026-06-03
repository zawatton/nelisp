#!/usr/bin/env bash
# Linux OS compatibility ERT smoke.
#
# Linux is the historical default path, so this smoke currently gates the
# explicit non-Windows stdlib selectors plus the full stdlib OS test file on
# native Linux hosts.  No Rust toolchain is used.
set -euo pipefail

EMACS="${EMACS:-emacs}"
SUITE="all"

while [ "$#" -gt 0 ]; do
  case "$1" in
    --emacs) EMACS="$2"; shift 2 ;;
    --suite|-s) SUITE="$2"; shift 2 ;;
    -h|--help) echo "usage: $0 [--suite all|non-windows|full] [--emacs EMACS]"; exit 0 ;;
    *) echo "usage: $0 [--suite all|non-windows|full] [--emacs EMACS]" >&2; exit 2 ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

case "$SUITE" in
  all|non-windows) SELECTOR="non-windows"; SELECTOR_EXPR="\"$SELECTOR\""; EXPECTED_COUNT=3 ;;
  full) SELECTOR="t"; SELECTOR_EXPR="t"; EXPECTED_COUNT=288 ;;
  *) echo "[linux-os] FAIL: unknown suite $SUITE" >&2; exit 2 ;;
esac

LOG_DIR="$REPO_ROOT/target/linux-os-compat"
mkdir -p "$LOG_DIR"
LOG="$LOG_DIR/$SUITE.log"

echo "--- Linux OS compatibility ERT smoke ---"
uname -a
"$EMACS" --version | head -1
echo "logs: $LOG_DIR"
echo "suite: $SUITE"

"$EMACS" --batch -Q -L lisp -L src -L test \
  -l ert -l test/nelisp-stdlib-os-test.el \
  --eval "(ert-run-tests-batch-and-exit $SELECTOR_EXPR)" >"$LOG" 2>&1

if ! grep -q "Ran $EXPECTED_COUNT tests, $EXPECTED_COUNT results as expected, 0 unexpected" "$LOG"; then
  echo "[linux-os] FAIL: $SUITE selector '$SELECTOR'"
  tail -40 "$LOG"
  exit 1
fi

echo "[linux-os] PASS: $SUITE selector '$SELECTOR'"
tail -8 "$LOG"
echo "[linux-os] $SUITE PASS - Linux OS compatibility ERT smoke OK"
