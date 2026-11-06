#!/bin/sh
# Doc 236 ratchet negative control: one new warning in lisp/ must fail the
# gate, and restoring the file must make it pass again.  Explicit
# `|| exit 1' on every step because `set -e' is unreliable here.
cd "$(dirname "$0")/.." || exit 1
TARGET=lisp/anvil-standalone-db.el
LOG=target/typecheck/ratchet-negative.log
mkdir -p target/typecheck || exit 1
test -f "$TARGET" || exit 1
SAVED=$(sha256sum "$TARGET" | cut -d' ' -f1) || exit 1
BACKUP=$(mktemp) || exit 1
cp "$TARGET" "$BACKUP" || exit 1
trap 'cp "$BACKUP" "$TARGET"; rm -f "$BACKUP"' EXIT

make typecheck-gate >"$LOG" 2>&1 || { echo "ratchet: baseline gate failed"; exit 1; }

printf '\n(defun nl-typecheck-ratchet-probe ()\n  "Inject one inferred-type warning."\n  (let ((x "a")) (car x)))\n' >>"$TARGET" || exit 1
if make typecheck-gate >"$LOG" 2>&1; then
  echo "ratchet: gate passed with an injected warning"; exit 1
fi
grep -q "$TARGET:[0-9]*:[0-9]*: warning: " "$LOG" || { echo "ratchet: output does not name the file"; exit 1; }

cp "$BACKUP" "$TARGET" || exit 1
NOW=$(sha256sum "$TARGET" | cut -d' ' -f1) || exit 1
test "$NOW" = "$SAVED" || { echo "ratchet: restore hash mismatch"; exit 1; }
make typecheck-gate >"$LOG" 2>&1 || { echo "ratchet: gate failed after restore"; exit 1; }
echo "ratchet: ok"
exit 0
