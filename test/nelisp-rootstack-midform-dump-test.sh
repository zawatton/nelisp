#!/usr/bin/env bash
# Regression: a multi-chunk flat dump must not collect evaluator continuations.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
NELISP_BIN="${NELISP_BIN:-$REPO_ROOT/target/nelisp}"
TEST_TMP="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-rootstack-dump.XXXXXX")"
trap 'rm -rf "$TEST_TMP"' EXIT

SOURCE="$TEST_TMP/rootstack-fixture.el"
ARTIFACT="$TEST_TMP/rootstack-fixture.neln"
IMAGE="$TEST_TMP/rootstack-fixture.flat.nlri"

printf '%s\n' \
  '(defun rootstack-add-one (x) (+ x 1))' \
  "(provide 'rootstack-fixture)" > "$SOURCE"

"$NELISP_BIN" compile-elisp-artifact \
  --kind neln --input "$SOURCE" --output "$ARTIFACT" >/dev/null

FORM="$(printf '%s' \
  "(let ((path \"$IMAGE\")" \
  "      (sentinel \"local-alive\") (n 123456) (i 0) (payload nil))" \
  "  (nelisp--debug-switch 5)" \
  "  (while (< i 140000)" \
  "    (setq payload (cons (make-string 1024 120) payload))" \
  "    (setq i (+ i 1)))" \
  "  (nelisp--arena-dump-image-stream path)" \
  "  (setq i 0)" \
  "  (while (< i 10) (setq i (+ i 1)))" \
  "  (list rootstack-global sentinel n i (length payload)" \
  "        (rootstack-add-one 41)))")"

OUT="$("$NELISP_BIN" eval-elisp-artifact "$ARTIFACT" \
  '(setq rootstack-global "global-alive")' "$FORM")"

EXPECTED='("global-alive" "local-alive" 123456 10 140000 42)'
if [[ "$OUT" != "$EXPECTED" ]]; then
  printf 'rootstack dump continuation mismatch: %s\n' "$OUT" >&2
  exit 1
fi

IMAGE_SIZE="$(stat -c '%s' "$IMAGE")"
if (( IMAGE_SIZE <= 268435456 )); then
  printf 'flat image did not cross the multi-chunk threshold: %s\n' \
    "$IMAGE_SIZE" >&2
  exit 1
fi

printf 'rootstack-midform-dump PASS image-size=%s result=%s\n' \
  "$IMAGE_SIZE" "$OUT"
