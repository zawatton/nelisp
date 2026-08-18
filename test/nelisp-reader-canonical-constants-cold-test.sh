#!/usr/bin/env bash
# Regression: interned nil/t and cold-restored plist nil stay canonical.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
NELISP_BIN="${NELISP_BIN:-$REPO_ROOT/target/nelisp}"
TEST_TMP="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-canonical-cold.XXXXXX")"
trap 'rm -rf "$TEST_TMP"' EXIT

SOURCE="$TEST_TMP/fixture.el"
ARTIFACT="$TEST_TMP/fixture.neln"
IMAGE="$TEST_TMP/canonical.flat.nlri"

printf '%s\n' \
  '(defun canonical-fixture-id (x) x)' \
  "(provide 'canonical-fixture)" > "$SOURCE"

DIRECT_OUT="$("$NELISP_BIN" --eval \
  '(list (null (intern "nil")) (eq (intern "nil") nil) (eq (intern "t") t))')"
if [[ "$DIRECT_OUT" != '(t t t)' ]]; then
  printf 'standalone intern did not canonicalize nil/t: %s\n' \
    "$DIRECT_OUT" >&2
  exit 1
fi

"$NELISP_BIN" compile-elisp-artifact \
  --kind neln --input "$SOURCE" --output "$ARTIFACT" >/dev/null

SOFT_OUT="$("$NELISP_BIN" eval-elisp-artifact "$ARTIFACT" \
  '(list (null (nelisp--intern-lookup "nil"))
         (eq (nelisp--intern-lookup "t") t)
         (null (nelisp--intern-lookup
                "definitely-missing-canonical-probe")))')"
if [[ "$SOFT_OUT" != '(t t t)' ]]; then
  printf 'standalone soft intern did not canonicalize nil/t: %s\n' \
    "$SOFT_OUT" >&2
  exit 1
fi

"$NELISP_BIN" eval-elisp-artifact "$ARTIFACT" \
  '(setq canonical-registry
         (list (list :relocs (intern "nil")
                     :extern-symbols (intern "nil"))))' \
  "(nelisp--arena-dump-image-stream \"$IMAGE\")" >/dev/null

COLD_OUT="$("$NELISP_BIN" --cold-load-from "$IMAGE" --eval \
  '(progn
     (setq canonical-probe (list 1 2 3))
     (defun canonical-cold-probe (section)
       (let* ((canonical-probe (plist-get section :relocs))
              (n (length canonical-probe)))
         (list canonical-probe n
               (null canonical-probe)
               (equal canonical-probe nil))))
     (canonical-cold-probe (car canonical-registry)))')"

EXPECTED='(nil 0 t t)'
if [[ "$COLD_OUT" != "$EXPECTED" ]]; then
  printf 'cold canonical nil mismatch: %s\n' "$COLD_OUT" >&2
  exit 1
fi

printf 'reader-canonical-cold PASS result=%s\n' "$COLD_OUT"
