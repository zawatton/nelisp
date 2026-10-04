#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/.." && pwd)"
emacs_cmd="${EMACS:-emacs}"
binary="$(realpath "${NELISP_BIN:?Set NELISP_BIN to the attested runtime-reload reader}")"
expected_sha="${NELISP_EXPECTED_BINARY_SHA256:?Set the expected reader SHA256}"
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/nelisp-unary-bytecode.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT
evidence_dir="${NELISP_UNARY_EVIDENCE_DIR:-$repo_root/target/agent-worktrees/unary-bytecode-evidence}"
mkdir -p "$evidence_dir"
source="$tmpdir/unary-template.el"
elc="$tmpdir/unary-template.elc"
car_artifact="$tmpdir/car.nelr"
cdr_artifact="$tmpdir/cdr.nelr"

actual_sha="$(sha256sum "$binary" | awk '{print $1}')"
[[ "$actual_sha" == "$expected_sha" ]]
cat >"$source" <<'EOF'
;;; -*- lexical-binding: t; -*-
(defun nelisp-unary-car (value)
  "Return the head. Multibyte docstring: 電気保安。"
  (car value))
(defun nelisp-unary-cdr (value)
  "Return the tail. Multibyte docstring: 点検記録。"
  (cdr value))
(provide 'nelisp-unary-template)
EOF
export NELISP_UNARY_SOURCE="$source"
"$emacs_cmd" --batch -Q -L "$repo_root/lisp" -L "$repo_root/src" \
  --eval '(byte-compile-file (getenv "NELISP_UNARY_SOURCE"))' \
  2>"$tmpdir/byte-compile.log"
test -s "$elc"
cp "$tmpdir/byte-compile.log" "$evidence_dir/byte-compile.log"
rm "$source"
test ! -e "$source"
export NELISP_UNARY_ELC="$elc"
"$emacs_cmd" --batch -Q -L "$repo_root/lisp" -L "$repo_root/src" \
  -L "$repo_root/scripts" --eval \
  '(progn
     (require (quote nelisp-bytecode-native-package))
     (let* ((forms (nelisp-bytecode-native-package--read-elc-forms (getenv "NELISP_UNARY_ELC")))
            (definitions (nelisp-bytecode-native-package--elc-definitions forms)))
       (dolist (name (quote (nelisp-unary-car nelisp-unary-cdr)))
         (let* ((function (cdr (assq name definitions)))
                (operation (if (eq name (quote nelisp-unary-car)) 64 65)))
           (unless (and function (= (aref function 0) 257)
                        (equal (aref function 1) (unibyte-string operation 135))
                        (equal (aref function 2) []) (= (aref function 3) 2))
             (error "GNU .elc unary template mismatch for %S: %S" name function))))))'
oracle="$("$emacs_cmd" --batch -Q -l "$elc" --eval \
  '(let ((value (cons (quote oracle-car) (list (quote oracle-tail)))))
     (princ (prin1-to-string
             (list :car (nelisp-unary-car value)
                   :cdr (nelisp-unary-cdr value)))))')"
printf '%s\n' "$oracle" >"$evidence_dir/gnu-oracle.txt"

export NELISP_UNARY_CAR_ARTIFACT="$car_artifact"
export NELISP_UNARY_CDR_ARTIFACT="$cdr_artifact"
export NELISP_UNARY_GNU_ORACLE="$oracle"
printf 'GNU 31.1 unary .elc templates: PASS (descriptor 257, bytes [64/65,135], constants [], depth 2)\n' >"$evidence_dir/template-attestation.txt"
set +e
"$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
  -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
  --load "$script_dir/standalone-bytecode-native-unary-driver.el" \
  --eval '(princ (prin1-to-string (nelisp-test-native-unary-bytecode-smoke)))' \
  >"$tmpdir/reader.stdout" 2>"$tmpdir/reader.stderr"
status=$?
set -e
cp "$tmpdir/reader.stdout" "$evidence_dir/reader.stdout"
python3 - "$tmpdir/reader.stderr" "$evidence_dir/reader.stderr" <<'PY'
from pathlib import Path
import re
import sys

raw = Path(sys.argv[1]).read_text(errors="replace")
match = re.search(r'error: \("([^"]+)', raw)
if match:
    summary = "NeLisp error: " + match.group(1)[:300] + " (details redacted)\n"
elif "invalid-read-syntax:" in raw:
    summary = "invalid-read-syntax while reading compiled input (payload redacted)\n"
else:
    summary = re.sub(r"\s+", " ", raw)[:1200] + "\n"
Path(sys.argv[2]).write_text(summary)
PY
if [[ "$status" -ne 0 ]]; then
  cat "$evidence_dir/reader.stderr" >&2
  printf 'unary bytecode reader exited %d\n' "$status" >&2
  exit "$status"
fi
actual="$(cat "$tmpdir/reader.stdout")"
if [[ "$actual" != t ]]; then
  printf 'unary bytecode smoke expected t, got %s\n' "$actual" >&2
  exit 1
fi
sha256sum "$car_artifact" "$cdr_artifact" >"$evidence_dir/artifact-sha256.txt"
printf '%s\n' 'bytecode-native-unary: PASS (GNU 31.1 .elc without source, fixed CAR/CDR gateways, GC identity/mutation, nil/wrong-type, malformed/near-shape/import refusal)'
printf 'evidence: %s\n' "$evidence_dir"
