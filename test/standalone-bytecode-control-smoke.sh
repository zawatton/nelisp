#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
binary="${NELISP_BIN:-$repo_root/target/nelisp}"
if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-control-smoke: missing executable: $binary" >&2
  exit 2
fi

tmp_out="$(mktemp)"
trap 'rm -f "$tmp_out"' EXIT

check() {
  local name="$1" expected="$2" expression="$3" actual
  actual="$("$binary" --eval "$expression")"
  if [[ "$actual" != "$expected" ]]; then
    echo "standalone-bytecode-control-smoke: $name expected $expected, got $actual" >&2
    exit 1
  fi
}

# GNU Emacs 31.1 byte-compile output for ordinary catch and matching throw.
check normal-catch 17 '(funcall (make-byte-code nil (unibyte-string 192 50 6 0 193 48 135) [k 17] 1))'
check matching-throw 17 '(funcall (make-byte-code nil (unibyte-string 192 50 9 0 193 192 194 34 48 135) [k throw 17] 3))'
check nested-throw 23 '(funcall (make-byte-code nil (unibyte-string 192 50 14 0 193 50 13 0 194 192 195 34 48 48 135) [outer inner throw 23] 3))'
# GNU Emacs 31.1 byte-compile output for dynamic let/catch/throw followed by
# a read of the variable; the result confirms unwind to its outer value.
check dynamic-binding-restored '(9 1)' '(progn (set (quote nelisp-control-dyn) 1) (list (funcall (make-byte-code nil (unibyte-string 193 24 194 50 11 0 195 194 196 34 48 136 8 41 135) [nelisp-control-dyn 9 k throw 17] 3)) (symbol-value (quote nelisp-control-dyn))))'
# GNU Emacs 31.1 byte-compile output: a condition-case for the parent
# `file-error' condition catches `file-missing' and binds (CONDITION . DATA).
condition_expr='(funcall (make-byte-code nil (unibyte-string 193 49 10 0 194 195 196 34 48 135 137 24 64 41 135) [e (file-error) signal file-missing ("x")] 4))'
check condition-hierarchy-and-error-object file-missing "$condition_expr"
if command -v emacs >/dev/null 2>&1; then
  host_actual="$(emacs -Q --batch --eval "(princ (prin1-to-string $condition_expr))")"
  if [[ "$host_actual" != file-missing ]]; then
    echo "standalone-bytecode-control-smoke: Host condition-case expected file-missing, got $host_actual" >&2
    exit 1
  fi
fi

# A non-matching condition must pass through an active catch frame.
if "$binary" --eval '(funcall (make-byte-code nil (unibyte-string 192 49 10 0 193 194 195 34 48 135 196 135) [(arith-error) signal file-missing ("x") 9] 4))' >"$tmp_out" 2>&1; then
  echo "standalone-bytecode-control-smoke: unmatched condition unexpectedly returned" >&2
  exit 1
fi
if ! rg -Fq 'file-missing: ("x")' "$tmp_out"; then
  cat "$tmp_out" >&2
  echo "standalone-bytecode-control-smoke: unmatched condition did not propagate" >&2
  exit 1
fi

# A signal skips a nested catch frame, while a throw skips a nested
# condition-case frame and still reaches its matching catch.
check condition-inside-catch 7 '(funcall (make-byte-code nil (unibyte-string 192 50 19 0 193 49 16 0 194 195 196 34 48 130 18 0 136 197 48 135) [tag (error) signal error nil 7] 3))'
check catch-inside-condition 7 '(funcall (make-byte-code nil (unibyte-string 192 49 15 0 193 50 13 0 194 193 195 34 48 48 135 196 135) [(no-catch) tag throw 7 8] 4))'

if "$binary" --eval '(funcall (make-byte-code nil (unibyte-string 192 193 194 34 135) [throw miss 31] 3))' >"$tmp_out" 2>&1; then
  echo "standalone-bytecode-control-smoke: unmatched throw unexpectedly returned" >&2
  exit 1
fi
if ! rg -q 'no-catch: \(miss 31\)' "$tmp_out"; then
  cat "$tmp_out" >&2
  echo "standalone-bytecode-control-smoke: unmatched throw did not preserve no-catch" >&2
  exit 1
fi
if "$binary" --eval '(funcall (make-byte-code nil (unibyte-string 192 50 9 0 193 192 194 34 48 135) [nil throw 3] 3))' >"$tmp_out" 2>&1; then
  echo "standalone-bytecode-control-smoke: nil-tag throw unexpectedly matched" >&2
  exit 1
fi
if ! rg -q 'no-catch: \(nil 3\)' "$tmp_out"; then
  cat "$tmp_out" >&2
  echo "standalone-bytecode-control-smoke: nil-tag throw did not preserve no-catch" >&2
  exit 1
fi
echo "standalone-bytecode-control-smoke: PASS"
