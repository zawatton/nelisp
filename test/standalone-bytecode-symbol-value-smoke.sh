#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
binary="${NELISP_BIN:-$repo_root/target/nelisp}"
if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-symbol-value-smoke: missing executable: $binary" >&2
  exit 2
fi

check() {
  local name="$1" expected="$2" expression="$3" actual
  actual="$("$binary" --eval "$expression")"
  if [[ "$actual" != "$expected" ]]; then
    echo "standalone-bytecode-symbol-value-smoke: $name expected $expected, got $actual" >&2
    exit 1
  fi
}

check bound-global 5 '(progn (set (quote nelisp-byte-symbol-value-global) 5) (funcall (make-byte-code nil (unibyte-string 192 74 135) [nelisp-byte-symbol-value-global] 1)))'
check dynamically-bound 9 '(progn (defvar nelisp-byte-symbol-value-dynamic 1) (let ((nelisp-byte-symbol-value-dynamic 9)) (funcall (make-byte-code nil (unibyte-string 192 74 135) [nelisp-byte-symbol-value-dynamic] 1))))'
check nil nil '(funcall (make-byte-code nil (unibyte-string 192 74 135) [nil] 1))'
check t t '(funcall (make-byte-code nil (unibyte-string 192 74 135) [t] 1))'
check keyword :nelisp-byte-symbol-value-keyword '(funcall (make-byte-code nil (unibyte-string 192 74 135) [:nelisp-byte-symbol-value-keyword] 1))'

tmp_out="$(mktemp)"
trap 'rm -f "$tmp_out"' EXIT
if "$binary" --eval '(funcall (make-byte-code nil (unibyte-string 192 74 135) [nelisp-byte-symbol-value-unbound] 1))' >"$tmp_out" 2>&1; then
  echo "standalone-bytecode-symbol-value-smoke: unbound symbol unexpectedly returned" >&2
  exit 1
fi
if ! rg -q 'void-variable:.*nelisp-byte-symbol-value-unbound' "$tmp_out"; then
  cat "$tmp_out" >&2
  echo "standalone-bytecode-symbol-value-smoke: unbound symbol did not preserve void-variable" >&2
  exit 1
fi
if "$binary" --eval '(funcall (make-byte-code nil (unibyte-string 192 74 135) ["not-a-symbol"] 1))' >"$tmp_out" 2>&1; then
  echo "standalone-bytecode-symbol-value-smoke: invalid input unexpectedly returned" >&2
  exit 1
fi
if ! rg -q 'wrong-type-argument:.*symbolp' "$tmp_out"; then
  cat "$tmp_out" >&2
  echo "standalone-bytecode-symbol-value-smoke: invalid input did not preserve symbolp type error" >&2
  exit 1
fi
echo "standalone-bytecode-symbol-value-smoke: PASS"
