#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
binary="${NELISP_BIN:-$repo_root/target/nelisp}"
bytecomp="$repo_root/vendor/emacs-lisp/emacs-lisp/bytecomp.el"
tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "standalone-byte-switch-parity-smoke: missing executable: $binary" >&2
  exit 2
fi

# Compiled by GNU Emacs 31.1 from:
# (lambda (x) (garbage-collect)
#   (cond ((eq x 'alpha) 11) ((eq x 'beta) 22) (t 33)))
# The GC call makes this exercise retain the jump table in function constants
# across a collection before opcode 183.  The table values are absolute byte
# offsets into the bytecode string, as emitted by the host compiler.
code='(unibyte-string 192 32 136 137 193 183 130 13 0 194 135 195 135 196 135)'
constants='[garbage-collect #s(hash-table test eq data (alpha 9 beta 11)) 11 22 33]'
function="(make-byte-code 257 $code $constants 3)"
actual="$("$binary" --eval "(let ((f $function)) (list (funcall f 'alpha) (funcall f 'beta) (funcall f 'miss) (funcall f nil) (funcall f 0)))")"
if [[ "$actual" != '(11 22 33 33 33)' ]]; then
  echo "standalone-byte-switch-parity-smoke: hit/miss/default mismatch: $actual" >&2
  exit 1
fi

# A hit must not trust a corrupt hash target as an instruction address.
bad_constants='[garbage-collect #s(hash-table test eq data (alpha 255 beta 11)) 11 22 33]'
malformed_expr="(funcall (make-byte-code 257 $code $bad_constants 3) 'alpha)"
if "$binary" --eval "$malformed_expr" >"$tmp_dir/malformed.out" 2>&1; then
  echo "standalone-byte-switch-parity-smoke: malformed hash target was accepted" >&2
  exit 1
fi

if command -v emacs >/dev/null 2>&1; then
  cat >"$tmp_dir/host-probe.el" <<'EOF'
;;; -*- lexical-binding: t; -*-
(let* ((f (byte-compile
           '(lambda (x)
              (garbage-collect)
              (cond ((eq x 'alpha) 11)
                    ((eq x 'beta) 22)
                    (t 33)))))
       (codes (append (aref f 1) nil))
       (values (mapcar (lambda (x) (funcall f x)) '(alpha beta miss nil 0))))
  (princ (format "%S|%S" codes values)))
EOF
  host="$(emacs -Q --batch -l "$bytecomp" -l "$tmp_dir/host-probe.el")"
  expected='(192 32 136 137 193 183 130 13 0 194 135 195 135 196 135)|(11 22 33 33 33)'
  if [[ "$host" != "$expected" ]]; then
    echo "standalone-byte-switch-parity-smoke: GNU Emacs byte-switch fixture changed: $host" >&2
    exit 1
  fi
fi

echo "standalone-byte-switch-parity-smoke: PASS"
