#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/target/nelisp"}
host_emacs=${EMACS:-emacs}
timeout_seconds=${NELISP_TIMEOUT_SECONDS:-90}
expected_fingerprint=102e639e742351efbc457d8517db951ead880750dbc9eb1b96409f5fe063762d

if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-jit-fixnump-smoke: missing executable: $binary" >&2
  exit 2
fi

host=$("$host_emacs" -Q --batch --eval \
  '(let* ((function (make-byte-code 257 (unibyte-string 137 168 133 14 0 8 1 88 133 14 0 137 9 88 135) [most-negative-fixnum most-positive-fixnum] 3)) (fingerprint (secure-hash (quote sha256) (prin1-to-string (list (aref function 0) (aref function 1) (aref function 2) (aref function 3))))) (values (mapcar (lambda (x) (funcall function x)) (list 17 most-negative-fixnum most-positive-fixnum (1+ most-positive-fixnum) 1.0 "text")))) (princ (prin1-to-string (list fingerprint values))))')
if [[ "$host" != "(\"$expected_fingerprint\" (t t t nil nil nil))" ]]; then
  echo "standalone-bytecode-jit-fixnump-smoke: unexpected Host result: $host" >&2
  exit 1
fi

actual=$(timeout "${timeout_seconds}s" env NELISP_REPO_ROOT="$repo_root" "$binary" --eval \
  "(load \"$repo_root/test/standalone-bytecode-jit-fixnump-driver.el\" nil nil t)")
for expected in "$expected_fingerprint" ":vm-values (t t t nil nil nil)" \
                ":native-value t" ":native-delta 1" \
                ":string-value nil" ":string-native-delta 1" \
                ":fallbacks ((t . t) (t . t) (nil . t) (nil . t))" \
                ":compile-seconds"; do
  if [[ "$actual" != *"$expected"* ]]; then
    echo "standalone-bytecode-jit-fixnump-smoke: missing '$expected': $actual" >&2
    exit 1
  fi
done
echo "standalone-bytecode-jit-fixnump-smoke: PASS (Host, VM, JIT, fallback); $actual"
