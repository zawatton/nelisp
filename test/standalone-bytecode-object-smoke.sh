#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
binary="${NELISP_BIN:-$repo_root/target/nelisp}"
if [[ ! -x "$binary" ]]; then
  echo "standalone-bytecode-object-smoke: missing executable: $binary" >&2
  exit 2
fi

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT
image="$tmp_dir/byte-code-tag17.nlri"
fixture='(nelisp--byte-code-wrap-test (record (quote ignored) 0 "\\300\\207" [42] 1))'
expected='(byte-code-function 4 nil nil nil t t t)'

actual="$("$binary" --eval "(let ((o $fixture)) (garbage-collect) (list (type-of o) (length o) (recordp o) (vectorp o) (consp o) (byte-code-function-p o) (functionp o) (equal (prin1-to-string o) (nelisp--repr o))))")"
if [[ "$actual" != "$expected" ]]; then
  echo "standalone-bytecode-object-smoke: GC/type result mismatch: $actual" >&2
  exit 1
fi

callable="$("$binary" --eval '(let ((o (make-byte-code 0 (unibyte-string 192 135) [42] 1))) (garbage-collect) (let ((direct (funcall o))) (fset (quote nelisp-bcf-call-test) o) (list direct (nelisp-bcf-call-test))))')"
if [[ "$callable" != '(42 42)' ]]; then
  echo "standalone-bytecode-object-smoke: zero-argument invocation mismatch: $callable" >&2
  exit 1
fi
required="$("$binary" --eval '(let ((o (make-byte-code 257 (unibyte-string 135) [] 2))) (funcall o 7))')"
if [[ "$required" != '7' ]]; then
  echo "standalone-bytecode-object-smoke: required-argument return mismatch: $required" >&2
  exit 1
fi
required_arity="$("$binary" --eval '(let ((o (make-byte-code 257 (unibyte-string 135) [] 2))) (list (condition-case nil (progn (funcall o) (quote no-error)) (wrong-number-of-arguments (quote signalled))) (condition-case nil (progn (funcall o 1 2) (quote no-error)) (wrong-number-of-arguments (quote signalled)))))')"
if [[ "$required_arity" != '(signalled signalled)' ]]; then
  echo "standalone-bytecode-object-smoke: required-argument arity mismatch: $required_arity" >&2
  exit 1
fi
gc_argument="$("$binary" --eval '(let ((o (make-byte-code 257 (unibyte-string 192 32 136 135) [garbage-collect] 2)) (s (make-string 1000 120))) (length (funcall o s)))')"
if [[ "$gc_argument" != '1000' ]]; then
  echo "standalone-bytecode-object-smoke: argument lost across byte-code GC: $gc_argument" >&2
  exit 1
fi
nil_descriptor="$("$binary" --eval '(funcall (make-byte-code nil (unibyte-string 192 135) [42] 1))')"
if [[ "$nil_descriptor" != '42' ]]; then
  echo "standalone-bytecode-object-smoke: nil zero-arg descriptor mismatch: $nil_descriptor" >&2
  exit 1
fi
packed_required="$("$binary" --eval '(condition-case nil (progn (funcall (make-byte-code 1 (unibyte-string 192 135) [42] 1)) (quote no-error)) (wrong-number-of-arguments (quote signalled)))')"
list_required="$("$binary" --eval '(condition-case nil (progn (funcall (make-byte-code (quote (x)) (unibyte-string 192 135) [42] 1)) (quote no-error)) (wrong-number-of-arguments (quote signalled)))')"
if [[ "$packed_required" != 'signalled' || "$list_required" != 'signalled' ]]; then
  echo "standalone-bytecode-object-smoke: nonzero arg descriptor mismatch: packed=$packed_required list=$list_required" >&2
  exit 1
fi

printed="$("$binary" --eval "$fixture")"
if [[ "$printed" != '#[0 "\\300\\207" [42] 1]' ]]; then
  echo "standalone-bytecode-object-smoke: printed object mismatch: $printed" >&2
  exit 1
fi

"$binary" dump-runtime-image "$image" "(defvar nelisp-bcf-test $fixture)" >/dev/null
roundtrip="$("$binary" eval-runtime-image "$image" '(list (type-of nelisp-bcf-test) (length nelisp-bcf-test) (recordp nelisp-bcf-test) (vectorp nelisp-bcf-test) (consp nelisp-bcf-test) (byte-code-function-p nelisp-bcf-test) (functionp nelisp-bcf-test) (equal (prin1-to-string nelisp-bcf-test) (nelisp--repr nelisp-bcf-test)))')"
if [[ "$roundtrip" != "$expected" ]]; then
  echo "standalone-bytecode-object-smoke: image roundtrip mismatch: $roundtrip" >&2
  exit 1
fi

constructed="$("$binary" --eval '(let ((o (make-byte-code 0 "\\300\\207" [42] 1))) (list (type-of o) (length o) (aref o 0) (aref o 1) (aref o 2) (aref o 3) (byte-code-function-p o) (functionp o) (fetch-bytecode o)))')"
if [[ "$constructed" != '(byte-code-function 4 0 "\\300\\207" [42] 1 t t nil)' ]]; then
  echo "standalone-bytecode-object-smoke: public constructor mismatch: $constructed" >&2
  exit 1
fi
arity="$("$binary" --eval '(condition-case nil (progn (make-byte-code) (quote no-error)) (wrong-number-of-arguments (quote signalled)))')"
if [[ "$arity" != 'signalled' ]]; then
  echo "standalone-bytecode-object-smoke: constructor arity mismatch: $arity" >&2
  exit 1
fi
extra="$("$binary" --eval '(let ((o (make-byte-code 0 "\\300\\207" [42] 1 nil nil nil))) (list (length o) (aref o 4) (aref o 5) (aref o 6) (functionp o)))')"
if [[ "$extra" != '(7 nil nil nil t)' ]]; then
  echo "standalone-bytecode-object-smoke: trailing element mismatch: $extra" >&2
  exit 1
fi
negative_depth="$("$binary" --eval '(condition-case nil (progn (make-byte-code 0 "\\300\\207" [42] -1) (quote no-error)) (error (quote signalled)))')"
if [[ "$negative_depth" != 'signalled' ]]; then
  echo "standalone-bytecode-object-smoke: negative stack depth mismatch: $negative_depth" >&2
  exit 1
fi

required_two="$("$binary" --eval '(funcall (make-byte-code 514 (unibyte-string 1 135) [] 3) 11 22)')"
if [[ "$required_two" != '11' ]]; then
  echo "standalone-bytecode-object-smoke: two-required-argument mismatch: $required_two" >&2
  exit 1
fi

optional_args="$("$binary" --eval '(let ((f (make-byte-code 769 (unibyte-string 1 135) [] 4))) (list (funcall f 10) (funcall f 10 22) (condition-case nil (progn (funcall f) (quote no-error)) (wrong-number-of-arguments (quote signalled))) (condition-case nil (progn (funcall f 1 2 3 4) (quote no-error)) (wrong-number-of-arguments (quote signalled)))))')"
if [[ "$optional_args" != '(nil 22 signalled signalled)' ]]; then
  echo "standalone-bytecode-object-smoke: optional-argument mismatch: $optional_args" >&2
  exit 1
fi

rest_args="$("$binary" --eval '(let ((f (make-byte-code 385 (unibyte-string 135) [] 3))) (list (funcall f 1) (funcall f 1 2 3) (condition-case nil (progn (funcall f) (quote no-error)) (wrong-number-of-arguments (quote signalled)))))')"
if [[ "$rest_args" != '(nil (2 3) signalled)' ]]; then
  echo "standalone-bytecode-object-smoke: rest-argument mismatch: $rest_args" >&2
  exit 1
fi

lambda_required="$("$binary" --eval '(funcall (make-byte-code (quote (x)) (unibyte-string 8 135) [x] 2) 41)')"
if [[ "$lambda_required" != '41' ]]; then
  echo "standalone-bytecode-object-smoke: lambda-list required argument mismatch: $lambda_required" >&2
  exit 1
fi

lambda_arity="$("$binary" --eval '(let ((f (make-byte-code (quote (x)) (unibyte-string 8 135) [x] 2))) (list (condition-case nil (progn (funcall f) (quote no-error)) (wrong-number-of-arguments (quote signalled))) (condition-case nil (progn (funcall f 1 2) (quote no-error)) (wrong-number-of-arguments (quote signalled)))))')"
if [[ "$lambda_arity" != '(signalled signalled)' ]]; then
  echo "standalone-bytecode-object-smoke: lambda-list arity mismatch: $lambda_arity" >&2
  exit 1
fi

lambda_optional="$("$binary" --eval '(let ((f (make-byte-code (quote (x &optional y)) (unibyte-string 9 135) [x y] 2))) (list (funcall f 41) (funcall f 41 7) (condition-case nil (progn (funcall f) (quote no-error)) (wrong-number-of-arguments (quote signalled))) (condition-case nil (progn (funcall f 1 2 3) (quote no-error)) (wrong-number-of-arguments (quote signalled)))))')"
if [[ "$lambda_optional" != '(nil 7 signalled signalled)' ]]; then
  echo "standalone-bytecode-object-smoke: lambda-list optional argument mismatch: $lambda_optional" >&2
  exit 1
fi

lambda_rest="$("$binary" --eval '(let ((f (make-byte-code (quote (x &rest r)) (unibyte-string 9 135) [x r] 2))) (list (funcall f 41) (funcall f 41 2 3) (condition-case nil (progn (funcall f) (quote no-error)) (wrong-number-of-arguments (quote signalled)))))')"
if [[ "$lambda_rest" != '(nil (2 3) signalled)' ]]; then
  echo "standalone-bytecode-object-smoke: lambda-list rest argument mismatch: $lambda_rest" >&2
  exit 1
fi

lambda_dynamic_restore="$("$binary" --eval '(let ((f (make-byte-code (quote (x)) (unibyte-string 8 135) [x] 2))) (set (quote x) 91) (list (funcall f 42) (symbol-value (quote x))))')"
if [[ "$lambda_dynamic_restore" != '(42 91)' ]]; then
  echo "standalone-bytecode-object-smoke: lambda-list dynamic binding was not restored: $lambda_dynamic_restore" >&2
  exit 1
fi

lambda_gc="$("$binary" --eval '(let ((f (make-byte-code (quote (x)) (unibyte-string 192 32 9 135) [garbage-collect x] 2)) (s (make-string 1000 120))) (length (funcall f s)))')"
if [[ "$lambda_gc" != '1000' ]]; then
  echo "standalone-bytecode-object-smoke: lambda-list argument lost across GC: $lambda_gc" >&2
  exit 1
fi

gc_two_args="$("$binary" --eval '(funcall (make-byte-code 514 (unibyte-string 192 32 2 135) [garbage-collect] 4) 11 22)')"
if [[ "$gc_two_args" != '11' ]]; then
  echo "standalone-bytecode-object-smoke: two arguments lost across VM GC: $gc_two_args" >&2
  exit 1
fi

echo "standalone-bytecode-object-smoke: PASS"
