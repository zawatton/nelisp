#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:-"$repo_root/../standalone-reader-fix/target/nelisp"}
tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-gnu-vm.XXXXXX")
source_copy="$tmp_dir/shared-store.el"
elc="$source_copy"c
trap 'rm -rf -- "$tmp_dir"' EXIT

if [[ ! -x "$binary" ]]; then
  echo "gnu-bytecode-vm: missing executable: $binary" >&2
  exit 2
fi
if [[ "$(emacs --batch -Q --eval '(princ emacs-version)')" != "31.1" ]]; then
  echo "gnu-bytecode-vm: requires GNU Emacs 31.1" >&2
  exit 2
fi

cp "$repo_root/test/fixtures/gnu-bytecode-vm/shared-store.el" "$source_copy"
emacs --batch -Q -f batch-byte-compile "$source_copy"
if [[ ! -f "$elc" ]]; then
  echo "gnu-bytecode-vm: GNU byte compiler did not emit .elc" >&2
  exit 1
fi

rm -- "$source_copy"
result=$(NELISP_REPO_ROOT="$repo_root" NELISP_GNU_ELC="$elc" \
  NELISP_GNU_SOURCE="$source_copy" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    --load "$repo_root/test/standalone-gnu-bytecode-vm-driver.el" \
    --eval '(setq nelisp-gnu-bytecode-vm-smoke-object (cons '\''across-gc nil))' \
    --eval '(prin1 (boundp '\''nelisp--functions))' \
    --eval '(prin1 (nelisp-gnu-bytecode-vm-load-file (getenv "NELISP_GNU_ELC")))' \
    --eval '(prin1 (nelisp-eval '\''nelisp-gnu-bytecode-vm-counter))' \
    --eval '(prin1 (nelisp-bcl-p (nelisp--function-of '\''nelisp-gnu-bytecode-vm-target)))' \
    --eval '(prin1 (nelisp-bcl-p (nelisp--function-of '\''nelisp-gnu-bytecode-vm-identity)))' \
    --eval '(prin1 (nelisp-bcl-p (nelisp--function-of '\''nelisp-gnu-bytecode-vm-plus-one)))' \
    --eval '(prin1 (nelisp-bcl-p (nelisp--function-of '\''nelisp-gnu-bytecode-vm-call-target)))' \
    --eval '(prin1 (nelisp-bcl-p (nelisp--function-of '\''nelisp-gnu-bytecode-vm-special-call)))' \
    --eval '(prin1 (nelisp-bcl-p (nelisp--function-of '\''nelisp-gnu-bytecode-vm-target2)))' \
    --eval '(prin1 (nelisp-bcl-p (nelisp--function-of '\''nelisp-gnu-bytecode-vm-call-target2)))' \
    --eval '(prin1 (nelisp-bcl-p (nelisp--function-of '\''nelisp-gnu-bytecode-vm-target3)))' \
    --eval '(prin1 (nelisp-bcl-p (nelisp--function-of '\''nelisp-gnu-bytecode-vm-call-target3)))' \
    --eval '(prin1 (nelisp-eval (list '\''nelisp-gnu-bytecode-vm-plus-one 41)))' \
    --eval '(prin1 (nelisp-eval (list '\''nelisp-gnu-bytecode-vm-call-target2 11 22)))' \
    --eval '(prin1 (nelisp-eval (list '\''nelisp-gnu-bytecode-vm-call-target3 11 22 33)))' \
    --eval '(garbage-collect)' \
    --eval '(prin1 (eq (nelisp-eval (list '\''nelisp-gnu-bytecode-vm-call-target
                                           (list '\''quote nelisp-gnu-bytecode-vm-smoke-object)))
                       nelisp-gnu-bytecode-vm-smoke-object))' \
    --eval '(prin1 (eq (nelisp-eval (list '\''nelisp-gnu-bytecode-vm-call-target3
                                           (list '\''quote nelisp-gnu-bytecode-vm-smoke-object)
                                           22 33))
                       nelisp-gnu-bytecode-vm-smoke-object))' \
    --eval '(prin1 (nelisp-eval '\''(nelisp-gnu-bytecode-vm-special-call)))' \
    --eval '(prin1 (nelisp-eval '\''nelisp-gnu-bytecode-vm-counter))' \
    --eval '(nelisp--builtin-defalias
       '\''nelisp-gnu-bytecode-vm-target
       (nelisp-bc-make nil '\''(ignored) [42] [1 0 0] 2 0))' \
    --eval '(prin1 (nelisp-eval (list '\''nelisp-gnu-bytecode-vm-call-target
                                       (list '\''quote nelisp-gnu-bytecode-vm-smoke-object))))' \
    --eval '(nelisp--builtin-defalias
       '\''nelisp-gnu-bytecode-vm-target2
       (nelisp-bc-make nil '\''(left right) [42] [1 0 0] 3 0))' \
    --eval '(prin1 (nelisp-eval (list '\''nelisp-gnu-bytecode-vm-call-target2 11 22)))' \
    --eval '(nelisp--builtin-defalias
       '\''nelisp-gnu-bytecode-vm-target3
       (nelisp-bc-make nil '\''(first second third) [42] [1 0 0] 4 0))' \
    --eval '(prin1 (nelisp-eval (list '\''nelisp-gnu-bytecode-vm-call-target3 11 22 33)))' \
    --eval '(nelisp--builtin-defalias
       '\''nelisp-gnu-bytecode-vm-target2
       (nelisp-bc-make nil '\''(left right) [] [255] 2 0))' \
    --eval '(prin1 (condition-case nil
                       (nelisp-eval (list '\''nelisp-gnu-bytecode-vm-call-target2 11 22))
                     (error t)))' \
    --eval '(nelisp--builtin-defalias
       '\''nelisp-gnu-bytecode-vm-target3
       (nelisp-bc-make nil '\''(first second third) [] [255] 3 0))' \
    --eval '(prin1 (condition-case nil
                       (nelisp-eval (list '\''nelisp-gnu-bytecode-vm-call-target3 11 22 33))
                     (error t)))' \
    --eval '(nelisp--builtin-defalias
       '\''nelisp-gnu-bytecode-vm-target
       (nelisp-bc-make nil '\''(value) [] [255] 1 0))' \
    --eval '(prin1 (condition-case nil
                       (nelisp-eval '\''(nelisp-gnu-bytecode-vm-special-call))
                     (error t)))' \
    --eval '(prin1 (nelisp-eval '\''nelisp-gnu-bytecode-vm-counter))')
expected='tnelisp-gnu-bytecode-vm-shared-store-fixture1ttttttttt421111tt771424242ttt1'
if [[ "$result" != "$expected" ]]; then
  printf 'gnu-bytecode-vm: cold process returned %s\n' "$result" >&2
  exit 1
fi

printf '%s\n' 'gnu-bytecode-vm: PASS (source-free cold ELC, CALL1/CALL2/CALL3 shared-store dispatch, earlier same-file target, normal/error return, redefinition, one-time effects, GC identity)'
