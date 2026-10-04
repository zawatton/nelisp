#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/.." && pwd)
binary=${NELISP_BINARY:?Set NELISP_BINARY to the verified runtime-reload reader}
expected_sha=aea8ea77673f590d5716b5861a37366c849706e6fa05f54c6813f1cd9872db4a
tmpdir=$(mktemp -d "$repo_root/target/bytecode-cons.XXXXXX")
if [[ "${NELISP_BC_CONS_KEEP:-0}" == 1 ]]; then
  trap 'printf "bytecode-cons artifacts: %s\\n" "$tmpdir"' EXIT
else
  trap 'rm -rf "$tmpdir"' EXIT
fi

actual_sha=$(sha256sum "$binary" | awk '{print $1}')
[[ "$actual_sha" == "$expected_sha" ]]
source="$tmpdir/cons-probe.el"
elc="$tmpdir/cons-probe.elc"
artifact="$tmpdir/cons-probe.nelr"
bad_artifact="$tmpdir/cons-probe-bad.nelr"
cat >"$source" <<'EL'
;;; cons-probe.el --- GNU 31.1 CONS bytecode fixture -*- lexical-binding: t; -*-
(defun nelisp_bytecode_cons_probe (left right)
  "CONS probe (日本語 documentation)."
  (cons left right))
(provide 'nelisp-bytecode-cons-probe)
EL
NELISP_BC_CONS_SOURCE="$source" NELISP_BC_CONS_ELC="$elc" \
  emacs --batch -Q --eval '
(progn
(require (quote bytecomp))
(unless (equal emacs-version "31.1")
  (error "CONS .elc fixture requires GNU Emacs 31.1"))
(unless (byte-compile-file (getenv "NELISP_BC_CONS_SOURCE"))
  (error "GNU byte compilation failed")))'
[[ -s "$elc" ]]
NELISP_BC_CONS_ELC="$elc" emacs --batch -Q --eval '
(progn
(unless (equal emacs-version "31.1")
  (error "CONS .elc oracle requires GNU Emacs 31.1"))
(load (getenv "NELISP_BC_CONS_ELC") nil nil t)
(let* ((left (list (quote left)))
       (right (list (quote right)))
       (result (nelisp_bytecode_cons_probe left right)))
  (garbage-collect)
  (unless (and (consp result) (eq (car result) left) (eq (cdr result) right))
    (error "GNU .elc CONS oracle lost argument identity across GC"))
  (setcar left (quote mutated-left))
  (setcdr right (list (quote mutated-right)))
  (unless (and (eq (car (car result)) (quote mutated-left))
               (eq (cadr (cdr result)) (quote mutated-right)))
    (error "GNU .elc CONS oracle lost mutation visibility")))
(princ "gnu-bytecode-cons: PASS (.elc load, GC identity, mutation)\n"))'
rm -f "$source"
[[ ! -e "$source" ]]

actual=$(NELISP_REPO_ROOT="$repo_root" \
  NELISP_BC_CONS_ELC="$elc" NELISP_BC_CONS_ARTIFACT="$artifact" \
  NELISP_BC_CONS_BAD_ARTIFACT="$bad_artifact" \
  "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
    -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
    --load "$repo_root/test/standalone-bytecode-native-cons-driver.el" \
    --eval '(princ (if (and (file-readable-p (getenv "NELISP_BC_CONS_ARTIFACT"))
                           (not (file-exists-p (getenv "NELISP_BC_CONS_BAD_ARTIFACT"))))
                      "t" "nil"))')
[[ "$actual" == "t" ]]
echo 'bytecode-native-cons: PASS (source-free GNU .elc, raw-v2 native allocation, stock GNU oracle, NeLisp VM/native parity, GC identity/mutation, wrong arity, strict refusals)'
