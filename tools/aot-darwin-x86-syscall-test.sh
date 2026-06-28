#!/usr/bin/env bash
# Verify x86_64 Darwin syscall emission in the pure-Elisp AOT compiler.
set -euo pipefail

EMACS_BIN="${EMACS:-emacs}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

"$EMACS_BIN" --batch -Q -L lisp -L src \
  --eval '(setq load-prefer-newer t)' \
  -l nelisp-aot-compiler \
  --eval '
(let* ((nelisp-aot-compiler--arch (quote x86_64))
       (nelisp-aot-compiler--os (quote darwin))
       (exit-buf (nelisp-asm-x86_64-make-buffer))
       (node (nelisp-aot-compiler--make-ir
              (quote syscall-direct)
              :nr (nelisp-aot-compiler--make-ir (quote imm) :value 4)
              :a0 (nelisp-aot-compiler--make-ir (quote imm) :value 1)
              :a1 (nelisp-aot-compiler--make-ir (quote imm) :value 2)
              :a2 (nelisp-aot-compiler--make-ir (quote imm) :value 3)
              :a3 (nelisp-aot-compiler--make-ir (quote imm) :value 0)
              :a4 (nelisp-aot-compiler--make-ir (quote imm) :value 0)
              :a5 (nelisp-aot-compiler--make-ir (quote imm) :value 0)))
       (sys-buf (nelisp-asm-x86_64-make-buffer)))
  (nelisp-aot-compiler--emit-exit
   exit-buf (nelisp-aot-compiler--make-ir (quote imm) :value 7))
  (unless (string-match-p
           (regexp-quote (unibyte-string #x48 #xc7 #xc0 #x01 #x00 #x00 #x02))
           (nelisp-asm-x86_64-buffer-bytes exit-buf))
    (error "Darwin x86_64 exit did not emit 0x02000001"))
  (nelisp-aot-compiler--emit-syscall-direct node sys-buf)
  (unless (string-match-p
           (regexp-quote (unibyte-string #x48 #x81 #xc0 #x00 #x00 #x00 #x02))
           (nelisp-asm-x86_64-buffer-bytes sys-buf))
    (error "Darwin x86_64 syscall-direct did not add syscall class offset"))
  (princ "aot-darwin-x86-syscall PASS\n"))'
