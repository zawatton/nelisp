#!/bin/bash
# cr.sh IN.el OUT [RUNARGS...]
#   Compile IN.el (a single `(seq ...)` AOT program sexp) to an x86_64 ELF at
#   OUT (path relative to dev/nelisp) using the mingw Emacs AOT compiler, then
#   run it inside WSL Debian and print its exit code.  This is the dynamic-FFI
#   dev loop: compile on Windows, execute the Linux ELF in WSL.
set -uo pipefail
IN="$1"; OUT="$2"; shift 2 || true
NLROOT="/c/Users/kuroz/Cowork/Notes/dev/nelisp"
WINOUT="C:/Users/kuroz/Cowork/Notes/dev/nelisp/${OUT}"
cd "$NLROOT" || exit 2

echo "=== compile ${IN} -> ${OUT} ==="
emacs -Q --batch -L lisp -L src -l nelisp-aot-compiler \
  --eval "(condition-case e (progn (nelisp-aot-compile-sexp (with-temp-buffer (insert-file-contents \"${IN}\") (goto-char (point-min)) (read (current-buffer))) \"${WINOUT}\") (princ \"COMPILED\\n\")) (error (princ (format \"COMPILE-ERR %S\\n\" e)) (kill-emacs 1)))" 2>&1 | tail -12
rc=${PIPESTATUS[0]}
if [ "$rc" -ne 0 ]; then echo "ABORT: compile failed (rc=$rc)"; exit "$rc"; fi

echo "=== run (WSL) ==="
wsl.exe -e bash -lc "cd /mnt/c/Users/kuroz/Cowork/Notes/dev/nelisp && chmod +x '${OUT}' && ./'${OUT}' $* ; echo exit=\$?" 2>&1 | tail -20
