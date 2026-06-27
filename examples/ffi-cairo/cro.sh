#!/bin/bash
# cro.sh IN.el OUT [LIBS...]
#   Compile IN.el (a single `(seq ...)` AOT program that defines a global
#   `main` defun returning an int) to a Linux x86_64 ELF .o via the mingw
#   Emacs AOT compiler, then cc-link it in WSL Debian with any extra LIBS
#   (e.g. -lcairo -lm -ldl) into an executable and run it.  cc supplies the
#   crt0/_start entry that calls `main'.  This is the static-extern-call FFI
#   dev loop (data-blob C-strings + arbitrary library linkage).
set -uo pipefail
IN="$1"; OUT="$2"; shift 2 || true
LIBS="$*"
NLROOT="/c/Users/kuroz/Cowork/Notes/dev/nelisp"
WINOBJ="C:/Users/kuroz/Cowork/Notes/dev/nelisp/${OUT}.o"
cd "$NLROOT" || exit 2

echo "=== compile-to-object ${IN} -> ${OUT}.o ==="
emacs -Q --batch -L lisp -L src -l nelisp-aot-compiler \
  --eval "(condition-case e (progn (nelisp-aot-compile-to-object (with-temp-buffer (insert-file-contents \"${IN}\") (goto-char (point-min)) (read (current-buffer))) \"${WINOBJ}\") (princ \"OBJ-OK\\n\")) (error (princ (format \"OBJ-ERR %S\\n\" e)) (kill-emacs 1)))" 2>&1 | tail -12
rc=${PIPESTATUS[0]}; if [ "$rc" -ne 0 ]; then echo "ABORT: compile failed (rc=$rc)"; exit "$rc"; fi

echo "=== cc-link + run (WSL) libs=[${LIBS}] ==="
wsl.exe -e bash -lc "cd /mnt/c/Users/kuroz/Cowork/Notes/dev/nelisp && cc '${OUT}.o' ${LIBS} -o '${OUT}' && ./'${OUT}'; echo exit=\$?" 2>&1 | tail -25
