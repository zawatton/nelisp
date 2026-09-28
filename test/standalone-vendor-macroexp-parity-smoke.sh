#!/bin/sh
set -eu
: "${NELISP_BIN:?Set NELISP_BIN to a standalone NeLisp executable}"
: "${EMACS_LISP_ROOT:?Set EMACS_LISP_ROOT to the GNU Emacs 31.1 lisp directory}"
EMACS_BIN=${EMACS_BIN:-emacs}

host_inline=$($EMACS_BIN --batch -Q -L "$EMACS_LISP_ROOT" -L "$EMACS_LISP_ROOT/emacs-lisp" \
  --eval "(progn (require 'inline) (princ (prin1-to-string (list (functionp (cdr (symbol-function 'inline-letevals))) (macroexpand-1 '(inline-letevals (x) x))))))")
nelisp_inline=$("$NELISP_BIN" -L "$EMACS_LISP_ROOT" -L "$EMACS_LISP_ROOT/emacs-lisp" \
  --eval "(progn (require 'inline) (list (functionp (cdr (symbol-function 'inline-letevals))) (macroexpand-1 '(inline-letevals (x) x))))")
if [ "$host_inline" != "$nelisp_inline" ]; then
  printf '%s\n' "FAIL vendor macroexpand parity" "host=$host_inline" "nelisp=$nelisp_inline" >&2
  exit 1
fi

host_call=$($EMACS_BIN --batch -Q --eval \
  "(progn (defmacro nelisp-macro-cell-smoke (x) (list 'quote x)) (princ (prin1-to-string (nelisp-macro-cell-smoke 42))))")
nelisp_call=$("$NELISP_BIN" --eval \
  "(progn (defmacro nelisp-macro-cell-smoke (x) (list 'quote x)) (nelisp-macro-cell-smoke 42))")
if [ "$host_call" != "$nelisp_call" ]; then
  printf '%s\n' "FAIL native macro call parity" "host=$host_call" "nelisp=$nelisp_call" >&2
  exit 1
fi
printf '%s\n' "PASS vendor macroexpand parity" "PASS native macro call parity"
