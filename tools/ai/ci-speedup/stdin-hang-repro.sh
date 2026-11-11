#!/usr/bin/env bash
# Show that a batch-Emacs C stack overflow hangs unless stdin is closed.
EMACS_BIN=${EMACS:-emacs}
F='(progn (setq max-lisp-eval-depth 1000000) (defun f (n) (1+ (f n))) (f 0))'
for mode in open closed; do
  s=$(date +%s)
  if [ $mode = closed ]; then
    (ulimit -s 1024; timeout 60 "$EMACS_BIN" --batch -Q --eval "$F" </dev/null >/dev/null 2>&1)
  else
    (ulimit -s 1024; timeout 60 "$EMACS_BIN" --batch -Q --eval "$F" < <(sleep 90) >/dev/null 2>&1)
  fi
  echo "stdin=$mode exit=$? elapsed=$(( $(date +%s)-s ))s"
done
