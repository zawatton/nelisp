#!/bin/bash
# Usage: run-parity.sh BINARY CORPUS.el  -> prints only DIFF rows between GNU emacs 31.1 and BINARY.
# Each form of the corpus is one top-level form; both sides print one result line per form.
BIN=$(realpath "$1"); C=$(realpath "$2")
PRE=${REPRS_PRELOAD:-}   # e.g. "(require (quote subr-x))": evaluated before the corpus on both sides
W=$(mktemp -d); trap 'rm -rf $W' EXIT
cat > $W/drv.el <<EOF
;;; -*- lexical-binding: t; -*-
(let ((forms nil) (n 0))
  (when (> (length "$PRE") 0) (eval (car (read-from-string "$PRE")) t))
  (with-temp-buffer
    (insert-file-contents "$C")
    (goto-char (point-min))
    (condition-case nil (while t (push (read (current-buffer)) forms)) (end-of-file nil)))
  (dolist (f (nreverse forms))
    (setq n (1+ n))
    (princ (format "%d %S\n" n (condition-case e (eval f t) (error (list 'SIGNAL e)))))))
(kill-emacs 0)
EOF
(cd $W; ulimit -v 8388608; timeout 120 /usr/local/bin/emacs -Q --batch -l $W/drv.el > $W/gnu.out 2>$W/gnu.err)
(cd $W; ulimit -v 8388608; timeout 120 $BIN --load $W/drv.el -- > $W/nl.out 2>$W/nl.err)
diff -a <(cat $W/gnu.out) <(cat $W/nl.out) > $W/d.txt; rc=$?
echo "forms: $(wc -l < $W/gnu.out) gnu, $(wc -l < $W/nl.out) nl; diff rc=$rc"
cat $W/d.txt | cut -c1-300 | head -60
head -c 400 $W/nl.err
exit $rc
