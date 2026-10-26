#!/bin/bash
# usage: check-flat.sh BINARY -- steady-state cl-generic dispatch must be flat in the method count
# and cheap: per-call(20 methods) <= 1.25 x per-call(1 method), per-call(1 method) <= 45 us.
BIN=$1; D=$(mktemp -d); trap 'rm -rf $D' EXIT
cat >$D/f.el <<'EL'
(require 'cl-lib)
(defun fl-run (n)
  (let ((name (intern (format "fl-g%d" n))))
    (eval `(cl-defgeneric ,name (x)) t)
    (dotimes (i n) (eval `(cl-defmethod ,name ((x (eql ,i))) ,i) t))
    (eval `(cl-defmethod ,name ((x t)) -1) t)
    ;; Cycle the argument over every eql method (and the default), like the survey4 drivers.
    (let ((m (1+ n)))
      (dotimes (i (* 2 m)) (funcall name (1- (% i m))))
      ;; best of 3 passes: the first pass after setup pays for GC of the setup garbage
      (let ((best 1e9))
        (dotimes (_ 3)
          (garbage-collect)
          (let ((s (float-time)))
            (dotimes (i 4000) (funcall name (1- (% i m))))
            (setq best (min best (/ (* 1e6 (- (float-time) s)) 4000)))))
        best))))
(let* ((a (fl-run 1)) (b (fl-run 5)) (c (fl-run 20)))
  (princ (format "FLAT us/call n1=%.1f n5=%.1f n20=%.1f\n" a b c))
  (kill-emacs (if (and (<= c (* 1.25 a)) (<= a 45.0)) 0 1)))
EL
(ulimit -v 8388608; timeout 100 $BIN --load $D/f.el 2>&1 | grep FLAT; exit ${PIPESTATUS[0]})
