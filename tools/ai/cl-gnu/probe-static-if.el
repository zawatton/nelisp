;;; probe-static-if.el --- GNU subr.el `static-if' is available  -*- lexical-binding: t; -*-
;; Usage: BIN --load tools/ai/cl-gnu/probe-static-if.el
;; Prints PROBE-STATIC-IF-PASS iff the macro expands and evaluates like GNU.
(let ((ok (and (fboundp 'static-if)
               (equal (macroexpand '(static-if t 1 2)) 1)
               (equal (macroexpand '(static-if nil 1 2 3)) '(progn 2 3))
               (equal (eval '(static-if (fboundp 'car) 'yes 'no) t) 'yes))))
  (princ (if ok "PROBE-STATIC-IF-PASS\n" "PROBE-STATIC-IF-FAIL\n")))
