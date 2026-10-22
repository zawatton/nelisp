;;; symbols-globals-probe.el --- Doc 226 S2 timing loops -*- lexical-binding: t; -*-

;; Run unchanged by GNU Emacs 31.1 (emacs -Q --batch -l FILE) and NeLisp
;; (nelisp --load FILE), as source or as GNU-compiled .elc (SYMP_ELC names
;; the compiled copy of this file).  Each loop is warmed, garbage is
;; collected, then the loop runs SYMP_N times; collections inside the
;; timed loop count.  An operation's own cost is its loop minus the loop of
;; a same-shape trivial subr in the same process (COMMON.md "Measurement"):
;;   symbol-value S  vs  symbolp S    (one argument; both are opcodes in
;;                                     GNU bytecode, data.c:1645)
;;   set S V         vs  eq S V       (two arguments; opcodes, data.c:1660)
;;   boundp S        vs  symbolp S    (one argument, data.c:715)
;; S holds the symbol in a variable so no compiler folds the call.
;; Workload loops (S2.2):
;;   call-global: an interpreted defun reading and writing a special
;;                variable per call.

(defvar symp-n (string-to-number (or (getenv "SYMP_N") "200000")))
(load (or (getenv "SYMP_ELC")
          (expand-file-name "symbols-globals-functions.el"
                            (file-name-directory (or load-file-name buffer-file-name))))
      nil t)
(let ((res nil))
  (dolist (f symp-loops) (funcall f (min symp-n 2000) 'symp-v))
  (dolist (f symp-loops)
    (garbage-collect)
    (let ((t0 (float-time)))
      (funcall f symp-n 'symp-v)
      (push (format "%s=%.9f" f (- (float-time) t0)) res)))
  (princ (format "SYMP %d %s\n" symp-n (mapconcat #'identity (nreverse res) " "))))
