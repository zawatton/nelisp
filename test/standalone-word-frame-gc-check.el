;;; standalone-word-frame-gc-check.el --- WORD frames keep their values -*- lexical-binding: t; -*-

;; The call path keeps evaluated arguments, VM locals and VM operand stacks
;; in WORD frames on the root stack (`nl_root_frame').  A collection that
;; runs while a later argument is evaluated, or inside a VM call, must keep
;; every word of every live frame, as GNU's mark_bytecode keeps a bytecode
;; frame (bytecode.c:396-428).  Each case below builds a fresh object that
;; only a frame references, collects and churns the heap so a freed block
;; would be reused, and then reads the object back.  GNU 31.1 prints the
;; same line:  emacs -Q --batch -l test/standalone-word-frame-gc-check.el

(defun wfgc-churn ()
  (garbage-collect)
  (let ((junk nil) (i 0))
    (while (< i 2000)
      (setq junk (cons (make-string 7 ?z) (make-list 6 i)) i (1+ i)))
    junk)
  (garbage-collect)
  'churned)

(defun wfgc-fresh (n) (list n (make-string 3 ?q) (cons n (1+ n)) (vector n n)))

;; Interpreted `funcall' of a builtin that is no WORD primitive: the first
;; argument lives only in the frame while the second one collects.
(defun wfgc-funcall-list (n)
  (funcall #'list (wfgc-fresh n) (wfgc-churn)))

;; Interpreted WORD primitive: `string<' reads its first argument, a fresh
;; string only the frame holds, after its second argument collected; a
;; freed and reused block would compare as the churn's "zzzzzzz".
(defun wfgc-primitive (n)
  (string< (concat "aa" (number-to-string n)) (progn (wfgc-churn) "ab")))

;; Interpreted closure through `funcall' with arguments from the frame.
(defun wfgc-closure (n)
  (funcall (lambda (a b) (list a b)) (wfgc-fresh n) (wfgc-churn)))

;; VM locals and operand stack: a VM function holds a fresh object in a
;; local and on its stack across a call that collects.
(defun wfgc-vm (n)
  (let ((x (wfgc-fresh n)))
    (list x (wfgc-churn) x)))
(when (fboundp 'nelisp--fx-register)
  (nelisp--fx-register 'wfgc-vm))

(defvar wfgc-results nil)
(dolist (n '(1 2 3))
  (push (wfgc-funcall-list n) wfgc-results)
  (push (wfgc-primitive n) wfgc-results)
  (push (wfgc-closure n) wfgc-results)
  (push (wfgc-vm n) wfgc-results))
(princ (format "WORD-FRAME-GC %S\n" (nreverse wfgc-results)))
(kill-emacs 0)
