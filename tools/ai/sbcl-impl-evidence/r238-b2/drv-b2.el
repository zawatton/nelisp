;;; -*- lexical-binding: t; -*-
;;; B2 controls: the native -> native site in bench-function-call2 (callee
;;; bench-call-helper, guarded direct entry nc_d_ID).  After warming the site,
;;; a redefinition of the callee between two calls must take effect on the next
;;; call, and the restore must return to the native callee.
(defvar b2--n 0)
(defun b2-check (tag ok) (princ (format "%s ok=%S\n" tag (and ok t))))
(let* ((k 64) (in (bench-input "function-call2"))
       (r0 (bench-function-call2 k in)) (orig (symbol-function 'bench-call-helper)))
  (bench-function-call2 k in) (bench-function-call2 k in)
  (fset 'bench-call-helper (lambda (&rest a) (setq b2--n (1+ b2--n)) (apply orig a)))
  (setq b2--n 0)
  (let ((r (bench-function-call2 k in))) (b2-check "B2-FSET" (and (= b2--n k) (equal r r0))))
  (fset 'bench-call-helper orig) (setq b2--n 0)
  (let ((r (bench-function-call2 k in))) (b2-check "B2-RESTORE" (and (= b2--n 0) (equal r r0))))
  (let ((w (lambda (f &rest a) (setq b2--n (1+ b2--n)) (apply f a))))
    (advice-add 'bench-call-helper :around w) (setq b2--n 0)
    (let ((r (bench-function-call2 k in))) (b2-check "B2-ADVICE" (and (= b2--n k) (equal r r0))))
    (advice-remove 'bench-call-helper w) (setq b2--n 0)
    (let ((r (bench-function-call2 k in))) (b2-check "B2-ADVICE-REMOVED" (and (= b2--n 0) (equal r r0)))))
  (fset 'bench-call-helper (lambda (&rest _) 0))
  (b2-check "B2-SEMANTIC-CHANGE" (not (equal (bench-function-call2 k in) r0)))
  (fset 'bench-call-helper orig)
  (b2-check "B2-SEMANTIC-RESTORE" (equal (bench-function-call2 k in) r0)))
