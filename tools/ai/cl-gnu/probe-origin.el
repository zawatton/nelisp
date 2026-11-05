;;; probe-origin.el --- check which file serves a feature  -*- lexical-binding: t; -*-
;; Usage: BIN --load tools/ai/cl-gnu/probe-origin.el -- FEATURE=PATH-SUBSTRING...
;; Prints OK/FAIL per argument (via `locate-library') and PROBE-ORIGIN-PASS
;; iff every located path contains its substring.
;; `locate-library' alone does not prove which definition is active, so
;; FEATURE=PATH may be followed by active=pcase: the active `pcase' macro must
;; be GNU's.  GNU-only internals (`pcase--expand', `pcase--u') must be defined,
;; and macroexpanding a fresh `pcase' form must actually call `pcase--expand'
;; (traced by temporarily wrapping it); the native macro never calls it.
(defun clgnu-probe-active-pcase ()
  "Return nil when the active `pcase' is GNU's, else a failure string."
  (let ((calls 0) (orig nil)
        (form (list 'pcase 'clgnu-probe-x
                    (list (list 'quote (make-symbol "clgnu-probe-unique")) 1)
                    (list '_ 2))))
    (cond
     ((not (fboundp 'pcase--expand)) "pcase--expand unbound")
     ((not (fboundp 'pcase--u)) "pcase--u unbound")
     ((not (macrop 'pcase)) "pcase is not a macro")
     (t
      (setq orig (symbol-function 'pcase--expand))
      (unwind-protect
          (progn
            (fset 'pcase--expand
                  (lambda (&rest args) (setq calls (1+ calls)) (apply orig args)))
            (macroexpand form))
        (fset 'pcase--expand orig))
      (if (> calls 0) nil "macroexpanding pcase does not reach pcase--expand")))))
(defun clgnu-probe-active-gv ()
  "Return nil when the active `setf' is GNU gv.el's, else a failure string.
GNU `setf' expands every place through `gv-get'; the native setf does not call
it for `car'.  `gv-get' must be bound at startup (no on-demand require) and a
fresh `(setf (car X) 1)' must reach it."
  (let ((calls 0) (orig nil))
    (cond
     ((not (fboundp 'gv-get)) "gv-get unbound at startup")
     ((not (macrop 'setf)) "setf is not a macro")
     (t
      (setq orig (symbol-function 'gv-get))
      (unwind-protect
          (progn
            (fset 'gv-get
                  (lambda (&rest args) (setq calls (1+ calls)) (apply orig args)))
            (macroexpand (list 'setf (list 'car (make-symbol "clgnu-probe-x")) 1)))
        (fset 'gv-get orig))
      (if (> calls 0) nil "macroexpanding setf does not reach gv-get")))))
(let ((ok t))
  (dolist (arg (cdr (member "--" command-line-args)))
    (let* ((i (string-search "=" arg))
           (feature (substring arg 0 i))
           (want (substring arg (1+ i)))
           (got (and (not (equal feature "active"))
                     (condition-case nil (locate-library feature) (error nil)))))
      (if (equal feature "active")
          (let ((bad (cond ((equal want "pcase") (clgnu-probe-active-pcase))
                           ((equal want "gv") (clgnu-probe-active-gv))
                           (t "unknown active target"))))
            (if (not bad)
                (princ (format "OK active=%s GNU\n" want))
              (setq ok nil)
              (princ (format "FAIL active=%s %s\n" want (or bad "unknown")))))
        (if (and got (string-search want got))
            (princ (format "OK %s %s\n" feature got))
          (setq ok nil)
          (princ (format "FAIL %s want=%s got=%S\n" feature want got))))))
  (princ (if ok "PROBE-ORIGIN-PASS\n" "PROBE-ORIGIN-FAIL\n")))
