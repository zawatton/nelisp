;;; let-alist.el --- let-bind dotted alist accessors  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 16 breadth round 20.  A lean reimplementation of the GNU `let-alist'
;; package for the NeLisp standalone runtime, where the macro was void.
;; `(let-alist ALIST BODY...)' deep-scans BODY for symbols whose name starts
;; with a dot (e.g. `.a', `.a.b') and binds each to the corresponding
;; `alist-get' access into ALIST.
;;
;; Definitions are UNCONDITIONAL (like the bundled `range.el'): `let-alist'
;; is only an autoload on host Emacs, so a `(fboundp ...)' gate would be
;; skipped yet the autoload would resolve back to this file and fail to
;; define the macro.  Loading this file therefore installs this
;; implementation outright; a stock host never loads it.  Helpers avoid
;; `replace-regexp-in-string' / `string-match' (using `substring' / `aref')
;; and the macro uses a single hygienic temporary (Doc 22 A11: same-named
;; `make-symbol' temps collide on this runtime).

;;; Code:

(defun let-alist--dot-symbol-p (sym)
  "Return non-nil if SYM is a symbol whose name starts with a single dot."
  (and sym (symbolp sym)
       (let ((name (symbol-name sym)))
         (and (> (length name) 1)
              (eq (aref name 0) ?.)
              (not (eq (aref name 1) ?.))))))

(defun let-alist--deep-dot-search (data)
  "Return a list of every dotted symbol appearing inside DATA.
Nested `let-alist' forms are not descended into (only their alist expr)."
  (cond
   ((let-alist--dot-symbol-p data) (list data))
   ((vectorp data)
    (apply #'append (mapcar #'let-alist--deep-dot-search (append data nil))))
   ((not (consp data)) nil)
   ((eq (car data) 'let-alist)
    (let-alist--deep-dot-search (car (cdr data))))
   (t (append (let-alist--deep-dot-search (car data))
              (let-alist--deep-dot-search (cdr data))))))

(defun let-alist--list-to-sexp (keys var)
  "Build nested `alist-get' accesses for KEYS (outer-first) into VAR.
KEYS = (a b) yields (alist-get \\='b (alist-get \\='a VAR))."
  (let ((sexp var))
    (dolist (k keys sexp)
      (setq sexp (list 'alist-get (list 'quote k) sexp)))))

(defun let-alist--access-sexp (symbol var)
  "Return the access form for the dotted SYMBOL relative to VAR."
  (let* ((name (symbol-name symbol))
         (clean (substring name 1))
         (parts (mapcar #'intern (split-string clean "\\."))))
    (let-alist--list-to-sexp parts var)))

(defun let-alist--dedup (list)
  "Return LIST with duplicate symbols (by `eq') removed, order preserved."
  (let ((out nil))
    (dolist (x list (nreverse out))
      (unless (memq x out) (setq out (cons x out))))))

(defmacro let-alist (alist &rest body)
  "Let-bind dotted symbols in BODY to their accesses into ALIST.
Inside BODY, `.k' refers to (alist-get \\='k ALIST) and `.k1.k2' nests."
  (declare (indent 1) (debug t))
  (let* ((var (make-symbol "--let-alist--"))
         (syms (let-alist--dedup (let-alist--deep-dot-search body)))
         (binds (mapcar (lambda (s) (list s (let-alist--access-sexp s var)))
                        syms)))
    (list 'let (list (list var alist))
          (cons 'let (cons binds body)))))

(provide 'let-alist)

;;; let-alist.el ends here
