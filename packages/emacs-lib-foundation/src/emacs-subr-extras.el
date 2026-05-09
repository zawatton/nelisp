;;; emacs-subr-extras.el --- subr.el primitive shims for cl-lib bootstrap  -*- lexical-binding: t; -*-

;; Phase B2 (= Doc anvil-runtime pure-elisp roadmap, 2026-05-09)
;;
;; Standalone NeLisp ships nelisp-stdlib's string-prefix-p /
;; string-suffix-p / split-string / plist-get etc., but four
;; primitives that vendor `cl-lib.el' / `subr-x.el' reach for at
;; load time are missing:
;;
;;   `number-sequence' / `assoc-default' / `string-join' /
;;   `member-ignore-case'
;;
;; Plus the four-level `caaaar' .. `cddddr' family — fifteen entries
;; (`cadddr' is provided by NeLisp itself; the other 15 are absent).
;; `cl-lib.el' line 424 onwards does `(defalias 'cl-caaaar 'caaaar)'
;; etc. and trips `void-function caaaar' on standalone NeLisp without
;; them.
;;
;; Those nineteen `defun's are gathered here so `(require 'cl-lib)' /
;; `(require 'subr-x)' load cleanly.  Once that is in place, the rest
;; of the `anvil-server.el' load chain (= json + anvil-server-metrics
;; + anvil-server itself) goes through.  See memory entry
;; `project_anvil_runtime_phase_b1_breakthroughs' for the full audit.
;;
;; Each shim is gated on `unless (fboundp ...)' so loading under
;; host Emacs is a cheap no-op.

;;; Code:

;; ---- subr.el primitives missing from nelisp-stdlib ----

(unless (fboundp 'number-sequence)
  (defun number-sequence (from &optional to inc)
    "Return a sequence of numbers from FROM to TO (inclusive) by INC.
INC defaults to 1.  TO can be nil (= return single-element list).
Negative INC is supported when FROM > TO."
    (let ((step (or inc 1))
          (acc nil)
          (cur from))
      (cond
       ((null to) (list from))
       ((> step 0)
        (while (<= cur to)
          (setq acc (cons cur acc))
          (setq cur (+ cur step)))
        (nreverse acc))
       (t
        (while (>= cur to)
          (setq acc (cons cur acc))
          (setq cur (+ cur step)))
        (nreverse acc))))))

(unless (fboundp 'assoc-default)
  (defun assoc-default (key alist &optional test default)
    "Find object KEY in pseudo-alist ALIST.
Each ALIST entry is either a cons (KEY . VALUE) or a bare KEY.
TEST is called with the element (or its car) and KEY; defaults
to `equal'.  When a match is found, return the cdr if the
element is a cons, otherwise DEFAULT.  When no element matches,
return nil."
    (let (found
          (tail alist)
          value)
      (while (and tail (not found))
        (let ((elt (car tail)))
          (when (and elt
                     (funcall (or test #'equal)
                              (if (consp elt) (car elt) elt)
                              key))
            (setq found t)
            (setq value (if (consp elt) (cdr elt) default))))
        (setq tail (cdr tail)))
      value)))

(unless (fboundp 'string-join)
  (defun string-join (strings &optional separator)
    "Join all STRINGS using SEPARATOR (default empty string)."
    (mapconcat #'identity strings (or separator ""))))

(unless (fboundp 'member-ignore-case)
  (defun member-ignore-case (elt list)
    "Like `member', but case-insensitive on string elements."
    (while (and list
                (not (eq t (compare-strings elt 0 nil (car list) 0 nil t))))
      (setq list (cdr list)))
    list))

;; ---- four-level caaaar..cddddr (cadddr is NeLisp builtin) ----

(unless (fboundp 'caaaar) (defun caaaar (x) (car (car (car (car x))))))
(unless (fboundp 'caaadr) (defun caaadr (x) (car (car (car (cdr x))))))
(unless (fboundp 'caadar) (defun caadar (x) (car (car (cdr (car x))))))
(unless (fboundp 'caaddr) (defun caaddr (x) (car (car (cdr (cdr x))))))
(unless (fboundp 'cadaar) (defun cadaar (x) (car (cdr (car (car x))))))
(unless (fboundp 'cadadr) (defun cadadr (x) (car (cdr (car (cdr x))))))
(unless (fboundp 'caddar) (defun caddar (x) (car (cdr (cdr (car x))))))
(unless (fboundp 'cdaaar) (defun cdaaar (x) (cdr (car (car (car x))))))
(unless (fboundp 'cdaadr) (defun cdaadr (x) (cdr (car (car (cdr x))))))
(unless (fboundp 'cdadar) (defun cdadar (x) (cdr (car (cdr (car x))))))
(unless (fboundp 'cdaddr) (defun cdaddr (x) (cdr (car (cdr (cdr x))))))
(unless (fboundp 'cddaar) (defun cddaar (x) (cdr (cdr (car (car x))))))
(unless (fboundp 'cddadr) (defun cddadr (x) (cdr (cdr (car (cdr x))))))
(unless (fboundp 'cdddar) (defun cdddar (x) (cdr (cdr (cdr (car x))))))
(unless (fboundp 'cddddr) (defun cddddr (x) (cdr (cdr (cdr (cdr x))))))

;; ---- if-let* / when-let* / if-let / when-let (subr.el 2626+) ----
;;
;; In modern Emacs these macros live in `subr.el' (NOT `subr-x.el')
;; and are preloaded at dump time so callers never `require' them
;; explicitly.  Standalone NeLisp does not preload `subr.el', so
;; `(void-function if-let*)' fires for any anvil-*.el module that
;; uses the macro.  The minimal expansion below is sufficient for
;; the spec form `((SYM VALFORM) ...)' that anvil-server.el and
;; friends actually use; the obsolete bare-symbol shorthand is not
;; supported.

(defun emacs-subr-extras--build-if-let (varlist then else)
  "Build the expansion for `if-let*'.  VARLIST is a list of
`(SYM VALFORM)' entries (or bare SYM, treated as `(SYM SYM)').
Returns a sexp that evaluates THEN when every binding is non-nil
and ELSE (a list of forms to splice into a `progn') otherwise."
  (let ((else-form (if else (cons 'progn else) nil))
        (continuation then)
        (entries (reverse varlist)))
    (while entries
      (let* ((entry (car entries))
             (sym (if (consp entry) (car entry) entry))
             (val (if (consp entry) (cadr entry) entry)))
        (setq continuation
              `(let ((,sym ,val))
                 (if ,sym ,continuation ,else-form))))
      (setq entries (cdr entries)))
    continuation))

(unless (fboundp 'if-let*)
  (defmacro if-let* (varlist then &rest else)
    "If all bindings in VARLIST evaluate non-nil, eval THEN, else ELSE.
Each VARLIST entry is `(SYMBOL VALUEFORM)' or just `SYMBOL'.  Bindings
are sequential — later forms see earlier symbols."
    (declare (indent 2))
    (emacs-subr-extras--build-if-let varlist then else)))

(unless (fboundp 'when-let*)
  (defmacro when-let* (varlist &rest body)
    "Bind variables sequentially per VARLIST and eval BODY when all bindings are non-nil."
    (declare (indent 1))
    `(if-let* ,varlist (progn ,@body))))

(unless (fboundp 'if-let)
  (defalias 'if-let 'if-let*
    "Compatibility alias for `if-let*' (= deprecated bare-symbol form
not supported in this minimal port)."))

(unless (fboundp 'when-let)
  (defalias 'when-let 'when-let*
    "Compatibility alias for `when-let*'."))

;; ---- interactive-context primitives ----
;;
;; Standalone NeLisp has no Emacs UI, so `called-interactively-p'
;; always returns nil — every call is a programmatic call.  This
;; matches the safe behaviour for batch-mode Emacs.

(unless (fboundp 'called-interactively-p)
  (defun called-interactively-p (&optional _kind)
    "Return non-nil when the calling fn is invoked interactively.
Standalone NeLisp has no interactive frontend; always nil."
    nil))

(unless (fboundp 'apply-partially)
  (defun apply-partially (fun &rest args)
    "Return a function that calls FUN with leading ARGS plus its callers' args."
    (lambda (&rest more) (apply fun (append args more)))))

(unless (fboundp 'ignore-errors)
  (defmacro ignore-errors (&rest body)
    "Eval BODY, returning nil if any error is signalled."
    (declare (indent 0))
    `(condition-case nil (progn ,@body) (error nil))))

(unless (fboundp 'ignore)
  (defun ignore (&rest _ignored)
    "Do nothing and return nil — for use as a callback that disregards args."
    nil))

;; ---- Compiler-macro helper for `cXXr' family ----
;;
;; Vendor `cl-macs.el' references `internal--compiler-macro-cXXr'
;; (= subr.el line 598) as a compiler-macro for the entire car/cdr
;; composition family.  Without it `(load cl-macs.el)' silently
;; truncates partway through — `cl-letf' / `cl-defstruct' register
;; but `cl-callf' / `cl-callf2' (= line 2886+) never get defined,
;; which then breaks `cl-incf' expansion at runtime.  Port verbatim
;; from subr.el so the cXXr forms collapse to nested car/cdr at
;; compile time.

(unless (fboundp 'internal--compiler-macro-cXXr)
  (defun internal--compiler-macro-cXXr (form x)
    (let* ((head (car form))
           (n (symbol-name head))
           (i (- (length n) 2)))
      (if (not (string-match "c[ad]+r\\'" n))
          (if (and (fboundp head) (symbolp (symbol-function head)))
              (internal--compiler-macro-cXXr
               (cons (symbol-function head) (cdr form)) x)
            (error "Compiler macro for cXXr applied to non-cXXr form"))
        (while (> i (match-beginning 0))
          (setq x (list (if (eq (aref n i) ?a) 'car 'cdr) x))
          (setq i (1- i)))
        x))))

(provide 'emacs-subr-extras)
;;; emacs-subr-extras.el ends here
