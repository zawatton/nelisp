;;; emacs-edebug-stubs.el --- Edebug primitive shims for cl-macs bootstrap  -*- lexical-binding: t; -*-

;; These declaration helpers must be available during bootstrap.  Edebug's
;; instrumentation and stepping functions are loaded on demand by `edebug.el'.

;;; Code:

;; ---- def-edebug-elem-spec / def-edebug-spec (subr.el 84-104) ----

(unless (fboundp 'def-edebug-spec)
  (defmacro def-edebug-spec (symbol spec)
    "Set the `edebug-form-spec' property of SYMBOL according to SPEC.
This is a no-op marker macro on standalone NeLisp — Edebug is not
shipped, the property is set for compat with code that does
\\=`(get FN \\='edebug-form-spec)\\=' lookups."
    (declare (indent 1))
    `(put (quote ,symbol) 'edebug-form-spec (quote ,spec))))

(unless (fboundp 'def-edebug-elem-spec)
  (defun def-edebug-elem-spec (name spec)
    "Define a new Edebug spec element NAME as shorthand for SPEC.
Verbatim port of the subr.el version — see vendor copy line 98."
    (declare (indent 1))
    (when (string-match "\\`[&:]" (symbol-name name))
      (error "Edebug spec name cannot start with '&' or ':'"))
    (unless (consp spec)
      (error "Edebug spec has to be a list: %S" spec))
    (put name 'edebug-elem-spec spec)))

(provide 'emacs-edebug-stubs)
;;; emacs-edebug-stubs.el ends here
