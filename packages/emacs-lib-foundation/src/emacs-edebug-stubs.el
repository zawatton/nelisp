;;; emacs-edebug-stubs.el --- Edebug primitive shims for cl-macs bootstrap  -*- lexical-binding: t; -*-

;; Phase B3 (= Doc anvil-runtime pure-elisp roadmap, 2026-05-09)
;;
;; Vendor `cl-macs.el' (= 3811 lines) declares debug specs for nearly
;; every macro it defines (= `def-edebug-elem-spec', `def-edebug-spec',
;; `(declare (debug ...))').  Standalone NeLisp ships none of the
;; Edebug runtime, so those declarations would silently swallow the
;; tail of the file load (= `cl-callf' / `cl-callf2' / `cl-set-getf'
;; etc. never register, breaking `cl-incf' expansion at runtime
;; under anvil-server-process-jsonrpc).
;;
;; Edebug is the interactive Emacs debugger and is irrelevant to the
;; standalone runtime semantics that anvil tools care about, so all
;; the shims below are either:
;;   - simple `put` calls that match host Emacs (= `def-edebug-elem-spec'
;;     verbatim port) so subsequent `(get NAME 'edebug-elem-spec)' works
;;   - empty / always-nil / always-t no-ops that satisfy the symbol
;;     reference without performing any instrumentation
;;
;; This file is INTENTIONALLY DISPOSABLE — once standalone NeLisp gains
;; a real Edebug port (or ignores it entirely via cfg-gate), these
;; can be removed.  The companion `emacs-subr-extras.el' covers the
;; non-edebug subr.el primitives.

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

;; ---- edebug-* runtime no-ops ----
;;
;; cl-macs.el and other vendor sources reference these from
;; `(declare (debug ...))' bodies and from the byte-compiler's
;; instrumentation pass.  On standalone NeLisp they should never
;; fire — wire to `ignore' / no-op so name lookups succeed.

(unless (fboundp 'edebug-after)
  (defmacro edebug-after (_before-form _index _form)
    "No-op replacement for the Edebug stepping wrapper."
    nil))

(unless (fboundp 'edebug-before)
  (defun edebug-before (&rest _ignored) nil))

(unless (fboundp 'edebug-instrument)
  (defun edebug-instrument (&rest _ignored) nil))

(unless (fboundp 'edebug-on-error)
  (defun edebug-on-error (&rest _ignored) nil))

(unless (fboundp 'edebug-x-tracking-functions)
  (defun edebug-x-tracking-functions (&rest _ignored) nil))

(provide 'emacs-edebug-stubs)
;;; emacs-edebug-stubs.el ends here
