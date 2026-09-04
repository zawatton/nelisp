;;; emacs-parity-rx.el --- fix rx `not' translation (load-time pcase) -*- lexical-binding: t; -*-

;; `(rx (not (syntax ...)))' (and other non-char `not' arguments) failed with
;; "Illegal argument to rx `not': ...".  `rx--translate-not' (src/rx.el)
;; dispatches on its normalised argument with `pcase', but the baked copy of
;; the function was compiled with a mis-expanded `pcase' -- a *fresh* `pcase'
;; matches these backquote patterns correctly (verified on the binary: with
;; the baked function `(rx-to-string '(not (syntax whitespace)))' errors;
;; after re-evaluating the definition it returns "\\S-").  So the
;; `(syntax . ,args)' / `(category . ,args)' arms never matched and every
;; non-char argument fell through to the error branch.
;;
;; Re-defining the function here re-expands its `pcase' at load time, which is
;; correct.  Body copied verbatim from `src/rx.el' (its helper callees --
;; `rx--normalise-char-pattern', `rx--translate-syntax', etc. -- are already
;; defined by the baked `rx.el').  Standalone only: guarded on a baked rx
;; helper so host Emacs (real C-free rx) is untouched.
(when (fboundp 'rx--translate-syntax)
  (defun rx--translate-not (negated body)
    "Translate a (not ...) construct.  Return (REGEXP . PRECEDENCE).
If NEGATED, negate the sense (thus making it positive)."
    (unless (and body (null (cdr body)))
      (error "rx `not' form takes exactly one argument"))
    (let ((arg (rx--normalise-char-pattern (car body))))
      (pcase arg
        (`(not . ,args)
         (rx--translate-not      (not negated) args))
        (`(syntax . ,args)
         (rx--translate-syntax   (not negated) args))
        (`(category . ,args)
         (rx--translate-category (not negated) args))
        ('word-boundary                     ; legacy syntax
         (rx--translate-symbol (if negated 'word-boundary 'not-word-boundary)))
        (_ (let ((char (rx--reduce-to-char-alt arg)))
             (if char
                 (rx--generate-alt (not negated) (car char) (cdr char))
               (error "Illegal argument to rx `not': %S"
                      (rx--human-readable arg)))))))))

(provide 'emacs-parity-rx)
;;; emacs-parity-rx.el ends here
