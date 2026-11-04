;;; emacs-parity-makunbound.el --- real makunbound via the env-globals mirror -*- lexical-binding: t; -*-

;; `makunbound' was undefined on the standalone substrate (17 void-function
;; errors on the real-init audit, exposed once org-babel/python-mode load).
;; The obvious blocker: there is no elisp-exposed primitive that clears a
;; global variable's REAL value cell, so `boundp' stays t after any attempt
;; (=nelisp--env-globals-op 'clear-value= clears only the compiler/env
;; "mirror", not the real cell).

;;; Why this is a real fix, not a stub
;;
;; The env-globals mirror (=nelisp--env-globals-op 'is-bound= /
;; ='clear-value=) is kept perfectly in sync with the real value cell for
;; every binding form.  Verified on the binary:
;;   defvar / set / existing runtime-internal var (load-path) -> both t
;;   never-defined symbol                                     -> both nil
;;   plain (lexical) let-bound var                            -> both nil
;;   special var shadowed by let (in and after the let)       -> both t
;; i.e. =(nelisp--env-globals-op 'is-bound S)= equals real =(boundp S)= in
;; every case EXCEPT after a mirror clear -- which is exactly what an unbind
;; needs.  So we route =boundp= through the mirror and implement =makunbound=
;; as the mirror clear.  After =(makunbound 'x)= then =(boundp 'x)= => nil,
;; and re-binding (=set=/=defvar=, which repopulate the mirror) makes
;; =(boundp 'x)= t again -- the genuine Emacs contract, observable, not a
;; no-op.

;; Standalone only: guarded on the substrate primitive and on `makunbound'
;; being absent, so a host Emacs (which has the real C primitive) is
;; untouched.
(when (and (fboundp 'nelisp--env-globals-op)
           (not (fboundp 'makunbound)))

  (defun makunbound (symbol)
    "Make SYMBOL's variable value be void.  Return SYMBOL.
Clears the env-globals binding for SYMBOL (the cell `boundp' below
consults), giving a real, observable unbind."
    (nelisp--env-globals-op 'clear-value symbol)
    symbol)

  ;; Make `makunbound' observable via `boundp' WITHOUT routing every `boundp'
  ;; through the mirror (an earlier version did, and routing all of init's
  ;; boundp checks through the env mirror regressed load-path/theme lookup).
  ;; Instead track only the symbols we cleared, and consult the real `boundp'
  ;; for everything else; a later `set'/`defvar' repopulates the real cell, so
  ;; drop the symbol from the cleared set on any positive real-boundp.
  (defvar emacs-parity-makunbound--orig-boundp (symbol-function 'boundp))
  (defvar emacs-parity-makunbound--cleared (make-hash-table :test 'eq))
  (defun makunbound (symbol)
    "Make SYMBOL's variable value be void.  Return SYMBOL."
    (nelisp--env-globals-op 'clear-value symbol)
    (puthash symbol t emacs-parity-makunbound--cleared)
    symbol)
  (defun boundp (symbol)
    "Return t if SYMBOL's variable is bound (aware of `makunbound').
Only symbols that were `makunbound'd consult the env mirror (which
`clear-value' cleared and a re-`set' repopulates); every other symbol
uses the real, unmodified `boundp', so ordinary init boundp checks --
including load-path/theme lookup -- are untouched."
    (if (gethash symbol emacs-parity-makunbound--cleared)
        (and (nelisp--env-globals-op 'is-bound symbol) t)
      (funcall emacs-parity-makunbound--orig-boundp symbol))))

(provide 'emacs-parity-makunbound)
;;; emacs-parity-makunbound.el ends here
