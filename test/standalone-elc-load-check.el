;;; standalone-elc-load-check.el --- Load an actual GNU .elc -*- lexical-binding: t; -*-

;; The checked-in .elc-data was generated from test/elc-load-fixture.el by
;; GNU Emacs 31.1. `elc-load-check.py prepare' verifies its reproducibility.
(let* ((root (file-name-directory (directory-file-name
                                 (file-name-directory load-file-name))))
       (source-root (or (getenv "ELC_LOAD_SOURCE_ROOT") root))
       (file (make-temp-file "elc-load-check-" nil ".elc")))
  (load (expand-file-name "packages/nelisp-emacs-foundation/src/emacs-fns.el" source-root) nil t t)
  (load (expand-file-name "packages/nelisp-emacs-foundation/src/emacs-eval.el" source-root) nil t t)
  (load (expand-file-name "packages/nelisp-emacs-foundation/src/emacs-load.el" source-root) nil t t)
  (copy-file (expand-file-name "test/fixtures/elc-load-fixture.elc-data" root) file t)
  (load file nil t t)
  (unless (and (featurep 'nelisp-elc-load)
               (byte-code-function-p (symbol-function 'elc-load-fixture-add))
               (= (elc-load-fixture-add 2 4) 23)
               (= (elc-load-fixture-alias 3 5) 25)
               (= (funcall (elc-load-fixture-closure 11) 7) 18)
               (equal elc-load-fixture-top '(4 5 6))
               (equal elc-load-fixture-const '(2 3 5)))
    (error "GNU .elc loading failed"))
  (delete-file file)
  (princ "ELC-LOAD PASS 7 cases\n")
  (kill-emacs 0))
