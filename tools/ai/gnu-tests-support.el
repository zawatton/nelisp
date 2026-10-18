;;; gnu-tests-support.el --- Load GNU ERT into the conformance image -*- lexical-binding: t -*-

;; Loaded by tools/ai/gnu-tests.sh after the nelisp-emacs-lib bundle, before
;; the image is dumped.  It only puts GNU's ert.el/ert-x.el on the load path
;; and supplies what a batch ERT run needs from display-side libraries that
;; neither the runtime nor the bundle provides.  Each shim says why it is
;; safe; anything a test itself exercises must be fixed in the runtime or
;; prelude instead of here.

(defvar gnu-tests-support-root
  (file-name-directory (or load-file-name buffer-file-name))
  "tools/ai/ of this tree.")

;; The bundle (nelisp-emacs-lib emacs-parity-clloop.el) replaces the
;; runtime's `cl-loop' with a minimal builder that expands any clause it
;; does not know, such as `finally', to nil; GNU ert.el's `ert-run-tests'
;; then runs no test at all.  This lane measures the runtime's Lisp, so put
;; back the definitions the runner recorded before loading the bundle.
(defvar gnu-tests--runtime-definitions nil
  "Alist (NAME . DEFINITION) saved before the bundle loaded.")
(dolist (cell gnu-tests--runtime-definitions)
  (when (cdr cell)
    (fset (car cell) (cdr cell))
    (when (and (eq (car cell) 'cl-loop) (boundp 'nelisp--macros)
               (hash-table-p nelisp--macros))
      (puthash 'cl-loop (cdr cell) nelisp--macros))))

;; GNU libraries ert.el requires (debug, backtrace, ewoc, find-func,
;; help-mode) are the library tree's vendored GNU 31.1 copies.
(let ((elib (getenv "GNU_TESTS_ELIB")))
  (when elib
    (dolist (dir '("vendor/emacs-lisp" "vendor/emacs-lisp/emacs-lisp"))
      (let ((d (expand-file-name dir elib)))
        (when (file-directory-p d)
          (setq load-path (append load-path (list d))))))))
;; GNU's ert.el and ert-x.el come first.  The other GNU 31.1 files vendored
;; beside them (debug, backtrace, find-func, ewoc, help-mode) go last, so a
;; provider the runtime or bundle already has wins.
(push (expand-file-name "../../vendor/emacs-test/lisp/emacs-lisp"
                        gnu-tests-support-root)
      load-path)
(setq load-path (append load-path
                        (list (expand-file-name "../../vendor/emacs-test/lisp"
                                                gnu-tests-support-root))))

;; backtrace.el reads three font-lock keyword lists from lisp-mode and uses
;; the Emacs Lisp syntax table; a batch ERT run never fontifies.  The
;; bundle's lisp-mode does not define the lists, so give them their GNU
;; type (a list) when nothing else has.
(defvar lisp-el-font-lock-keywords-for-backtraces nil)
(defvar lisp-el-font-lock-keywords-for-backtraces-1 nil)
(defvar lisp-el-font-lock-keywords-for-backtraces-2 nil)
(unless (or (featurep 'lisp-mode) (locate-library "lisp-mode"))
  (defvar emacs-lisp-mode-syntax-table (make-syntax-table))
  (defvar lisp-mode-syntax-table emacs-lisp-mode-syntax-table)
  (provide 'lisp-mode))

;; GNU's loaddefs autoload cl-print; the runtime has no loaddefs, so
;; autoload the entry points ERT and the tests use from this tree's
;; vendored GNU 31.1 cl-print.el (vendor/emacs-lisp-api).
(setq load-path (append load-path
                        (list (expand-file-name "../../vendor/emacs-lisp-api/emacs-lisp"
                                                gnu-tests-support-root))))
(dolist (f '(cl-prin1 cl-prin1-to-string cl-print-to-string-with-limit
             cl-print-object))
  (unless (fboundp f) (autoload f "cl-print")))

;; help-mode.el adds tool-bar buttons at load time; there is no tool bar in
;; batch.
(unless (fboundp 'tool-bar-local-item)
  (defun tool-bar-local-item (&rest _) nil))
(unless (fboundp 'tool-bar-local-item-from-menu)
  (defun tool-bar-local-item-from-menu (&rest _) nil))

;; The bundle's `pp' is a stub that returns a string and ignores STREAM, so
;; ERT's failure report (ert--pp-with-indentation-and-newline) inserts
;; nothing and then fails its own cl-assert.  Load GNU 31.1 pp.el over it.
(load (expand-file-name "../../vendor/emacs-test/lisp/emacs-lisp/pp.el"
                        gnu-tests-support-root)
      nil t)

(require 'ert)
(require 'ert-x)
(load (expand-file-name "gnu-tests-run.el" gnu-tests-support-root) nil t)

(provide 'gnu-tests-support)
;;; gnu-tests-support.el ends here
