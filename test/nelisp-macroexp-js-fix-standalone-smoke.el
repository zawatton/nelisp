;;; nelisp-macroexp-js-fix-standalone-smoke.el --- macroexp/pcase load smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This smoke runs on the standalone binary because host ERT already has
;; Emacs's own `pcase' and `macroexp' implementations.  The vendor load
;; path is derived from this file's repository location so the probe remains
;; usable from any checkout.

;;; Code:

(let* ((source-file (or load-file-name buffer-file-name))
       (source-file (and source-file
                         (expand-file-name source-file default-directory)))
       (test-dir (and source-file (file-name-directory source-file)))
       (repo-root (and test-dir
                       (file-name-directory
                        (directory-file-name test-dir))))
       ;; The standalone loader currently leaves `load-file-name' nil.  A
       ;; caller outside the checkout can therefore provide the checkout
       ;; root explicitly; normal in-tree invocations need no environment.
       (repo-root (or (getenv "NELISP_REPO_ROOT")
                      repo-root
                      (expand-file-name "." default-directory)))
       (dev-root (and repo-root
                      (file-name-directory
                       (directory-file-name repo-root))))
       (lib-root (and dev-root (concat dev-root "nelisp-emacs-lib/"))))
  (load (expand-file-name "scripts/nelisp-ert-shim.el" repo-root))
  ;; `define-obsolete-function-alias' is supplied by the larger Emacs
  ;; bootstrap image, while this focused smoke loads only macroexp.el.
  ;; Keep the test's dependency surface minimal with the same alias effect.
  (unless (fboundp 'define-obsolete-function-alias)
    (defmacro define-obsolete-function-alias (old new &optional _when _doc)
      `(defalias ,old ,new)))
  (defvar macroexp--dynvars nil)
  (defvar load-in-progress nil)
  (dolist (dir '("vendor/emacs-lisp"
                 "vendor/emacs-lisp/emacs-lisp"
                 "vendor/emacs-lisp/progmodes"))
    (let ((path (and lib-root (expand-file-name dir lib-root))))
      (when (and path (file-directory-p path))
        (add-to-list 'load-path path))))
  ;; Resolve the source explicitly as a fallback for the standalone loader:
  ;; its `locate-file' implementation does not search every dynamically
  ;; added directory during the first load.
  (load (expand-file-name
         "vendor/emacs-lisp/emacs-lisp/macroexp.el" lib-root))

  (ert-deftest nelisp-macroexp-js-fix/pcase-or-binding-fallback-nil ()
    "Nested `or' keeps bindings and makes the fallback projections nil."
    (should (equal
             (pcase (cons 1 nil)
               ((cons (or `(,a . ,b) pcase--dontcare) _) (list a b)))
             '(nil nil)))
    (should (equal
             (pcase (cons (cons 1 2) nil)
               ((cons (or `(,a . ,b) pcase--dontcare) _) (list a b)))
             '(1 2))))

  (ert-deftest nelisp-macroexp-js-fix/macroexpand-all-let ()
    "The vendor macroexpander handles a simple let after bootstrap."
    (should (equal (macroexpand-all '(let ((x 1)) x))
                   '(let ((x 1)) x))))

  (ert-deftest nelisp-macroexp-js-fix/exact-macroexp-pcase-pattern ()
    "The vendor's exact let matcher keeps only bindings and body."
    (should (equal
             (pcase '(let ((x 1)) x)
               (`(,(and fun (or 'let 'let*))
                  . ,(or `(,bindings . ,body) pcase--dontcare))
                (list bindings body)))
             '(((x 1)) (x)))))

  (let* ((result (nelisp-ert-run-all "nelisp-macroexp-js-fix"))
         (pass (nth 0 result))
         (fail (nth 1 result)))
    (princ (format "GATE-COUNT checked=%d findings=%d\n" (+ pass fail) fail))
    (if (> fail 0)
        (error "nelisp-macroexp-js-fix: %d failure(s), %d passed" fail pass)
      (princ (format "nelisp-macroexp-js-fix: PASS (%d tests)\n" pass)))))

;;; nelisp-macroexp-js-fix-standalone-smoke.el ends here
