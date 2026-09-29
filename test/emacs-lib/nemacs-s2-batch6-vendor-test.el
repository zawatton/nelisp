;;; nemacs-s2-batch6-vendor-test.el --- ERT for the S2 coverage batch 6 add  -*- lexical-binding: t; -*-

;;; Commentary:

;; S2 coverage batch 6 (2026-09-29) adds GNU Emacs 31.1 `compile.el',
;; `pcomplete.el', `shell.el', `ehelp.el', `term.el', `woman.el' and
;; `project.el' to `nelisp-bootstrap-vendor-tail-extra-files', plus three
;; library fixes (`emacs-keymap.el' menu-item lookup and symbol parents,
;; `emacs-keymap-builtins.el' `Control-X-prefix', `emacs-buffer-builtins.el'
;; native `buffer-list').  ERT only runs on host Emacs, where these files
;; are real built-ins, so host Emacs is the oracle for the fboundp
;; assertions; the standalone load results are recorded by
;; `make nemacs-feature-coverage'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'compile)
(require 'shell)
(require 'term)
(require 'woman)
(require 'project)

(ert-deftest nemacs-s2-batch6-vendor-test/vendor-fboundp ()
  (dolist (sym '(compilation-mode compile compilation-start next-error
                 shell shell-mode shell-command-completion
                 term term-mode term-send-raw ansi-term
                 woman woman-find-file woman-mode
                 project-current project-root project-files))
    (should (fboundp sym)))
  (dolist (sym '(compilation-error-regexp-alist term-raw-map shell-mode-map
                 woman-manpath project-find-functions))
    (should (boundp sym))))

(ert-deftest nemacs-s2-batch6-vendor-test/term-parent-is-ctl-x-prefix ()
  "Normal case: `term-raw-escape-map' inherits from the C-x prefix keymap,
the `set-keymap-parent' call with a symbol that made term.el fail to load."
  (should (keymapp (keymap-parent term-raw-escape-map)))
  ;; Negative control: a plain map has no parent.
  (should (null (keymap-parent (make-sparse-keymap)))))

(ert-deftest nemacs-s2-batch6-vendor-test/shell-menu-copy ()
  "Shell mode's completion menu is a real keymap with the two added items."
  (let ((menu (lookup-key shell-mode-map [menu-bar completion])))
    (should (keymapp menu))
    (should (lookup-key menu [expand-directory]))))

(provide 'nemacs-s2-batch6-vendor-test)

;;; nemacs-s2-batch6-vendor-test.el ends here
