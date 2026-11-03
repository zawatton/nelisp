;;; eval-region-candidate.el --- Reload the guarded source for a focused probe -*- lexical-binding: t; -*-
(fmakunbound 'eval-region)
(load (expand-file-name "packages/nelisp-emacs-foundation/src/emacs-cc-eval-region-1.el") nil t)
