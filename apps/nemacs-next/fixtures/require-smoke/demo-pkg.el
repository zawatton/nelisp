;;; demo-pkg.el --- macro-free fixture package for require-smoke  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;;; Commentary:

;; Fixture consumed by `apps/nemacs-next/scripts/require-smoke.sh' (nemacs
;; init loader reconcile, Phase 1).  Deliberately avoids `defmacro',
;; `cl-lib', and `define-inline' so it loads through the bare NeLisp
;; reader without any host-Emacs macro-expansion pre-pass; it exercises
;; only the plain `defvar'/`defun'/`provide' substrate that
;; `emacs-fns.el' require/provide/featurep polyfills must resolve.

;;; Code:

(defvar require-smoke-demo-pkg-loaded t
  "Non-nil once `demo-pkg' has been loaded.")

(defun require-smoke-demo-pkg-greeting ()
  "Return a fixed marker string proving `demo-pkg' functions are callable.
The body wraps the literal in `identity' rather than returning the bare
string: the standalone NeLisp runtime currently mis-parses a `defun'
whose entire body is a single string literal as an (elided) docstring
and returns nil instead (pre-existing runtime quirk, out of scope for
this fixture -- tracked separately, not something this Phase 1 change
touches)."
  (identity "demo-pkg-hello"))

(provide 'demo-pkg)

;;; demo-pkg.el ends here
