;;; broken-pkg.el --- fixture that never calls `provide'  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;;; Commentary:

;; Negative-case fixture for `apps/nemacs-next/scripts/require-smoke.sh'
;; (nemacs init loader reconcile, Phase 1).  This file is a real,
;; loadable file on `load-path' that deliberately forgets
;; `(provide 'broken-pkg)'.  `require' must treat this as a loud
;; failure (an explicit error, or nil under NOERROR) instead of the
;; historical silent "success" where `require' returned the feature
;; symbol even though nothing was ever provided.

;;; Code:

(defvar require-smoke-broken-pkg-loaded t
  "Non-nil once `broken-pkg.el' has executed, proving `load' itself ran.
This file intentionally never calls `provide', so `featurep' for
`broken-pkg' must stay nil even after this variable is set.")

;;; broken-pkg.el ends here
