;;; nemacs-next-m5-fixture-extra.el --- M5 companion fixture -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Companion feature used by the Doc 31 M5 package smoke to prove more
;; than one pure-Elisp fixture feature can be loaded without app/bootstrap.

;;; Code:

(require 'nemacs-next-m5-fixture)

(defconst nemacs-next-m5-fixture-extra-capability 'companion-loaded
  "Marker exported by the companion package fixture.")

(defun nemacs-next-m5-fixture-extra-loaded-p ()
  "Return non-nil when the companion package fixture is loaded."
  (eq nemacs-next-m5-fixture-extra-capability 'companion-loaded))

(provide 'nemacs-next-m5-fixture-extra)

;;; nemacs-next-m5-fixture-extra.el ends here
