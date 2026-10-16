;;; diag-f1b-wrap.el --- run the F1B driver after adjusting the collector  -*- lexical-binding: t; -*-
;; Diagnostic wrapper for test/support/diag-windows-f1b.py (not a gate).
;; DIAG_WRAP=threshold  raise `gc-cons-threshold' so allocation never triggers a
;;                      collection, then load the F1B driver unchanged.
(when (equal (getenv "DIAG_WRAP") "threshold")
  (setq gc-cons-threshold most-positive-fixnum)
  (when (boundp 'gc-cons-percentage) (setq gc-cons-percentage 1000.0))
  (princ (format "DIAG-WRAP threshold=%S\n" gc-cons-threshold)))
(load (expand-file-name "test/standalone-native-funcall-v2-exits-driver.el"
                        (file-name-directory (directory-file-name (file-name-directory load-file-name))))
      nil t)
;;; diag-f1b-wrap.el ends here
