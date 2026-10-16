;;; diag-f1b-wrap.el --- run the F1B driver after adjusting the runtime  -*- lexical-binding: t; -*-
;; Diagnostic wrapper for test/support/diag-windows-f1b.py (not a gate).
;; DIAG_WRAP is a comma-separated list of:
;;   threshold  raise `gc-cons-threshold' so allocation never triggers a collection;
;;   nomidform  disarm the mid-form collector, (nelisp--debug-switch 6);
;;   nomxcache  make every macroexpansion-cache lookup miss, (nelisp--debug-switch 13).
;; Then the F1B driver is loaded unchanged.
(let ((modes (split-string (or (getenv "DIAG_WRAP") "") "," t)))
  (when (member "threshold" modes)
    (setq gc-cons-threshold most-positive-fixnum)
    (when (boundp 'gc-cons-percentage) (setq gc-cons-percentage 1000.0)))
  (when (member "nomidform" modes) (nelisp--debug-switch 6))
  (when (member "nomxcache" modes) (nelisp--debug-switch 13))
  (princ (format "DIAG-WRAP modes=%S\n" modes)))
(load (expand-file-name "test/standalone-native-funcall-v2-exits-driver.el" default-directory) nil t)
;;; diag-f1b-wrap.el ends here
