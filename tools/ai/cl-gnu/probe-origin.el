;;; probe-origin.el --- check which file serves a feature  -*- lexical-binding: t; -*-
;; Usage: BIN --load tools/ai/cl-gnu/probe-origin.el -- FEATURE=PATH-SUBSTRING...
;; Prints OK/FAIL per argument (via `locate-library') and PROBE-ORIGIN-PASS
;; iff every located path contains its substring.
(let ((ok t))
  (dolist (arg (cdr (member "--" command-line-args)))
    (let* ((i (string-search "=" arg))
           (feature (substring arg 0 i))
           (want (substring arg (1+ i)))
           (got (condition-case nil (locate-library feature) (error nil))))
      (if (and got (string-search want got))
          (princ (format "OK %s %s\n" feature got))
        (setq ok nil)
        (princ (format "FAIL %s want=%s got=%S\n" feature want got)))))
  (princ (if ok "PROBE-ORIGIN-PASS\n" "PROBE-ORIGIN-FAIL\n")))
