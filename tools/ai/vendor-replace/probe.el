;;; probe.el --- Is FEATURE served by its vendored GNU file?  -*- lexical-binding: t; -*-

;; Usage (from the repository root):
;;   $VENDOR_BIN --load tools/ai/vendor-replace/probe.el -- FEATURE LIBRARY KEYFN
;; Prints PROBE-VENDOR-PASS when `(require FEATURE)' succeeds, LIBRARY
;; resolves to a file under a vendor/ directory, and KEYFN (a public GNU
;; function of that library) is defined.

;;; Code:

(let* ((args (cdr (member "--" command-line-args)))
       (feature (intern (nth 0 args)))
       (library (nth 1 args))
       (keyfn (intern (nth 2 args)))
       (err (condition-case e (progn (require feature) nil) (error e)))
       (path (locate-library library)))
  (princ
   (cond (err (format "PROBE-VENDOR-FAIL require: %.120S\n" err))
         ((not (and path (string-match-p "/vendor/" path)))
          (format "PROBE-VENDOR-FAIL %s resolves to %s\n" library path))
         ((not (fboundp keyfn)) (format "PROBE-VENDOR-FAIL %s undefined\n" keyfn))
         (t "PROBE-VENDOR-PASS\n"))))

;;; probe.el ends here
