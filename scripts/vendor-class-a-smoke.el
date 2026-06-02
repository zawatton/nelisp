;;; vendor-class-a-smoke.el --- require Doc 03 class-A vendor candidates  -*- lexical-binding: t; -*-

;; This file is loaded by `make verify-vendor-class-a' under the NeLisp
;; CLI.  It assumes the caller already set load-path and loaded
;; `emacs-init' so the Layer 2 substrate is present.

;;; Code:

(defvar vendor-class-a-smoke-modules
  '(case-table cdl backquote (lisp-float-type . "emacs-lisp/float-sup")
    hex-util lisp map-ynp range regi
    charprop charscript emoji-labels iso-transl cp51932 eucjp-ms
    fontset idna-mapping ja-dic-utl)
  "Class-A vendor modules from the generated Doc 03 inventory.

The first smoke lane intentionally starts with files that have no
static `(require ...)' edges in `docs/design/03-vendor-inventory.csv'.
Broader user-facing candidates such as `files' and `dired' depend on
Class-C/D bootstrap features and belong in later strict gates.")

(defvar vendor-class-a-smoke-default-limit 18
  "Default number of modules to smoke when no environment override exists.")

(defvar vendor-class-a-smoke-strict nil
  "Non-nil means signal an error when any smoke require fails.")

(defun vendor-class-a-smoke--env-number (name default)
  "Return numeric env var NAME, or DEFAULT."
  (let ((value (and (fboundp 'getenv) (getenv name))))
    (if (and value (not (string= value "")))
        (string-to-number value)
      default)))

(defun vendor-class-a-smoke--env-flag-p (name)
  "Return non-nil when env var NAME is set to 1, t, or yes."
  (let ((value (and (fboundp 'getenv) (getenv name))))
    (and value (member value '("1" "t" "true" "yes")))))

(defun vendor-class-a-smoke--strict-p ()
  "Return non-nil when the smoke should fail on module errors."
  (or vendor-class-a-smoke-strict
      (vendor-class-a-smoke--env-flag-p "VENDOR_CLASS_A_STRICT")))

(defun vendor-class-a-smoke--selected-modules ()
  "Return modules to smoke based on VENDOR_CLASS_A_LIMIT.
A limit of 0 means the full candidate list.  The default is 18 so
`make verify-vendor' stays usable while the cold-load path is still
slow."
  (let ((limit (vendor-class-a-smoke--env-number
                "VENDOR_CLASS_A_LIMIT"
                vendor-class-a-smoke-default-limit))
        (modules vendor-class-a-smoke-modules)
        selected)
    (if (<= limit 0)
        modules
      (while (and modules (> limit 0))
        (push (car modules) selected)
        (setq modules (cdr modules)
              limit (1- limit)))
      (nreverse selected))))

(defun vendor-class-a-smoke--entry-feature (entry)
  "Return feature symbol for smoke ENTRY."
  (if (consp entry) (car entry) entry))

(defun vendor-class-a-smoke--entry-file (entry)
  "Return optional load filename for smoke ENTRY."
  (if (consp entry) (cdr entry) nil))

(defun vendor-class-a-smoke--require-one (entry)
  "Require smoke ENTRY and return (FEATURE STATUS DETAIL)."
  (let ((feature (vendor-class-a-smoke--entry-feature entry))
        (filename (vendor-class-a-smoke--entry-file entry)))
    (condition-case err
        (progn
          (require feature filename)
          (list feature 'pass ""))
      (error
       (list feature 'fail (format "%S" err))))))

(defun vendor-class-a-smoke-batch ()
  "Run the class-A vendor require smoke.
  By default this is a baseline report and does not fail on module
failures.  Set VENDOR_CLASS_A_STRICT=1 to turn failures into a hard
gate."
  (let ((failures 0)
        (modules (vendor-class-a-smoke--selected-modules))
        results)
    (dolist (entry modules)
      (let ((result (vendor-class-a-smoke--require-one entry)))
        (push result results)
        (when (eq (cadr result) 'fail)
          (setq failures (1+ failures)))
        (princ (format "vendor-class-a module=%S status=%S detail=%s\n"
                       (car result) (cadr result) (caddr result)))))
    (princ (format "vendor-class-a-summary total=%d candidates=%d failures=%d strict=%S\n"
                   (length modules) (length vendor-class-a-smoke-modules)
                   failures (vendor-class-a-smoke--strict-p)))
    (when (and (> failures 0)
               (vendor-class-a-smoke--strict-p))
      (error "vendor class-A smoke failed: %d/%d"
             failures (length modules)))
    (nreverse results)))

(provide 'vendor-class-a-smoke)

;;; vendor-class-a-smoke.el ends here
