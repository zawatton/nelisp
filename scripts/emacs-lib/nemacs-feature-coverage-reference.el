;;; nemacs-feature-coverage-reference.el --- what Emacs's own copy of a feature defines -*- lexical-binding: t; -*-

;;; Commentary:

;; Half of a coverage measurement (see scripts/nemacs-feature-coverage.sh).
;; This half runs under HOST Emacs and answers, for each feature name the
;; substrate claims to provide: which functions and variables does Emacs's
;; own copy of that feature define?
;;
;; The list comes from `load-history', not from an obarray diff.  Emacs
;; records, per loaded file, exactly what that file defined and which feature
;; it provided, so the attribution is the loader's own rather than a guess
;; about who interned what.  Requiring everything into one process is
;; therefore safe: entries are keyed by file, and each file names its own
;; feature.
;;
;; Functions and variables are kept, which is the surface a caller can hit.
;; Names are reported as-is, private `--' ones included: a stub that omits a
;; private helper its own public functions call is just as broken as one that
;; omits the public name, and pretending otherwise would flatter the result.
;;
;; The entry shapes matter and are easy to get backwards.  In `load-history'
;; a BARE SYMBOL is a VARIABLE, and a function is `(defun . NAME)'.  There is
;; no `(defvar . NAME)' shape at all -- measured across simple, isearch,
;; term, dired and comint on 2026-09-12: 5880 `defun', 2904 bare symbols,
;; 252 `defface', 110 `cl-defmethod', and ZERO `defvar'.
;;
;; This file had it inverted until then: bare symbols were emitted as "fn",
;; so every variable was probed with `fboundp' and counted missing even where
;; the substrate defines it.  `kill-ring' is the clean example -- in host
;; Emacs itself `(fboundp 'kill-ring)' is nil and `(boundp 'kill-ring)' is t.
;; The inflated figure that produced was 4542 missing / 966 present.
;;
;; `defface' is skipped rather than guessed at: a face is neither `fboundp'
;; nor `boundp', so calling it either kind manufactures a miss that says
;; nothing about the port.

;;; Code:

(defun nemacs-feature-coverage-reference--features ()
  "Read the requested feature names, one per line, from the input file."
  (let ((path (getenv "NEMACS_FEATURE_COVERAGE_INPUT"))
        (names nil))
    (unless (and path (file-readable-p path))
      (error "nemacs-feature-coverage-reference: cannot read %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
          (when (> (length line) 0)
            (setq names (cons (intern line) names))))
        (forward-line 1)))
    (nreverse names)))

(defun nemacs-feature-coverage-reference-batch ()
  "Write feature/kind/name rows for every requestable feature."
  (let* ((requested (nemacs-feature-coverage-reference--features))
         (out (getenv "NEMACS_FEATURE_COVERAGE_REFERENCE"))
         (status-out (getenv "NEMACS_FEATURE_COVERAGE_STATUS"))
         (available nil))
    (unless (and out status-out)
      (error "nemacs-feature-coverage-reference: output paths are unset"))
    (dolist (feature requested)
      ;; `require' with NOERROR: a name the substrate invented (emacs-keymap)
      ;; or one this Emacs does not ship simply has no reference, which is a
      ;; fact about the comparison, not a failure of it.
      (when (ignore-errors (require feature nil t))
        (setq available (cons feature available))))
    (setq available (nreverse available))
    (let ((coding-system-for-write 'utf-8-unix)
          (rows nil))
      (dolist (entry load-history)
        (let* ((provided (catch 'found
                           (dolist (item (cdr entry))
                             (when (and (consp item) (eq (car item) 'provide))
                               (throw 'found (cdr item))))
                           nil)))
          (when (and provided (memq provided available))
            (dolist (item (cdr entry))
              (cond
               ;; A bare symbol in `load-history' is a VARIABLE.  See the
               ;; commentary: this was emitted as "fn" until 2026-09-12,
               ;; which probed every variable with `fboundp'.
               ((symbolp item)
                (setq rows (cons (list provided "var" item) rows)))
               ((and (consp item) (eq (car item) 'defun))
                (setq rows (cons (list provided "fn" (cdr item)) rows)))
               ;; A generic's own name is `fboundp' once any method defines
               ;; it, so callers can hit it exactly like a `defun'.
               ((and (consp item) (eq (car item) 'cl-defmethod))
                (setq rows (cons (list provided "fn" (cdr item)) rows)))
               ;; `provide'/`require' are features, `defface' is a face, and
               ;; `define-type'/`define-symbol-props' are neither a function
               ;; nor a variable.  None of them is a name a caller binds or
               ;; calls, so none is counted.
               )))))
      (with-temp-buffer
        (dolist (row (nreverse rows))
          (insert (format "%s\t%s\t%s\n" (nth 0 row) (nth 1 row) (nth 2 row))))
        (write-region (point-min) (point-max) out nil 'silent))
      (with-temp-buffer
        (dolist (feature requested)
          (insert (format "%s\t%s\n" feature
                          (if (memq feature available) "available" "no-reference"))))
        (write-region (point-min) (point-max) status-out nil 'silent)))
    (message "nemacs-feature-coverage-reference: %d requested, %d with a reference"
             (length requested) (length available))))

(provide 'nemacs-feature-coverage-reference)

;;; nemacs-feature-coverage-reference.el ends here
