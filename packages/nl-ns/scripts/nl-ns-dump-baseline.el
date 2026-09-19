;;; nl-ns-dump-baseline.el --- Dump a host baseline for nl-ns -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run under a real Emacs:
;;
;;   emacs -Q --batch -l scripts/nl-ns-dump-baseline.el \
;;     --eval "(nl-ns-dump-baseline-write \"baseline/emacs-30.2.el\")"
;;
;; This writes the FULL dump (every `fboundp' symbol, every `boundp'
;; symbol, every library basename on `load-path').  The checked-in
;; baseline is a curated subset of that output, not the output itself;
;; see README.org, "Host baseline", before overwriting it.

;;; Code:

(require 'cl-lib)

(dolist (feature '(cl-lib cl-macs cl-extra cl-seq seq subr-x map pcase))
  (require feature nil t))

(defun nl-ns-dump-baseline--sorted-symbols (predicate)
  "Return interned symbols satisfying PREDICATE, sorted by name."
  (let (out)
    (mapatoms
     (lambda (sym)
       (when (funcall predicate sym)
         (push sym out))))
    (sort out (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun nl-ns-dump-baseline--library-names ()
  "Return reachable library basenames on `load-path'."
  (let ((seen (make-hash-table :test 'equal))
        out)
    (dolist (dir load-path)
      (when (and (stringp dir) (file-directory-p dir))
        (dolist (file (directory-files dir nil "\\.elc?$"))
          (let ((name (file-name-base file)))
            (unless (gethash name seen)
              (puthash name t seen)
              (push name out))))))
    (sort out #'string<)))

(defun nl-ns-dump-baseline-data ()
  "Return a plist describing the current host runtime."
  (list :emacs-version emacs-version
        :generated-at (format-time-string "%Y-%m-%d")
        :functions (nl-ns-dump-baseline--sorted-symbols #'fboundp)
        :variables (nl-ns-dump-baseline--sorted-symbols #'boundp)
        :libraries (nl-ns-dump-baseline--library-names)))

(defun nl-ns-dump-baseline-write (path)
  "Write the current host baseline to PATH."
  (with-temp-file path
    (let ((print-length nil)
          (print-level nil))
      (prin1 (nl-ns-dump-baseline-data) (current-buffer))
      (insert "\n"))))

(provide 'nl-ns-dump-baseline)

;;; nl-ns-dump-baseline.el ends here
