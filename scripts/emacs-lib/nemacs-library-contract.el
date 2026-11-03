;;; nemacs-library-contract.el --- verify public library contract -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate a small, stable contract artifact for external consumers of the
;; `nelisp-emacs' facade.  This is narrower than the public API inventory:
;; it lists only the symbols consumers may rely on after requiring
;; `nelisp-emacs'.

;;; Code:

(require 'nelisp-emacs)

(defvar nemacs-library-contract-output
  (expand-file-name "build/nemacs-library-contract.tsv"
                    (expand-file-name ".." (file-name-directory
                                            (or load-file-name
                                                buffer-file-name))))
  "TSV output path.")

(defvar nemacs-library-contract-summary-output
  (expand-file-name "build/nemacs-library-contract.org"
                    (expand-file-name ".." (file-name-directory
                                            (or load-file-name
                                                buffer-file-name))))
  "Org summary output path.")

(defconst nemacs-library-contract--symbols
  '((feature nelisp-emacs)
    (variable nelisp-emacs-library-contract-version)
    (function nelisp-emacs-library-package-names)
    (function nelisp-emacs-library-package)
    (function nelisp-emacs-library-package-features)
    (function nelisp-emacs-library-package-lazy-features)
    (function nelisp-emacs-library-package-manifest)
    (function nelisp-emacs-library-stable-api-manifest)
    (function nelisp-emacs-library-stable-api-symbols)
    (function nelisp-emacs-library-stable-api-entry)
    (function nelisp-emacs-library-stable-lazy-api-manifest)
    (function nelisp-emacs-library-stable-lazy-api-symbols)
    (function nelisp-emacs-library-stable-lazy-api-entry))
  "External consumer contract symbols.")

(defun nemacs-library-contract--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-contract--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-contract--tsv-cell cells "\t"))

(defun nemacs-library-contract--status (kind symbol)
  "Return verification status for contract SYMBOL of KIND."
  (cond
   ((eq kind 'feature) (if (featurep symbol) "ok" "fail"))
   ((eq kind 'variable) (if (boundp symbol) "ok" "fail"))
   ((eq kind 'function) (if (fboundp symbol) "ok" "fail"))
   (t "fail")))

(defun nemacs-library-contract--details (kind symbol)
  "Return details for contract SYMBOL of KIND."
  (cond
   ((and (eq kind 'variable) (boundp symbol))
    (format "%S" (symbol-value symbol)))
   ((and (eq kind 'function) (fboundp symbol))
    (or (documentation symbol t) ""))
   ((eq kind 'feature)
    "facade feature loaded by require")
   (t "")))

(defun nemacs-library-contract--rows ()
  "Return external consumer contract rows."
  (mapcar
   (lambda (entry)
     (let ((kind (nth 0 entry))
           (symbol (nth 1 entry)))
       (list kind
             symbol
             (nemacs-library-contract--status kind symbol)
             (nemacs-library-contract--details kind symbol))))
   nemacs-library-contract--symbols))

(defun nemacs-library-contract--write-tsv (rows output)
  "Write contract ROWS to TSV OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert
     (nemacs-library-contract--row "kind" "symbol" "status" "details")
     "\n")
    (dolist (row rows)
      (insert (apply #'nemacs-library-contract--row row) "\n"))))

(defun nemacs-library-contract--write-summary (rows output)
  "Write contract ROWS summary to OUTPUT."
  (let ((failures 0))
    (dolist (row rows)
      (when (equal (nth 2 row) "fail")
        (setq failures (1+ failures))))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: nemacs library consumer contract\n\n")
      (insert (format "* Summary\n\n- symbols: %d\n- failures: %d\n\n"
                      (length rows)
                      failures))
      (insert "* Contract symbols\n\n")
      (insert "| Kind | Symbol | Status |\n")
      (insert "|------+--------+--------|\n")
      (dolist (row rows)
        (insert (format "| %s | =%s= | %s |\n"
                        (nth 0 row)
                        (nth 1 row)
                        (nth 2 row))))
      (insert "\n* Notes\n\n")
      (insert "- This artifact is the narrow external consumer contract after requiring =nelisp-emacs=.\n")
      (insert "- The broader public API inventory remains advisory until package owners document, test, and add a symbol to the stable package API manifest.\n"))))

;;;###autoload
(defun nemacs-library-contract-batch ()
  "Write and verify external consumer contract artifacts."
  (let* ((rows (nemacs-library-contract--rows))
         (failures 0))
    (dolist (row rows)
      (when (equal (nth 2 row) "fail")
        (setq failures (1+ failures))))
    (nemacs-library-contract--write-tsv rows nemacs-library-contract-output)
    (nemacs-library-contract--write-summary
     rows nemacs-library-contract-summary-output)
    (princ
     (format
      "nemacs-library-contract: symbols=%d failures=%d output=%s summary=%s\n"
      (length rows)
      failures
      nemacs-library-contract-output
      nemacs-library-contract-summary-output))
    (when (> failures 0)
      (kill-emacs 1))))

(provide 'nemacs-library-contract)

;;; nemacs-library-contract.el ends here
