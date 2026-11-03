;;; elprop-run.el --- property-comparison runner (Doc 03 §6.3) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Evaluate each form in `elprop-forms' on (a) this host Emacs (the oracle)
;; and (b) the vendored NeLisp binary, then compare the printed results.
;; Reader sugar (`#'' / `'') is normalized away when feeding NeLisp so the
;; comparison reflects semantics, not reader-feature coverage.
;;
;; Env: NEMACS_NELISP (binary), ELPROP_FORMS (corpus file), ELPROP_STRICT
;; (non-zero exit on any mismatch).

;;; Code:

(require 'cl-lib)

(defun elprop-run--repo-root ()
  "Return the repository root."
  (let* ((origin (or load-file-name buffer-file-name default-directory))
         (dir (file-name-directory (expand-file-name origin))))
    (if (file-exists-p (expand-file-name "vendor/emacs-lisp/format-spec.el" dir))
        dir
      (expand-file-name ".." dir))))

(defun elprop-run--nelisp-bin (root)
  "Return the NeLisp binary path, preferring the vendored build."
  (or (getenv "NEMACS_NELISP")
      (let ((v (expand-file-name "vendor/nelisp/target/nelisp" root)))
        (and (file-executable-p v) v))
      (let ((e (expand-file-name "build/nelisp-experiment" root)))
        (and (file-executable-p e) e))))

(defun elprop-run--forms-file (root)
  "Return the corpus file path."
  (or (getenv "ELPROP_FORMS")
      (expand-file-name "test/elprop-forms-auto.el" root)))

(defun elprop-run--host-eval (form)
  "Return (STATUS . OUTPUT) for FORM on host Emacs."
  (condition-case e
      (cons 'value (prin1-to-string (eval form t)))
    (error (cons 'error (format "%s" (car e))))))

(defun elprop-run--nelisp-eval (bin form)
  "Return (STATUS . OUTPUT) for FORM on the NeLisp BIN."
  (with-temp-buffer
    (let* ((src (let ((print-quoted nil)) (prin1-to-string form)))
           (rc (call-process bin nil t nil "--eval" src))
           (out (string-trim (buffer-string))))
      (if (and (integerp rc) (= rc 0))
          (cons 'value out)
        (cons 'error out)))))

(defun elprop-run--csv-escape (s)
  "Return S quoted for CSV."
  (concat "\"" (replace-regexp-in-string "\"" "\"\"" (or s "")) "\""))

(defun elprop-run-batch ()
  "Run the property comparison and report a summary + CSV."
  (let* ((root (elprop-run--repo-root))
         (bin (elprop-run--nelisp-bin root))
         (forms-file (elprop-run--forms-file root))
         (out-csv (expand-file-name "build/elprop-results.csv" root))
         (total 0) (match 0) (mismatch 0) (nelisp-err 0)
         rows)
    (unless bin (error "elprop-run: no NeLisp binary (set NEMACS_NELISP)"))
    (load forms-file nil t)
    (make-directory (file-name-directory out-csv) t)
    (dolist (entry (symbol-value 'elprop-forms))
      (let* ((id (plist-get entry :id))
             (type (or (plist-get entry :type) 'value))
             (form (plist-get entry :form))
             (h (elprop-run--host-eval form))
             (n (elprop-run--nelisp-eval bin form))
             (ok (if (eq type 'error)
                     (and (eq (car h) 'error) (eq (car n) 'error))
                   (and (eq (car h) 'value) (eq (car n) 'value)
                        (string= (cdr h) (cdr n))))))
        (setq total (1+ total))
        (cond (ok (setq match (1+ match)))
              ((and (not (eq type 'error)) (eq (car n) 'error))
               (setq nelisp-err (1+ nelisp-err)))
              (t (setq mismatch (1+ mismatch))))
        (push (list id (symbol-name type) (if ok "ok" "FAIL")
                    (format "%s:%s" (car h) (cdr h))
                    (format "%s:%s" (car n) (cdr n)))
              rows)
        (unless ok
          (princ (format "MISMATCH %s  form=%S\n  host=%s:%S  nelisp=%s:%S\n"
                         id form (car h) (cdr h) (car n) (cdr n))))))
    (setq rows (nreverse rows))
    (with-temp-file out-csv
      (insert "id,type,result,host,nelisp\n")
      (dolist (r rows)
        (insert (mapconcat #'elprop-run--csv-escape r ",") "\n")))
    (let ((pct (if (> total 0) (/ (* 100.0 match) total) 0.0)))
      (princ (format "ELPROP total=%d match=%d mismatch=%d nelisp-error=%d pass=%.1f%%\n"
                     total match mismatch nelisp-err pct))
      (princ (format "ELPROP csv=%s\n" out-csv))
      (princ (format "ELPROP nelisp-bin=%s\n" bin))
      (when (and (getenv "ELPROP_STRICT") (< match total))
        (kill-emacs 1)))))

(provide 'elprop-run)
;;; elprop-run.el ends here
