;;; nelisp-printer-fastpath-test.el --- structural pins for the fmt1 printer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Structural tests: read defuns of the fmt1 native printer from the build
;; script and assert on their shape.  NELISP_PRINTER_FASTPATH_BUILD points at
;; another copy of the build script.

;;; Code:

(require 'ert)

(defconst nelisp-printer-fastpath-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(defun nelisp-printer-fastpath-test--build ()
  (or (getenv "NELISP_PRINTER_FASTPATH_BUILD")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        nelisp-printer-fastpath-test--dir)))

(defun nelisp-printer-fastpath-test--defun (name)
  "Return the defun text of NAME from the build script, or nil."
  (with-temp-buffer
    (insert-file-contents (nelisp-printer-fastpath-test--build))
    (when (re-search-forward (concat "^    (defun " (regexp-quote name) "[ \n]") nil t)
      (goto-char (match-beginning 0))
      (let ((start (point)))
        (forward-sexp)
        (buffer-substring-no-properties start (point))))))

(defun nelisp-printer-fastpath-test--has (name regexp)
  (let ((text (nelisp-printer-fastpath-test--defun name)))
    (and text (string-match-p regexp text))))

(ert-deftest nelisp-printer-fastpath-escape-vars-do-not-decline ()
  "escape-nonascii, escape-multibyte and gensym no longer force a decline."
  (let ((text (nelisp-printer-fastpath-test--defun "bf_pf_vars_ok")))
    (should text)
    (should (string-match-p "print-escape-nonascii" text))
    (should (string-match-p "print-escape-multibyte" text))
    (should (string-match-p "print-gensym" text))
    ;; They feed flags, not the decline chain: no "0 0" decline arm for them.
    (dolist (v '("print-escape-nonascii" "print-escape-multibyte" "print-gensym"))
      (should-not (string-match-p
                   (concat (regexp-quote v) " env 0)) 0) 0 (if") text)))
    (should (string-match-p (regexp-quote "(+ 8 flags)") text))))

(ert-deftest nelisp-printer-fastpath-strings-ascii-only-in-escape-mode ()
  (should (nelisp-printer-fastpath-test--defun "bf_pf_strict_ascii_p"))
  (should (nelisp-printer-fastpath-test--has "bf_pf_str" "bf_pf_strict_ascii_p st"))
  (should (nelisp-printer-fastpath-test--has "bf_pf_str" "(defun bf_pf_str (ms s esc st)")))

(ert-deftest nelisp-printer-fastpath-symbols-interned-under-gensym ()
  (should (nelisp-printer-fastpath-test--defun "bf_pf_strict_interned_p"))
  (should (nelisp-printer-fastpath-test--has "bf_pf_sym" "bf_pf_strict_interned_p st"))
  (should (nelisp-printer-fastpath-test--has "bf_pf_sym" "(nl_val_tag a) 4")))

(ert-deftest nelisp-printer-fastpath-tick-budget-covers-large-acyclic-data ()
  "The tick budget lets the native printer finish large acyclic data."
  (should (nelisp-printer-fastpath-test--has "bf_pf_top" "(ptr-write-u64 st 0 16777216)"))
  (should-not (nelisp-printer-fastpath-test--has "bf_pf_top" "(ptr-write-u64 st 0 100000)")))

(provide 'nelisp-printer-fastpath-test)
;;; nelisp-printer-fastpath-test.el ends here
