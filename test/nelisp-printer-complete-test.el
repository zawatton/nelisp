;;; nelisp-printer-complete-test.el --- structural pins: fmt1 printer coverage  -*- lexical-binding: t; -*-

;;; Commentary:

;; Reads the fmt1 printer defuns from the build script and asserts that the
;; whole-object declines for simple shapes are gone.  NELISP_PRINTER_COMPLETE_BUILD
;; points at another copy of the build script (negative control: .seed-build.el).

;;; Code:

(require 'ert)

(defconst nelisp-printer-complete-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(defconst nelisp-printer-complete-test--build
  (or (getenv "NELISP_PRINTER_COMPLETE_BUILD")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        nelisp-printer-complete-test--dir)))

(defun nelisp-printer-complete-test--defun (name)
  (with-temp-buffer
    (insert-file-contents nelisp-printer-complete-test--build)
    (when (re-search-forward (concat "^    (defun " (regexp-quote name) "[ \n]") nil t)
      (goto-char (match-beginning 0))
      (let ((start (point)))
        (forward-sexp)
        (buffer-substring-no-properties start (point))))))

(defun nelisp-printer-complete-test--has (name regexp)
  (let ((text (nelisp-printer-complete-test--defun name)))
    (and text (string-match-p regexp text))))

(ert-deftest nelisp-printer-complete-vector-car-not-declined ()
  "A cons whose car is a vector with a symbol slot 0 is only declined for hash-table."
  (should (nelisp-printer-complete-test--defun "bf_pf_hash_name_p"))
  (should (nelisp-printer-complete-test--has "bf_pf_kind" "bf_pf_hash_name_p"))
  (should-not (nelisp-printer-complete-test--has
               "bf_pf_kind" "(if (= et 4) 9 (if (= et 16) 9 0))")))

(ert-deftest nelisp-printer-complete-backquote-native ()
  "Backquote, comma and comma-at have their own kinds and are not declined."
  (should (nelisp-printer-complete-test--has "bf_pf_kind" "(if (= b0 96) 3 "))
  (should-not (nelisp-printer-complete-test--has "bf_pf_kind" "(if (= b0 96) 9 "))
  (should (nelisp-printer-complete-test--has "bf_pf_cons" "(mut-str-push-byte ms 96)"))
  (should (nelisp-printer-complete-test--has "bf_pf_cons" "(mut-str-push-byte ms 44)"))
  (should (nelisp-printer-complete-test--has "bf_pf_top" "(ptr-write-u64 st 16 0)")))

(ert-deftest nelisp-printer-complete-closure-builtin-still-declined ()
  "Closures and builtins stay with the Lisp printer."
  (should (nelisp-printer-complete-test--has "bf_pf_cons" "(= kind 9) 0"))
  (should (nelisp-printer-complete-test--has "bf_pf_list" "(= (bf_pf_kind cur) 9)")))

(provide 'nelisp-printer-complete-test)
;;; nelisp-printer-complete-test.el ends here
