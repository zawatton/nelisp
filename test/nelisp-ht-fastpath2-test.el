;;; nelisp-ht-fastpath2-test.el --- structural pins for direct-string hash lookup  -*- lexical-binding: t; -*-

;;; Commentary:

;; Pins the shape of the direct-string `equal' hash-table fast path: one shared
;; byte hash, header-based reads for direct strings, and a header-level key
;; comparison in `nl_ht_fast_find'.  NELISP_HT2_BUILD overrides the build
;; script path (negative control against the pristine seed).

;;; Code:

(require 'ert)

(defconst nelisp-ht-fastpath2-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(defconst nelisp-ht-fastpath2-test--build
  (or (getenv "NELISP_HT2_BUILD")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        nelisp-ht-fastpath2-test--dir)))

(defun nelisp-ht-fastpath2-test--defun (name)
  "Return the text of the first DSL defun NAME in the build script."
  (with-temp-buffer
    (insert-file-contents nelisp-ht-fastpath2-test--build)
    (let* ((text (buffer-string))
           (start (string-match (format "^    (defun %s " (regexp-quote name)) text))
           (end (and start (string-match "^    (defun " text (1+ start)))))
      (should start)
      (substring text start end))))

(ert-deftest nelisp-ht-fastpath2-str-hash-single-definition ()
  "The hash body lives in `nl_ht_str_hash_raw'; `nl_ht_str_hash' reads a direct header."
  (let ((raw (nelisp-ht-fastpath2-test--defun "nl_ht_str_hash_raw"))
        (h (nelisp-ht-fastpath2-test--defun "nl_ht_str_hash")))
    (should (string-match-p "2166136261" raw))
    (should (string-match-p "(<= n 128)" raw))
    (should (string-match-p "(= (logand str_ptr 7) 4)" h))
    (should (string-match-p "(\\(?:heap\\|ptr\\)-read-u64 hd 24)" h))
    (should (string-match-p "9223372036854775807" h))
    (should (= 2 (1- (length (split-string h "(nl_ht_str_hash_raw " nil)))))
    (should-not (string-match-p "2166136261" h))))

(ert-deftest nelisp-ht-fastpath2-key-hash-shortcut ()
  "A direct string key skips the tag dispatch chain."
  (let ((b (nelisp-ht-fastpath2-test--defun "nl_ht_key_hash")))
    (should (string-match-p "(if (= (logand key_ptr 7) 4) (nl_ht_str_hash key_ptr)" b))))

(ert-deftest nelisp-ht-fastpath2-dstr-eq-same-representation-only ()
  "Direct strings compare by bytes only when both are unibyte or both multibyte."
  (let ((b (nelisp-ht-fastpath2-test--defun "nl_ht_dstr_eq"))
        (d (nelisp-ht-fastpath2-test--defun "nl_ht_dbytes_eq")))
    (should (string-match-p "(< nk 0)" b))
    (should (string-match-p "(< no 0)" b))
    (should (string-match-p " 2)" b))
    (should (string-match-p "nl_ht_dbytes_eq" b))
    (should (string-match-p "(/= nk no)" d))
    (should-not (string-match-p "alloc-bytes\\|m5_length" (concat b d)))))

(ert-deftest nelisp-ht-fastpath2-fast-find-uses-header-compare ()
  "The probe loop tries the header compare before the generic key match."
  (let* ((b (nelisp-ht-fastpath2-test--defun "nl_ht_fast_find"))
         (dstr (string-match "nl_ht_dstr_eq (- key 4) (- other 4)" b))
         (gen (string-match "(nl_ht_key_match key other mode)" b)))
    (should dstr)
    (should gen)
    (should (< dstr gen))
    (should (string-match-p "(dk (if (= mode 0)" b))
    (should (string-match-p "(= r 2)" b))))

(provide 'nelisp-ht-fastpath2-test)
;;; nelisp-ht-fastpath2-test.el ends here
