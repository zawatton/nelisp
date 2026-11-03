;;; nelisp-ht-fastpath-test.el --- structural pins for string-key hash lookup  -*- lexical-binding: t; -*-

;;; Commentary:

;; Pins the shape of the allocation-free, derive-once string comparison used
;; by `equal' hash-table lookups.  The build script path can be overridden
;; with NELISP_HT_FASTPATH_BUILD to run a negative control on a temp copy.

;;; Code:

(require 'ert)

(defconst nelisp-ht-fastpath-test--build
  (or (getenv "NELISP_HT_FASTPATH_BUILD")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-ht-fastpath-test--defun (name)
  "Return the text of the first DSL defun NAME in the build script."
  (with-temp-buffer
    (insert-file-contents nelisp-ht-fastpath-test--build)
    (let* ((text (buffer-string))
           (start (string-match (format "^    (defun %s " (regexp-quote name)) text))
           (end (and start (string-match "^    (defun " text (1+ start)))))
      (should start)
      (substring text start end))))

(ert-deftest nelisp-ht-fastpath-key-eq-strings-skip-work-stack ()
  "Two strings are compared before the per-call work stack is allocated."
  (let* ((b (nelisp-ht-fastpath-test--defun "wf_key_eq_depth"))
         (fast (string-match "(m5_streq ka kb)" b))
         (alloc (string-match "(alloc-bytes (\\* limit 16) 8)" b)))
    (should fast)
    (should alloc)
    (should (< fast alloc))
    (should (string-match-p "(= ka 0)" b))
    (should (string-match-p "(= kb 0)" b))))

(ert-deftest nelisp-ht-fastpath-streq-bytes-derives-pointers-once ()
  "The byte loop reads raw data pointers fetched once, never `m5_byte_at'."
  (let ((b (nelisp-ht-fastpath-test--defun "m5_streq_bytes")))
    (should (string-match-p "(pa (nl_bi_strptr a))" b))
    (should (string-match-p "(pb (nl_bi_strptr b))" b))
    (should (string-match-p "heap-read-u64 pa i" b))
    (should-not (string-match-p "m5_byte_at" b))
    (should-not (string-match-p "(m5_streq_bytes " (substring b 10)))
    (should-not (string-match-p "alloc-bytes" b))))

(ert-deftest nelisp-ht-fastpath-streq-keeps-length-checks ()
  "Character count and byte length are still compared; strlen derived once."
  (let ((b (nelisp-ht-fastpath-test--defun "m5_streq")))
    (should (string-match-p "(= (m5_length a) (m5_length b))" b))
    (should (string-match-p "(= n (m5_strlen b))" b))
    (should (= 2 (1- (length (split-string b "(m5_strlen " nil)))))))

(provide 'nelisp-ht-fastpath-test)
;;; nelisp-ht-fastpath-test.el ends here
