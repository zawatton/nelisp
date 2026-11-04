;;; nelisp-reader-speed-test.el --- structural checks for reader hot paths  -*- lexical-binding: t -*-
(require 'ert)
(defconst nelisp-reader-speed-test--build
  (or (getenv "NELISP_READER_BUILD")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        (file-name-directory (or load-file-name buffer-file-name)))))
(defun nelisp-reader-speed-test--text ()
  (with-temp-buffer (insert-file-contents nelisp-reader-speed-test--build) (buffer-string)))
(defun nelisp-reader-speed-test--defun (text name)
  (let ((start (string-match (concat "(defun " (regexp-quote name) " ") text)))
    (should start)
    (substring text start (min (length text) (+ start 2500)))))
(ert-deftest nelisp-reader-speed-word-data-view-fast-path ()
  "nl_word_data answers a String/UnibyteStr view before the kind scan."
  (let ((body (nelisp-reader-speed-test--defun (nelisp-reader-speed-test--text) "nl_word_data")))
    (should (string-match-p "Reader fast path" body))
    (should (< (string-match "(= k0 5)" body) (string-match "(= kind 8)" body)))))
(ert-deftest nelisp-reader-speed-compat-scan-hoists-data-pointer ()
  "The post-parse compatibility scan reads through one data pointer."
  (let ((body (nelisp-reader-speed-test--defun (nelisp-reader-speed-test--text)
                                               "bf_read_one_native_compatible_p")))
    (should (string-match-p "(data (nl_bi_strptr src))" body))
    (should (string-match-p "(heap-read-u8 data at)" body))))
(provide 'nelisp-reader-speed-test)
