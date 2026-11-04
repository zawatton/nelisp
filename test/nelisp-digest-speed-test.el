;;; nelisp-digest-speed-test.el --- structural check for the vector printer fast path  -*- lexical-binding: t; -*-
(require 'ert)
(defconst nelisp-digest-speed-test--build
  (or (getenv "NELISP_DIGEST_BUILD")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        (file-name-directory (or load-file-name buffer-file-name)))))
(defun nelisp-digest-speed-test--src ()
  (with-temp-buffer (insert-file-contents nelisp-digest-speed-test--build) (buffer-string)))
(ert-deftest nelisp-digest-speed-vector-arm-prefilters-marker ()
  "The vector arm of m5_prin1 must not call bf_marker_vec_is directly."
  (let ((src (nelisp-digest-speed-test--src)))
    (should (string-match-p "defun m5_prin1_vec_buffer_p" src))
    (should (string-match-p "((= tag 8) (if (= (m5_prin1_vec_buffer_p vptr) 1)" src))
    (should-not (string-match-p "((= tag 8) (if (= (bf_marker_vec_is vptr 125779835254114 0 6) 1)" src))))
(provide 'nelisp-digest-speed-test)
