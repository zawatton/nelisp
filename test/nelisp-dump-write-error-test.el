;;; nelisp-dump-write-error-test.el --- dump write failures signal (S2.12)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Structural checks that the heap-image stream dump signals file errors
;; instead of answering -1.  The build script is read as text; set
;; NELISP_DUMP_WRITE_BUILD to point at another copy.

;;; Code:

(require 'ert)

(defconst nelisp-dump-write-error-test--build
  (or (getenv "NELISP_DUMP_WRITE_BUILD")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name default-directory))))
  "Build script path, captured at load time.")

(defun nelisp-dump-write-error-test--text ()
  (with-temp-buffer
    (insert-file-contents nelisp-dump-write-error-test--build)
    (buffer-string)))

(defun nelisp-dump-write-error-test--body ()
  (let* ((text (nelisp-dump-write-error-test--text))
         (start (string-match "(defun bf_arena_dump_image_stream_body_w " text))
         (end (and start (string-match "(defun bf_arena_dump_image_stream " text start))))
    (should start) (should end)
    (substring text start end)))

(ert-deftest nelisp-dump-write-error-helper-defined ()
  (let ((text (nelisp-dump-write-error-test--text)))
    (should (string-match-p "(defun nl_dump_file_error_w " text))
    (should (string-match-p "(nl_dump_sym_file_missing tag)" text))
    (should (string-match-p "(nl_dump_sym_file_error tag)" text))
    (should (string-match-p "(nl_exit_signal_w tag data)" text))))

(ert-deftest nelisp-dump-write-error-open-failure-signals ()
  "A failed open signals with the OS errno rather than answering -1."
  (let ((body (nelisp-dump-write-error-test--body)))
    (should (string-match-p
             "(nl_dump_file_error_w (- 0 fd) 0 (wf_arg_ptr args 0))" body))
    (should-not (string-match-p
                 "(nl_os_free_chunk tblbase tblcap) *(wf_write_int_w 0 -1))" body))))

(ert-deftest nelisp-dump-write-error-short-write-signals ()
  "Every stream write is accounted and a short write or close signals."
  (let ((body (nelisp-dump-write-error-test--body)))
    (should (string-match-p "(setq written (nl_fa_write_all fd hdr 64 0))" body))
    (should (string-match-p "(setq written (\\+ written (nl_fa_write_all fd trl 48 0)))" body))
    (should (string-match-p "(setq crc (nl_os_close_handle fd))" body))
    (should (string-match-p "(nl_dump_file_error_w 5 1 (wf_arg_ptr args 0))" body))
    ;; the old unchecked tail must be gone
    (should-not (string-match-p "(if (= cw ctotal)" body))))

(ert-deftest nelisp-dump-write-error-success-path-unchanged ()
  (let ((body (nelisp-dump-write-error-test--body)))
    (should (string-match-p
             "(wf_write_int_w 0 (\\+ 64 (\\+ (\\* tlen 8) (\\+ ctotal (\\+ isz 48)))))" body))
    (should (string-match-p "(nl_dump_oor_error_w oor)" body))))

(provide 'nelisp-dump-write-error-test)
;;; nelisp-dump-write-error-test.el ends here
