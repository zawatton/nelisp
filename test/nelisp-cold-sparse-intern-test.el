;;; nelisp-cold-sparse-intern-test.el --- Doc 241 S1 sparse intern section  -*- lexical-binding: t; -*-

;;; Commentary:

;; Structural checks on the cold-image generator: the stream dump writes the
;; intern section sparsely (trailer version 3; version 4 since Doc 241 S3.1) unless
;; NELISP_COLD_IMAGE_FORMAT=dense, and the cold loader refuses every other
;; version and restores the occupied buckets by slot index.  The generator is
;; read as text; each DSL defun is cut at the next top-level DSL defun.

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst nelisp-cold-sparse-intern-test--script
  (expand-file-name "../scripts/nelisp-standalone-build.el"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory))))

(defun nelisp-cold-sparse-intern-test--defun (name)
  "Return the source text of DSL defun NAME, or nil when absent."
  (with-temp-buffer
    (insert-file-contents nelisp-cold-sparse-intern-test--script)
    (when (re-search-forward (format "^    (defun %s " (regexp-quote name)) nil t)
      (let ((start (match-beginning 0)))
        (goto-char (match-end 0))
        (let ((end (if (re-search-forward "^    (defun " nil t)
                       (match-beginning 0)
                     (point-max))))
          (buffer-substring-no-properties start end))))))

(defun nelisp-cold-sparse-intern-test--count (name)
  "Return how many DSL defuns named NAME the generator has."
  (with-temp-buffer
    (insert-file-contents nelisp-cold-sparse-intern-test--script)
    (let ((n 0))
      (while (re-search-forward (format "^ *(defun %s " (regexp-quote name)) nil t)
        (setq n (1+ n)))
      n)))

(defun nelisp-cold-sparse-intern-test--has (text &rest needles)
  (and text (cl-every (lambda (s) (string-search s text)) needles)))

(ert-deftest nelisp-cold-sparse-intern-writer-has-sparse-and-dense-paths ()
  (let ((w (nelisp-cold-sparse-intern-test--defun
            "bf_arena_dump_image_stream_body_w")))
    (should (nelisp-cold-sparse-intern-test--has
             w "(nl_cold_sparse_write fd ib isz 0)" "(nl_cold_fmt_dense_p)"
             "(nl_cold_sparse_write fd ib isz (- 35185445830656 ib))"
             "(ptr-write-u64 trl 8 (if (= dense 1) 2 ,(if (nelisp-standalone--cold-v5-p) 5 4)))"
             "(nl_fa_write_all fd ib isz 0)"))))

(ert-deftest nelisp-cold-sparse-intern-dense-switch-reads-environment ()
  (should (nelisp-cold-sparse-intern-test--has
           (nelisp-cold-sparse-intern-test--defun "nl_cold_fmt_dense_p")
           "nelisp-standalone--cold-cstr-equal-form"
           "NELISP_COLD_IMAGE_FORMAT=dense"))
  (should (equal (nelisp-cold-sparse-intern-test--count "nl_cold_fmt_dense_p") 1)))

(ert-deftest nelisp-cold-sparse-intern-writer-encodes-records ()
  (should (nelisp-cold-sparse-intern-test--has
           (nelisp-cold-sparse-intern-test--defun "nl_cold_sparse_write")
           "1397773107" "(ptr-write-u64 r 0 i)" "(ptr-write-u64 r 8 w0)"
           "(ptr-write-u64 r 16 (if (= w0 0) w1 (+ w1 bias)))" "(nl_cold_zero scratch esz)"))
  (should (equal (nelisp-cold-sparse-intern-test--count "nl_cold_sparse_write") 1)))

(ert-deftest nelisp-cold-sparse-intern-loader-refuses-unknown-versions ()
  (let ((tv (nelisp-cold-sparse-intern-test--defun "nl_cold_trailer_valid_p"))
        (ld (nelisp-cold-sparse-intern-test--defun "nl_cold_load_arena")))
    (should (nelisp-cold-sparse-intern-test--has
             tv "(/= (heap-read-u64 trl 8) ,(if (nelisp-standalone--cold-v5-p) 5 4))"))
    (should-not (string-search "(= (heap-read-u64 trl 8) 2) 0" (or tv "")))
    (should (nelisp-cold-sparse-intern-test--has
             ld "(nl_cold_read_intern fd ib isz oldib)" "(nl_cold_diag_bad_intern)"
             "(liveonly (if (= (heap-read-u64 trl 8) 5) 1 0))"
             "(liveonly (if (= (heap-read-u64 trl 8) 4) 1 0))"))
    (should-not (string-search "(nl_fa_read_all fd ib isz 0)" (or ld "")))))

(ert-deftest nelisp-cold-sparse-intern-loader-writes-records-by-index ()
  (let ((r (nelisp-cold-sparse-intern-test--defun "nl_cold_read_intern")))
    (should (nelisp-cold-sparse-intern-test--has
             r "1397773107" "(> nrec nslots)" "(>= idx nslots)"
             "(ptr-write-u64 sl 0 w0)" "(ptr-write-u64 sl 8"
             "(nl_cold_zero scratch")))
  (should (equal (nelisp-cold-sparse-intern-test--count "nl_cold_read_intern") 1)))

(provide 'nelisp-cold-sparse-intern-test)
;;; nelisp-cold-sparse-intern-test.el ends here
