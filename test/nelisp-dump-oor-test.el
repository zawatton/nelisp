;;; nelisp-dump-oor-test.el --- dump out-of-image check shape (S2.8)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Structural checks that the out-of-image reference check of the heap dump
;; is history independent and reports its offenders.  The build script is
;; read as text; set NELISP_DUMP_OOR_BUILD to point at another copy.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nelisp-dump-oor-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory of this test file, captured at load time.")

(defun nelisp-dump-oor-test--text ()
  (with-temp-buffer
    (insert-file-contents
     (or (getenv "NELISP_DUMP_OOR_BUILD")
         (expand-file-name "../scripts/nelisp-standalone-build.el"
                           nelisp-dump-oor-test--dir)))
    (buffer-string)))

(defun nelisp-dump-oor-test--defun (text name)
  "Return the DSL defun NAME of TEXT up to the next defun at any indentation."
  (let* ((start (string-match (format "(defun %s " (regexp-quote name)) text))
         (end (and start (string-match "(defun " text (1+ start)))))
    (should start)
    (substring text start end)))

(defun nelisp-dump-oor-test--count (needle text)
  (let ((n 0) (pos 0))
    (while (setq pos (string-match (regexp-quote needle) text pos))
      (cl-incf n) (setq pos (1+ pos)))
    n))

(ert-deftest nelisp-dump-oor-reset-clears-whole-block ()
  "Both dump bodies reset the whole diagnostics block, not just the count."
  (let* ((text (nelisp-dump-oor-test--text))
         (reset (nelisp-dump-oor-test--defun text "nl_oor_reset")))
    (should (string-match-p "(< k 272)" reset))
    (should (= 2 (nelisp-dump-oor-test--count "(nl_oor_reset)" text)))
    (should (= 0 (nelisp-dump-oor-test--count
                  "(ptr-write-u64 (data-addr nl_oor_diag) 0 0)" text)))))

(ert-deftest nelisp-dump-oor-note-records-holder-and-eight-entries ()
  "Offenders are recorded with holder header and slot index, up to eight."
  (let* ((text (nelisp-dump-oor-test--text))
         (note (nelisp-dump-oor-test--defun text "nl_oor_note"))
         (vw (nelisp-dump-oor-test--defun text "nl_dump_verify_word"))
         (fk (nelisp-dump-oor-test--defun text "nl_fa_field_kind")))
    (should (string-match-p "(\\* 32 n)" note))
    (should (string-match-p "nl_dump_bsearch waddr 0" note))
    (should (string-match-p "ptr-write-u64 slot 16 hw" note))
    (should (string-match-p "ptr-write-u64 slot 24 ix" note))
    (should (string-match-p "(nl_oor_note waddr tg)" vw))
    (should (string-match-p "(nl_oor_note waddr tgt)" fk))
    (should (string-match-p " 0) 8)" fk))
    (should (string-match-p "\\b272\\b" text))))

(ert-deftest nelisp-dump-oor-unplaced-slot-is-counted-not-skipped ()
  "A heap slot with no image position and a foreign target is counted."
  (let* ((text (nelisp-dump-oor-test--text))
         (fk (nelisp-dump-oor-test--defun text "nl_fa_field_kind"))
         (un (nelisp-dump-oor-test--defun text "nl_fa_unplaced_note"))
         (vs (nelisp-dump-oor-test--defun text "nl_fa_vec_slots")))
    (should (string-match-p "(if (< wlog 0) (nl_fa_unplaced_note " fk))
    (should (string-match-p "nl_oor_foreign_p" un))
    (should (cl-search "(ptr-write-u64 cout 0 (+ (heap-read-u64 cout 0) 1))" un))
    (should (string-match-p "nl_fa_foreign_buf" vs))))

(ert-deftest nelisp-dump-oor-error-carries-offender-list ()
  "The refusal error appends the recorded offenders as extra data."
  (let* ((text (nelisp-dump-oor-test--text))
         (er (nelisp-dump-oor-test--defun text "nl_dump_oor_error_w")))
    (should (string-match-p "(while (> k 0)" er))
    (should (string-match-p "(\\+ 16 (\\* 32 k))" er))))

(ert-deftest nelisp-dump-oor-signal-keeps-offenders-in-data ()
  "S2.11: the offender list reaches the signal data instead of being dropped.
`bf_error_w' keeps only the formatted message, so the refusal must signal
through `nl_error_with_data_w', whose data is (MESSAGE . OFFENDERS)."
  (let* ((text (nelisp-dump-oor-test--text))
         (er (nelisp-dump-oor-test--defun text "nl_dump_oor_error_w"))
         (sig (nelisp-dump-oor-test--defun text "nl_error_with_data_w")))
    (should (string-match-p "(nl_error_with_data_w arguments offenders)" er))
    (should-not (string-match-p "(bf_error_w arguments 0)" er))
    (should sig)
    (should (string-match-p "(bf_sig_copy32 rest-slot extra)" sig))
    (should (string-match-p "(nelisp_cons_construct msg rest-slot sig-val)" sig))))

(ert-deftest nelisp-dump-oor-verify-pass-does-not-recount ()
  "S2.11: the post-copy verify runs only when the walk refused nothing.
Adding both counts reported each offending slot twice (1 slot -> 2)."
  (let ((text (nelisp-dump-oor-test--text)))
    (should (string-match-p "(if (= oor 0) (setq oor (nl_dump_verify_flat dest slen (\\+ slen isz))) 0)" text))
    (should (string-match-p "(if (if (= cok 1) (= oor 0) 0) (setq oor (nl_dump_verify_walk (\\+ ctotal isz))) 0)" text))
    (should-not (string-match-p "(setq oor (\\+ oor (nl_dump_verify_" text))))

(provide 'nelisp-dump-oor-test)
;;; nelisp-dump-oor-test.el ends here
