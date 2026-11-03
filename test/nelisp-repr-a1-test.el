;;; nelisp-repr-a1-test.el --- Doc 238 A1 view-producer checks (structural)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Structural checks that the native substring search no longer resolves a
;; string data pointer per byte (m5_byte_at -> nl_bi_strptr -> nl_word_data on
;; a legacy view, 4.3M calls on the reader probe).  Nothing here runs a
;; NeLisp image.  NELISP_A1_BUILD overrides the build script path;
;; NELISP_A1_SEED names a pristine build script for the negative control.

;;; Code:

(require 'ert)

(defconst nelisp-repr-a1-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(defconst nelisp-repr-a1-test--build
  (or (getenv "NELISP_A1_BUILD")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        nelisp-repr-a1-test--dir)))

(defconst nelisp-repr-a1-test--seed
  (let ((f (or (getenv "NELISP_A1_SEED")
               (expand-file-name "../.seed-build.el" nelisp-repr-a1-test--dir))))
    (and (file-exists-p f) f)))

(defun nelisp-repr-a1-test--defun (file name)
  "Return the text of defun NAME in FILE (exactly one definition)."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((re (format "^    (defun %s[ \n]" (regexp-quote name))) (texts nil))
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (let ((beg (match-beginning 0)))
          (goto-char (+ beg 4))
          (forward-sexp 1)
          (push (buffer-substring beg (point)) texts)))
      (should (= (length texts) 1))
      (car texts))))

(defun nelisp-repr-a1-test--no-per-byte-strptr-p (file)
  "Non-nil when the match loop of FILE has no per-byte string-pointer call."
  (let ((body (nelisp-repr-a1-test--defun file "m5_strmatch_at")))
    (not (or (string-match-p "m5_byte_at" body)
             (string-match-p "nl_bi_strptr" body)
             (string-match-p "nl_word_data" body)))))

(defun nelisp-repr-a1-test--search-hoists-p (file)
  "Non-nil when `m5_string_search' resolves both data pointers once."
  (let ((body (nelisp-repr-a1-test--defun file "m5_string_search")))
    (and (string-match-p "(hdata (nl_bi_strptr hay))" body)
         (string-match-p "(ndata (nl_bi_strptr needle))" body)
         (string-match-p "(m5_strmatch_at hdata i ndata 0 nlen)" body))))

;; Round 2: startup is dominated by m5_sha256 -> m5_sha_copy (861k of 940k
;; nl_bi_strptr calls on the ON driver).
(defun nelisp-repr-a1-test--sha-copy-hoists-p (file)
  (let ((body (nelisp-repr-a1-test--defun file "m5_sha_copy")))
    (and (string-match-p "(data (nl_bi_strptr hay))" body)
         (string-match-p "(heap-read-u8 data i)" body)
         (not (string-match-p "m5_byte_at" body)))))

(ert-deftest nelisp-repr-a1-sha-copy-hoists-data-pointer ()
  (should (nelisp-repr-a1-test--sha-copy-hoists-p nelisp-repr-a1-test--build)))

(ert-deftest nelisp-repr-a1-strmatch-no-per-byte-view ()
  (should (nelisp-repr-a1-test--no-per-byte-strptr-p nelisp-repr-a1-test--build)))

(ert-deftest nelisp-repr-a1-search-hoists-data-pointers ()
  (should (nelisp-repr-a1-test--search-hoists-p nelisp-repr-a1-test--build)))

(ert-deftest nelisp-repr-a1-strmatch-callers-use-new-signature ()
  ;; Only m5_string_search may call m5_strmatch_at (hay/needle args would be
  ;; misread as data pointers).
  (with-temp-buffer
    (insert-file-contents nelisp-repr-a1-test--build)
    (let ((n 0))
      (while (re-search-forward "(m5_strmatch_at " nil t) (setq n (1+ n)))
      ;; recursive call + one caller (the defun head has no leading paren).
      (should (= n 2)))))

;; Round 3: nelisp_reader_dispatch re-read the first byte through
;; `str-byte-at' up to ten times per token (108k nl_word_data view entries at
;; startup); it now reads once.  NELISP_A1_LEXER / NELISP_A1_LEXER_SEED
;; override the file paths (the seed has no in-tree default).
(defconst nelisp-repr-a1-test--lexer
  (or (getenv "NELISP_A1_LEXER")
      (expand-file-name "../lisp/nelisp-cc-reader-lexer.el" nelisp-repr-a1-test--dir)))

(defconst nelisp-repr-a1-test--lexer-seed (getenv "NELISP_A1_LEXER_SEED"))

(defun nelisp-repr-a1-test--dispatch-reads-once-p (file)
  (let ((body (nelisp-repr-a1-test--defun file "nelisp_reader_dispatch"))
        (n 0) (pos 0))
    (while (string-match "(str-byte-at " body pos)
      (setq n (1+ n) pos (match-end 0)))
    (and (= n 1) (string-match-p "(let\\* ((b0 (str-byte-at str-ptr cursor)))" body) t)))

(ert-deftest nelisp-repr-a1-dispatch-reads-first-byte-once ()
  (should (nelisp-repr-a1-test--dispatch-reads-once-p nelisp-repr-a1-test--lexer)))

(ert-deftest nelisp-repr-a1-dispatch-negative-control-seed ()
  (skip-unless nelisp-repr-a1-test--lexer-seed)
  (should-not (nelisp-repr-a1-test--dispatch-reads-once-p nelisp-repr-a1-test--lexer-seed)))

;; Round 4: lexer helpers and two hot nl_bi_strptr callers resolve the data
;; pointer once.  NELISP_A1R4_SEED_ROOT names a pristine tree for the negative
;; controls (the predicates must fail there).
(defconst nelisp-repr-a1-test--r4-seed-root (getenv "NELISP_A1R4_SEED_ROOT"))

(defun nelisp-repr-a1-test--count (re body)
  (let ((n 0) (pos 0))
    (while (string-match re body pos) (setq n (1+ n) pos (match-end 0)))
    n))

(defun nelisp-repr-a1-test--lex-one-p (file)
  (let ((b (nelisp-repr-a1-test--defun file "nelisp_reader_lex_one")))
    (and (= (nelisp-repr-a1-test--count "(str-len " b) 1)
         (= (nelisp-repr-a1-test--count "(str-data-ptr " b) 1)
         (string-match-p "(nelisp_reader_skip_ws_p dp cursor n)" b) t)))

(defun nelisp-repr-a1-test--lex-atom-p (file)
  (let ((b (nelisp-repr-a1-test--defun file "nelisp_reader_lex_atom")))
    (and (= (nelisp-repr-a1-test--count "(str-data-ptr " b) 1)
         (= (nelisp-repr-a1-test--count "(str-byte-at " b) 0)
         (= (nelisp-repr-a1-test--count "(nelisp_reader_scan_atom_p dp " b) 3)
         (not (string-match-p "(nelisp_reader_scan_atom " b)) t)))

(defun nelisp-repr-a1-test--finalize-p (file)
  (let ((b (nelisp-repr-a1-test--defun file "nelisp_reader_lex_atom_finalize")))
    (and (= (nelisp-repr-a1-test--count "(str-byte-at " b) 1)
         (not (string-match-p "nelisp_reader_atom_is_lone_dot" b))
         (not (string-match-p "nelisp_reader_classify_atom" b)) t)))

(defun nelisp-repr-a1-test--seqwidth-p (file)
  (let ((b (nelisp-repr-a1-test--defun file "bf_external_sequence_width")))
    (and (= (nelisp-repr-a1-test--count "(nl_bi_strptr " b) 1)
         (not (string-match-p "m5_byte_at" b)) t)))

(defun nelisp-repr-a1-test--bytes-to-chars-p (file)
  (let ((b (nelisp-repr-a1-test--defun file "bf_read_one_bytes_to_chars")))
    (and (= (nelisp-repr-a1-test--count "(nl_bi_strptr " b) 1)
         (string-match-p "(heap-read-u8 dp i)" b)
         (not (string-match-p "m5_byte_at" b)) t)))

(ert-deftest nelisp-repr-a1r4-lex-one-hoists ()
  (should (nelisp-repr-a1-test--lex-one-p nelisp-repr-a1-test--lexer)))
(ert-deftest nelisp-repr-a1r4-lex-atom-hoists ()
  (should (nelisp-repr-a1-test--lex-atom-p nelisp-repr-a1-test--lexer)))
(ert-deftest nelisp-repr-a1r4-finalize-reads-first-byte-once ()
  (should (nelisp-repr-a1-test--finalize-p nelisp-repr-a1-test--lexer)))
(ert-deftest nelisp-repr-a1r4-external-sequence-width-hoists ()
  (should (nelisp-repr-a1-test--seqwidth-p nelisp-repr-a1-test--build)))
(ert-deftest nelisp-repr-a1r4-bytes-to-chars-hoists ()
  (should (nelisp-repr-a1-test--bytes-to-chars-p nelisp-repr-a1-test--build)))

(ert-deftest nelisp-repr-a1r4-negative-control-seed ()
  (skip-unless nelisp-repr-a1-test--r4-seed-root)
  (let ((lx (expand-file-name "lisp/nelisp-cc-reader-lexer.el" nelisp-repr-a1-test--r4-seed-root))
        (bd (expand-file-name "scripts/nelisp-standalone-build.el" nelisp-repr-a1-test--r4-seed-root)))
    (should-not (nelisp-repr-a1-test--lex-one-p lx))
    (should-not (nelisp-repr-a1-test--lex-atom-p lx))
    (should-not (nelisp-repr-a1-test--finalize-p lx))
    (should-not (nelisp-repr-a1-test--seqwidth-p bd))
    (should-not (nelisp-repr-a1-test--bytes-to-chars-p bd))))

(provide 'nelisp-repr-a1-test)
;;; nelisp-repr-a1-test.el ends here
