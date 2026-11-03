;;; nelisp-cold-load-fast-test.el --- Doc 242 S1 fast cold-load loader  -*- lexical-binding: t; -*-

;;; Commentary:

;; Structural checks on the cold loader in the generator: no full 2^20-slot
;; intern walk on the cold-load path, intern word1 rebased per decoded record,
;; physical (ptr-read) loads in the loader loops with every bounds check kept,
;; and a targeted scratch zero.  The generator is read as text.

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst nelisp-cold-load-fast-test--script
  (expand-file-name "../scripts/nelisp-standalone-build.el"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory))))

(defun nelisp-cold-load-fast-test--defun (name)
  "Return the source text of DSL defun NAME, or nil when absent."
  (with-temp-buffer
    (insert-file-contents nelisp-cold-load-fast-test--script)
    (when (re-search-forward (format "^    (defun %s " (regexp-quote name)) nil t)
      (let ((start (match-beginning 0)))
        (goto-char (match-end 0))
        (let ((end (if (re-search-forward "^    (defun " nil t)
                       (match-beginning 0)
                     (point-max))))
          (buffer-substring-no-properties start end))))))

(defun nelisp-cold-load-fast-test--has (text &rest needles)
  (and text (cl-every (lambda (s) (string-search s text)) needles)))

(defun nelisp-cold-load-fast-test--lacks (text &rest needles)
  (and text (cl-notany (lambda (s) (string-search s text)) needles)))

(ert-deftest nelisp-cold-load-fast-no-full-intern-walk ()
  (let ((ld (nelisp-cold-load-fast-test--defun "nl_cold_load_arena")))
    (should ld)
    (should (nelisp-cold-load-fast-test--lacks ld "nl_cold_reloc_intern" "1048576"))
    (should (null (nelisp-cold-load-fast-test--defun "nl_cold_reloc_intern")))))

(ert-deftest nelisp-cold-load-fast-record-loop-relocates-word1 ()
  (let ((r (nelisp-cold-load-fast-test--defun "nl_cold_read_intern")))
    (should (nelisp-cold-load-fast-test--has
             r "(fd ib isz oldib)" "(- ib oldib)" "(ptr-write-u64 sl 8"
             "(>= idx nslots)"))))

(ert-deftest nelisp-cold-load-fast-loops-use-physical-loads ()
  (dolist (n '("nl_cold_reloc_unpack" "nl_cold_read_intern" "nl_cold_reloc_read"))
    (let ((d (nelisp-cold-load-fast-test--defun n)))
      (should d)
      (should (nelisp-cold-load-fast-test--lacks d "heap-read-u64" "heap-read-u8"))
      (should (nelisp-cold-load-fast-test--has d "ptr-read-u64")))))

(ert-deftest nelisp-cold-load-fast-unpack-keeps-bounds-checks ()
  (let ((u (nelisp-cold-load-fast-test--defun "nl_cold_reloc_unpack")))
    (should (nelisp-cold-load-fast-test--has
             u "(> (+ f 8) slen)" "(>= p end)" "(>= nb 5)" "(= p end)"
             "ptr-read-u8"))))

(ert-deftest nelisp-cold-load-fast-zero-is-targeted ()
  (let ((ld (nelisp-cold-load-fast-test--defun "nl_cold_load_arena")))
    (should (nelisp-cold-load-fast-test--has
             ld "(nl_cold_zero trl 48)" "(logand (+ (ptr-read-u64 (+ tbl 8) 0) 23) -8)"))
    (should (nelisp-cold-load-fast-test--lacks
             ;; the success path no longer zeroes tlen*8+64; the reject scrub does
             (car (split-string ld "nl_cold_reject_scrub ds slen tbl tlen ib isz" t))
             "(nl_cold_zero tbl (+ (* tlen 8) 64))"))))

(provide 'nelisp-cold-load-fast-test)
;;; nelisp-cold-load-fast-test.el ends here
