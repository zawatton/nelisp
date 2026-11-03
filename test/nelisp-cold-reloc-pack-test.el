;;; nelisp-cold-reloc-pack-test.el --- Doc 241 S3.1 packed relocation section  -*- lexical-binding: t; -*-

;;; Commentary:

;; Structural checks on the cold-image generator: the stream dump writes the
;; relocation table as a sorted delta-varint stream (trailer version 4) unless
;; NELISP_COLD_IMAGE_FORMAT=dense, and the cold loader decodes and relocates in
;; one pass, bounds-checking every decoded field offset.  The generator is read
;; as text; each DSL defun is cut at the next top-level DSL defun.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defconst nelisp-cold-reloc-pack-test--script
  (expand-file-name "../scripts/nelisp-standalone-build.el"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory))))

(defun nelisp-cold-reloc-pack-test--defun (name)
  "Return the source text of DSL defun NAME, or nil when absent."
  (with-temp-buffer
    (insert-file-contents nelisp-cold-reloc-pack-test--script)
    (when (re-search-forward (format "^    (defun %s " (regexp-quote name)) nil t)
      (let ((start (match-beginning 0)))
        (goto-char (match-end 0))
        (let ((end (if (re-search-forward "^    (defun " nil t)
                       (match-beginning 0)
                     (point-max))))
          (buffer-substring-no-properties start end))))))

(defun nelisp-cold-reloc-pack-test--count (name)
  (with-temp-buffer
    (insert-file-contents nelisp-cold-reloc-pack-test--script)
    (let ((n 0))
      (while (re-search-forward (format "^ *(defun %s " (regexp-quote name)) nil t)
        (setq n (1+ n)))
      n)))

(defun nelisp-cold-reloc-pack-test--has (text &rest needles)
  (and text (cl-every (lambda (s) (string-search s text)) needles)))

(ert-deftest nelisp-cold-reloc-pack-writer-sorts-and-encodes ()
  (let ((w (nelisp-cold-reloc-pack-test--defun "nl_cold_reloc_pack_write")))
    (should (nelisp-cold-reloc-pack-test--has
             w "1380732980" "nl_os_alloc_chunk" "nl_os_free_chunk"
             "(ptr-write-u8 base (/ f 8)" "(> (+ f 8) ctotal)"
             "(logand val 127)" "(= n tlen)"))
    (should (equal (nelisp-cold-reloc-pack-test--count "nl_cold_reloc_pack_write") 1))))

(ert-deftest nelisp-cold-reloc-pack-dump-has-packed-and-dense-paths ()
  (let ((w (nelisp-cold-reloc-pack-test--defun "bf_arena_dump_image_stream_body_w")))
    (should (nelisp-cold-reloc-pack-test--has
             w "(nl_cold_reloc_pack_write fd tbl tlen ctotal)"
             "(nl_fa_write_all fd tbl (* tlen 8) 0)"
             "(ptr-write-u64 trl 8 (if (= dense 1) 2 ,(if (nelisp-standalone--cold-v5-p) 5 4)))"
             "(+ ,(if (nelisp-standalone--cold-v5-p) '(if (= dense 1) 64 5120) 64) (+ rw (+ ctotal (+ esz 48))))"))))

(ert-deftest nelisp-cold-reloc-pack-loader-decodes-with-bounds-checks ()
  (let ((u (nelisp-cold-reloc-pack-test--defun "nl_cold_reloc_unpack"))
        (r (nelisp-cold-reloc-pack-test--defun "nl_cold_reloc_read"))
        (ld (nelisp-cold-reloc-pack-test--defun "nl_cold_load_arena")))
    (should (nelisp-cold-reloc-pack-test--has
             u "(> (+ f 8) slen)" "(>= nb 5)" "(>= p end)" "(= p end)"
             "(if (< raw slen) (+ ds o) (+ ib (- o slen)))"))
    (should (nelisp-cold-reloc-pack-test--has
             r "1380732980" "(> rsz (* tlen 8))"))
    (should (nelisp-cold-reloc-pack-test--has
             ld "(nl_cold_reloc_read fd tbl tlen)" "(nl_cold_reloc_unpack tbl tlen ds slen ib)"
             "(nl_cold_diag_bad_reloc)" "(/= (heap-read-u64 trl 8) ,(if (nelisp-standalone--cold-v5-p) 5 4))"))
    (should (equal (nelisp-cold-reloc-pack-test--count "nl_cold_reloc_unpack") 1))))

(ert-deftest nelisp-cold-reloc-pack-loader-verifies-digest-before-relocating ()
  (let* ((ld (nelisp-cold-reloc-pack-test--defun "nl_cold_load_arena"))
         (dig (and ld (string-search "(nl_cold_trailer_valid_p trl trl_n)" ld)))
         (unp (and ld (string-search "(nl_cold_reloc_unpack tbl" ld))))
    (should (and dig unp (< dig unp)))
    (should (string-search "(trl (+ tbl (+ (* tlen 8) 16)))" ld))))

(provide 'nelisp-cold-reloc-pack-test)
;;; nelisp-cold-reloc-pack-test.el ends here
