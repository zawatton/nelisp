;;; nelisp-cold-fixed-base-test.el --- Doc 242 S2 fixed arena base + mmap cold-load  -*- lexical-binding: t; -*-

;;; Commentary:

;; Structural checks on the generator (read as text): chunk 0 and the intern
;; region are reserved at fixed bases with MAP_FIXED_NOREPLACE and a
;; kernel-chosen fallback, the writer pre-biases the image to those bases,
;; the loader maps the content without relocating when the bases match, the
;; force-relocate switch is read, and only trailer version 5 is accepted.

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst nelisp-cold-fixed-base-test--script
  (expand-file-name "../scripts/nelisp-standalone-build.el"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory))))

(defun nelisp-cold-fixed-base-test--defun (name &optional after)
  "Return the source text of DSL defun NAME at any indentation, or nil.
When AFTER is a string, search only after its first occurrence (to skip
target-neutral stubs of the same name)."
  (with-temp-buffer
    (insert-file-contents nelisp-cold-fixed-base-test--script)
    (when after (search-forward after nil t))
    (when (re-search-forward (format "^ *(defun %s " (regexp-quote name)) nil t)
      (let ((start (match-beginning 0)))
        (goto-char (match-end 0))
        (let ((end (if (re-search-forward "^ *(defun " nil t)
                       (match-beginning 0)
                     (point-max))))
          (buffer-substring-no-properties start end))))))

(defun nelisp-cold-fixed-base-test--file ()
  (with-temp-buffer
    (insert-file-contents nelisp-cold-fixed-base-test--script)
    (buffer-string)))

(defun nelisp-cold-fixed-base-test--has (text &rest needles)
  (and text (cl-every (lambda (s) (string-search s text)) needles)))

(ert-deftest nelisp-cold-fixed-base-reserves-with-noreplace-and-fallback ()
  (let ((f (nelisp-cold-fixed-base-test--file))
        (fx (nelisp-cold-fixed-base-test--defun
             "nl_os_alloc_fixed" "(defun nelisp-standalone--linux-alloc-chunk-form"))
        (first (nelisp-cold-fixed-base-test--defun
                "nl_os_alloc_first" "(defun nelisp-standalone--linux-alloc-chunk-form"))
        (ireg (nelisp-cold-fixed-base-test--defun "nl_intern_region_init")))
    (should (nelisp-cold-fixed-base-test--has
             f "nelisp-standalone--cold-fixed-base #x200000000000"
             "nelisp-standalone--cold-fixed-ib #x200040000000"
             "(nl_os_alloc_first ,nelisp-standalone--cold-fixed-base ,size)"))
    ;; 1048610 = MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE
    (should (nelisp-cold-fixed-base-test--has
             fx "(syscall-direct 9 addr size 3 1048610 -1 0)" "(syscall-direct 11 p size"
             "(= p addr)"))
    ;; fallback to the kernel-chosen chunk
    (should (nelisp-cold-fixed-base-test--has first "nl_os_alloc_fixed" "(nl_os_alloc_chunk size)"))
    (should (nelisp-cold-fixed-base-test--has
             ireg "(nl_os_alloc_fixed 35185445830656 67108864)" "(syscall-direct 9 0 67108864 3 34 -1 0)"))))

(ert-deftest nelisp-cold-fixed-base-writer-prebiases-content-and-intern ()
  (let ((w (nelisp-cold-fixed-base-test--defun "bf_arena_dump_image_stream_body_w"))
        (b (nelisp-cold-fixed-base-test--defun "nl_cold_bias_file"))
        (sp (nelisp-cold-fixed-base-test--defun "nl_cold_sparse_write")))
    ;; Linux x86_64 open(2) is syscall 2 (5 is fstat; 5 is open only on macOS).
    (should (nelisp-cold-fixed-base-test--has b "(syscall-direct 2 cpath 2 0 0 0 0)"))
    (should (nelisp-cold-fixed-base-test--has
             w "(nl_cold_bias_file cpath tbl tlen ctotal)"
             "(nl_cold_sparse_write fd ib isz (- 35185445830656 ib))"
             "(ptr-write-u64 hdr 64 #x35425553)"))
    (should (nelisp-cold-fixed-base-test--has
             b "(syscall-direct 9 0 len 3 1 fd 4096)"
             ",(+ nelisp-standalone--cold-fixed-base 1024)"
             ",nelisp-standalone--cold-fixed-ib" "(> (+ f 8) ctotal)"))
    (should (nelisp-cold-fixed-base-test--has sp "(+ w1 bias)"))))

(ert-deftest nelisp-cold-fixed-base-loader-fast-path-maps-and-skips-relocation ()
  (let ((ld (nelisp-cold-fixed-base-test--defun "nl_cold_load_arena"))
        (rc (nelisp-cold-fixed-base-test--defun "nl_cold_read_content")))
    (should (nelisp-cold-fixed-base-test--has
             ld "(= base fixb) (= ib fixi)" "(nl_cold_read_content fd base ds slen fco fast)"
             "(if (= fast 1) 1" "nl_cold_reloc_rebase"
             "(if (= fast 1) 0"))
    ;; the relocation section is only read on the fallback path
    (should (nelisp-cold-fixed-base-test--has ld "(rok (if (= fast 1) 1 (nl_cold_reloc_read fd tbl tlen)))"))
    ;; 18 = MAP_PRIVATE|MAP_FIXED over whole pages of the content
    (should (nelisp-cold-fixed-base-test--has
             rc "(syscall-direct 9 (+ base 4096) l 3 18 fd (+ fco head))" "(< sz (+ fco slen))"))))

(ert-deftest nelisp-cold-fixed-base-force-relocate-switch-is-read ()
  (let ((fr (nelisp-cold-fixed-base-test--defun "nl_cold_force_reloc_p"))
        (ld (nelisp-cold-fixed-base-test--defun "nl_cold_load_arena")))
    (should (nelisp-cold-fixed-base-test--has
             fr "nelisp-standalone--cold-cstr-equal-form" "NELISP_COLD_FORCE_RELOCATE=1"))
    (should (nelisp-cold-fixed-base-test--has
             ld "(nl_cold_force_reloc_p)" "(nl_cold_move_intern)"))))

(ert-deftest nelisp-cold-fixed-base-requires-trailer-version-5 ()
  (let ((tv (nelisp-cold-fixed-base-test--defun "nl_cold_trailer_valid_p"))
        (ld (nelisp-cold-fixed-base-test--defun "nl_cold_load_arena"))
        (f (nelisp-cold-fixed-base-test--file)))
    (should (nelisp-cold-fixed-base-test--has
             tv "(/= (heap-read-u64 trl 8) ,(if (nelisp-standalone--cold-v5-p) 5 4))"))
    (should (nelisp-cold-fixed-base-test--has
             ld "(/= (heap-read-u64 ib0 64) 893539667)" "(/= fco 5120)" "(/= fixi oldib)"
             "(liveonly (if (= (heap-read-u64 trl 8) 5) 1 0))"))
    (should (nelisp-cold-fixed-base-test--has
             f "(defun nelisp-standalone--cold-v5-p ()" "'linux-x86_64"
             "not a complete version 5 packed image"))))

(provide 'nelisp-cold-fixed-base-test)
;;; nelisp-cold-fixed-base-test.el ends here
