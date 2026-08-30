;;; nelisp-doc200-unibyte-repr-test.el --- Doc 200 unibyte representation gate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 1 wrote both Sexp layouts by hand and sent them through the real
;; consumer defuns.  Phase 2 keeps that coverage and additionally executes the
;; production tag-14/tag-15 allocators and tag-15 -> tag-14 finalizer.  The GC
;; mark-buffer shim records the pointer it was asked to mark, making a missing
;; mark arm observable rather than merely checking that `nl_gc_mark_slot'
;; returns.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (repo-root (and test-dir
                       (file-name-directory
                        (directory-file-name test-dir)))))
  (dolist (dir '("lisp" "src" "scripts"))
    (let ((path (and repo-root (expand-file-name dir repo-root))))
      (when (and path (file-directory-p path))
        (add-to-list 'load-path path)))))

(require 'nelisp-aot-compiler)
(require 'nelisp-sexp-layout)
(require 'nelisp-standalone-build)
(require 'nelisp-cc-sexp-clone-into)
(require 'nelisp-cc-evalport-nonenv-mut-str-set-cp)
(require 'nelisp-cc-jit-mut-str-set-codepoint)
(require 'nelisp-cc-jit-type-of)
(require 'nelisp-cc-nlstr-direct-ops)

(defconst nelisp-doc200-unibyte-repr-test--page #x30000000
  "Fixed scratch page used only by the freestanding representation probe.")

(defconst nelisp-doc200-unibyte-repr-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir
         (file-name-directory (directory-file-name test-dir))))
  "Repository root used to locate the already-built standalone binary.")

(defun nelisp-doc200-unibyte-repr-test--forms (source)
  "Return SOURCE's top-level forms, accepting a `(seq ...)' or a list."
  (cond
   ((eq (car-safe source) 'seq) (cdr source))
   ((eq (car-safe source) 'defun) (list source))
   (t source)))

(defun nelisp-doc200-unibyte-repr-test--defun (name source)
  "Return the defun named NAME from SOURCE, or fail the test."
  (or (cl-find-if
       (lambda (form)
         (and (consp form) (eq (car form) 'defun) (eq (cadr form) name)))
       (nelisp-doc200-unibyte-repr-test--forms source))
      (ert-fail (format "Doc 200 probe could not find production defun %S" name))))

(defun nelisp-doc200-unibyte-repr-test--driver-defun (name)
  "Return production driver defun NAME."
  (nelisp-doc200-unibyte-repr-test--defun
   name (nelisp-standalone--reader-driver-source)))

(defun nelisp-doc200-unibyte-repr-test--stub (name args value)
  "Build a test-local defun NAME with ARGS returning VALUE."
  `(defun ,name ,args ,value))

(defun nelisp-doc200-unibyte-repr-test--contains-p (tree needle)
  "Return non-nil when TREE contains a subtree equal to NEEDLE."
  (or (equal tree needle)
      (and (consp tree)
           (or (nelisp-doc200-unibyte-repr-test--contains-p
                (car tree) needle)
               (nelisp-doc200-unibyte-repr-test--contains-p
               (cdr tree) needle)))))

(defun nelisp-doc200-unibyte-repr-test--m5-defun (name)
  "Return production M5 helper defun NAME."
  (nelisp-doc200-unibyte-repr-test--defun
   name nelisp-standalone--applyfn-m5-helpers))

(defun nelisp-doc200-unibyte-repr-test--bf-defun (name)
  "Return production standalone builtin helper defun NAME."
  (nelisp-doc200-unibyte-repr-test--defun
   name nelisp-standalone--applyfn-bf-helpers))

(defun nelisp-doc200-unibyte-repr-test--eval-standalone (expression)
  "Evaluate EXPRESSION with the prepared standalone binary and return stdout."
  (let ((binary (expand-file-name
                 "target/nelisp"
                 nelisp-doc200-unibyte-repr-test--repo-root)))
    (unless (file-executable-p binary)
      (ert-skip "target/nelisp is not built; standalone-reader-test owns it"))
    (with-temp-buffer
      (let ((rc (call-process binary nil t nil "--eval" expression)))
        (unless (= rc 0)
          (ert-fail
           (format "standalone Doc 200 expression failed: rc=%S stdout=%S"
                   rc (buffer-string))))
        (buffer-string)))))

(defun nelisp-doc200-unibyte-repr-test--eval-standalone-error (expression)
  "Evaluate EXPRESSION and return (EXIT-CODE STDERR) for a failing read."
  (let ((binary (expand-file-name
                 "target/nelisp"
                 nelisp-doc200-unibyte-repr-test--repo-root))
        (stderr-file (make-temp-file "nelisp-doc200-reader-stderr-")))
    (unless (file-executable-p binary)
      (ert-skip "target/nelisp is not built; standalone-reader-test owns it"))
    (unwind-protect
        (with-temp-buffer
          (let ((rc (call-process binary nil (list t stderr-file) nil
                                  "--eval" expression)))
            (with-temp-buffer
              (insert-file-contents stderr-file)
              (list rc (buffer-string)))))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun nelisp-doc200-unibyte-repr-test--mutation-probe-source ()
  "Return a freestanding proof of fixed-width `aset' on all string tags."
  (let* ((base nelisp-doc200-unibyte-repr-test--page)
         (slot14 (+ base 32))
         (slot15 (+ base 64))
         (slot5 (+ base 96))
         (slot6 (+ base 128))
         (box15 (+ base 160))
         (box6 (+ base 192))
         (buf14 (+ base 224))
         (buf15 (+ base 240))
         (buf5 (+ base 256))
         (buf6 (+ base 272))
         (val255 (+ base 320))
         (val0 (+ base 352))
         (val98 (+ base 384))
         (val99 (+ base 416))
         (val12354 (+ base 448))
         (val97 (+ base 480))
         (out (+ base 512))
         (bf-layout-forms
          (mapcar #'nelisp-doc200-unibyte-repr-test--bf-defun
                  '(bf_str_ptr bf_str_len)))
         (m5-forms
          (mapcar #'nelisp-doc200-unibyte-repr-test--m5-defun
                  '(nl_u8_clen_at nl_str_charlen nl_u8_cidx_byte)))
         (bf-aset-forms
          (mapcar #'nelisp-doc200-unibyte-repr-test--bf-defun
                  '(bf_aset_string_write bf_aset_unibyte_string
                    bf_aset_multibyte_string))))
    `(seq
      ;; The signal helpers are made numeric in this probe so forbidden
      ;; writes can be asserted without needing the interpreter's catch
      ;; machinery.  The production helpers themselves are used unchanged.
      (defun wf_dirty () 1)
      (defun wf_copy32 (dst src)
        (seq
         (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
         (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
         (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
         (ptr-write-u64 dst 24 (ptr-read-u64 src 24))
         1))
      (defun bf_wrong_type_integerp (_offender) 2)
      (defun bf_args_out_of_range (_arr _idx) 3)
      (defun bf_args_out_of_range_byte (_offender) 4)
      (defun bf_aset_fixed_width_rejected (_out) 5)
      ,@bf-layout-forms
      (defun m5_strlen (src) (bf_str_len src))
      (defun m5_byte_at (src i) (ptr-read-u8 (bf_str_ptr src) i))
      ,@m5-forms
      ,@bf-aset-forms
      (defun doc200_write_int (slot value)
        (seq (ptr-write-u64 slot 0 2) (ptr-write-u64 slot 8 value) 0))
      (defun doc200_aset_setup ()
        (seq
         ;; mmap(BASE, 4096, PROT_RW, MAP_FIXED|PRIVATE|ANON, -1, 0)
         (syscall-direct 9 ,base 4096 3 50 -1 0)
         ;; Inline UnibyteStr and boxed UnibyteMutStr, one raw byte each.
         (ptr-write-u64 ,slot14 0 14)
         (ptr-write-u64 ,slot14 8 1)
         (ptr-write-u64 ,slot14 16 ,buf14)
         (ptr-write-u64 ,slot14 24 1)
         (ptr-write-u8 ,buf14 0 200)
         (ptr-write-u64 ,slot15 0 15)
         (ptr-write-u64 ,slot15 8 ,box15)
         (ptr-write-u64 ,box15 0 1)
         (ptr-write-u64 ,box15 8 ,buf15)
         (ptr-write-u64 ,box15 16 1)
         (ptr-write-u64 ,box15 24 1)
         (ptr-write-u8 ,buf15 0 201)
         ;; Inline Str and boxed MutStr each hold "a\u3042" (four UTF-8 bytes,
         ;; two characters).  Only replacing the leading ASCII character is
         ;; permitted by the Emacs 31.1 fixed-width rule.
         (ptr-write-u64 ,slot5 0 5)
         (ptr-write-u64 ,slot5 8 4)
         (ptr-write-u64 ,slot5 16 ,buf5)
         (ptr-write-u64 ,slot5 24 4)
         (ptr-write-u64 ,slot6 0 6)
         (ptr-write-u64 ,slot6 8 ,box6)
         (ptr-write-u64 ,box6 0 4)
         (ptr-write-u64 ,box6 8 ,buf6)
         (ptr-write-u64 ,box6 16 4)
         (ptr-write-u64 ,box6 24 1)
         (ptr-write-u8 ,buf5 0 97)
         (ptr-write-u8 ,buf5 1 227)
         (ptr-write-u8 ,buf5 2 129)
         (ptr-write-u8 ,buf5 3 130)
         (ptr-write-u8 ,buf6 0 97)
         (ptr-write-u8 ,buf6 1 227)
         (ptr-write-u8 ,buf6 2 129)
         (ptr-write-u8 ,buf6 3 130)
         (doc200_write_int ,val255 255)
         (doc200_write_int ,val0 0)
         (doc200_write_int ,val98 98)
         (doc200_write_int ,val99 99)
         (doc200_write_int ,val12354 12354)
         (doc200_write_int ,val97 97)
         0))
      (defun doc200_aset_allowed_probe ()
        (let* ((r14 (bf_aset_unibyte_string ,slot14 0 ,val0 ,val255 ,out))
               (r15 (bf_aset_unibyte_string ,slot15 0 ,val0 ,val0 ,out))
               (r5 (bf_aset_multibyte_string ,slot5 0 ,val0 ,val98 ,out))
               (r6 (bf_aset_multibyte_string ,slot6 0 ,val0 ,val99 ,out)))
          (+
           (if (= r14 0) 0 1)
           (if (= (ptr-read-u8 ,slot14 0) 14) 0 1)
           (if (= (ptr-read-u64 ,slot14 24) 1) 0 1)
           (if (= (ptr-read-u8 ,buf14 0) 255) 0 1)
           (if (= r15 0) 0 1)
           (if (= (ptr-read-u8 ,slot15 0) 15) 0 1)
           (if (= (ptr-read-u64 ,box15 16) 1) 0 1)
           (if (= (ptr-read-u8 ,buf15 0) 0) 0 1)
           (if (= r5 0) 0 1)
           (if (= (ptr-read-u8 ,slot5 0) 5) 0 1)
           (if (= (ptr-read-u64 ,slot5 24) 4) 0 1)
           (if (= (ptr-read-u8 ,buf5 0) 98) 0 1)
           (if (= r6 0) 0 1)
           (if (= (ptr-read-u8 ,slot6 0) 6) 0 1)
           (if (= (ptr-read-u64 ,box6 16) 4) 0 1)
           (if (= (ptr-read-u8 ,buf6 0) 99) 0 1))))
      (defun doc200_aset_rejected_probe ()
        ;; Exact first parity divergence: replacing the non-ASCII character in
        ;; "a\u3042" with ASCII must signal under 31.1.  Host Emacs 30.1 permits
        ;; `(aset (copy-sequence "\u3042") 0 ?a)' and returns "a".
        ;; Exact second divergence: replacing ASCII with a non-ASCII character
        ;; must signal under 31.1.  Host Emacs 30.1 permits `(aset
        ;; (copy-sequence "ab") 0 ?\u3042)' and returns "\u3042b".
        (let* ((bad 0))
          (if (= (bf_aset_unibyte_string
                  ,slot14 0 ,val0 ,val12354 ,out)
                 4)
              nil (setq bad (+ bad 1)))
          (if (= (ptr-read-u8 ,slot14 0) 14) nil (setq bad (+ bad 1)))
          (if (= (ptr-read-u64 ,slot14 24) 1) nil (setq bad (+ bad 1)))
          (if (= (ptr-read-u8 ,buf14 0) 255) nil (setq bad (+ bad 1)))
          (if (= (bf_aset_unibyte_string
                  ,slot15 0 ,val0 ,val12354 ,out)
                 4)
              nil (setq bad (+ bad 1)))
          (if (= (ptr-read-u8 ,slot15 0) 15) nil (setq bad (+ bad 1)))
          (if (= (ptr-read-u64 ,box15 16) 1) nil (setq bad (+ bad 1)))
          (if (= (ptr-read-u8 ,buf15 0) 0) nil (setq bad (+ bad 1)))
          (if (= (bf_aset_multibyte_string ,slot5 1 ,val0 ,val97 ,out)
                 5)
              nil (setq bad (+ bad 1)))
          (if (= (ptr-read-u8 ,slot5 0) 5) nil (setq bad (+ bad 1)))
          (if (= (ptr-read-u64 ,slot5 24) 4) nil (setq bad (+ bad 1)))
          (if (= (ptr-read-u8 ,buf5 1) 227) nil (setq bad (+ bad 1)))
          (if (= (bf_aset_multibyte_string
                  ,slot6 0 ,val0 ,val12354 ,out)
                 5)
              nil (setq bad (+ bad 1)))
          (if (= (ptr-read-u8 ,slot6 0) 6) nil (setq bad (+ bad 1)))
          (if (= (ptr-read-u64 ,box6 16) 4) nil (setq bad (+ bad 1)))
          (if (= (ptr-read-u8 ,buf6 0) 99) nil (setq bad (+ bad 1)))
          bad))
      (defun doc200_aset_probe ()
        (seq (doc200_aset_setup)
             (if (= (doc200_aset_allowed_probe) 0)
                 (doc200_aset_rejected_probe)
               1)))
      (exit (doc200_aset_probe)))))

(defun nelisp-doc200-unibyte-repr-test--probe-source ()
  "Return the freestanding AOT source for the tag-14/tag-15 consumer probe."
  (let* ((base nelisp-doc200-unibyte-repr-test--page)
         (slot14 (+ base 32))
         (slot15 (+ base 64))
         (box15 (+ base 96))
         (buf14 (+ base 160))
         (buf15 (+ base 176))
         (dst14 (+ base 208))
         (dst15 (+ base 240))
         (printbuf (+ base 288))
         (producer14 (+ base 352))
         (producer15 (+ base 384))
         (finalized14 (+ base 416))
         (producerbox (+ base 448))
         (producerstrbuf (+ base 480))
         (producerbuf (+ base 512))
         (finalizedbuf (+ base 544))
         (gc-mark-slot
          (nelisp-doc200-unibyte-repr-test--defun
           'nl_gc_mark_slot nelisp-standalone--gc-source))
         (clone-names '(nl_sci_prog2 nl_sci_copy nl_sci_bump
                        nl_sci_rc nl_sci_dispatch))
         (clone-forms
          (mapcar (lambda (name)
                    (nelisp-doc200-unibyte-repr-test--defun
                     name nelisp-cc-sexp-clone-into--source))
                  clone-names))
         (type-dispatch
          (nelisp-doc200-unibyte-repr-test--defun
           'nl_jit_type_of_tag nelisp-cc-jit-type-of--source))
         (arrayp
          (nelisp-doc200-unibyte-repr-test--defun
           'bf_arrayp_raw nelisp-standalone--applyfn-bf-helpers))
         (driver-names '(nl_cli_put_byte nl_cli_put_raw_bytes
                         nl_cli_put_octal_byte
                         nl_cli_put_unibyte_string_value
                         nl_cli_value_to_buf))
         (driver-forms
          (mapcar #'nelisp-doc200-unibyte-repr-test--driver-defun
                  driver-names))
         (strptr
          (nelisp-doc200-unibyte-repr-test--defun
           'nl_bi_strptr nelisp-standalone--fileio-forms-part1))
         (strlen
          (nelisp-doc200-unibyte-repr-test--defun
           'nl_bi_strlen nelisp-standalone--fileio-forms-part1))
         (producer-forms
          (append
           (mapcar
            (lambda (name)
              (nelisp-doc200-unibyte-repr-test--defun
               name nelisp-cc-nlstr-direct-ops--alloc-str-source))
            '(nl_alloc_str_copy_loop nl_alloc_str_write_tag))
           (list
            (nelisp-doc200-unibyte-repr-test--defun
             'nl_alloc_mut_str_write_tag
             nelisp-cc-nlstr-direct-ops--alloc-mut-str-source))
           (mapcar
            (lambda (name)
              (nelisp-doc200-unibyte-repr-test--defun
               name nelisp-cc-nlstr-direct-ops--mut-str-finalize-source))
            '(nl_mut_str_finalize_copy_loop
              nl_mut_str_finalize_unibyte_write))))
         (gc-stubs
          `((defun nl_gc_mark_buf (ptr)
              (seq (ptr-write-u64 ,base 0 ptr) 0))
            (defun nl_gc_mark_block (_ptr) 1)
            (defun nl_gc_mark_cons (_ptr) 0)
            (defun nl_gc_mark_vec_slots (_ptr _i _n) 0)
            (defun nl_gc_mark_char_table_box (_ptr) 0)
            (defun nl_gc_mark_bool_vector_box (_ptr) 0)))
         (clone-stubs
          (cons
           '(defun nelisp_nlstr_clone (box)
              (seq (ptr-write-u64 box 24 (+ (ptr-read-u64 box 24) 1)) box))
           (mapcar
            (lambda (name)
              (nelisp-doc200-unibyte-repr-test--stub name '(box) 'box))
            '(nelisp_nlconsbox_clone nelisp_nlvector_clone
              nelisp_nlchartable_clone nelisp_nlboolvector_clone
              nelisp_nlcell_clone nelisp_nlrecord_clone))))
         (type-stubs
          (append
           (mapcar
            (lambda (name)
              (nelisp-doc200-unibyte-repr-test--stub name '(_out) 0))
            '(nl_jit_to_write_cons nl_jit_to_write_symbol
              nl_jit_to_write_integer nl_jit_to_write_float
              nl_jit_to_write_vector nl_jit_to_write_char_table
              nl_jit_to_write_bool_vector))
           '((defun nl_jit_to_write_string (_out) 77))))
         (printer-stubs
          '((defun nl_cli_put_nil (_buf off) off)
            (defun nl_cli_put_dec (_buf off _v) off)
            (defun nl_cli_put_string_value (_buf off _sx _quoted) off)
            (defun nl_cli_put_list_tail (_buf off _node _first) off)
            (defun nl_cli_put_vector_loop (_buf off _vec _i _n) off)
            (defun nl_cli_put_object (_buf off) off)))
         (other-stubs
          '((defun nl_alloc_str (_src _len dst) dst)
            (defun nl_alloc_symbol (_src _len dst) dst)))
         (probe-defs
          `((defun doc200_stringp (p)
              ;; This call is intentionally compiled through the production
              ;; AOT direct-tag lowering, not duplicated as numeric tests here.
              (if (stringp p) 1 0))
            (defun doc200_sequencep (p)
              ;; This is the production `nelisp-stdlib.el' composition.
              (if (or (null p) (consp p) (stringp p) (vectorp p)) 1 0))
            (defun doc200_predicate_probe ()
              (+
               (if (= (doc200_stringp ,slot14) 1) 0 1)
               (if (= (doc200_stringp ,slot15) 1) 0 2)
               (if (= (bf_arrayp_raw ,slot14) 1) 0 4)
               (if (= (bf_arrayp_raw ,slot15) 1) 0 8)
               (if (= (doc200_sequencep ,slot14) 1) 0 16)
               (if (= (doc200_sequencep ,slot15) 1) 0 32)
               (if (= (nl_jit_type_of_tag 14 0) 77) 0 64)
               (if (= (nl_jit_type_of_tag 15 0) 77) 0 128)))
            (defun doc200_gc_probe ()
              (seq
               (ptr-write-u64 ,base 0 0)
               (nl_gc_mark_slot ,slot14)
               (if (= (ptr-read-u64 ,base 0) ,buf14)
                   (seq
                    (ptr-write-u64 ,base 0 0)
                    (nl_gc_mark_slot ,slot15)
                    (if (= (ptr-read-u64 ,base 0) ,buf15) 0 2))
                 1)))
            (defun doc200_clone_probe ()
              (seq
               (nl_sci_dispatch ,slot14 ,dst14 14)
               (nl_sci_dispatch ,slot15 ,dst15 15)
               (+
                (if (= (ptr-read-u8 ,dst14 0) 14) 0 1)
                (if (= (ptr-read-u64 ,dst14 16) ,buf14) 0 2)
                (if (= (ptr-read-u64 ,dst14 24) 3) 0 4)
                (if (= (ptr-read-u8 (ptr-read-u64 ,dst14 16) 1) 200) 0 8)
                (if (= (ptr-read-u8 ,dst15 0) 15) 0 16)
                (if (= (ptr-read-u64 ,dst15 8) ,box15) 0 32)
                (if (= (ptr-read-u64 (ptr-read-u64 ,dst15 8) 16) 3) 0 64)
                (if (= (ptr-read-u8
                        (ptr-read-u64 (ptr-read-u64 ,dst15 8) 8) 1)
                       201)
                    0 128)
                (if (= (ptr-read-u64 ,box15 24) 2) 0 1))))
            (defun doc200_print_probe ()
              (seq
               (nl_cli_value_to_buf ,printbuf 0 ,slot14)
               (nl_cli_value_to_buf (+ ,printbuf 32) 0 ,slot15)
               (+
                (if (= (ptr-read-u8 ,printbuf 0) 34) 0 1)
                (if (= (ptr-read-u8 ,printbuf 1) 65) 0 2)
                (if (= (ptr-read-u8 ,printbuf 2) 92) 0 4)
                (if (= (ptr-read-u8 ,printbuf 3) 51) 0 8)
                (if (= (ptr-read-u8 ,printbuf 4) 49) 0 16)
                (if (= (ptr-read-u8 ,printbuf 5) 48) 0 32)
                (if (= (ptr-read-u8 ,printbuf 6) 66) 0 64)
                (if (= (ptr-read-u8 ,printbuf 7) 34) 0 128)
                (if (= (ptr-read-u8 (+ ,printbuf 32) 2) 92) 0 1)
                (if (= (ptr-read-u8 (+ ,printbuf 32) 3) 51) 0 2)
                (if (= (ptr-read-u8 (+ ,printbuf 32) 4) 49) 0 4)
                (if (= (ptr-read-u8 (+ ,printbuf 32) 5) 49) 0 8))))
            (defun doc200_producer_probe ()
              (seq
               ;; Immutable raw-byte allocation must write tag 14 and retain
               ;; the byte payload without UTF-8 interpretation.
               (nl_alloc_str_write_tag
                ,buf14 3 3 ,producer14 ,producerstrbuf 14)
               ;; Mutable raw-byte allocation must write tag 15.  Populate
               ;; its fresh NlStr buffer, then exercise the production
               ;; finalizer and require a tag-14 immutable result.
               (nl_alloc_mut_str_write_tag
                3 ,producer15 ,producerbox ,producerbuf 15)
               (ptr-write-u8 ,producerbuf 0 200)
               (ptr-write-u8 ,producerbuf 1 201)
               (ptr-write-u8 ,producerbuf 2 202)
               (ptr-write-u64 ,producerbox 16 3)
               (nl_mut_str_finalize_unibyte_write
                ,producerbuf 3 3 ,finalized14 ,finalizedbuf)
               (+
                (if (= (ptr-read-u8 ,producer14 0) 14) 0 1)
                (if (= (ptr-read-u64 ,producer14 24) 3) 0 2)
                (if (= (ptr-read-u8 (ptr-read-u64 ,producer14 16) 1) 200)
                    0 4)
                (if (= (ptr-read-u8 ,producer15 0) 15) 0 8)
                (if (= (ptr-read-u8 ,finalized14 0) 14) 0 16)
                (if (= (ptr-read-u64 ,finalized14 24) 3) 0 32)
                (if (= (ptr-read-u8 (ptr-read-u64 ,finalized14 16) 2) 202)
                    0 64))))
            (defun doc200_setup ()
              (seq
               ;; mmap(BASE, 4096, PROT_RW, MAP_FIXED|PRIVATE|ANON, -1, 0)
               (syscall-direct 9 ,base 4096 3 50 -1 0)
               ;; UnibyteStr: tag@0, cap@8, ptr@16, byte-len@24.
               (ptr-write-u64 ,slot14 0 14)
               (ptr-write-u64 ,slot14 8 3)
               (ptr-write-u64 ,slot14 16 ,buf14)
               (ptr-write-u64 ,slot14 24 3)
               (ptr-write-u8 ,buf14 0 65)
               (ptr-write-u8 ,buf14 1 200)
               (ptr-write-u8 ,buf14 2 66)
               ;; UnibyteMutStr: tag@0, NlStr*@8; box cap/ptr/len/rc.
               (ptr-write-u64 ,slot15 0 15)
               (ptr-write-u64 ,slot15 8 ,box15)
               (ptr-write-u64 ,box15 0 3)
               (ptr-write-u64 ,box15 8 ,buf15)
               (ptr-write-u64 ,box15 16 3)
               (ptr-write-u64 ,box15 24 1)
               (ptr-write-u8 ,buf15 0 65)
               (ptr-write-u8 ,buf15 1 201)
               (ptr-write-u8 ,buf15 2 66)
               0))
            (defun doc200_probe ()
              (seq
               (doc200_setup)
               (+ (doc200_predicate_probe)
                  (doc200_gc_probe)
                  (doc200_clone_probe)
                  (doc200_print_probe)
                  (doc200_producer_probe)))))))
    `(seq
      ,@gc-stubs
      ,@clone-stubs
      ,@type-stubs
      ,@printer-stubs
      ,@other-stubs
      ,gc-mark-slot
      ,@clone-forms
      ,type-dispatch
      ,arrayp
      ,strptr
      ,strlen
      ,@producer-forms
      ,@driver-forms
      ,@probe-defs
      (exit (doc200_probe)))))

(ert-deftest nelisp-doc200-unibyte-repr/production-and-consumers-use-unibyte-tags ()
  "Production alloc/finalize and hand-built tags survive every representation path."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (let ((path (make-temp-file "nelisp-doc200-unibyte-repr-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-doc200-unibyte-repr-test--probe-source) path)
          (should (file-executable-p path))
          (let ((rc (call-process path nil nil nil)))
            (should (= rc 0))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest nelisp-doc200-unibyte-repr/aot-stringp-expansion-has-all-four-tags ()
  "The direct AOT stringp expansion admits both string representations."
  (should
   (equal
    (nelisp-aot-compiler--aot-direct-tag-predicate-form '(stringp arg))
    '(or (= (sexp-tag arg) 5)
         (= (sexp-tag arg) 6)
         (= (sexp-tag arg) 14)
         (= (sexp-tag arg) 15)))))

(ert-deftest nelisp-doc200-unibyte-repr/public-producers-select-unibyte-tags ()
  "Public raw-string alloc/finalize wrappers select tags 14 and 15."
  (let ((str
         (nelisp-doc200-unibyte-repr-test--defun
          'nl_alloc_unibyte_str_pos
          nelisp-cc-nlstr-direct-ops--alloc-str-source))
        (mut
         (nelisp-doc200-unibyte-repr-test--defun
          'nl_alloc_unibyte_mut_str_inner
          nelisp-cc-nlstr-direct-ops--alloc-mut-str-source))
        (finalize
         (nelisp-doc200-unibyte-repr-test--defun
          'nl_mut_str_finalize
          nelisp-cc-nlstr-direct-ops--mut-str-finalize-source)))
    (should
     (nelisp-doc200-unibyte-repr-test--contains-p
      str '(alloc-bytes (if (= n 0) 1 n) 1)))
    (should (nelisp-doc200-unibyte-repr-test--contains-p str 14))
    (should (nelisp-doc200-unibyte-repr-test--contains-p mut 15))
    (should
     (nelisp-doc200-unibyte-repr-test--contains-p
      finalize '(if (= (ptr-read-u8 ptr 0) 15) 14 5)))))

(ert-deftest nelisp-doc200-unibyte-repr/aset-preserves-exact-tag-and-byte-length ()
  "Fixed-width mutation preserves tags 5/6/14/15 and their byte lengths."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (let ((path (make-temp-file "nelisp-doc200-aset-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-doc200-unibyte-repr-test--mutation-probe-source) path)
          (should (file-executable-p path))
          (should (= (call-process path nil nil nil) 0)))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest nelisp-doc200-unibyte-repr/jit-aset-enforces-the-same-rule ()
  "The non-standalone mutable-string path admits tag 15 and stays fixed-width."
  (let ((jit (nelisp-doc200-unibyte-repr-test--defun
              'nl_jit_mut_str_set_codepoint
              nelisp-cc-jit-mut-str-set-codepoint--source))
        (raw (nelisp-doc200-unibyte-repr-test--defun
              'nl_mut_str_set_codepoint_raw
              nelisp-cc-evalport-nonenv-mut-str-set-cp--source)))
    (should
     (nelisp-doc200-unibyte-repr-test--contains-p
      jit '(or (= (sexp-tag arg) 6) (= (sexp-tag arg) 15))))
    (should
     (nelisp-doc200-unibyte-repr-test--contains-p
      raw '(nl_msscp_unibyte_write arg idx val-cp)))
    (should
     (nelisp-doc200-unibyte-repr-test--contains-p
      raw '(nl_msscp_multibyte_write arg idx val-cp)))
    (should
     (nelisp-doc200-unibyte-repr-test--contains-p
      nelisp-cc-evalport-nonenv-mut-str-set-cp--source
      '(if (> val-cp 255) 1 0)))
    (should
     (nelisp-doc200-unibyte-repr-test--contains-p
      nelisp-cc-evalport-nonenv-mut-str-set-cp--source
      '(if (> val-cp 127) 1 0)))
    (should
     (nelisp-doc200-unibyte-repr-test--contains-p
      nelisp-cc-evalport-nonenv-mut-str-set-cp--source
      '(nelisp_ptr_write_u8 data byte-idx val-cp)))))

(ert-deftest nelisp-doc200-unibyte-repr/aset-rejects-nonascii-replaced-char ()
  "Adopt the Emacs 31.1 old-character restriction, deliberately unlike 30.1."
  ;; Measured host Emacs 30.1 value: the same mutation returns "a".  It is
  ;; intentionally absent from the host-version-pinned parity corpus.
  (should
   (equal
    (nelisp-doc200-unibyte-repr-test--eval-standalone
     "(let* ((s (copy-sequence \"\u3042\")) (tag0 (unibyte-string-p s)) (n0 (string-bytes s)) (r (condition-case nil (progn (aset s 0 ?a) 'no-signal) (error 'signalled)))) (list r tag0 (unibyte-string-p s) n0 (string-bytes s) (append s nil)))")
    "(signalled nil nil 3 3 (12354))\n")))

(ert-deftest nelisp-doc200-unibyte-repr/aset-rejects-nonascii-new-char ()
  "Adopt the Emacs 31.1 new-character restriction, deliberately unlike 30.1."
  ;; Measured host Emacs 30.1 value: the same mutation returns "\u3042b".  The
  ;; derived result stays in ERT rather than the 30.1 parity corpus.
  (should
   (equal
    (nelisp-doc200-unibyte-repr-test--eval-standalone
     "(let* ((s (copy-sequence \"ab\")) (tag0 (unibyte-string-p s)) (n0 (string-bytes s)) (r (condition-case nil (progn (aset s 0 ?\u3042) 'no-signal) (error 'signalled)))) (list r tag0 (unibyte-string-p s) n0 (string-bytes s) (append s nil)))")
    "(signalled t t 2 2 (97 98))\n")))

(ert-deftest nelisp-doc200-unibyte-repr/aset-allows-fixed-width-writes ()
  "Allow a byte in unibyte storage and ASCII-for-ASCII in multibyte storage."
  (should
   (equal
    (nelisp-doc200-unibyte-repr-test--eval-standalone
     "(list (let* ((s (copy-sequence (unibyte-string 200))) (tag0 (unibyte-string-p s)) (n0 (string-bytes s)) (r (aset s 0 255))) (list r tag0 (unibyte-string-p s) n0 (string-bytes s) (aref s 0))) (let* ((s (copy-sequence \"a\u3042\")) (tag0 (unibyte-string-p s)) (n0 (string-bytes s)) (r (aset s 0 ?b))) (list r tag0 (unibyte-string-p s) n0 (string-bytes s) (append s nil))))")
    "((255 t t 1 1 255) (98 nil nil 4 4 (98 12354)))\n")))

(ert-deftest nelisp-doc200-unibyte-repr/ascii-unibyte-keys-match-literals ()
  "Use `equal' string semantics for alist and member lookup across tags 5/14."
  (should
   (equal
    (nelisp-doc200-unibyte-repr-test--eval-standalone
     "(let* ((u (unibyte-string 97 98 99)) (lit \"abc\") (utbl (list (cons u 'from-unibyte))) (ltbl (list (cons lit 'from-literal)))) (list (cdr (assoc lit utbl)) (cdr (assoc u ltbl)) (if (member lit (list u)) t nil) (if (member u (list lit)) t nil)))")
    "(from-unibyte from-literal t t)\n")))

(ert-deftest nelisp-doc200-unibyte-repr/reader-numeric-escapes-produce-tag14 ()
  "Octal/hex escapes decode, and high-byte literals retain unibyte identity."
  (should
   (equal
    (nelisp-doc200-unibyte-repr-test--eval-standalone
     (concat
      "(list (append \"\\310\" nil) (append \"\\x41\" nil)"
      " (append \"\\101\" nil) (append \"\\12\" nil)"
      " (append \"\\1\" nil)"
      " (multibyte-string-p \"\\310\")"
      " (string-bytes \"\\310\") (unibyte-string-p \"\\310\"))"))
    "((200) (65) (65) (10) (1) nil 1 t)\n")))

(ert-deftest nelisp-doc200-unibyte-repr/reader-rejects-raw-multibyte-mixtures ()
  "A raw byte and a multibyte character cannot share a literal in either order."
  (dolist (source '("\"\\310あ\"" "\"あ\\310\""))
    (pcase-let ((`(,rc ,stderr)
                 (nelisp-doc200-unibyte-repr-test--eval-standalone-error
                  source)))
      (should (= rc 1))
      (should (string-match-p "nelisp-raw-byte-unrepresentable" stderr)))))

(provide 'nelisp-doc200-unibyte-repr-test)
;;; nelisp-doc200-unibyte-repr-test.el ends here
