;;; nelisp-standalone-gc-test.el --- executable standalone GC probes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The reader build's GC helpers are normally private AOT functions, so a
;; structural test cannot catch a write at the wrong address.  This test
;; links the production `nl_gc_mark_block' body into a tiny freestanding
;; executable and exercises both a real header and a checked allocator
;; guard+8 interior edge.  The latter must be ignored without changing the
;; guard word.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-standalone-build)

(defconst nelisp-standalone-gc-test--page #x32000000
  "Fixed scratch page used only by the freestanding GC probe.

The page is mapped with MAP_FIXED by the probe and is not part of the
standalone reader's arena.")

(defun nelisp-standalone-gc-test--source ()
  "Return a self-contained executable probe for `nl_gc_mark_block'."
  (let ((guard-word (ash #x5AFEC4EC 32))
        (mark (cl-find-if
               (lambda (form)
                 (and (consp form) (eq (car form) 'defun)
                      (eq (cadr form) 'nl_gc_mark_block)))
               (cdr nelisp-standalone--gc-source))))
    (unless mark
      (error "production nl_gc_mark_block is absent"))
    `(seq
      ;; Keep the production marker's memory effects while replacing only
      ;; arena membership and the header accessors with tiny test doubles.
      (defun nl_seq2 (_a b) b)
      (defun nl_gc_in_arena (_obj) 1)
      ;; This older probe isolates the raw high-half guard.  The exact-start
      ;; tests below exercise the production membership implementation.
      (defun nl_gc_object_start_p (_obj) 1)
      (defun nl_hdr_mark (hdr)
        (logand (ptr-read-u64 hdr 0) 7))
      (defun nl_hdr_set_mark (hdr m)
        (let ((x (ptr-read-u64 hdr 0)))
          (ptr-write-u64 hdr 0 (+ (logand x 4294967288) m))))
      ,mark
      (defun nelisp_standalone_gc_probe ()
        (seq
         ;; mmap(BASE, 4096, PROT_READ|PROT_WRITE, MAP_FIXED|PRIVATE|ANON,
         ;;      -1, 0)
         (syscall-direct 9 ,nelisp-standalone-gc-test--page 4096 3 50 -1 0)
         ;; A normal 40-byte block header must still be marked.
         (ptr-write-u64 ,nelisp-standalone-gc-test--page 0 40)
         (let ((live (nl_gc_mark_block
                      (+ ,nelisp-standalone-gc-test--page 8)))
               (marked (ptr-read-u64 ,nelisp-standalone-gc-test--page 0)))
           ;; At object+40 is the first word of a checked allocator suffix;
           ;; object+48 is the stale interior pointer that used to be passed
           ;; to `nl_gc_mark_block'.
           (ptr-write-u64 (+ ,nelisp-standalone-gc-test--page 40) 0
                          ,guard-word)
           (let ((interior (nl_gc_mark_block
                            (+ ,nelisp-standalone-gc-test--page 48)))
                 (guard (ptr-read-u64
                         (+ ,nelisp-standalone-gc-test--page 40) 0)))
             (if (and (= live 1)
                      (= marked 41)
                      (= interior 0)
                      (= guard ,guard-word))
                 0
               1))))
         )
      (exit (nelisp_standalone_gc_probe)))))

(defconst nelisp-standalone-gc-test--freelist-page #x10000000
  "Fixed page used for the production free-list head probe.

The boundary and compaction helpers retain these low-address small/fallback
heads for compatibility.  The probe maps the page before calling the native
production bodies, so it observes their actual writes rather than an Elisp
model of the lists.")

(defconst nelisp-standalone-gc-test--freelist-large-page #x32001000
  "Scratch page used as the eight large free-list heads in the probe.")

(defconst nelisp-standalone-gc-test--freelist-chunk-page #x32002000
  "Scratch page used as the fake current arena chunk descriptor.")

(defun nelisp-standalone-gc-test--find-defun (source name)
  "Find the production defun named NAME in SOURCE, or nil when absent."
  (cl-find-if
   (lambda (form)
     (and (consp form)
          (eq (car form) 'defun)
          (eq (cadr form) name)))
   (cdr source)))

(defun nelisp-standalone-gc-test--freelist-clear-source (&optional mode)
  "Return a native probe for the production free-list clear helpers.

The helper bodies are copied from the build's source constants at test
generation time.  Every seeded head is then read back after each native call;
checking only that a form exists would miss the original fallback-only clear.
MODE zero runs the REPL-boundary entry point; MODE one runs compaction.  The
pre-fix entry points remain executable but leave seeded heads behind, so this
same native probe returns a nonzero status there."
  (let* ((boundary-reclaim
          (nelisp-standalone-gc-test--find-defun
           nelisp-standalone--reader-boundary-source
           'nl_boundary_reclaim))
         (boundary-large
          (nelisp-standalone-gc-test--find-defun
           nelisp-standalone--reader-boundary-source
           'nl_boundary_clear_large_fl))
         (boundary-small
          (nelisp-standalone-gc-test--find-defun
           nelisp-standalone--reader-boundary-source
           'nl_boundary_clear_fl))
         (compact-large
          (nelisp-standalone-gc-test--find-defun
           nelisp-standalone--gc-source
           'nl_compact_clear_large_fl))
         (compact-small
          (nelisp-standalone-gc-test--find-defun
           nelisp-standalone--gc-source
           'nl_compact_clear_fl)))
    (unless (and boundary-reclaim compact-small)
      (error "production free-list reclaim entry points are absent"))
    `(seq
      ;; Keep the production reclaim/clear bodies intact.  Only the address
      ;; provider and unrelated cache/chunk dependencies are stubbed so all
      ;; writes stay inside this probe's mapped scratch pages.
      (defun nl_seq2 (_a b) b)
      (defun nl_freelist_small_mask_ptr ()
        (+ ,nelisp-standalone-gc-test--freelist-page 1024))
      (defun nl_freelist_large_head (bin)
        (+ ,nelisp-standalone-gc-test--freelist-large-page (* bin 8)))
      (defun nl_chunk_cursor_addr (chunk) (+ chunk 64))
      (defun nl_boundary_reset_tail_chunks (_chunk _reclaimed) 0)
      (defun nl_aref_cache_clear () 0)
      ;; `nl_boundary_reclaim' (production body, spliced in verbatim below)
      ;; zero-fills its reclaimed head span before rewinding the cursor
      ;; (fix for the reader-boundary reuse-without-zeroing defect); the
      ;; real implementation lives in the arena unit, out of reach of this
      ;; isolated probe, so stub it the same way every other cross-unit
      ;; dependency here is stubbed.  MARK_CURSOR == CURSOR in every mode
      ;; this probe drives (both call `nl_boundary_reclaim' with the fake
      ;; chunk's cursor slot pre-set to the same 1024 the mark itself
      ;; passes), so `head-reclaimed' is always 0 and this stub is never
      ;; actually invoked; it only needs to exist for the probe to compile.
      (defun nl_alloc_zero_fill (_obj _off _nbytes) 0)
      ,(or boundary-large
           '(defun nl_boundary_clear_large_fl (_n) 0))
      ,(or boundary-small
           '(defun nl_boundary_clear_fl (_n) 0))
      ,boundary-reclaim
      ,(or compact-large
           '(defun nl_compact_clear_large_fl (_n) 0))
      ,compact-small
      (defun nl_probe_seed-large (n)
        (if (> n 7) 0
          (nl_seq2
           (ptr-write-u64 (nl_freelist_large_head n) 0 (+ 9000 n))
           (nl_probe_seed-large (+ n 1)))))
      (defun nl_probe_seed-small (n)
        (if (> n 57)
            (ptr-write-u64 268435552 0 7777)
          (nl_seq2
           (ptr-write-u64 (+ 268435696 (* n 8)) 0 (+ 1000 n))
           (nl_probe_seed-small (+ n 1)))))
      (defun nl_probe_seed-all ()
        (nl_seq2
         (nl_probe_seed-small 0)
         (nl_seq2 (nl_probe_seed-large 0)
                  (ptr-write-u64
                   (nl_freelist_small_mask_ptr) 0 ,(1- (ash 1 58))))))
      (defun nl_probe-zero-large (n)
        (if (> n 7) 1
          (if (= (ptr-read-u64 (nl_freelist_large_head n) 0) 0)
              (nl_probe-zero-large (+ n 1))
            0)))
      (defun nl_probe-zero-small (n)
        (if (> n 57)
            (if (= (ptr-read-u64 268435552 0) 0) 1 0)
          (if (= (ptr-read-u64 (+ 268435696 (* n 8)) 0) 0)
              (nl_probe-zero-small (+ n 1))
            0)))
      (defun nl_probe-all-zero ()
        (if (and (= (ptr-read-u64 (nl_freelist_small_mask_ptr) 0) 0)
                 (= (nl_probe-zero-small 0) 1)
                 (= (nl_probe-zero-large 0) 1))
            1
          0))
      (defun nelisp_standalone_gc_freelist_probe (mode)
        ;; MAP_FIXED|MAP_PRIVATE|MAP_ANONYMOUS, read/write scratch pages.
        (syscall-direct 9 ,nelisp-standalone-gc-test--freelist-page 4096
                        3 50 -1 0)
        (syscall-direct 9 ,nelisp-standalone-gc-test--freelist-large-page 4096
                        3 50 -1 0)
        (syscall-direct 9 ,nelisp-standalone-gc-test--freelist-chunk-page 4096
                        3 50 -1 0)
        (if (= mode 0)
            (seq
             (nl_probe_seed-all)
             (ptr-write-u64
              (+ ,nelisp-standalone-gc-test--freelist-chunk-page 64) 0 1024)
             ;; The production REPL boundary is the entry point here.  On
             ;; the pre-fix tree it clears only the fallback head, so this
             ;; native probe returns 1 after seeded heads remain live.
             (nl_boundary_reclaim
              ,nelisp-standalone-gc-test--freelist-chunk-page 1024)
             (if (= (nl_probe-all-zero) 1) 0 1))
          (seq
           (nl_probe_seed-all)
           ;; The compaction helper is called directly, isolating its clear
           ;; contract from compaction's unrelated movement phases.
           (nl_compact_clear_fl 0)
           (if (= (nl_probe-all-zero) 1) 0 2))
         )
      )
      (exit (nelisp_standalone_gc_freelist_probe ,(or mode 0))))))

(defun nelisp-standalone-gc-test--run-freelist-probe (mode)
  "Compile and run the native free-list probe for MODE."
  (let ((path (make-temp-file "nelisp-standalone-gc-freelist-probe-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-standalone-gc-test--freelist-clear-source mode) path)
          (should (file-executable-p path))
          (call-process path nil nil nil))
      (when (file-exists-p path)
        (delete-file path)))))

(defun nelisp-standalone-gc-test--mask-source ()
  "Return a native probe for every small-bucket mask mutation route.

The production free, split, relink, pop, and mask-search bodies are linked
unchanged.  Only OS/address and allocator diagnostics are stubbed so the probe
can use a scratch page while still observing the real mask transitions."
  (let* ((arena (cdr nelisp-standalone--arena-source))
         (gc (cdr nelisp-standalone--gc-source))
         (names '(nl_freelist_small_mask_bit
                  nl_freelist_small_mask_set
                  nl_freelist_small_mask_clear
                  nl_freelist_small_mask_sync_head
                  nl_freelist_small_first_bit
                  nl_freelist_small_next_group
                  nl_freelist_small_next_index
                  nl_freelist_bucket_pop
                  nl_freelist_split_tail nl_freelist_large_bin
                  nl_freelist_bucket_split
                  nl_hdr_bt nl_hdr_mark nl_hdr_set_mark
                  nl_gc_free_block_link nl_gc_free_block
                  nl_gc_relink_free_one nl_gc_freelist_purge_buckets))
         (forms nil))
    (dolist (name names)
      (let ((form (or (nelisp-standalone-gc-test--find-defun
                      nelisp-standalone--arena-source name)
                      (nelisp-standalone-gc-test--find-defun
                       nelisp-standalone--gc-source name))))
        (unless form
          (error "production mask probe defun absent: %S" name))
        ;; Keep the production `data-addr nl_gc_diag' form intact in the
        ;; generator.  The extracted native probe redirects that external BSS
        ;; slot to its scratch page without adding a production call bridge.
        (setq form
              (cl-subst (+ nelisp-standalone-gc-test--freelist-page 1040)
                        '(data-addr nl_gc_diag) form))
        (push form forms)))
    `(seq
      (defun nl_seq2 (_a b) b)
      (defun nl_freelist_small_mask_ptr ()
        (+ ,nelisp-standalone-gc-test--freelist-page 1024))
      (defun nl_freelist_large_head (bin)
        (+ ,nelisp-standalone-gc-test--freelist-large-page (* bin 8)))
      (defun nl_gc_in_arena (_obj) 1)
      (defun nl_gc_is_boot (_hdr) 0)
      (defun nl_fl_record_trip (_cur _bt _want) 0)
      (defun nl_alloc_check_verify (_hdr) 0)
      (defun nl_gc_poison_fill (_hdr _off _bt) 0)
      ;; The production purge walker is exercised below.  This stub models
      ;; its already-tested chain removal while keeping unrelated scratch/
      ;; chunk state out of this focused mask probe.
      (defun nl_gc_freelist_purge_chain (head _base _size _want)
        (if (= (ptr-read-u64 head 0) 8888)
            8888
          (ptr-write-u64 head 0 0)))
      (defun nl_gc_freelist_purge_large_bins (_n _base _size) 0)
      ,@(nreverse forms)
      (defun nl_probe-head (i)
        (+ 268435696 (* i 8)))
      (defun nl_probe-hdr (i)
        (+ ,nelisp-standalone-gc-test--freelist-page 2048 (* i 16)))
      (defun nl_probe-free-all (i)
        (if (> i 57) 0
          (let* ((bt (+ 16 (* i 8)))
                 (hdr (nl_probe-hdr i)))
            (seq
             (ptr-write-u64 hdr 0 bt)
             (nl_gc_free_block hdr)
             (nl_probe-free-all (+ i 1))))))
      (defun nl_probe-pop-all (i)
        (if (> i 57) 0
          (let* ((bt (+ 16 (* i 8)))
                 (obj (nl_freelist_bucket_pop bt bt)))
            (if (= obj 0) (+ 10 i)
              (nl_probe-pop-all (+ i 1))))))
      (defun nl_probe-mask-all-p ()
        (if (= (ptr-read-u64 (nl_freelist_small_mask_ptr) 0)
               ,(1- (ash 1 58))) 1 0))
      (defun nl_probe-next-index-p ()
        (let ((start 0) (bit 0) (mask 1) (round 0) (expected -1)
              (bad (if (and (= (nl_freelist_small_next_index 0 0) -1)
                              (= (nl_freelist_small_next_index 1 58) -1))
                         0
                       1)))
          (seq
           (while (and (< start 58) (= bad 0))
             (setq bit 0)
             (while (and (< bit 58) (= bad 0))
               (if (= (nl_freelist_small_next_index
                       (nl_freelist_small_mask_bit (+ 16 (* bit 8))) start)
                      (if (>= bit start) bit -1))
                   0
                 (setq bad 1))
               (setq bit (+ bit 1)))
             (setq start (+ start 1)))
           ;; Mixed masks use an independent linear oracle.  Include starts
           ;; beyond the valid 58-bit mask and the machine shift width: a
           ;; shift by 64 must not wrap around to an eligible bucket.
           (while (and (< round 64) (= bad 0))
             (setq mask (logand (+ (* mask 1103515245) 12345)
                               ,(1- (ash 1 58))))
             (setq start 0)
             (while (and (< start 65) (= bad 0))
               (setq bit start)
               (setq expected -1)
               (while (and (< bit 58) (= expected -1))
                 (if (= (logand mask (shl 1 bit)) 0)
                     (setq bit (+ bit 1))
                   (setq expected bit)))
               (if (= (nl_freelist_small_next_index mask start) expected)
                   0
                 (setq bad 1))
               (setq start (+ start 1)))
             (setq round (+ round 1)))
           bad)))
      (defun nl_probe-split-route ()
        (let ((hdr (nl_probe-hdr 0)))
          (seq
           (ptr-write-u64 hdr 0 32)
           (nl_gc_free_block hdr)
           (let ((obj (nl_freelist_bucket_split 16)))
             (if (= obj 0) 20
               (if (and (= (logand
                            (ptr-read-u64 (nl_freelist_small_mask_ptr) 0) 1)
                           1)
                        (= (logand
                            (ptr-read-u64 (nl_freelist_small_mask_ptr) 0) 4)
                           0))
                   (if (= (nl_freelist_bucket_pop 16 16) 0) 21 0)
                 22))))))
      (defun nl_probe-split-tail-route ()
        (let ((hdr (nl_probe-hdr 1)))
          (seq
           (nl_freelist_split_tail hdr 32 16)
           (if (= (logand
                   (ptr-read-u64 (nl_freelist_small_mask_ptr) 0) 1) 1)
               (if (= (nl_freelist_bucket_pop 16 16) 0) 30 0)
             31))))
      (defun nl_probe-relink-route ()
        (let ((hdr (nl_probe-hdr 2)))
          (seq
           (ptr-write-u64 hdr 0 (+ 24 2))
           (nl_gc_relink_free_one hdr)
           (if (= (logand
                   (ptr-read-u64 (nl_freelist_small_mask_ptr) 0) 2) 2)
               (if (= (nl_freelist_bucket_pop 24 24) 0) 40 0)
             41))))
      (defun nl_probe-stale-positive ()
        (seq
         (nl_freelist_small_mask_set 32)
         (if (= (nl_freelist_bucket_split 16) 0)
             (if (= (ptr-read-u64 (nl_freelist_small_mask_ptr) 0) 0)
                 0
               50)
           51)))
      (defun nl_probe-purge-route ()
        (seq
         (ptr-write-u64 268435696 0 7777)
         (nl_freelist_small_mask_set 16)
         (nl_gc_freelist_purge_buckets 0 0 0)
         (if (and (= (ptr-read-u64 268435696 0) 0)
                  (= (ptr-read-u64 (nl_freelist_small_mask_ptr) 0) 0))
             0
           70)))
      (defun nl_probe-purge-nonempty-route ()
        (seq
         ;; The stub preserves this sentinel, so production purge must take
         ;; its nonempty sync branch and retain bit 16.
         (ptr-write-u64 268435696 0 8888)
         (nl_freelist_small_mask_set 16)
         (nl_gc_freelist_purge_buckets 0 0 0)
         (if (and (= (ptr-read-u64 268435696 0) 8888)
                  (= (logand
                      (ptr-read-u64 (nl_freelist_small_mask_ptr) 0) 1)
                     1))
             0
           71)))
      (defun nelisp_standalone_gc_small_mask_probe ()
        (syscall-direct 9 ,nelisp-standalone-gc-test--freelist-page 4096
                        3 50 -1 0)
        (syscall-direct 9 ,nelisp-standalone-gc-test--freelist-large-page 4096
                        3 50 -1 0)
        (if (= (nl_probe-next-index-p) 0)
            (if (= (nl_probe-free-all 0) 0)
                (if (= (nl_probe-mask-all-p) 1)
                    (if (= (nl_probe-pop-all 0) 0)
                        (if (= (nl_probe-split-route) 0)
                            (if (= (nl_probe-split-tail-route) 0)
                            (if (= (nl_probe-relink-route) 0)
                                (if (= (nl_probe-stale-positive) 0)
                                    (if (= (nl_probe-purge-route) 0)
                                        (nl_probe-purge-nonempty-route)
                                      72)
                                  60)
                              61)
                          62)
                      63)
                  64)
              65)
          66)))
      (exit (nelisp_standalone_gc_small_mask_probe)))))

(ert-deftest nelisp-standalone-gc-small-mask-native-routes ()
  "Native production routes maintain and consume every small mask bit."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (let ((path (make-temp-file "nelisp-standalone-gc-small-mask-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-standalone-gc-test--mask-source) path)
          (should (file-executable-p path))
          (should (= (call-process path nil nil nil) 0)))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest nelisp-standalone-gc-free-list-clear-boundary-clears-every-head ()
  "REPL boundary clears every small, fallback, and large head."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (should (= (nelisp-standalone-gc-test--run-freelist-probe 0) 0)))

(ert-deftest nelisp-standalone-gc-free-list-clear-compaction-clears-every-head ()
  "Compaction clears every small, fallback, and large head."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (should (= (nelisp-standalone-gc-test--run-freelist-probe 1) 0)))

(ert-deftest nelisp-standalone-gc-small-mask-init-resets-generated-metadata ()
  "Both cold-init metadata generators reset the non-persisted mask slot."
  (let ((mask-write
         (lambda (forms)
           (cl-find-if
            (lambda (form)
              (and (consp form)
                   (eq (car form) 'ptr-write-u64)
                   (equal (nth 1 form) '(nl_freelist_small_mask_ptr))
                   (= (nth 2 form) 0)
                   (= (nth 3 form) 0)))
            forms))))
    (should (funcall mask-write
                     (nelisp-standalone--arena-init-metadata-forms
                      #x10000000 4096)))
    (should (funcall mask-write
                     (nelisp-standalone--arena-init-metadata-forms-dynamic
                      'base 4096)))))

(defun nelisp-standalone-gc-test--strip-mask-reset (tree)
  "Remove mask reset writes from TREE for the against-the-bug red probe."
  (let ((target '(ptr-write-u64 (nl_freelist_small_mask_ptr) 0 0)))
    (if (consp tree)
        (let (out)
          (dolist (item tree (nreverse out))
            (unless (equal item target)
              (push (nelisp-standalone-gc-test--strip-mask-reset item) out))))
      tree)))

(ert-deftest nelisp-standalone-gc-small-mask-clear-without-reset-is-red ()
  "The seeded mask makes omission of the production reset observable."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (let ((path (make-temp-file "nelisp-standalone-gc-small-mask-red-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-standalone-gc-test--strip-mask-reset
            (nelisp-standalone-gc-test--freelist-clear-source 0)) path)
          (should (file-executable-p path))
          (should-not (= (call-process path nil nil nil) 0)))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest nelisp-standalone-gc-mark-block-protects-interior-guard-edge ()
  "A guard+8 interior edge is ignored while a real block is marked."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (let ((path (make-temp-file "nelisp-standalone-gc-probe-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-standalone-gc-test--source) path)
          (should (file-executable-p path))
          (should (= (call-process path nil nil nil) 0)))
      (when (file-exists-p path)
        (delete-file path)))))

(defun nelisp-standalone-gc-test--exact-source ()
  "Exercise production indexing and marking against a mutable synthetic heap."
  (let* ((base #x32000000) (extra #x32040000)
         (ctx (+ base 4096))
         (descriptor #x10000300)
         (forms
          (mapcar
           (lambda (name)
             (or (nelisp-standalone-gc-test--find-defun
                  nelisp-standalone--gc-source name)
                 (nelisp-standalone-gc-test--find-defun
                  nelisp-standalone--arena-source name)
                 (error "Missing production helper %s" name)))
           '(nl_align_up nl_hdr_bt nl_hdr_mark nl_hdr_set_mark
             nl_alloc_zero_fill nl_gc_bt_ok nl_gc_chunk_cursor nl_gc_chunk_end
             nl_gc_chunk_contains nl_gc_in_arena nl_gc_is_boot nl_gc_index_end
             nl_gc_index_bytes nl_gc_index_fill nl_gc_index_prepare
             nl_gc_index_contains nl_gc_index_test nl_gc_object_start_p
             nl_gc_mark_block))))
    (cl-subst
     ctx '(data-addr nl_gc_start_index)
     `(seq
       (defun nl_seq2 (_a b) b)
       ;; Only the OS boundary is doubled; all header walks and bit tests
       ;; below are the actual compiled production bodies.
       (defun nl_os_alloc_chunk (_size)
         (if (= (ptr-read-u64 ,ctx 48) 1) 0 ,(+ base 8192)))
       (defun nl_os_commit_range (_base _old _new)
         (if (= (ptr-read-u64 ,ctx 48) 2) 0 1))
       (defun nl_os_free_chunk (mapping _size)
         (seq (ptr-write-u64 ,ctx 56 mapping) 1))
       ,@forms
       (defun gc_probe_commit_failure ()
         (seq
          (ptr-write-u64 ,ctx 48 2)
          ;; A different old mapping must survive a failed replacement.
          (ptr-write-u64 ,ctx 0 ,(+ base 12288))
          (ptr-write-u64 ,ctx 8 1024)
          (ptr-write-u64 ,ctx 56 0)
          (if (or (= (nl_gc_index_prepare) 1)
                  (= (ptr-read-u64 ,ctx 16) 1)
                  (not (= (ptr-read-u64 ,ctx 0) ,(+ base 12288)))
                  (not (= (ptr-read-u64 ,ctx 8) 1024))
                  (not (= (ptr-read-u64 ,ctx 56) ,(+ base 8192)))) 20 0)))

       (defun gc_probe_seed ()
         (seq
          (ptr-write-u64 268436160 0 ,descriptor)
          (ptr-write-u64 268436168 0 ,descriptor)
          (ptr-write-u64 268435456 0 96)
          (ptr-write-u64 ,descriptor 0 ,base)
          (ptr-write-u64 ,descriptor 24 ,base)
          (ptr-write-u64 ,base 0 40)
          (ptr-write-u64 ,base 8 4)
          ;; A plausible size followed by another plausible size is still
          ;; payload, not a header; the old two-level owner check accepts it.
          (ptr-write-u64 ,base 16 32)
          (ptr-write-u64 ,base 40 56)
          (ptr-write-u64 ,base 48 32)
          (ptr-write-u64 ,base 80 ,(ash #x5afec4ec 32))))
       (defun gc_probe_edges ()
         (if (and
              (= (nl_gc_mark_block ,(+ base 16)) 0)
              (= (ptr-read-u64 ,base 8) 4)
              (= (nl_gc_mark_block ,(+ base 24)) 0)
              (= (nl_gc_object_start_p ,(+ base 24)) 0)
              (= (ptr-read-u64 ,base 16) 32)
              (= (nl_gc_mark_block ,(+ base 88)) 0)
              (= (ptr-read-u64 ,base 80) ,(ash #x5afec4ec 32))
              (= (nl_gc_mark_block ,base) 0)
              (= (nl_gc_mark_block ,(+ base 9)) 0)
              (= (nl_gc_mark_block ,(+ base 96)) 0)
              (= (nl_gc_mark_block ,(+ base 8)) 1)
              (= (ptr-read-u64 ,base 0) 41)
              (= (nl_gc_mark_block ,(+ base 8)) 0)
              (= (nl_gc_object_start_p ,(+ base 48)) 1)
              (= (ptr-read-u64 ,base 40) 56)
              (= (nl_gc_mark_block ,(+ base 48)) 1)
              (= (ptr-read-u64 ,base 40) 57)) 1 0))
       (defun gc_probe_run ()
         (seq
          (syscall-direct 9 #x10000000 4096 3 50 -1 0)
          (syscall-direct 9 ,base 131072 3 50 -1 0)
          (gc_probe_seed)
          (if (= (gc_probe_edges) 0) 11
            (seq
             (gc_probe_seed)
             (if (= (nl_gc_index_prepare) 0) 12
               (if (= (gc_probe_edges) 0) 13
                 (seq
                  ;; Coalesce/reset/restore change the header chain only
                  ;; outside the mark phase.  Rebuild must erase old starts.
                  (nl_gc_index_end)
                  (ptr-write-u64 ,base 0 96)
                  (if (or (= (nl_gc_object_start_p ,(+ base 48)) 1)
                          (= (nl_gc_index_prepare) 0)
                          (= (nl_gc_object_start_p ,(+ base 48)) 1)) 14
                    (seq
                     (nl_gc_index_end)
                     (ptr-write-u64 268435456 0 0)
                     (if (or (= (nl_gc_index_prepare) 0)
                             (= (nl_gc_object_start_p ,(+ base 8)) 1)) 15
                       (seq
                        (nl_gc_index_end)
                        (gc_probe_seed)
                        ;; Add a growth chunk, then unmap and unlink it.
                        (syscall-direct 9 ,extra 4096 3 50 -1 0)
                        (ptr-write-u64 ,extra 0 40)
                        (ptr-write-u64 ,(+ descriptor 128) 0 ,extra)
                        (ptr-write-u64 ,(+ descriptor 128) 16 40)
                        (ptr-write-u64 ,(+ descriptor 128) 24 ,extra)
                        (ptr-write-u64 ,descriptor 48 ,(+ descriptor 128))
                        (if (or (= (nl_gc_index_prepare) 0)
                                (= (nl_gc_mark_block ,(+ extra 8)) 0)) 16
                          (seq
                           (nl_gc_index_end)
                           (ptr-write-u64 ,descriptor 48 0)
                           (syscall-direct 11 ,extra 4096 0 0 0 0)
                           (if (or (= (nl_gc_index_prepare) 0)
                                   (= (nl_gc_object_start_p ,(+ extra 8)) 1)) 17
                             (seq
                              (nl_gc_index_end)
                              ;; Malformed headers and OS allocation failure
                              ;; must not publish a partial active index.
                              (ptr-write-u64 ,base 0 0)
                              (if (or (= (nl_gc_index_prepare) 1)
                                      (= (ptr-read-u64 ,ctx 16) 1)) 18
                                (seq
                                 (gc_probe_seed)
                                 (ptr-write-u64 ,ctx 8 0)
                                 (ptr-write-u64 ,ctx 48 1)
                                 (if (or (= (nl_gc_index_prepare) 1)
                                         (= (ptr-read-u64 ,ctx 16) 1)) 19
                                   (gc_probe_commit_failure)))))))))))))))))))
       (exit (gc_probe_run)))
     :test #'equal)))

(ert-deftest nelisp-standalone-gc-exact-start-rejects-interiors-and-rebuilds ()
  "Interior payload is immutable; genuine starts survive index rebuilds."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (let ((path (make-temp-file "nelisp-gc-exact-start-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-standalone-gc-test--exact-source) path)
          (should (= (call-process path nil nil nil) 0)))
      (delete-file path))))

(defun nelisp-standalone-gc-test--conserv-pin-source ()
  "Return a production conservative-pin regression probe.

The probe deliberately uses the real mark/index/conservative bodies.  The
only doubles are the OS mapping boundary and the unrelated free/live hooks;
the heap and all mark decisions remain native production code."
  (let* ((base #x34000000)
         (index-ctx #x35000000)
         (index-map #x35010000)
         (conserv-state #x35020000)
         (queue-map #x35030000)
         (queue-grow-map #x35040000)
         (test-flag #x35050000)
         (grow-base #x36000000)
         (descriptor #x10000300)
         (names '(nl_align_up nl_hdr_bt nl_hdr_mark nl_hdr_set_mark
                  nl_alloc_zero_fill
                  nl_gc_bt_ok nl_gc_chunk_cursor nl_gc_chunk_end
                  nl_gc_chunk_contains nl_gc_in_arena nl_gc_is_boot
                  nl_gc_index_end nl_gc_index_bytes nl_gc_index_fill
                  nl_gc_index_prepare nl_gc_index_contains nl_gc_index_test
                  nl_gc_object_start_p nl_gc_mark_block nl_gc_mark_buf
                  nl_gc_block_elem_cap nl_gc_mark_vec_slots
                  nl_gc_mark_cons nl_gc_mark_slot nl_gc_conserv_state_clear
                  nl_gc_mark_char_table_slots nl_gc_mark_char_table_box
                  nl_gc_mark_bool_vector_box
                  nl_gc_conserv_failed_p nl_gc_conserv_pinned_p
                  nl_gc_conserv_queue_grow nl_gc_conserv_queue_push
                  nl_gc_conserv_begin nl_gc_conserv_valid_p nl_gc_conserv_pin
                  nl_gc_conserv_owner_slow nl_gc_conserv_owner_indexed
                  nl_gc_conserv_resolve nl_gc_mark_recorded_slot
                  nl_gc_conserv_owner nl_gc_conserv_word nl_gc_conserv_scan
                  nl_gc_conserv_drain nl_gc_conserv_rollback_chunk
                  nl_gc_conserv_rollback nl_gc_conserv_finish
                  nl_gc_conserv_maybe))
         (forms
          (mapcar
           (lambda (name)
             (let ((form (or (nelisp-standalone-gc-test--find-defun
                             nelisp-standalone--gc-source name)
                            (nelisp-standalone-gc-test--find-defun
                             nelisp-standalone--arena-source name))))
               (unless form (error "Missing production conservative helper %s" name))
               (setq form
                     (cl-subst index-ctx '(data-addr nl_gc_start_index) form
                               :test #'equal))
               (setq form
                     (cl-subst conserv-state '(data-addr nl_gc_conserv_state) form
                               :test #'equal))
               form))
           names)))
    `(seq
      (defun nl_seq2 (_a b) b)
      ;; The index allocation is the large request; the conservative queue is
      ;; the smaller request.  A test flag at state+56 forces queue allocation
      ;; failure after the heap/index have been prepared.
      (defun nl_os_alloc_chunk (size)
        (if (and (= size 65536)
                 (= (ptr-read-u64 ,test-flag 0) 2))
            ,queue-grow-map
          (if (and (< size 65536)
                 (= (ptr-read-u64 ,conserv-state 56) 1))
              0
            (if (> size 65536)
                (syscall-direct 9 0 size 3 34 -1 0)
              (if (= size 65536) ,index-map ,queue-map)))))
      (defun nl_os_commit_range (_base _old _new) 1)
      (defun nl_os_free_chunk (base _size)
        (if (and (= (ptr-read-u64 ,test-flag 0) 2)
                 (= base ,queue-map))
            (syscall-direct 11 ,queue-map 65536 0 0 0 0)
          1))
      ,@forms
      ;; H0 root Cons Sexp; Hpad makes S's low byte 08; B is a raw ConsBox.
      ;; D is a genuine Vector Sexp and E its NlVector box.  T is reachable
      ;; through E's one vector data slot only when D is typed as a Vector.
      (defun probe-seed ()
        (seq
         (ptr-write-u64 ,base 0 40)
         (ptr-write-u64 (+ ,base 40) 0 216)
         (ptr-write-u64 (+ ,base 256) 0 40)
         (ptr-write-u64 (+ ,base 296) 0 16)
         (ptr-write-u64 (+ ,base 312) 0 32)
         (ptr-write-u64 (+ ,base 344) 0 40)
         (ptr-write-u64 (+ ,base 384) 0 40)
         (ptr-write-u64 (+ ,base 424) 0 16)
         (ptr-write-u64 (+ ,base 440) 0 40)
         (ptr-write-u64 (+ ,base 480) 0 16)
         ;; R Cons Sexp -> B.
         (ptr-write-u64 (+ ,base 8) 0 7)
         (ptr-write-u64 (+ ,base 8) 8 (+ ,base 320))
         ;; S Symbol -> byte buffer.
         (ptr-write-u64 (+ ,base 264) 0 4)
         (ptr-write-u64 (+ ,base 264) 8 1)
         (ptr-write-u64 (+ ,base 264) 16 (+ ,base 304))
         (ptr-write-u64 (+ ,base 264) 24 1)
         ;; B car S (low byte 08), cdr D.
         (ptr-write-u64 (+ ,base 320) 0 (+ ,base 264))
         (ptr-write-u64 (+ ,base 320) 8 (+ ,base 352))
         (ptr-write-u64 (+ ,base 320) 16 1)
         ;; D Vector Sexp -> E; D+16 is zero, which triggers the old bug.
         (ptr-write-u64 (+ ,base 352) 0 8)
         (ptr-write-u64 (+ ,base 352) 8 (+ ,base 392))
         (ptr-write-u64 (+ ,base 352) 16 0)
         (ptr-write-u64 (+ ,base 352) 24 0)
         ;; E NlVector -> vector data -> T.
         (ptr-write-u64 (+ ,base 392) 0 1)
         (ptr-write-u64 (+ ,base 392) 8 (+ ,base 432))
         (ptr-write-u64 (+ ,base 392) 16 1)
         (ptr-write-u64 (+ ,base 392) 24 1)
         (ptr-write-u64 (+ ,base 432) 0 (+ ,base 448))
         ;; T String -> byte buffer.
         (ptr-write-u64 (+ ,base 448) 0 5)
         (ptr-write-u64 (+ ,base 448) 8 1)
         (ptr-write-u64 (+ ,base 448) 16 (+ ,base 488))
         (ptr-write-u64 (+ ,base 448) 24 1)
         ;; Descriptor/header chain and index context.
         (ptr-write-u64 268436160 0 ,descriptor)
         (ptr-write-u64 268436168 0 ,descriptor)
         (ptr-write-u64 268435456 0 496)
         (ptr-write-u64 ,descriptor 0 ,base)
         (ptr-write-u64 ,descriptor 16 496)
         (ptr-write-u64 ,descriptor 24 ,base)
         (ptr-write-u64 ,descriptor 48 0)
         (ptr-write-u64 268435664 0 0)
         (ptr-write-u64 268436464 0 0)
         (ptr-write-u64 ,index-ctx 0 0)
         (ptr-write-u64 ,index-ctx 8 0)
         (ptr-write-u64 ,index-ctx 16 0)
         (ptr-write-u64 ,index-ctx 24 0)
         (ptr-write-u64 ,index-ctx 32 0)
         (ptr-write-u64 ,conserv-state 56 0)))
      (defun probe-prepare ()
        (if (and (= (nl_gc_index_prepare) 1)
                 (= (ptr-read-u64 ,index-ctx 16) 1)
                 (= (nl_gc_object_start_p (+ ,base 8)) 1)
                 (= (nl_gc_object_start_p (+ ,base 320)) 1)
                 (= (nl_gc_object_start_p (+ ,base 448)) 1))
            1 0))
      (defun probe-precise ()
        (probe-seed)
        (if (= (probe-prepare) 1)
            (if (= (nl_gc_mark_slot (+ ,base 8)) 0)
                (if (= (nl_hdr_mark (+ ,base 440)) 1) 0 11)
              12)
          13))
      ;; This is the against-the-bug case.  The raw B candidate has car's
      ;; low byte 08, but production conservative code must treat it as opaque
      ;; and pin the transitive payload words rather than dispatching Vector.
      (defun probe-raw-cons ()
        (probe-seed)
        (if (= (probe-prepare) 1)
            (if (= (nl_gc_conserv_begin) 1)
                (seq
                 (nl_gc_conserv_word (+ ,base 320))
                 (nl_gc_conserv_drain)
                 (if (= (nl_gc_conserv_failed_p) 1) 14
                   (nl_gc_conserv_finish)
                   (if (/= (nl_hdr_mark (+ ,base 312)) 4) 31
                     (if (/= (nl_hdr_mark (+ ,base 344)) 4) 32
                       (if (/= (nl_hdr_mark (+ ,base 384)) 4) 33
                         (if (/= (nl_hdr_mark (+ ,base 440)) 4) 34
                           (if (= (nl_gc_mark_slot (+ ,base 8)) 0)
                               (if (= (nl_hdr_mark (+ ,base 440)) 1) 0 35)
                             36)))))))
              16)
          17))
      ;; Capacity=8 is a raw VecBox false-tag case.  It must be pinned and
      ;; scanned as opaque words without any Sexp interpretation or fault.
      (defun probe-raw-vector-box ()
        (probe-seed)
        (ptr-write-u64 (+ ,base 392) 0 8)
        (if (= (probe-prepare) 1)
            (if (= (nl_gc_conserv_begin) 1)
                (seq
                 (nl_gc_conserv_word (+ ,base 392))
                 (nl_gc_conserv_drain)
                 (if (= (nl_gc_conserv_failed_p) 1) 18
                   (nl_gc_conserv_finish)
                   (if (= (nl_hdr_mark (+ ,base 384)) 4)
                       (if (= (nl_hdr_mark (+ ,base 440)) 4) 0 19)
                     19)))
              20)
          21))
      ;; A self-cycle exercises mark4 as both queued and already scanned.
      (defun probe-cycle ()
        (probe-seed)
        (ptr-write-u64 (+ ,base 392) 8 (+ ,base 392))
        (if (= (probe-prepare) 1)
            (if (= (nl_gc_conserv_begin) 1)
                (seq
                 (nl_gc_conserv_word (+ ,base 392))
                 (nl_gc_conserv_drain)
                 (nl_gc_conserv_finish)
                 (if (= (nl_hdr_mark (+ ,base 384)) 4) 0 22))
              23)
          24))
      ;; Initial queue allocation failure must leave all block marks clear.
      (defun probe-oom ()
        (probe-seed)
        (if (= (probe-prepare) 1)
            (seq
             (ptr-write-u64 ,conserv-state 56 1)
             (ptr-write-u64 268436464 0 1)
             (if (= (nl_gc_conserv_maybe) 0)
                 (if (and (= (nl_hdr_mark (+ ,base 312)) 0)
                          (= (nl_hdr_mark (+ ,base 344)) 0)
                          (= (nl_hdr_mark (+ ,base 384)) 0)
                          (= (nl_hdr_mark (+ ,base 440)) 0))
                     0 25)
               26))
          27))
      ;; Fill a separate synthetic chunk with 4097 exact 16-byte blocks.  The
      ;; conservative queue must grow while the root payload is being drained;
      ;; the free stub unmaps the old queue so a stale drain base faults.
      (defun probe-queue-grow ()
        (let ((i 0))
          (ptr-write-u64 268436160 0 #x10000400)
          (ptr-write-u64 268436168 0 #x10000400)
          ;; One large root payload fans out to 4097 leaves.  Growth therefore
          ;; happens inside nl_gc_conserv_drain, after it has captured head=0.
          (ptr-write-u64 268435456 0 98336)
          (ptr-write-u64 #x10000400 0 ,grow-base)
          (ptr-write-u64 #x10000400 24 ,grow-base)
          (ptr-write-u64 #x10000400 48 0)
          (ptr-write-u64 ,grow-base 0 32784)
          (while (< i 4097)
            (ptr-write-u64 (+ ,grow-base 8 (* i 8)) 0
                           (+ ,grow-base 32792 (* i 16)))
            (ptr-write-u64 (+ ,grow-base 32784 (* i 16)) 0 16)
            (ptr-write-u64 (+ ,grow-base 32792 (* i 16)) 0 1)
            (setq i (+ i 1)))
          (ptr-write-u64 ,test-flag 0 0)
          (if (= (nl_gc_index_prepare) 1)
              (seq
               (ptr-write-u64 ,test-flag 0 2)
               (if (= (nl_gc_conserv_begin) 1)
                   (seq
                    (nl_gc_conserv_word (+ ,grow-base 8))
                    (nl_gc_conserv_drain)
                    (if (= (nl_gc_conserv_failed_p) 1) 38
                      (nl_gc_conserv_finish)
                    (if (and (= (nl_hdr_mark ,grow-base) 4)
                             (= (nl_hdr_mark (+ ,grow-base 98320)) 4))
                          0 39)))
                 40))
            41)))
      (defun probe-interior-and-boot ()
        (probe-seed)
        ;; Boot B contains pointers into the mutable post-boot graph.
        (ptr-write-u64 268435664 0 (+ ,base 344))
        (if (= (probe-prepare) 0) 50
          (if (= (nl_gc_conserv_begin) 0) 51
            ;; This is an unaligned interior pointer, not a Sexp start.
            (nl_gc_conserv_word (+ ,base 329))
            (nl_gc_conserv_drain)
            (nl_gc_conserv_finish)
            (if (and (= (nl_hdr_mark (+ ,base 312)) 4)
                     (= (nl_hdr_mark (+ ,base 440)) 4)
                     (= (nl_hdr_mark (+ ,base 480)) 4)) 0 52))))
      (defun probe-recorded-owner ()
        (probe-seed)
        (if (= (probe-prepare) 0) 53
          (nl_gc_mark_recorded_slot (+ ,base 264))
          ;; Keeping an owning allocation must not claim its payload has
          ;; already received every possible precise traversal.
          (if (and (= (nl_hdr_mark (+ ,base 256)) 4)
                   (= (nl_hdr_mark (+ ,base 296)) 1)
                   (= (nl_gc_mark_block (+ ,base 264)) 1)) 0 54)))
      (defun probe-large-interior ()
        (probe-seed)
        (syscall-direct 9 #x37000000 16781312 3 50 -1 0)
        (ptr-write-u64 #x37000000 0 16777232)
        (ptr-write-u64 #x37000000 8 (+ ,base 448))
        (ptr-write-u64 ,descriptor 48 #x10000400)
        (ptr-write-u64 #x10000400 0 #x37000000)
        (ptr-write-u64 #x10000400 16 16777232)
        (ptr-write-u64 #x10000400 24 #x37000000)
        (ptr-write-u64 #x10000400 48 0)
        ;; Deliberately keep CURRENT pointing to the other chunk.
        (if (= (nl_gc_index_prepare) 0) 55
          (if (= (nl_gc_conserv_begin) 0) 56
            (nl_gc_conserv_word (+ #x37000000 16777231))
            (nl_gc_conserv_drain)
            (nl_gc_conserv_finish)
            (if (and (= (nl_hdr_mark #x37000000) 4)
                     (= (nl_hdr_mark (+ ,base 440)) 4)
                     (= (nl_hdr_mark (+ ,base 480)) 4)) 0 57))))
      (defun probe-assert (code)
        (if (/= code 0) (syscall-direct 60 code 0 0 0 0 0) 0))
      (defun probe-run ()
        (seq
         (syscall-direct 9 #x10000000 4096 3 50 -1 0)
         (syscall-direct 9 ,base 131072 3 50 -1 0)
         (syscall-direct 9 ,index-ctx 4096 3 50 -1 0)
         (syscall-direct 9 ,index-map 65536 3 50 -1 0)
         (syscall-direct 9 ,conserv-state 4096 3 50 -1 0)
         (syscall-direct 9 ,queue-map 65536 3 50 -1 0)
         (syscall-direct 9 ,queue-grow-map 65536 3 50 -1 0)
         (syscall-direct 9 ,test-flag 4096 3 50 -1 0)
         (syscall-direct 9 ,grow-base 131072 3 50 -1 0)
         (probe-assert (probe-interior-and-boot))
         (probe-assert (probe-recorded-owner))
         (probe-assert (probe-large-interior))
         (let ((a (probe-precise))
               (b (probe-raw-cons))
               (c (probe-raw-vector-box))
               (d (probe-cycle))
               (e (probe-oom))
               (f (probe-queue-grow)))
           (if (= a 0) (if (= b 0) (if (= c 0) (if (= d 0) (if (= e 0) f 28) 29) 30) 31) 32))))
      (exit (probe-run)))))

(ert-deftest nelisp-standalone-gc-conservative-pin-is-untyped-and-abortable ()
  "Raw boxes are pinned opaquely and queue allocation failure aborts safely."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (let ((path (make-temp-file "nelisp-gc-conserv-pin-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-standalone-gc-test--conserv-pin-source) path)
          (should (= (call-process path nil nil nil) 0)))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-standalone-gc-conservative-root-regressions-are-red ()
  "Execute omissions of interior, boot, large, and untyped-owner protection."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (dolist (case '((interior . 52) (boot . 52) (large . 57) (owner-type . 54)))
    (let* ((source (nelisp-standalone-gc-test--conserv-pin-source))
           (path (make-temp-file "nelisp-gc-conserv-mutant-"))
           (mutation (car case)))
      (setq source
            (cons 'seq
                  (mapcar
                   (lambda (form)
                     (cond
                      ((and (eq mutation 'interior)
                            (eq (cadr form) 'nl_gc_conserv_word))
                       '(defun nl_gc_conserv_word (w) (nl_gc_conserv_pin w)))
                      ((and (memq mutation '(boot large))
                            (eq (cadr form) 'nl_gc_conserv_valid_p))
                       `(defun nl_gc_conserv_valid_p (w)
                          (if ,(if (eq mutation 'boot)
                                   '(= (nl_gc_is_boot (- w 8)) 1)
                                 '(>= (nl_hdr_bt (- w 8)) 16777216))
                              0 ,(nth 3 form))))
                      ((and (eq mutation 'owner-type)
                            (eq (cadr form) 'nl_gc_mark_recorded_slot))
                       (cl-subst '(nl_hdr_set_mark (- sp 8) 1)
                                 '(nl_hdr_set_mark (- sp 8) 4)
                                 form :test #'equal))
                      (t form)))
                   (cdr source))))
      (unwind-protect
          (progn
            (nelisp-aot-compile-sexp source path)
            (should (= (call-process path nil nil nil) (cdr case))))
        (delete-file path)))))

(provide 'nelisp-standalone-gc-test)

;;; nelisp-standalone-gc-test.el ends here
