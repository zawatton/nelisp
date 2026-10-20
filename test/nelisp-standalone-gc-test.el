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
         (large-mask-clear
          (cl-subst (+ nelisp-standalone-gc-test--freelist-large-page 2048)
                    '(data-addr nl_freelist_large_mask)
                    (nelisp-standalone-gc-test--find-defun
                     nelisp-standalone--arena-source 'nl_freelist_large_mask_clear_all)
                    :test #'equal))
         (compact-small
          (nelisp-standalone-gc-test--find-defun
           nelisp-standalone--gc-source
           'nl_compact_clear_fl))
         ;; The boundary reclaim also drops the allocator's current run;
         ;; its nl_gc_diag slot is redirected to the scratch page.
         (run-reset
          (cl-subst (+ nelisp-standalone-gc-test--freelist-page 1040)
                    '(data-addr nl_gc_diag)
                    (nelisp-standalone-gc-test--find-defun
                     nelisp-standalone--arena-source 'nl_alloc_run_reset)
                    :test #'equal)))
    (unless (and boundary-reclaim compact-small run-reset)
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
      ;; The checked allocator's reclaim poisoning, stubbed for the same
      ;; reason `nl_alloc_zero_fill' is: this probe compiles
      ;; `nl_boundary_reclaim' in isolation and does not link the arena unit
      ;; that defines them.  Both are inert unless NELISP_ALLOC_CHECK=1
      ;; armed them, so a zero stub is also their production behaviour here.
      (defun nl_alloc_check_poison_span (_addr _nbytes) 0)
      (defun nl_alloc_check_expect_zero (_addr _nbytes) 0)
      ,large-mask-clear
      ,run-reset
      ,(or boundary-large
           '(defun nl_boundary_clear_large_fl (_n) 0))
      ,(or boundary-small
           '(defun nl_boundary_clear_fl (_n) 0))
      ,boundary-reclaim
      ,(or compact-large
           '(defun nl_compact_clear_large_fl (_n) 0))
      ,compact-small
      (defun nl_probe_seed-large (n)
        (if (> n 203) 0
          (nl_seq2
           (ptr-write-u64 (nl_freelist_large_head n) 0 (+ 9000 n))
           (nl_probe_seed-large (+ n 1)))))
      (defun nl_probe_seed-small (n)
        (if (> n 57)
            (ptr-write-u64 268435552 0 7777)
          (nl_seq2
           (ptr-write-u64 (+ 268435696 (* n 8)) 0 (+ 1000 n))
           (nl_probe_seed-small (+ n 1)))))
      (defun nl_probe_seed-large-mask ()
        (seq
         (ptr-write-u64 ,(+ nelisp-standalone-gc-test--freelist-large-page 2048) 0 ,(1- (ash 1 51)))
         (ptr-write-u64 ,(+ nelisp-standalone-gc-test--freelist-large-page 2048) 8 ,(1- (ash 1 51)))
         (ptr-write-u64 ,(+ nelisp-standalone-gc-test--freelist-large-page 2048) 16 ,(1- (ash 1 51)))
         (ptr-write-u64 ,(+ nelisp-standalone-gc-test--freelist-large-page 2048) 24 ,(1- (ash 1 51)))))
      (defun nl_probe_seed-all ()
        (nl_seq2
         (seq (nl_probe_seed-small 0) (nl_probe_seed-large-mask))
         (nl_seq2 (nl_probe_seed-large 0)
                  (ptr-write-u64
                   (nl_freelist_small_mask_ptr) 0 ,(1- (ash 1 58))))))
      (defun nl_probe-zero-large (n)
        (if (> n 203) 1
          (if (= (ptr-read-u64 (nl_freelist_large_head n) 0) 0)
              (nl_probe-zero-large (+ n 1))
            0)))
      (defun nl_probe-zero-small (n)
        (if (> n 57)
            (if (= (ptr-read-u64 268435552 0) 0) 1 0)
          (if (= (ptr-read-u64 (+ 268435696 (* n 8)) 0) 0)
              (nl_probe-zero-small (+ n 1))
            0)))
      (defun nl_probe_zero-large-mask ()
        (if (and (= (ptr-read-u64 ,(+ nelisp-standalone-gc-test--freelist-large-page 2048) 0) 0)
                 (= (ptr-read-u64 ,(+ nelisp-standalone-gc-test--freelist-large-page 2048) 8) 0)
                 (= (ptr-read-u64 ,(+ nelisp-standalone-gc-test--freelist-large-page 2048) 16) 0)
                 (= (ptr-read-u64 ,(+ nelisp-standalone-gc-test--freelist-large-page 2048) 24) 0)) 1 0))
      (defun nl_probe-all-zero ()
        (if (and (= (ptr-read-u64 (nl_freelist_small_mask_ptr) 0) 0)
                 (= (nl_probe-zero-small 0) 1)
                 (= (nl_probe-zero-large 0) 1)
                 (= (nl_probe_zero-large-mask) 1))
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
             ;; A current run must not survive the rewind either.
             (ptr-write-u64 ,(+ nelisp-standalone-gc-test--freelist-page 1040 48) 0 4242)
             (nl_boundary_reclaim
              ,nelisp-standalone-gc-test--freelist-chunk-page 1024)
             (if (= (nl_probe-all-zero) 1)
                 (if (= (ptr-read-u64 ,(+ nelisp-standalone-gc-test--freelist-page 1040 48) 0) 0)
                     0 3)
               1))
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
         (names '(nl_freelist_large_mask_sync
                  nl_freelist_small_mask_bit
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
                  nl_gc_relink_free_one nl_gc_freelist_purge_buckets
                  nl_alloc_run_adopt nl_alloc_run_take))
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
                        '(data-addr nl_gc_diag) form :test #'equal))
        (setq form (cl-subst (+ nelisp-standalone-gc-test--freelist-large-page 2048)
                            '(data-addr nl_freelist_large_mask) form :test #'equal))
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
      ;; A small request's split remainder becomes the allocator's current
      ;; run (`nl_alloc_run_adopt'), not a bucket entry: bucket 32 empties,
      ;; bucket 16 stays clear, and the run hands the remainder out whole.
      (defun nl_probe-split-route ()
        (let ((hdr (nl_probe-hdr 0)))
          (seq
           (ptr-write-u64 hdr 0 32)
           (nl_gc_free_block hdr)
           (let ((obj (nl_freelist_bucket_split 16)))
             (if (= obj 0) 20
               (if (and (= (logand
                            (ptr-read-u64 (nl_freelist_small_mask_ptr) 0) 1)
                           0)
                        (= (logand
                            (ptr-read-u64 (nl_freelist_small_mask_ptr) 0) 4)
                           0)
                        (= (ptr-read-u64 ,(+ nelisp-standalone-gc-test--freelist-page 1088) 0)
                           (+ hdr 16)))
                   (if (= (nl_alloc_run_take 16) (+ hdr 24)) 0 21)
                 22))))))
      ;; A run replaced by a newer one is relinked into its bucket, which
      ;; sets that bucket's bit.
      (defun nl_probe-split-tail-route ()
        (let ((a (nl_probe-hdr 20)) (b (nl_probe-hdr 30)))
          (seq
           (nl_freelist_split_tail a 64 16)
           (nl_freelist_split_tail b 32 16)
           (if (= (logand
                   (ptr-read-u64 (nl_freelist_small_mask_ptr) 0) 16) 16)
               (if (= (nl_freelist_bucket_pop 48 48) (+ a 24)) 0 30)
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


(defun nelisp-standalone-gc-test--large-mask-source ()
  "Exercise the real large-mask, free/relink/split/scan/purge routes."
  (let* ((source (nelisp-standalone-gc-test--mask-source))
         (forms (butlast (cdr source)))
         (mask (+ nelisp-standalone-gc-test--freelist-large-page 2048)))
    ;; Replace the small-probe purge stub with the production large walker.
    (setq forms (cl-remove-if (lambda (form)
                               (eq (cadr form) 'nl_gc_freelist_purge_large_bins)) forms))
    (dolist (name '(nl_freelist_large_next nl_freelist_large_mask_clear_all
                   nl_freelist_scan_drop_tail nl_freelist_scan_head
                   nl_freelist_scan_large_from nl_gc_freelist_purge_large_bins))
      (let ((form (or (nelisp-standalone-gc-test--find-defun nelisp-standalone--arena-source name)
                      (nelisp-standalone-gc-test--find-defun nelisp-standalone--gc-source name))))
        (unless form (error "production large-mask probe missing: %S" name))
        (push (cl-subst mask '(data-addr nl_freelist_large_mask) form :test #'equal) forms)))
    `(seq
      ,@forms
      (defun nl_alloc_diag_linear (obj) obj)
      (defun nl_probe_large_bt (i)
        (if (< i 50) (+ 480 (* i 32))
          (if (< i 114) (+ 2056 (* (- i 50) 32))
            (if (< i 178) (+ 4104 (* (- i 114) 64))
              (if (< i 202) (+ 8200 (* (- i 178) 1024))
                (if (= i 202) 32776 262152))))))
      (defun nl_probe_large_hdr (i)
        (+ ,nelisp-standalone-gc-test--freelist-chunk-page (* i 16)))
      (defun nl_probe_large_oracle (start)
        (let ((i start) (result -1))
          (seq
           (while (and (< i 204) (= result -1))
             (if (= (ptr-read-u64 (nl_freelist_large_head i) 0) 0)
                 (setq i (+ i 1)) (setq result i)))
           result)))
      (defun nl_probe_large_compare ()
        (let ((i 0) (bad 0))
          (seq
           (while (and (< i 205) (= bad 0))
             (if (= (nl_freelist_large_next i) (nl_probe_large_oracle i)) 0
               (setq bad 1))
             (setq i (+ i 1)))
           bad)))
      (defun nl_probe_large_routes ()
        (let ((i 0) (bin 0) (hdr 0) (bad 0))
          (seq
           (syscall-direct 9 ,nelisp-standalone-gc-test--freelist-page 4096 3 50 -1 0)
           (syscall-direct 9 ,nelisp-standalone-gc-test--freelist-large-page 4096 3 50 -1 0)
           (syscall-direct 9 ,nelisp-standalone-gc-test--freelist-chunk-page 4096 3 50 -1 0)
           ;; 73 is coprime to 204; visit every bin in a dispersed order.
           (while (and (< i 204) (= bad 0))
             (setq bin (mod (* i 73) 204))
             (setq hdr (nl_probe_large_hdr bin))
             (ptr-write-u64 hdr 0 (nl_probe_large_bt bin))
             (if (= (mod i 2) 0) (nl_gc_free_block hdr)
               (nl_gc_relink_free_one hdr))
             (if (= (nl_probe_large_compare) 0) 0 (setq bad 10))
             (setq i (+ i 1)))
           (setq i 0)
           (while (and (< i 204) (= bad 0))
             (setq bin (mod (* i 73) 204))
             (ptr-write-u64 (nl_freelist_large_head bin) 0 0)
             (nl_freelist_large_mask_sync bin (nl_freelist_large_head bin))
             (if (= (nl_probe_large_compare) 0) 0 (setq bad 11))
             (setq i (+ i 1)))
           ;; Split a 1024-byte block into a 496-byte head and a 528-byte
           ;; tail.  A request this large never makes a current run, so the
           ;; tail goes to large bin 1.
           (setq hdr (nl_probe_large_hdr 0))
           (nl_freelist_split_tail hdr 1024 496)
           (if (= (nl_freelist_large_next 0) 1) 0 (setq bad 12))
           ;; Consume that exact tail through the production scanner.
           (if (= (nl_freelist_scan_large_from 0 528) (+ hdr 504)) 0 (setq bad 13))
           (if (= (nl_freelist_large_next 0) -1) 0 (setq bad 14))
           ;; Purge must both remove empty bits and preserve nonempty bits.
           (ptr-write-u64 (nl_freelist_large_head 51) 0 7777)
           (ptr-write-u64 (nl_freelist_large_head 203) 0 8888)
           (nl_freelist_large_mask_sync 51 (nl_freelist_large_head 51))
           (nl_freelist_large_mask_sync 203 (nl_freelist_large_head 203))
           (nl_gc_freelist_purge_large_bins 0 0 0)
           (if (= (nl_freelist_large_next 0) 203) 0 (setq bad 15))
           (nl_freelist_large_mask_clear_all)
           (if (= (nl_freelist_large_next 0) -1) 0 (setq bad 16))
           bad)))
      (exit (nl_probe_large_routes)))))

(ert-deftest nelisp-standalone-gc-large-mask-native-routes ()
  "All 204 bins obey the independent oracle and production mutation routes."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (let ((path (make-temp-file "nelisp-standalone-large-mask-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp (nelisp-standalone-gc-test--large-mask-source) path)
          (should (= (call-process path nil nil nil) 0)))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-standalone-gc-large-clear-with-eight-bin-bound-is-red ()
  "Keeping the old eight-bin bound leaves later heads and masks observable."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (let ((path (make-temp-file "nelisp-standalone-large-clear-red-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (cons 'seq
                 (mapcar (lambda (form)
                           (if (memq (cadr form) '(nl_boundary_clear_large_fl
                                                 nl_compact_clear_large_fl))
                               (cl-subst '(> n 7) '(> n 203) form :test #'equal)
                             form))
                         (cdr (nelisp-standalone-gc-test--freelist-clear-source 0)))) path)
          (should (equal (call-process path nil nil nil) 1)))
      (when (file-exists-p path) (delete-file path)))))

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
             nl_gc_chunk_contains nl_gc_in_arena nl_boot_hole_p nl_gc_is_boot nl_gc_index_end
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
                  nl_gc_chunk_contains nl_gc_in_arena nl_boot_hole_p nl_gc_is_boot
                  nl_gc_index_end nl_gc_index_bytes nl_gc_index_fill
                  nl_gc_index_prepare nl_gc_index_contains nl_gc_index_test
                  nl_gc_object_start_p nl_gc_mark_block nl_gc_mark_buf
                  nl_gc_symname_mark_one nl_gc_block_elem_cap nl_gc_mark_vec_slots
                  nl_gc_typed_probe nl_gc_typed_grow nl_gc_typed_add nl_gc_typed_slot
                  nl_gc_mark_char_table_word
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
               (setq form
                     (cl-subst (+ test-flag 64) '(data-addr nl_symbol_name_state)
                               form :test #'equal))
               (setq form
                     (cl-subst (+ test-flag 128) '(data-addr nl_gc_typed_state)
                               form :test #'equal))
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
      ;; This synthetic heap has no external symbol-name sidecar. Double
      ;; only its empty lookup service; retain the production marking helper.
      (defun nl_symbol_name_entry_for (_kind _key _len) 0)
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

(defun nelisp-standalone-gc-test--membership-source (&optional broken)
  "Probe live half-open chunk bounds without caching descriptors."
  (let* ((base #x32000000) (d0 (+ base 4096))
         (d1 (+ d0 64)) (d2 (+ d1 64))
         (body (copy-tree (nelisp-standalone-gc-test--find-defun
                          nelisp-standalone--gc-source 'nl_gc_in_arena)))
         (points '(0 1 31 32 127 128 129 255 256 287 288 399 400 511 512 543 544 799 800 850)))
    (when broken
      ;; An inclusive fast-path upper bound accepts the first unallocated byte.
      (setq body (cl-subst '<= '< body)))
    `(seq
      ,body
      (defun membership_check (stage)
        (let* ((i 0) (addr 0) (expected 0))
          (seq
           ,@(mapcar
              (lambda (offset)
                `(seq
                  (setq addr ,(+ base offset))
                  (setq expected
                        (if (or (and (< stage 4) (>= addr ,(+ base 32))
                                     (< addr ,(+ base 128)))
                                (and (< stage 2) (>= addr ,(+ base 288))
                                     (< addr ,(+ base 400)))
                                (and (< stage 3) (>= addr ,(+ base 544))
                                     (< addr ,(+ base 800)))) 1 0))
                  (if (/= (nl_gc_in_arena addr) expected)
                      (syscall-direct 60 77 0 0 0 0 0) 0))) points)
           0)))
      (defun membership_run ()
        (seq
         (syscall-direct 9 268435456 65536 3 50 -1 0)
         (syscall-direct 9 ,base 8192 3 50 -1 0)
         (ptr-write-u64 268436160 0 ,d0) (ptr-write-u64 268436168 0 ,d2)
         (ptr-write-u64 268435456 0 128)
         (ptr-write-u64 ,d0 0 ,base) (ptr-write-u64 ,d0 24 ,(+ base 32))
         ;; Head cursor comes from the global, not this deliberately stale slot.
         (ptr-write-u64 ,d0 16 9999) (ptr-write-u64 ,d0 48 ,d1)
         (ptr-write-u64 ,d1 0 ,(+ base 256)) (ptr-write-u64 ,d1 24 ,(+ base 288))
         (ptr-write-u64 ,d1 16 144) (ptr-write-u64 ,d1 48 ,d2)
         (ptr-write-u64 ,d2 0 ,(+ base 512)) (ptr-write-u64 ,d2 24 ,(+ base 544))
         (ptr-write-u64 ,d2 16 288) (ptr-write-u64 ,d2 48 0)
         (membership_check 0)
         ;; Unlink a reclaimed middle descriptor, then remove the tail.
         (ptr-write-u64 ,d0 48 ,d2) (membership_check 2)
         (ptr-write-u64 ,d0 48 0) (ptr-write-u64 268436168 0 ,d0)
         (membership_check 3)
         (ptr-write-u64 268436160 0 0) (ptr-write-u64 268436168 0 0)
         (membership_check 4)
         0))
      (exit (membership_run)))))

(ert-deftest nelisp-standalone-gc-membership-live-bounds-and-reclaimed-chunks ()
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (dolist (broken '(t nil))
    (let ((path (make-temp-file "nelisp-gc-membership-")))
      (unwind-protect
          (progn
            (nelisp-aot-compile-sexp
             (nelisp-standalone-gc-test--membership-source broken) path)
            (if broken (should-not (= (call-process path nil nil nil) 0))
              (should (= (call-process path nil nil nil) 0))))
        (delete-file path)))))

(defun nelisp-standalone-gc-test--predecessor-source ()
  "Probe production owner lookup against an independent full header walk."
  (let* ((base #x32000000) (large #x400000) (ctx #x35000000)
         (mapping #x35100000) (counter (+ ctx 1024))
         (descriptor #x10000300)
         (names '(nl_align_up nl_hdr_bt nl_alloc_zero_fill nl_gc_bt_ok nl_gc_chunk_cursor
                  nl_gc_chunk_end nl_gc_chunk_contains nl_gc_index_end
                  nl_gc_index_bytes nl_gc_index_fill nl_gc_index_prepare
                  nl_gc_index_contains nl_gc_index_test nl_gc_object_start_p
                  nl_gc_conserv_owner_slow nl_gc_conserv_owner_indexed))
         (forms
          (mapcar (lambda (name)
                    (or (nelisp-standalone-gc-test--find-defun nelisp-standalone--gc-source name)
                        (nelisp-standalone-gc-test--find-defun nelisp-standalone--arena-source name)
                        (error "Missing production helper %s" name))) names))
         (source
          `(seq
            (defun nl_seq2 (_a b) b)
            (defun nl_os_alloc_chunk (_size) ,mapping)
            (defun nl_os_commit_range (_base _old _new) 1)
            (defun nl_os_free_chunk (_base _size) 1)
            (defun gc_predecessor_read_u8 (address offset)
              (seq (ptr-write-u64 ,counter 0 (+ (ptr-read-u64 ,counter 0) 1))
                   (ptr-read-u8 address offset)))
            ,@forms
            (defun gc_predecessor_gc_bench ()
              (let* ((i 0) (failed 0) (word 0))
                (seq
                 (syscall-direct 9 #x10000000 4096 3 50 -1 0)
                 (syscall-direct 9 ,base ,(+ large 4096) 3 50 -1 0)
                 (syscall-direct 9 ,ctx 4096 3 50 -1 0)
                 (syscall-direct 9 ,mapping 1048576 3 50 -1 0)
                 (ptr-write-u64 268436160 0 ,descriptor)
                 (ptr-write-u64 268436168 0 ,descriptor)
                 (ptr-write-u64 268435456 0 ,(+ large 32))
                 (ptr-write-u64 ,descriptor 0 ,base)
                 (ptr-write-u64 ,descriptor 24 ,base)
                 (ptr-write-u64 ,base 0 ,large)
                 (ptr-write-u64 ,(+ base large) 0 32)
                 (if (= (nl_gc_index_prepare) 0) (setq failed 10) 0)
                 (ptr-write-u64 ,counter 0 0)
                 ;; All expected owners come from the independent complete
                 ;; header walk, without reading a candidate's payload as a header.
                 (while (and (< i 1000) (= failed 0))
                   (setq word (+ ,base (+ 1048576 (mod (* i 2141) 3145728))))
                   (if (= (nl_gc_conserv_owner_indexed word)
                          (nl_gc_conserv_owner_slow word)) 0 (setq failed 11))
                   (setq i (+ i 1)))
                 (if (and (= failed 0)
                          (not (= (nl_gc_conserv_owner_indexed ,(+ base large)) 0)))
                     (setq failed 12) 0)
                 (if (and (= failed 0)
                          (not (= (nl_gc_conserv_owner_indexed ,(+ base large 8))
                                  ,(+ base large 8)))) (setq failed 13) 0)
                 (ptr-write-u64 ,counter 8 i)
                 (syscall-direct 1 1 ,counter 16 0 0 0)
                 failed)))
            (exit (gc_predecessor_gc_bench)))))
    ;; Count actual bitmap reads in the compiled production bodies, after
    ;; preparation. The counted helper itself uses the real unwrapped read.
    (setq source
          (cl-labels ((walk (form)
                        (if (not (consp form)) form
                          (if (and (eq (car form) 'defun) (eq (cadr form) 'gc_predecessor_read_u8)) form
                            (cons (if (eq (car form) 'ptr-read-u8) 'gc_predecessor_read_u8 (car form))
                                  (mapcar #'walk (cdr form)))))))
            (walk source)))
    (setq source (cl-subst ctx '(data-addr nl_gc_start_index) source :test #'equal))
    source))

(ert-deftest nelisp-standalone-gc-predecessor-is-parallel-to-header-walk-and-bounded ()
  "Large-block interiors agree with full walking using at most a page of bits."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (let ((path (make-temp-file "nelisp-gc-predecessor-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-standalone-gc-test--predecessor-source) path)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (let ((coding-system-for-read 'no-conversion))
              (should (= (call-process path nil (current-buffer) nil) 0)))
            (should (= (buffer-size) 16))
            (let ((bytes (buffer-string)) (reads 0) (checked 0))
              (dotimes (i 8)
                (setq reads (+ reads (ash (aref bytes i) (* i 8)))
                      checked (+ checked (ash (aref bytes (+ i 8)) (* i 8)))))
              (should (= checked 1000))
              (should (<= reads 65010)))))
      (delete-file path))))

(defun nelisp-standalone-gc-test--envelope-source ()
  "Compare the production resolver with independent walking across 16 chunks."
  (let* ((base #x32000000) (ctx #x35000000) (state (+ ctx 128))
         (mapping #x35100000) (counter (+ ctx 1024)) (desc #x10000400)
         (names '(nl_align_up nl_hdr_bt nl_alloc_zero_fill nl_gc_bt_ok
                  nl_gc_chunk_cursor nl_gc_chunk_end nl_gc_chunk_contains
                  nl_gc_in_arena nl_gc_index_end nl_gc_index_bytes
                  nl_gc_index_fill nl_gc_index_prepare nl_gc_index_contains
                  nl_gc_index_test nl_gc_object_start_p nl_gc_conserv_owner_slow
                  nl_gc_conserv_owner_indexed nl_gc_conserv_resolve))
         (forms
          (mapcar (lambda (name)
                    (or (nelisp-standalone-gc-test--find-defun nelisp-standalone--gc-source name)
                        (nelisp-standalone-gc-test--find-defun nelisp-standalone--arena-source name)
                        (error "Missing production owner %s" name))) names))
         (source
          `(seq
            (defun nl_seq2 (_a b) b)
            (defun nl_os_alloc_chunk (_size) ,mapping)
            (defun nl_os_commit_range (_base _old _new) 1)
            (defun nl_os_free_chunk (_base _size) 1)
            (defun gc_bounds_read (address offset)
              (seq (ptr-write-u64 ,counter 0 (+ (ptr-read-u64 ,counter 0) 1))
                   (ptr-read-u64 address offset)))
            ,@forms
            (defun gc_bounds_probe ()
              (let* ((i 0) (failed 0) (data 0) (category 0) (word 0))
                (seq
                 (syscall-direct 9 #x10000000 4096 3 50 -1 0)
                 (syscall-direct 9 ,base 131072 3 50 -1 0)
                 (syscall-direct 9 ,ctx 4096 3 50 -1 0)
                 (syscall-direct 9 ,mapping 1048576 3 50 -1 0)
                 (ptr-write-u64 268436160 0 ,desc)
                 (ptr-write-u64 268436168 0 ,(+ desc (* 15 64)))
                 (ptr-write-u64 268435456 0 32)
                 (while (< i 16)
                   ;; A permutation makes neither head nor tail an envelope.
                   (setq data (+ ,base (* (mod (+ 3 (* i 7)) 16) 4096)))
                   (ptr-write-u64 (+ ,desc (* i 64)) 0 data)
                   (ptr-write-u64 (+ ,desc (* i 64)) 16 32)
                   (ptr-write-u64 (+ ,desc (* i 64)) 24 data)
                   (ptr-write-u64 (+ ,desc (* i 64)) 48
                                  (if (= i 15) 0 (+ ,desc (* (+ i 1) 64))))
                   (ptr-write-u64 data 0 32)
                   (setq i (+ i 1)))
                 (if (= (nl_gc_index_prepare) 0) (setq failed 10) 0)
                 (ptr-write-u64 ,counter 0 0)
                 (setq i 0)
                 (while (and (< i 1000) (= failed 0))
                   (setq data (+ ,base (* (mod (* i 11) 16) 4096)))
                   (setq category (mod i 10))
                   (setq word
                         (cond ((= category 0) 0)
                               ((= category 1) #x7070707070707070)
                               ((= category 2) -1)
                               ((= category 3) ,(- base 1))
                               ((= category 4) ,(+ base (* 15 4096) 32))
                               ((= category 5) (+ data 40))
                               ((= category 6) data)
                               ((= category 7) (+ data 8))
                               ((= category 8) (+ data 16))
                               (t (+ data 31))))
                   (if (= (nl_gc_conserv_resolve word)
                          (nl_gc_conserv_owner_slow word)) 0 (setq failed 11))
                   (setq i (+ i 1)))
                 (ptr-write-u64 ,counter 8 i)
                 (syscall-direct 1 1 ,counter 16 0 0 0)
                 failed)))
            (exit (gc_bounds_probe)))))
    (setq source
          (cl-labels ((walk (form)
                        (if (not (consp form)) form
                          (cons (if (eq (car form) 'ptr-read-u64) 'gc_bounds_read (car form))
                                (mapcar #'walk (cdr form))))))
            (mapcar (lambda (form)
                      (if (and (eq (car-safe form) 'defun)
                               (memq (cadr form) '(nl_gc_in_arena nl_gc_conserv_resolve)))
                          (walk form) form)) source)))
    (setq source (cl-subst ctx '(data-addr nl_gc_start_index) source :test #'equal)
          source (cl-subst state '(data-addr nl_gc_conserv_state) source :test #'equal))
    source))

(ert-deftest nelisp-standalone-gc-envelope-refuses-outside-without-losing-chunks ()
  "The stopped-heap envelope excludes outside words and retains holes and edges."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (let ((path (make-temp-file "nelisp-gc-envelope-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-standalone-gc-test--envelope-source) path)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (let ((coding-system-for-read 'no-conversion))
              (should (= (call-process path nil (current-buffer) nil) 0)))
            (should (= (buffer-size) 16))
            (let ((bytes (buffer-string)) (reads 0) (checked 0))
              (dotimes (i 8)
                (setq reads (+ reads (ash (aref bytes i) (* i 8)))
                      checked (+ checked (ash (aref bytes (+ i 8)) (* i 8)))))
              (should (= checked 1000))
              (should (< reads 26000)))))
      (delete-file path))))

;;; One-pass sweep ---------------------------------------------------------

(defconst nelisp-standalone-gc-test--sweep-heap #x33000000
  "Scratch heap for the one-pass sweep probe (256 KiB mapped).")

(defconst nelisp-standalone-gc-test--sweep-holes #x32003000
  "Scratch page used as the boot hole bitmap in the sweep probe.")

(defconst nelisp-standalone-gc-test--sweep-boot-bytes 4096
  "Headers below heap + this many bytes are boot blocks unless a hole.")

(defun nelisp-standalone-gc-test--sweep-blocks ()
  "Return the sweep probe heap as a list of (BT MARK HOLE) in address order.
The head covers every boot/hole/mark case and runs that cross the
watermark; a 160000-byte run spans a whole 64 KiB page; a seeded tail
mixes small and large sizes and ends on a run the walk must flush."
  (let ((blocks
         (list '(32 0 nil) '(48 1 nil) '(64 4 nil)   ; boot blocks live
               '(40 2 nil) '(24 0 t) '(56 2 t)        ; boot free + holes: run of 120
               '(32 1 t)                              ; a kept hole ends the run
               '(88 0 t) '(16 0 nil)                  ; dead hole run, boot block ends it
               '(120 2 nil)))                         ; boot free run, crosses below
        (used 0) (seed 12345))
    (dolist (b blocks) (setq used (+ used (car b))))
    ;; Fill the boot region up to the watermark with live boot blocks,
    ;; except the last block before it, which is free and opens a run
    ;; that the first non-boot dead block continues.
    (while (< (+ used 64) (- nelisp-standalone-gc-test--sweep-boot-bytes 64))
      (setq blocks (append blocks (list '(64 0 nil))) used (+ used 64)))
    (setq blocks (append blocks
                         (list (list (- nelisp-standalone-gc-test--sweep-boot-bytes used) 2 nil))))
    (setq blocks (append blocks
                         (list '(120 0 nil) '(32 1 nil)          ; run across the watermark
                               '(120 0 nil) '(32 4 nil)          ; same size again: LIFO order
                               '(40000 0 nil) '(40000 2 nil) '(40000 0 nil) '(40000 0 nil)
                               '(24 1 nil)
                               '(512 0 nil) '(16 1 nil) '(520 2 nil) '(16 4 nil))))
    (dotimes (_ 160)
      (setq seed (logand (+ (* seed 1103515245) 12345) #x7fffffff))
      (let* ((r (ash seed -8))
             (bt (if (= (% r 9) 0) (+ 480 (* 8 (% (ash r -4) 400)))
                   (+ 16 (* 8 (% (ash r -4) 58)))))
             (m (nth (% (ash r -12) 6) '(0 0 1 1 2 4))))
        (setq blocks (append blocks (list (list bt m nil))))))
    (append blocks (list '(64 0 nil) '(72 2 nil)))))

(defun nelisp-standalone-gc-test--sweep-large-bin (bt)
  "Elisp copy of `nl_freelist_large_bin' for the oracle."
  (cond ((< bt 2049) (if (< bt 481) 0 (/ (- bt 480) 32)))
        ((< bt 4097) (+ 50 (/ (- bt 2049) 32)))
        ((< bt 8193) (+ 114 (/ (- bt 4097) 64)))
        ((< bt 32769) (+ 178 (/ (- bt 8193) 1024)))
        ((< bt 262145) 202)
        (t 203)))

(defun nelisp-standalone-gc-test--sweep-oracle (blocks &optional markonly desync)
  "Expected state after the one-pass sweep over BLOCKS.
MARKONLY models the mark-only debug mode; DESYNC is the index of a block
whose header is malformed.  Returns a plist of :headers ((ADDR . WORD)),
:runs ((HEAD . TOTAL)) in address order, :kept, :freed and :live."
  (let ((heap nelisp-standalone-gc-test--sweep-heap)
        (addr nelisp-standalone-gc-test--sweep-heap)
        (headers nil) (runs nil) (run nil) (runbt 0)
        (kept 0) (freed 0) (live 0) (stop nil) (i 0))
    (dolist (b blocks)
      (let* ((bt (nth 0 b)) (m (nth 1 b)) (hole (nth 2 b))
             (word (if (and desync (= i desync)) 8 (+ bt m))))
        (when (and desync (= i desync)) (setq stop t))
        (if stop
            (push (cons addr word) headers)
          (let ((boot (and (< addr (+ heap nelisp-standalone-gc-test--sweep-boot-bytes))
                           (not hole)))
                (free nil))
            (cond
             (boot (if (= m 2) (setq free t) (setq word bt live (+ live bt))))
             ((memq m '(1 4)) (setq word bt kept (1+ kept) live (+ live bt)))
             ((= m 0) (unless markonly (setq word (+ bt 2) freed (1+ freed) free t)))
             ((= m 2) (setq free t)))
            (push (cons addr word) headers)
            (if free
                (if run (setq runbt (+ runbt bt)) (setq run addr runbt bt))
              (when run (push (cons run runbt) runs) (setq run nil)))))
        (setq addr (+ addr bt) i (1+ i))))
    (when run (push (cons run runbt) runs))
    (setq runs (nreverse runs))
    (dolist (r runs) (setf (alist-get (car r) headers) (+ (cdr r) 2)))
    (list :headers (nreverse headers) :runs runs :kept kept :freed freed
          :live live :end addr)))

(defun nelisp-standalone-gc-test--sweep-source (blocks mode)
  "Native probe running `nl_gc_sweep_merge_chunk' over BLOCKS.
MODE is one of plain, diag, check, markonly or desync.  The production
walk, run linker, free path, relink, discard, bin and mask bodies are
linked unchanged; the OS discard, poison fill and redzone verify are
counting stubs.  Exit 0 when every header, list, mask, counter and
stub count matches the Elisp oracle."
  (let* ((page nelisp-standalone-gc-test--freelist-page)
         (large nelisp-standalone-gc-test--freelist-large-page)
         (chunk nelisp-standalone-gc-test--freelist-chunk-page)
         (heap nelisp-standalone-gc-test--sweep-heap)
         (holes nelisp-standalone-gc-test--sweep-holes)
         (diag (+ page 1040)) (check (+ page 1200)) (hmap (+ page 1304))
         (stats (+ page 1400)) (counts (+ page 1600)) (lmask (+ large 2048))
         (desync (and (eq mode 'desync) 120))
         (oracle (nelisp-standalone-gc-test--sweep-oracle
                  blocks (eq mode 'markonly) desync))
         (plain (memq mode '(plain markonly desync)))
         (end (plist-get oracle :end))
         (forms nil) (setup nil) (checks nil) (code 0))
    (dolist (name '(nl_gc_sweep_merge_chunk nl_gc_sweep_link_run
                    nl_gc_sweep_free_dead nl_gc_relink_free_one
                    nl_gc_discard_free_block nl_gc_bt_ok nl_hdr_bt nl_hdr_mark
                    nl_freelist_small_mask_bit nl_freelist_small_mask_set
                    nl_freelist_large_bin nl_freelist_large_mask_sync))
      (let ((form (or (nelisp-standalone-gc-test--find-defun
                       nelisp-standalone--arena-source name)
                      (nelisp-standalone-gc-test--find-defun
                       nelisp-standalone--gc-source name))))
        (unless form (error "one-pass sweep probe defun absent: %S" name))
        (dolist (sub `((,diag . nl_gc_diag) (,check . nl_alloc_check)
                       (,hmap . nl_boot_hole_map) (,stats . nl_gc_stats)
                       (,lmask . nl_freelist_large_mask)))
          (setq form (cl-subst (car sub) `(data-addr ,(cdr sub)) form
                               :test #'equal)))
        (push form forms)))
    ;; Heap headers and hole bits.
    (let ((addr heap) (i 0) (bits nil))
      (dolist (b blocks)
        (push `(ptr-write-u64 ,addr 0 ,(if (and desync (= i desync)) 8
                                         (+ (nth 0 b) (nth 1 b))))
              setup)
        (when (nth 2 b)
          (let ((g (/ (- addr heap) 8)))
            (setf (alist-get (/ g 64) bits)
                  (logior (alist-get (/ g 64) bits 0) (ash 1 (% g 64))))))
        (setq addr (+ addr (nth 0 b)) i (1+ i)))
      (dolist (w bits)
        (push `(ptr-write-u64 ,(+ holes (* 8 (car w))) 0 ,(cdr w)) setup)))
    (cl-flet ((expect (form value)
                (setq code (1+ code))
                (push `(if (= ,form ,value) 0 (if (= bad 0) (setq bad ,(min code 250)) 0))
                      checks)))
      (dolist (h (plist-get oracle :headers))
        (expect `(ptr-read-u64 ,(car h) 0) (cdr h)))
      ;; Free lists: each run head pushed in address order.
      (let ((lists nil) (small 0) (lmasks (make-vector 4 0)))
        (dolist (r (plist-get oracle :runs))
          (let* ((total (cdr r))
                 (slot (if (< total 473) (+ 268435696 (- total 16))
                         (let ((bin (nelisp-standalone-gc-test--sweep-large-bin total)))
                           (aset lmasks (/ bin 51)
                                 (logior (aref lmasks (/ bin 51)) (ash 1 (% bin 51))))
                           (+ large (* 8 bin))))))
            (when (< total 473)
              (setq small (logior small (ash 1 (/ (- total 16) 8)))))
            (push (car r) (alist-get slot lists))))
        (dolist (l lists)
          (let ((at (car l)))
            (dolist (node (cdr l))
              (expect `(ptr-read-u64 ,at 0) (+ node 8))
              (setq at (+ node 8)))
            (expect `(ptr-read-u64 ,at 0) 0)))
        (expect `(ptr-read-u64 ,(+ page 1024) 0) small)
        (dotimes (w 4)
          (expect `(ptr-read-u64 ,(+ lmask (* 8 w)) 0) (aref lmasks w))))
      (expect '(ptr-read-u64 268435640 0) (plist-get oracle :kept))
      (expect '(ptr-read-u64 268435632 0) (plist-get oracle :freed))
      (expect `(ptr-read-u64 ,(+ stats 16) 0) (plist-get oracle :live))
      ;; Counting stubs: verify, poison, poison counter, discards.
      (let ((freed (plist-get oracle :freed))
            (discards nil))
        (when plain
          (dolist (r (plist-get oracle :runs))
            (let* ((hdr (car r)) (stop (+ hdr (cdr r)))
                   (start (if (= (logand (+ hdr 16) 65535) 0) (+ hdr 16)
                            (+ hdr 16 (- 65536 (logand (+ hdr 16) 65535)))))
                   (finish (- stop (logand stop 65535))))
              (when (and (>= (cdr r) 65552) (< start finish)
                         (>= (- finish start) 65536))
                (push (cons start (- finish start)) discards)))))
        (expect `(ptr-read-u64 ,counts 0) (if plain 0 freed))
        (expect `(ptr-read-u64 ,(+ counts 8) 0) (if (eq mode 'diag) freed 0))
        (expect `(ptr-read-u64 ,(+ diag 40) 0) (if (eq mode 'diag) freed 0))
        (expect `(ptr-read-u64 ,(+ counts 16) 0) (length discards))
        (when discards
          (expect `(ptr-read-u64 ,(+ counts 24) 0) (car (car discards)))
          (expect `(ptr-read-u64 ,(+ counts 32) 0) (cdr (car discards))))))
    `(seq
      (defun nl_seq2 (_a b) b)
      (defun nl_freelist_small_mask_ptr () ,(+ page 1024))
      (defun nl_freelist_large_head (bin) (+ ,large (* bin 8)))
      (defun nl_gc_chunk_end (_chunk) ,end)
      (defun nl_alloc_check_verify (_hdr)
        (ptr-write-u64 ,counts 0 (+ (ptr-read-u64 ,counts 0) 1)))
      (defun nl_gc_poison_fill (_hdr _off _bt)
        (ptr-write-u64 ,(+ counts 8) 0 (+ (ptr-read-u64 ,(+ counts 8) 0) 1)))
      (defun nl_os_discard_free_pages (start len)
        (seq (ptr-write-u64 ,(+ counts 16) 0 (+ (ptr-read-u64 ,(+ counts 16) 0) 1))
             (ptr-write-u64 ,(+ counts 24) 0 start)
             (ptr-write-u64 ,(+ counts 32) 0 len)
             0))
      ,@(nreverse forms)
      (defun nl_probe_sweep_setup ()
        (seq
         (syscall-direct 9 ,page 4096 3 50 -1 0)
         (syscall-direct 9 ,large 4096 3 50 -1 0)
         (syscall-direct 9 ,chunk 4096 3 50 -1 0)
         (syscall-direct 9 ,holes 4096 3 50 -1 0)
         (syscall-direct 9 ,heap 262144 3 50 -1 0)
         (ptr-write-u64 268435664 0 ,(+ heap nelisp-standalone-gc-test--sweep-boot-bytes))
         (ptr-write-u64 268435584 0 ,(if (eq mode 'markonly) 1 0))
         (ptr-write-u64 268436160 0 ,chunk)
         (ptr-write-u64 ,chunk 0 ,heap)
         (ptr-write-u64 ,(+ chunk 24) 0 ,heap)
         (ptr-write-u64 ,(+ chunk 48) 0 0)
         (ptr-write-u64 ,(+ diag 32) 0 ,(if (eq mode 'diag) 1 0))
         (ptr-write-u64 ,(+ check 8) 0 ,(if (eq mode 'check) 1 0))
         (ptr-write-u64 ,hmap 0 ,holes)
         (ptr-write-u64 ,(+ hmap 8) 0 ,heap)
         (ptr-write-u64 ,(+ hmap 16) 0
                        ,(+ heap nelisp-standalone-gc-test--sweep-boot-bytes))
         ,@(nreverse setup)
         0))
      (defun nl_probe_sweep_check ()
        (let ((bad 0))
          (seq ,@(nreverse checks) bad)))
      (defun nl_probe_sweep ()
        (seq (nl_probe_sweep_setup)
             (nl_gc_sweep_merge_chunk ,chunk)
             (nl_probe_sweep_check)))
      (exit (nl_probe_sweep)))))

(defun nelisp-standalone-gc-test--run-sweep-probe (mode)
  "Compile and run the one-pass sweep probe in MODE; return its exit code."
  (let ((path (make-temp-file "nelisp-standalone-gc-sweep-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-standalone-gc-test--sweep-source
            (nelisp-standalone-gc-test--sweep-blocks) mode)
           path)
          (call-process path nil nil nil))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-standalone-gc-one-pass-sweep-native ()
  "The one-pass sweep leaves the state the sweep/coalesce/relink walks left.
Every header, free-list chain, mask word, counter and live byte total is
compared with an independent oracle, with plain blocks, the poison and
checked-allocator paths, the mark-only debug mode and a malformed header."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (dolist (mode '(plain diag check markonly desync))
    (should (equal (list mode (nelisp-standalone-gc-test--run-sweep-probe mode))
                   (list mode 0)))))

(ert-deftest nelisp-standalone-gc-one-pass-sweep-oracle-is-red ()
  "A walk that never merges runs fails the sweep probe."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (let ((path (make-temp-file "nelisp-standalone-gc-sweep-red-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (cl-subst '(ptr-write-u64 run 0 (+ bt 2))
                     '(ptr-write-u64 run 0 (+ runbt 2))
                     (nelisp-standalone-gc-test--sweep-source
                      (nelisp-standalone-gc-test--sweep-blocks) 'plain)
                     :test #'equal)
           path)
          (should-not (equal (call-process path nil nil nil) 0)))
      (when (file-exists-p path) (delete-file path)))))

;;; Allocator current run -----------------------------------------------

(defun nelisp-standalone-gc-test--zero-source (&optional broken)
  "Compare production zeroing with the original eight-byte store sequence."
  (let* ((p #x37000000) (q (+ p 4096)) (meta (+ p 8192))
         (checked (cons 'seq (nelisp-standalone--alloc-check-arena-forms)))
         (forms nil))
    (dolist (name '(nl_alloc_zero_fill nl_alloc_pop_fast nl_alloc_bytes_run))
      (let ((form (or (nelisp-standalone-gc-test--find-defun checked name)
                      (nelisp-standalone-gc-test--find-defun
                       nelisp-standalone--arena-source name))))
        (dolist (pair `((,meta . nl_gc_diag) (,(+ meta 64) . nl_gc_stats)
                       (,(+ meta 128) . nl_freelist_small_mask)))
          (setq form (cl-subst (car pair) `(data-addr ,(cdr pair)) form :test #'equal)))
        (when (eq name broken)
          (let* ((store (if (eq name 'nl_alloc_pop_fast)
                            '(ptr-write-u64 (+ cur o) 0 0)
                          '(ptr-write-u64 (+ obj o) 0 0)))
                 (mutated (cl-subst 0 store form :test #'equal)))
            (when (equal form mutated) (error "Zeroing mutant did not apply"))
            (setq form mutated)))
        (push form forms)))
    `(seq
      (defun nl_alloc_pop_in_arena (_cur) 1)
      (defun nl_alloc_bytes_slow (_size _align) -1)
      ,@(nreverse forms)
      (defun nl_probe_zero_init ()
        (let ((i 0))
          (while (< i 512)
            (ptr-write-u64 ,p i -6510615555426900571)
            (ptr-write-u64 ,q i -6510615555426900571)
            (setq i (+ i 8)))))
      (defun nl_probe_zero_reference (obj off nbytes)
        (let ((o off))
          (while (< o nbytes)
            (ptr-write-u64 (+ obj o) 0 0)
            (setq o (+ o 8)))))
      (defun nl_probe_zero_compare ()
        (let ((i 0) (bad 0))
          (seq
           (while (and (< i 512) (= bad 0))
             (if (= (ptr-read-u64 ,p i) (ptr-read-u64 ,q i)) 0 (setq bad 1))
             (setq i (+ i 8)))
           bad)))
      (defun nl_probe_zero_small (want run)
        (let ((head (+ 268435696 (- want 16))) (obj 0))
          (seq
           (nl_probe_zero_init)
           (ptr-write-u64 ,(+ meta 64) 0 0)
           (ptr-write-u64 268435632 0 1)
           (ptr-write-u64 ,q 0 want)
           (if (= run 0)
               (seq
                (ptr-write-u64 ,p 0 (+ want 2))
                (ptr-write-u64 ,p 8 0)
                (ptr-write-u64 head 0 ,(+ p 8))
                (ptr-write-u64 ,(+ meta 128) 0 (shl 1 (/ (- want 16) 8)))
                (setq obj (nl_alloc_pop_fast want)))
             (seq
              (ptr-write-u64 ,p 0 (+ want 18))
              (ptr-write-u64 ,q want 18)
              (ptr-write-u64 ,meta 48 ,p)
              (ptr-write-u64 ,(+ meta 128) 0 0)
              (setq obj (nl_alloc_bytes_run (- want 8) 8 want))))
           (nl_probe_zero_reference ,(+ q 8) 0 (- want 8))
           (if (= obj ,(+ p 8))
               (if (= (ptr-read-u64 ,(+ meta 64) 0) want)
                   (if (= (ptr-read-u64 ,(+ meta 128) 0) 0)
                       (if (= run 0)
                           (if (= (ptr-read-u64 head 0) 0) (nl_probe_zero_compare) 4)
                         (if (= (ptr-read-u64 ,meta 48) (+ ,p want))
                             (nl_probe_zero_compare) 5)) 3) 2) 1))))
      (defun nl_probe_zero_main ()
        (let ((off 0) (n 0) (bad 0) (want 16))
          (seq
           (syscall-direct 9 ,p 16384 3 50 -1 0)
           (syscall-direct 9 268435456 4096 3 50 -1 0)
           (while (and (< off 18) (= bad 0))
             (setq n 0)
             (while (and (< n 130) (= bad 0))
               (nl_probe_zero_init)
               (nl_alloc_zero_fill ,p off n)
               (nl_probe_zero_reference ,q off n)
               (setq bad (nl_probe_zero_compare))
               (setq n (+ n 1)))
             (setq off (+ off 1)))
           (while (and (< want 473) (= bad 0))
             (setq bad (nl_probe_zero_small want 0))
             (if (= bad 0) (setq bad (nl_probe_zero_small want 1)) 0)
             (setq want (+ want 8)))
           bad)))
      (exit (nl_probe_zero_main)))))

(defun nelisp-standalone-gc-test--run-zero (&optional broken)
  (let ((path (make-temp-file "nelisp-zero-payload-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp (nelisp-standalone-gc-test--zero-source broken) path)
          (call-process path nil nil nil))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-standalone-gc-zeroing-native ()
  "Preserve every original store and neighboring bytes, headers and accounting."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64\\|amd64" system-configuration)))
  (should (equal (nelisp-standalone-gc-test--run-zero) 0)))

(ert-deftest nelisp-standalone-gc-zeroing-missing-store-is-red ()
  "The oracle rejects omitted stores in all three production paths."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64\\|amd64" system-configuration)))
  (dolist (name '(nl_alloc_zero_fill nl_alloc_pop_fast nl_alloc_bytes_run))
    (should-not (equal (nelisp-standalone-gc-test--run-zero name) 0))))

(defun nelisp-standalone-gc-test--hole-bits-source (&optional short-range)
  "Native hole-bitmap probe with an independent per-bit oracle."
  (let ((map #x36000000))
    `(seq
      ,(nelisp-standalone-gc-test--find-defun
        nelisp-standalone--gc-source 'nl_boot_hole_set_bits)
      (defun nl_probe_hole_case (begin end seed)
        (let ((i 0) (actual 0) (expected 0) (bad 0))
          (seq
           (while (< i 6)
             (ptr-write-u64 ,map (* i 8) seed)
             (setq i (+ i 1)))
           (nl_boot_hole_set_bits ,map begin ,(if short-range '(- end 1) 'end))
           (setq i 0)
           (while (and (< i 384) (= bad 0))
             (setq actual
                   (logand (sar (ptr-read-u64 ,map (* (sar i 6) 8))
                                (logand i 63)) 1))
             (setq expected
                   (if (and (>= i begin) (< i end)) 1
                     (logand (sar seed (logand i 63)) 1)))
             (if (= actual expected) 0 (setq bad 1))
             (setq i (+ i 1)))
           bad)))
      (defun nl_probe_hole_main ()
        (let ((begin 0) (width 0) (bad 0))
          (seq
           (syscall-direct 9 ,map 4096 3 50 -1 0)
           (while (and (< begin 129) (= bad 0))
             (setq width 0)
             (while (and (< width 131) (= bad 0))
               (setq bad (nl_probe_hole_case begin (+ begin width) 0))
               (if (= bad 0)
                   (setq bad (nl_probe_hole_case begin (+ begin width)
                                                 -6148914691236517206)) 0)
               (if (= bad 0)
                   (setq bad (nl_probe_hole_case begin (+ begin width) -1)) 0)
               (setq width (+ width 1)))
             (setq begin (+ begin 1)))
           bad)))
      (exit (nl_probe_hole_main)))))

(defun nelisp-standalone-gc-test--run-hole-bits (&optional short-range)
  (let ((path (make-temp-file "nelisp-hole-bits-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-standalone-gc-test--hole-bits-source short-range) path)
          (call-process path nil nil nil))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-standalone-gc-boot-hole-bits-native ()
  "All alignments, empty/full/cross-word ranges preserve neighboring bits."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64\\|amd64" system-configuration)))
  (should (equal (nelisp-standalone-gc-test--run-hole-bits) 0)))

(ert-deftest nelisp-standalone-gc-boot-hole-short-range-is-red ()
  "Dropping the last requested bit is rejected by the native oracle."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64\\|amd64" system-configuration)))
  (should-not (equal (nelisp-standalone-gc-test--run-hole-bits t) 0)))

(defconst nelisp-standalone-gc-test--run-heap #x35000000
  "Scratch heap for the current-run allocator probe (256 KiB mapped).")

(defun nelisp-standalone-gc-test--run-source (&optional mutate)
  "Native probe: up to 300 `nl_freelist_take' calls over a synthetic heap.
The production take, split, run, scan, bucket and mask bodies are linked
unchanged; `nl_gc_in_arena' and the allocation diagnostics are stubs.
After every take the probe walks the heap and the free lists and checks
that the headers chain exactly to the end, that every free block except
the current run is on a list and every list node is a free block, that
the run (if any) is a free block on no list, and that the allocated bytes
are the starting live bytes plus every request.  MUTATE is an optional
function applied to the production forms (for red checks)."
  (let* ((page nelisp-standalone-gc-test--freelist-page)
         (large nelisp-standalone-gc-test--freelist-large-page)
         (heap nelisp-standalone-gc-test--run-heap)
         (diag (+ page 1040))
         (blocks '((64 . 0) (40000 . 2) (32 . 0) (20000 . 2) (48 . 0)
                   (104 . 2) (32 . 0) (9000 . 2) (24 . 0)))
         (end (+ heap (apply #'+ (mapcar #'car blocks))))
         (live (apply #'+ (mapcar (lambda (b) (if (= (cdr b) 0) (car b) 0)) blocks)))
         (forms nil) (setup nil) (addr heap))
    (dolist (name '(nl_hdr_bt nl_hdr_mark nl_hdr_set_mark
                    nl_freelist_small_mask_bit nl_freelist_small_mask_set
                    nl_freelist_small_mask_clear nl_freelist_small_mask_sync_head
                    nl_freelist_small_first_bit nl_freelist_small_next_group
                    nl_freelist_small_next_index nl_freelist_large_bin
                    nl_freelist_large_mask_sync nl_freelist_large_next
                    nl_freelist_bucket_pop nl_freelist_bucket_split
                    nl_freelist_split_tail nl_freelist_scan_drop_tail
                    nl_freelist_scan_head nl_freelist_scan nl_freelist_scan_large_from
                    nl_freelist_scan_large nl_freelist_take nl_gc_relink_free_one
                    nl_alloc_run_reset nl_alloc_run_adopt nl_alloc_run_take))
      (let ((form (or (nelisp-standalone-gc-test--find-defun nelisp-standalone--arena-source name)
                      (nelisp-standalone-gc-test--find-defun nelisp-standalone--gc-source name))))
        (unless form (error "current-run probe defun absent: %S" name))
        (setq form (cl-subst diag '(data-addr nl_gc_diag) form :test #'equal))
        (setq form (cl-subst (+ large 2048) '(data-addr nl_freelist_large_mask) form :test #'equal))
        (push form forms)))
    (setq forms (nreverse forms))
    (when mutate (setq forms (funcall mutate forms)))
    (dolist (b blocks)
      (push `(ptr-write-u64 ,addr 0 ,(+ (car b) (cdr b))) setup)
      (when (= (cdr b) 2) (push `(nl_gc_relink_free_one ,addr) setup))
      (setq addr (+ addr (car b))))
    `(seq
      (defun nl_seq2 (_a b) b)
      (defun nl_freelist_small_mask_ptr () ,(+ page 1024))
      (defun nl_freelist_large_head (bin) (+ ,large (* bin 8)))
      (defun nl_gc_in_arena (_obj) 1)
      (defun nl_fl_record_trip (_cur _bt _want) 0)
      (defun nl_alloc_diag_bucket (obj) obj)
      (defun nl_alloc_diag_linear (obj) obj)
      ,@forms
      ;; Count list nodes, each of which must be a free block in the heap.
      (defun nl_probe_run_list_nodes (head)
        (let ((node (ptr-read-u64 head 0)) (n 0) (bad 0))
          (seq
           (while (and (= bad 0) (> node 0))
             (if (if (< (- node 8) ,heap) 1 (if (< (- node 8) ,end) 0 1))
                 (setq bad 1)
               (if (= (nl_hdr_mark (- node 8)) 2)
                   (seq (setq n (+ n 1)) (setq node (ptr-read-u64 node 0)))
                 (setq bad 1))))
           (if (= bad 0) n -1))))
      (defun nl_probe_run_lists ()
        (let ((i 0) (n 0) (k 0))
          (seq
           (setq k (nl_probe_run_list_nodes 268435552))
           (if (< k 0) (setq n -100000) (setq n (+ n k)))
           (while (< i 58)
             (setq k (nl_probe_run_list_nodes (+ 268435696 (* i 8))))
             (if (< k 0) (setq n -100000) (setq n (+ n k)))
             (setq i (+ i 1)))
           (setq i 0)
           (while (< i 204)
             (setq k (nl_probe_run_list_nodes (nl_freelist_large_head i)))
             (if (< k 0) (setq n -100000) (setq n (+ n k)))
             (setq i (+ i 1)))
           n)))
      ;; 0 when every invariant holds, else the failing invariant's code.
      (defun nl_probe_run_check (want_sum)
        (let ((hdr ,heap) (bt 0) (free 0) (used 0) (bad 0)
              (run (ptr-read-u64 ,(+ diag 48) 0)) (nodes 0))
          (seq
           (while (and (= bad 0) (< hdr ,end))
             (setq bt (nl_hdr_bt hdr))
             (if (if (< bt 16) 1 (if (< ,end (+ hdr bt)) 1 0))
                 (setq bad 1)
               (seq
                (if (= (nl_hdr_mark hdr) 2) (setq free (+ free 1))
                  (if (= (nl_hdr_mark hdr) 0) (setq used (+ used bt)) (setq bad 1)))
                (setq hdr (+ hdr bt)))))
           (if (= bad 0) (if (= hdr ,end) 0 (setq bad 2)) 0)
           (if (= bad 0)
               (if (= run 0) 0
                 (if (if (< run ,heap) 1 (if (< run ,end) 0 1)) (setq bad 6)
                   (if (= (nl_hdr_mark run) 2) 0 (setq bad 6))))
             0)
           (if (= bad 0)
               (seq (setq nodes (nl_probe_run_lists))
                    (if (< nodes 0) (setq bad 3)
                      (if (= free (+ nodes (if (= run 0) 0 1))) 0 (setq bad 4))))
             0)
           (if (= bad 0) (if (= used (+ ,live want_sum)) 0 (setq bad 5)) 0)
           bad)))
      (defun nl_probe_run_want (k)
        (let ((w (logand k 15)))
          (if (< w 3) 32 (if (< w 5) 16 (if (< w 7) 48 (if (< w 9) 24
            (if (= w 9) 64 (if (= w 10) 96 (if (= w 11) 200
              (if (= w 12) 472 (if (= w 13) 600 (if (= w 14) 1024 40))))))))))))
      (defun nl_probe_run_main ()
        (let ((i 0) (seed 12345) (want 0) (obj 0) (sum 0) (bad 0) (seen_run 0) (taken 0)
              (rr 0) (rbt 0))
          (seq
           (syscall-direct 9 ,page 4096 3 50 -1 0)
           (syscall-direct 9 ,large 4096 3 50 -1 0)
           (syscall-direct 9 ,heap 262144 3 50 -1 0)
           (ptr-write-u64 268435632 0 1)
           ,@(nreverse setup)
           (setq bad (nl_probe_run_check 0))
           (while (and (= bad 0) (< i 300))
             (setq seed (logand (+ (* seed 1103515245) 12345) 2147483647))
             (setq want (nl_probe_run_want (sar seed 8)))
             (setq obj (nl_freelist_take want))
             (if (= obj 0)
                 (setq i 300)
               (seq
                (if (= (ptr-read-u64 (- obj 8) 0) want) 0 (setq bad 7))
                (setq sum (+ sum want))
                (setq taken (+ taken 1))
                (if (= (ptr-read-u64 ,(+ diag 48) 0) 0) 0 (setq seen_run 1))
                (if (= bad 0) (setq bad (nl_probe_run_check sum)) 0)
                (setq i (+ i 1)))))
           (if (= bad 0) (if (= seen_run 1) 0 (setq bad 8)) 0)
           (if (= bad 0) (if (< taken 100) (setq bad 9) 0) 0)
           ;; An exact fit takes the whole run and clears it.
           (setq rr (ptr-read-u64 ,(+ diag 48) 0))
           (if (if (= bad 0) (if (= rr 0) 0 1) 0)
               (seq
                (setq rbt (nl_hdr_bt rr))
                (setq obj (nl_alloc_run_take rbt))
                (if (= obj (+ rr 8)) 0 (setq bad 11))
                (if (= (ptr-read-u64 ,(+ diag 48) 0) 0) 0 (setq bad 12))
                (setq sum (+ sum rbt))
                (if (= bad 0) (setq bad (nl_probe_run_check sum)) 0))
             (if (= bad 0) (setq bad 13) 0))
           ;; A reset drops the run; the walk then sees one more listless
           ;; free block, which the sweep's rebuild would relink.
           (if (= bad 0)
               (seq (nl_alloc_run_reset)
                    (if (= (ptr-read-u64 ,(+ diag 48) 0) 0) 0 (setq bad 10)))
             0)
           bad)))
      (exit (nl_probe_run_main)))))

(defun nelisp-standalone-gc-test--run-run-probe (&optional mutate)
  (let ((path (make-temp-file "nelisp-standalone-gc-run-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp (nelisp-standalone-gc-test--run-source mutate) path)
          (call-process path nil nil nil))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-standalone-gc-current-run-native ()
  "Small requests carve the current run and the heap stays exact.
600 mixed requests over a heap of live blocks and four free blocks: the
headers chain to the end after every request, every free block but the
run is on exactly one list, and the allocated bytes add up."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (should (equal (nelisp-standalone-gc-test--run-run-probe) 0)))

(ert-deftest nelisp-standalone-gc-current-run-unlinked-old-run-is-red ()
  "Dropping a replaced run without relinking it fails the probe."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires x86_64 Linux for the freestanding AOT executable"))
  (should-not
   (equal (nelisp-standalone-gc-test--run-run-probe
           (lambda (forms)
             (cl-subst 0 '(nl_gc_relink_free_one old) forms :test #'equal)))
          0)))
