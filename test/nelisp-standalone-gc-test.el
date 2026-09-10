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
      (defun nl_freelist_large_head (bin)
        (+ ,nelisp-standalone-gc-test--freelist-large-page (* bin 8)))
      (defun nl_chunk_cursor_addr (chunk) (+ chunk 64))
      (defun nl_boundary_reset_tail_chunks (_chunk _reclaimed) 0)
      (defun nl_aref_cache_clear () 0)
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
        (nl_seq2 (nl_probe_seed-small 0) (nl_probe_seed-large 0)))
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
        (if (and (= (nl_probe-zero-small 0) 1)
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

(provide 'nelisp-standalone-gc-test)

;;; nelisp-standalone-gc-test.el ends here
