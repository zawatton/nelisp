;;; nelisp-cc-native-call-boundary.el --- Native call boundary trampoline  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase47-compatible native call boundary source for x86_64 SysV.
;; The `defconst' below is consumed as a `(seq (defun ...))' datum by the
;; AOT compiler and emits a tiny RWX trampoline page that bridges the
;; compiler's direct `call-ptr' primitive into a native body address.

;;; Code:

(defconst nelisp-cc-native-call-boundary--source
  '(seq
    (defun bf_native_call_boundary--even-round (n)
      (if (= (logand n 1) 0) n (+ n 1)))
    (defun bf_native_call_boundary--frame-bytes (arity rt-slot-count)
      (+ (* 8 arity)
         (if (= (logand arity 1) 1) 8 0)
         (* 8 (bf_native_call_boundary--even-round rt-slot-count))))
    (defun bf_native_call_boundary--slot-disp (slot-index)
      (- (* 8 (+ slot-index 1))))
    (defun bf_native_call_boundary--emit-u8 (page off byte)
      (ptr-write-u8 page off byte))
    (defun bf_native_call_boundary--emit-u32 (page off value)
      (ptr-write-u32 page off value))
    (defun bf_native_call_boundary--emit-u64 (page off value)
      (ptr-write-u64 page off value))
    (defun bf_native_call_boundary--zero-qwords (base count)
      (let* ((i 0))
        (while (< i count)
          (ptr-write-u64 (+ base (* i 8)) 0 0)
          (setq i (+ i 1)))
        base))
    (defun bf_native_call_boundary--emit-prologue (page off frame-bytes)
      (seq
       (bf_native_call_boundary--emit-u8 page off #x55)
       (bf_native_call_boundary--emit-u8 page (+ off 1) #x48)
       (bf_native_call_boundary--emit-u8 page (+ off 2) #x89)
       (bf_native_call_boundary--emit-u8 page (+ off 3) #xe5)
       (bf_native_call_boundary--emit-u8 page (+ off 4) #x48)
       (bf_native_call_boundary--emit-u8 page (+ off 5) #x81)
       (bf_native_call_boundary--emit-u8 page (+ off 6) #xec)
       (bf_native_call_boundary--emit-u32 page (+ off 7) frame-bytes)
       (+ off 11)))
    (defun bf_native_call_boundary--emit-movabs-rax (page off imm64)
      (seq
       (bf_native_call_boundary--emit-u8 page off #x48)
       (bf_native_call_boundary--emit-u8 page (+ off 1) #xb8)
       (bf_native_call_boundary--emit-u64 page (+ off 2) imm64)
       (+ off 10)))
    (defun bf_native_call_boundary--emit-spill-code (page off rex modrm8 modrm32 disp)
      (seq
       (bf_native_call_boundary--emit-u8 page off rex)
       (bf_native_call_boundary--emit-u8 page (+ off 1) #x89)
       (if (and (<= -128 disp) (<= disp 127))
           (seq
            (bf_native_call_boundary--emit-u8 page (+ off 2) modrm8)
            (bf_native_call_boundary--emit-u8 page (+ off 3)
                                             (logand disp #xff))
            (+ off 4))
         (seq
          (bf_native_call_boundary--emit-u8 page (+ off 2) modrm32)
          (bf_native_call_boundary--emit-u32 page (+ off 3)
                                             (logand disp #xffffffff))
          (+ off 7)))))
    (defun bf_native_call_boundary--emit-jump (page off body)
      (seq
       (setq off (bf_native_call_boundary--emit-movabs-rax page off body))
       (bf_native_call_boundary--emit-u8 page off #xff)
       (bf_native_call_boundary--emit-u8 page (+ off 1) #xe0)
       (+ off 2)))
    (defun bf_native_call_boundary (args env out)
      (let* ((body (wf_argval args 0))
             (arity (wf_argval args 1))
             (rt-slot-count (wf_argval args 2)))
        (if (or (<= body 0)
                (< arity 0)
                (> arity 6)
                (< rt-slot-count 17)
                (> rt-slot-count 512))
            1
          (let* ((page (syscall-direct 9 0 4096 7 34 -1 0)))
            (if (< page 4096)
                1
              (let* ((scratch (alloc-bytes 32 8))
                     (name (alloc-bytes 32 8))
                     (callbacks (alloc-bytes (* 12 32) 8))
                     ;; Doc 166: this function's frame is invisible to the GC
                     ;; (compiled cc code), and the callee keeps live values in
                     ;; SCRATCH's rotation slots across bridge calls — and may
                     ;; even return one of those boxes as its result word.  Root
                     ;; the three caller-owned blocks on the dynamic rootstack
                     ;; for the whole call INCLUDING result materialization
                     ;; (mirrors `nelisp_aot_call8_core').  Track failures are
                     ;; ignored: degraded behavior equals the historical one.
                     ;; Bound here in the OUTER let* (not inside the
                     ;; trampoline-emit `(let* ((off 0)) ...)' block below)
                     ;; because that block closes before the call-ptr /
                     ;; result-materialization block runs below it — ROOT-MARKER
                     ;; must stay in scope for the `nl_root_release' call there.
                     (_rootstack-init (nl_rootstack_init))
                     (root-marker (nl_root_mark))
                     (_scratch-track (nl_root_track scratch))
                     (_name-track (nl_root_track name))
                     (_callbacks-track (nl_root_track callbacks))
                     (frame-bytes
                      (bf_native_call_boundary--frame-bytes
                       arity rt-slot-count))
                     ;; Doc 166 §2.2: the boundary wordifies each Sexp arg
                     ;; into a canonical value word (int 4N+1 / nil 3 / t 7 /
                     ;; slot pointer) via `wf_argword'.
                     (call-arg0 (if (> arity 0) (wf_argword args 3) 0))
                     (call-arg1 (if (> arity 1) (wf_argword args 4) 0))
                     (call-arg2 (if (> arity 2) (wf_argword args 5) 0))
                     (call-arg3 (if (> arity 3) (wf_argword args 6) 0))
                     (call-arg4 (if (> arity 4) (wf_argword args 7) 0))
                     (call-arg5 (if (> arity 5) (wf_argword args 8) 0)))
                (seq
                 (bf_native_call_boundary--zero-qwords scratch 4)
                 (bf_native_call_boundary--zero-qwords name 4)
                 (bf_native_call_boundary--zero-qwords callbacks 48)
                 ;; Doc 129 ABI: `scratch' is the standard AOT scratch vector.
                 ;; Compiled literal materialization writes THROUGH
                 ;; `nl_vector_slot_ptr (scratch, DEPTH)' views and re-reads
                 ;; them later, so every slot must hold a POINTER word (a
                 ;; stable 32B box): immediate words (the `nl_alloc_vector'
                 ;; Nil prefill) yield throwaway view boxes and the writes
                 ;; vanish.  Install a 32-slot Sexp::Vector (tag 8 + box
                 ;; payload) and prefill the data buffer with raw pointer
                 ;; words to zeroed (= Nil) boxes.  Slots 0..15 serve the
                 ;; literal-materializer depth protocol; slots 16..31 are the
                 ;; compiler's rotating temp pool for staged argument values.
                 (let* ((scratch-vec (nl_alloc_vector 32)))
                   (seq
                    (ptr-write-u64 scratch 0 8)
                    (ptr-write-u64 (+ scratch 8) 0 scratch-vec)
                    (let* ((data-ptr (ptr-read-u64 scratch-vec 8))
                           (i 0))
                      (while (< i 32)
                        (let* ((box (alloc-bytes 32 8)))
                          (seq
                           (bf_native_call_boundary--zero-qwords box 4)
                           (ptr-write-u64 (+ data-ptr (* i 8)) 0 box)
                           (setq i (+ i 1))))))))
                 (let* ((off 0))
                   (setq off
                         (bf_native_call_boundary--emit-prologue
                          page off frame-bytes))
                   (if (> arity 0)
                       (setq off
                             (bf_native_call_boundary--emit-spill-code
                              page off #x48 #x7d #xbd
                              (bf_native_call_boundary--slot-disp 0)))
                     off)
                   (if (> arity 1)
                       (setq off
                             (bf_native_call_boundary--emit-spill-code
                              page off #x48 #x75 #xb5
                              (bf_native_call_boundary--slot-disp 1)))
                     off)
                   (if (> arity 2)
                       (setq off
                             (bf_native_call_boundary--emit-spill-code
                              page off #x48 #x55 #x95
                              (bf_native_call_boundary--slot-disp 2)))
                     off)
                   (if (> arity 3)
                       (setq off
                             (bf_native_call_boundary--emit-spill-code
                              page off #x48 #x4d #x8d
                              (bf_native_call_boundary--slot-disp 3)))
                     off)
                   (if (> arity 4)
                       (setq off
                             (bf_native_call_boundary--emit-spill-code
                              page off #x4c #x45 #x85
                              (bf_native_call_boundary--slot-disp 4)))
                     off)
                   (if (> arity 5)
                       (setq off
                             (bf_native_call_boundary--emit-spill-code
                              page off #x4c #x4d #x8d
                              (bf_native_call_boundary--slot-disp 5)))
                     off)
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off out))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 0))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off env))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 1))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off (+ env 32)))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 2))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off scratch))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 3))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off name))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 4))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off callbacks))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 5))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off (+ callbacks 32)))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 6))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off (+ callbacks 64)))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 7))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off (+ callbacks 96)))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 8))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off (+ callbacks 128)))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 9))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off (+ callbacks 160)))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 10))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off (+ callbacks 192)))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 11))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off (+ callbacks 224)))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 12))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off (+ callbacks 256)))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 13))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off (+ callbacks 288)))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 14))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off (+ callbacks 320)))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 15))))
                   (setq off
                         (bf_native_call_boundary--emit-movabs-rax
                          page off (+ callbacks 352)))
                   (setq off
                         (bf_native_call_boundary--emit-spill-code
                          page off #x48 #x45 #x85
                          (bf_native_call_boundary--slot-disp (+ arity 16))))
                   (setq off (bf_native_call_boundary--emit-jump page off body))
                   off))
                (let* ((native-result
                        (call-ptr page
                                  call-arg0 call-arg1 call-arg2
                                  call-arg3 call-arg4 call-arg5)))
                  (seq
                   (syscall-direct 11 page 4096 0 0 0 0)
                   ;; General-ABI bodies receive OUT as their result
                   ;; slot and return that same pointer; boxing the raw
                   ;; return as an integer here clobbered the correct
                   ;; boxed result with OUT's own address (measured:
                   ;; (p9m-user) printed the address instead of 42).
                   ;; Leave OUT as written by the body in that case;
                   ;; box the raw value only when the body returned
                   ;; something else (legacy integer-ABI behaviour).
                   ;; Doc 166 §2.4: bodies return a canonical value word.
                   ;; A body that wrote OUT directly returns OUT itself
                   ;; (pointer word equal to the slot); any other word is
                   ;; materialized into OUT via `nl_val_load'.
                   (if (= native-result out)
                       0
                     (seq (nl_val_load native-result out) 0))
                   (nl_root_release root-marker)
                   0)))))))))
  "AOT source for the native call boundary trampoline.

The source is intentionally self-contained and emits an x86_64 SysV
trampoline page with the following shape:
  - validate `body', `arity', and `rt-slot-count'
  - mmap one 4096-byte RWX page
  - allocate and zero scratch/name/callback blocks
  - spill incoming args and hidden runtime addresses
  - jump to the native body
  - munmap the trampoline page after the call returns.

The runtime call site uses the direct compiler primitive `call-ptr', unboxes
the Elisp-facing Sexp arguments through `wf_argval', and leaves OUT as
written by general-ABI bodies (which return OUT), boxing the raw i64
return only for bodies that return another value.")

(provide 'nelisp-cc-native-call-boundary)

;;; nelisp-cc-native-call-boundary.el ends here
