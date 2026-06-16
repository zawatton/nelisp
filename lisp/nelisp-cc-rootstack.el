;;; nelisp-cc-rootstack.el --- Doc 152 §11.37 Stage 2: dynamic root stack  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 152 §11.37 (B+E handle-based root API) Stage 2 — a dynamic root
;; stack so that, in Stage 3, every eval transient box can be parked in a
;; REGISTERED root slot (instead of an unenumerable in-flight C-stack /
;; arena-scratch pointer).  This is the foundation that lets a future
;; mid-form / safepoint GC be SOUND: the marker no longer has to "guess"
;; the live in-flight roots — they are all on this stack.
;;
;; STATE: additive + DORMANT.  Nothing reserves a slot yet (Stage 3 wires
;; the eval ABI), so `nl_rootstack_init' is never reached, the base ptr
;; stays 0, and `nl_gc_mark_rootstack' returns 0 immediately.  Adding the
;; call into `nl_gc_mark_roots' therefore introduces NO new runtime
;; behaviour until Stage 3 — any error here surfaces at build time, not as
;; a silent GC heisenbug.
;;
;; Control slots (arena reserved prefix [0,0x400); free gap [0xF8,0x2b8],
;; below the chunk metadata @0x2c0 and chunk-0 desc @0x300):
;;   (data-addr nl_rootstack_base) (base+0xF8)  = root-stack region base ptr (0 = uninit)
;;   (data-addr nl_rootstack_top) (base+0x100) = root-stack top ptr (next free slot addr)
;; Region = a dedicated `nl_os_alloc_chunk' mmap (8 MiB = 262144 32-byte
;; Sexp slots), like the §11.18 compaction forwarding table: a raw side
;; region the sweep never walks, so slots are stable.  Each root slot is a
;; full 32-byte Sexp slot, marked via `nl_gc_mark_slot' exactly like the
;; ctx / result / out roots, so immediates (Nil/T/Int) are skipped and
;; heap boxes are traced — identical semantics to the existing roots.
;;
;; API (consumed by Stage 3):
;;   nl_root_mark      -> current top (a release marker; LIFO)
;;   nl_root_reserve   -> lazy-init + reserve one zeroed 32B slot, return addr
;;   nl_root_release M -> restore top to marker M (pop the frame)

;;; Code:

(defconst nelisp-cc-rootstack--source
  '(seq
    ;; Region = FIXED bss array (data-addr nl_rootstack_region); top = bss slot.
    ;; Do NOT mmap (os_alloc_chunk perturbs the arena chunk-growth VA layout ->
    ;; freelist corruption on the next collect, Doc 152 §11.30-33 class).
    ;; top == 0 means uninitialised (bss zero-fill); after init top >= region
    ;; addr (non-zero), so the zero-check is a reliable "not yet armed" gate.
    (defun nl_rootstack_init ()
      (if (= (ptr-read-u64 (data-addr nl_rootstack_top) 0) 0)
          (ptr-write-u64 (data-addr nl_rootstack_top) 0 (data-addr nl_rootstack_region))
        0))
    (defun nl_root_mark () (ptr-read-u64 (data-addr nl_rootstack_top) 0))
    ;; Reserve one 32-byte slot at top, zero it, bump top, return slot addr.
    (defun nl_root_reserve_slot (slot)
      (if (= slot 0) 0
          (seq (ptr-write-u64 slot 0 0)
               (ptr-write-u64 (+ slot 8) 0 0)
               (ptr-write-u64 (+ slot 16) 0 0)
               (ptr-write-u64 (+ slot 24) 0 0)
               (ptr-write-u64 (data-addr nl_rootstack_top) 0 (+ slot 32))
               slot)))
    (defun nl_root_reserve ()
      (seq (if (= (ptr-read-u64 (data-addr nl_rootstack_top) 0) 0) (nl_rootstack_init) 0)
           (nl_root_reserve_slot (ptr-read-u64 (data-addr nl_rootstack_top) 0))))
    (defun nl_root_release (marker) (ptr-write-u64 (data-addr nl_rootstack_top) 0 marker))
    ;; GC: walk [region, top) in 32-byte steps, mark each slot like a root.
    (defun nl_gc_mark_rootstack_walk (p end)
      (if (>= p end) 0
          (seq (extern-call nl_gc_mark_slot p)
               (nl_gc_mark_rootstack_walk (+ p 32) end))))
    (defun nl_gc_mark_rootstack ()
      (if (= (ptr-read-u64 (data-addr nl_rootstack_top) 0) 0) 0
          (nl_gc_mark_rootstack_walk (data-addr nl_rootstack_region)
                                     (ptr-read-u64 (data-addr nl_rootstack_top) 0)))))
  "AOT source for the Doc 152 §11.37 Stage 2 dynamic root stack.

Additive + dormant: lazy-inits on the first `nl_root_reserve' (none yet
until Stage 3), so the base ptr stays 0 and `nl_gc_mark_rootstack' is a
no-op until the eval ABI parks transients here.  See the Commentary for
the control-slot layout and the soundness rationale.")

(provide 'nelisp-cc-rootstack)

;;; nelisp-cc-rootstack.el ends here
