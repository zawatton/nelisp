;;; nelisp-cc-alloc-mem.el --- mmap-based alloc/dealloc via syscall-direct  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 125 §125.B — mmap-based allocator proof-of-concept using the new
;; `syscall-direct' Phase 47 grammar op.
;;
;; STATUS (2026-05-20): PoC shipping under DIFFERENT symbol names
;; (`nl_mmap_alloc' / `nl_mmap_dealloc') rather than replacing
;; `nl_alloc_bytes' / `nl_dealloc_bytes'.
;;
;; BLOCKER for full replacement:
;;   `NlConsBoxRef::new()' and siblings in `nlconsbox.rs' / `nlcell.rs'
;;   etc. allocate via Rust's `std::alloc::alloc'.  The Drop path
;;   (`nl_ref_common!' macro → `nlconsbox_drop') calls `dealloc-bytes'
;;   → `nl_dealloc_bytes'.  If `nl_dealloc_bytes' becomes mmap/munmap,
;;   all Rust-heap-allocated boxes crash on drop with "free(): invalid
;;   pointer" because munmap rejects non-mmap pointers.
;;
;;   Resolution: all `NlConsBoxRef::new()' / `NlCellRef::new()' etc.
;;   Rust allocation sites must migrate to call `nl_alloc_bytes' (the
;;   new mmap version) before the global swap can be activated.  That
;;   migration is deferred to a follow-up (= Doc 125 §125.C).
;;
;; What this file ships:
;;   - `nl_mmap_alloc(size) -> i64' — validates size > 0, calls
;;     mmap(2) via `syscall-direct', returns ptr or 0.
;;   - `nl_mmap_dealloc(ptr size) -> i64' — calls munmap(2) via
;;     `syscall-direct', returns 1 sentinel.
;;   These are standalone symbols usable by any future code that
;;   explicitly chooses the mmap-backed path.
;;
;; `syscall-direct NR A0 A1 A2 A3 A4 A5':
;;   New Phase 47 grammar op (Doc 125 §125.B) emitting inline SYSCALL
;;   instruction.  rax=NR rdi=A0 rsi=A1 rdx=A2 r10=A3 r8=A4 r9=A5.
;;   Returns kernel return value in rax.
;;
;; mmap(2) call: SYS_mmap (= 9)
;;   addr=0, length=size, prot=3 (PROT_READ|PROT_WRITE),
;;   flags=34 (MAP_ANON|MAP_PRIVATE), fd=-1, offset=0.
;;
;; munmap(2) call: SYS_munmap (= 11)
;;   addr=ptr, length=size.
;;
;; Alignment: mmap returns page-aligned (4096-byte) addresses.
;; Any alignment ≤ 4096 bytes is satisfied trivially.  The `align'
;; argument is accepted by `nl_mmap_alloc' only for interface parity
;; with `nl_alloc_bytes'; it is validated but not passed to the kernel.
;;
;; Size granularity: mmap allocates in page multiples (4096-byte
;; minimum), wasting memory for small objects.  A slab allocator on
;; top is deferred to Doc 125 §125.D.

;;; Code:

;; ---- nl_mmap_alloc ----

(defconst nelisp-cc-alloc-mem--alloc-source
  '(seq
    ;; Helper: validate raw mmap return value.  Returns ptr on success,
    ;; 0 on error (kernel returns -errno < 0 for failure).
    (defun nl_mmap_alloc_check (raw)
      (if (< raw 0) 0 raw))

    ;; Helper: issue mmap syscall and validate result.
    ;; Takes validated `size' as param so it travels through the call chain.
    (defun nl_mmap_alloc_do (size)
      ;; mmap(0, size, PROT_READ|PROT_WRITE=3, MAP_ANON|MAP_PRIVATE=34, -1, 0)
      ;; SYS_mmap = 9
      (nl_mmap_alloc_check (syscall-direct 9 0 size 3 34 -1 0)))

    ;; Public entry: `nl_mmap_alloc(size, align) -> *mut u8 as i64'.
    ;; align is validated (must be pow2 > 0) but unused — mmap always
    ;; returns page-aligned memory satisfying any align <= 4096.
    ;; Returns 0 on bad layout or kernel error.
    (defun nl_mmap_alloc (size align)
      ;; Guard: size > 0 AND align > 0 AND align is a power of two.
      ;; Power-of-two check: (align & (align - 1)) == 0.
      (if (and (> size 0)
               (> align 0)
               (= (logand align (- align 1)) 0))
          (nl_mmap_alloc_do size)
        0)))
  "Phase 47 source for mmap-based `nl_mmap_alloc'.

Three-entry `(seq DEFUN ...)' manifest:
- `nl_mmap_alloc_check (raw)' — validates mmap return value.
- `nl_mmap_alloc_do (size)' — issues SYS_mmap via syscall-direct.
- `nl_mmap_alloc (size align)' — public entry; validates layout,
  delegates to mmap helper.

Phase 47 ops: `syscall-direct' (SYS_mmap=9), `cmp' (< > =),
  `arith' (- logand), `logic' (and), `if'.

Note: `nl_alloc_bytes' (Rust `std::alloc') is NOT replaced by this
function.  The global swap (Doc 125 §125.C) is blocked on migrating
all Rust-native allocation sites (`NlConsBoxRef::new()' etc.) away
from `std::alloc'.  This PoC validates the `syscall-direct' grammar
op and mmap plumbing without disrupting existing code paths.")

;; ---- nl_mmap_dealloc ----

(defconst nelisp-cc-alloc-mem--dealloc-source
  '(seq
    ;; Sink: discards the munmap return value, returns 1 sentinel.
    ;; Necessary because `and' short-circuits on 0, and munmap returns
    ;; 0 on success.  A helper that ignores the argument is the
    ;; cleanest way to discard a void-return in Phase 47.
    (defun nl_mmap_dealloc_ignore (_raw)
      1)

    ;; Helper: issue munmap syscall; pass result to sink → 1 sentinel.
    (defun nl_mmap_dealloc_do (ptr size)
      ;; munmap(ptr, size) — SYS_munmap = 11
      (nl_mmap_dealloc_ignore (syscall-direct 11 ptr size 0 0 0 0)))

    ;; Public entry: `nl_mmap_dealloc(ptr, size, align) -> i64'.
    ;; Null-safe (ptr = 0 → no-op, returns 1).  Bad layout (size <= 0,
    ;; align <= 0, align not pow2) → no-op, returns 1.
    ;; ptr > 0 check: valid user-space addresses on x86_64 Linux are
    ;; positive when interpreted as signed i64.
    (defun nl_mmap_dealloc (ptr size align)
      (if (and (> ptr 0)
               (> size 0)
               (> align 0)
               (= (logand align (- align 1)) 0))
          (nl_mmap_dealloc_do ptr size)
        1)))
  "Phase 47 source for munmap-based `nl_mmap_dealloc'.

Three-entry `(seq DEFUN ...)' manifest:
- `nl_mmap_dealloc_ignore (_raw)' — discards munmap rc, returns 1.
- `nl_mmap_dealloc_do (ptr size)' — issues SYS_munmap via syscall-direct.
- `nl_mmap_dealloc (ptr size align)' — public entry; validates, delegates.

Note: `nl_dealloc_bytes' (Rust) is NOT replaced.  See nl_mmap_alloc
commentary above for the blocker.")

(provide 'nelisp-cc-alloc-mem)

;;; nelisp-cc-alloc-mem.el ends here
