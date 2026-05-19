;;; nelisp-cc-nlconsbox-alloc.el --- nl_alloc_consbox elisp swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_alloc_consbox' body in
;; `build-tool/src/eval/nlconsbox.rs' with a Phase 47-compiled elisp
;; object.  The function allocates a fresh `NlConsBox' (car=Nil,
;; cdr=Nil, refcount=1) using the existing `alloc-bytes' /
;; `sexp-write-nil' / `ptr-write-u64' grammar ops and returns the
;; raw pointer as i64.
;;
;; NlConsBox layout (pinned by `#[repr(C)]' + compile-time asserts
;; in `build-tool/src/eval/nlconsbox.rs'):
;;
;;   car      @ 0   (32 bytes — sizeof(Sexp))
;;   cdr      @ 32  (32 bytes — sizeof(Sexp))
;;   refcount @ 64  (8 bytes  — AtomicUsize)
;;   total = 72 bytes, align = 8
;;
;; The two-function manifest (init helper + public entry) avoids
;; needing `let' in a value context (= Phase 47 `let' only supports
;; compile-time constant bindings).  The helper takes `box-ptr' as a
;; parameter, allowing it to reference the allocated pointer from
;; three different write sites without re-calling `alloc-bytes'.
;;
;; `sexp-write-nil SLOT' writes only the tag byte (1 byte) at SLOT,
;; leaving the remaining 31 Sexp payload bytes uninitialised.  That
;; is safe here because:
;;
;;   (a) `cons-make-with-clone' (the primary caller of
;;       `nl_alloc_consbox') immediately overwrites both fields
;;       via `nl_sexp_clone_into', which uses `core::ptr::write'
;;       (= unconditional 32-byte write, no prior-drop step).
;;
;;   (b) `cons-make' (the original caller) overwrites both fields
;;       via 32-byte `movdqu' pairs from the caller-supplied
;;       car/cdr Sexp slots.
;;
;;   In both call paths the "pre-initialised to Nil" guarantee is
;;   fulfilled by the tag byte alone; the uninitialised payload
;;   bytes are never observed.
;;
;; `ptr-write-u64 box-ptr 64 1' writes the u64 value 1 to the
;; AtomicUsize trailer at offset 64.  On x86_64 an aligned 8-byte
;; store is inherently atomic at the hardware level; initialising
;; an AtomicUsize this way before handing the pointer to any
;; concurrent code is safe (matches the `AtomicUsize::new(1)'
;; semantics in the original Rust body).
;;
;; OOM behaviour: `alloc-bytes' calls `nl_alloc_bytes' which returns
;; null on OOM, matching the `std::alloc::alloc' contract.  Callers
;; that do not check for null (= `cons-make' + `cons-make-with-clone'
;; emitters) will segfault on a null-pointer dereference, which is
;; equivalent to the original Rust `Box::new' abort-on-OOM.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this
;; feature; `build.rs' compiles the source into `nl_alloc_consbox.o'
;; and archives it.  `build-tool/src/lib.rs' declares the extern
;; (keeping the symbol alive against DCE) and exposes a `cc_wrap!'
;; safe wrapper for the integration probe in
;; `tests/elisp_cc_nlconsbox_alloc_probe.rs'.

;;; Code:

(defconst nelisp-cc-nlconsbox-alloc--source
  '(seq
    ;; Helper: takes a freshly-allocated box pointer, writes the
    ;; three fields, and returns the pointer.  Defined as a separate
    ;; defun because Phase 47 `let' only supports compile-time
    ;; constants; the runtime pointer from `alloc-bytes' must travel
    ;; as a function argument to be spilled to a named slot.
    ;;
    ;; The body is an `(and ...)' chain that sequences three side-
    ;; effecting writes and returns `box-ptr' as the final value:
    ;;   - `sexp-write-nil box-ptr'         writes NIL tag at car (offset 0)
    ;;   - `sexp-write-nil (+ box-ptr 32)'  writes NIL tag at cdr (offset 32)
    ;;   - `ptr-write-u64 box-ptr 64 1'     writes refcount=1 at offset 64
    ;;   - `box-ptr'                         return the input pointer
    ;; `sexp-write-nil' returns the slot pointer (non-zero), and
    ;; `ptr-write-u64' returns 1 sentinel, so the chain never short-
    ;; circuits under normal conditions.
    (defun nl_alloc_consbox_init (box-ptr)
      (and (sexp-write-nil box-ptr)
           (sexp-write-nil (+ box-ptr 32))
           (ptr-write-u64 box-ptr 64 1)
           box-ptr))

    ;; Public entry — allocate 72-byte NlConsBox with alignment 8,
    ;; initialise fields, return raw pointer as i64.
    (defun nl_alloc_consbox ()
      (nl_alloc_consbox_init (alloc-bytes 72 8))))
  "Phase 47 source for the `nl_alloc_consbox' allocator swap.

Two-entry `(seq DEFUN ...)' manifest:
- `nl_alloc_consbox_init (box-ptr) -> box-ptr' — helper that
  initialises the three NlConsBox fields and returns the pointer.
- `nl_alloc_consbox () -> *mut NlConsBox' — public entry; calls
  `alloc-bytes(72, 8)' then delegates to the init helper.

Phase 47 ops consumed:
  `alloc-bytes'    — 2-arg `nl_alloc_bytes(size, align) -> *mut u8'
                     call; size=72 / align=8 are compile-time immediates.
  `sexp-write-nil' — writes `SEXP_TAG_NIL' (1 byte) to a `*mut Sexp'
                     slot; used at offsets 0 (car) and 32 (cdr).
  `ptr-write-u64'  — `nl_ptr_write_u64(ptr, offset, val)'; writes
                     the u64 value 1 to offset 64 (refcount trailer).

Net Rust delta: deletes the 10-LOC `nl_alloc_consbox' body from
`build-tool/src/eval/nlconsbox.rs' (= `#[no_mangle]' extern decl +
`Box::new(NlConsBox {...})' + `Box::into_raw' + closing brace).
The Rust `use std::alloc::{self, Layout};' import remains needed by
`NlConsBoxRef::new' / `rc_dec_raw'.")

(provide 'nelisp-cc-nlconsbox-alloc)

;;; nelisp-cc-nlconsbox-alloc.el ends here
