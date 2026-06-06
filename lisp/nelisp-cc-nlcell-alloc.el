;;; nelisp-cc-nlcell-alloc.el --- nl_alloc_cell AOT migration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_alloc_cell' body in
;; `build-tool/src/eval/nlcell.rs' with a AOT-compiled elisp
;; object.  The function allocates a fresh `NlCell' (value = clone of
;; *initial, refcount = 1) using `alloc-bytes', `extern-call
;; nl_sexp_clone_into', and `ptr-write-u64' grammar ops.
;;
;; NlCell layout (pinned by `#[repr(C)]' + compile-time asserts
;; in `build-tool/src/eval/sexp_abi_assert.rs'):
;;
;;   value    @ 0   (32 bytes — sizeof(Sexp))
;;   refcount @ 32  (8 bytes  — AtomicUsize)
;;   total = 40 bytes, align = 8
;;
;; The two-function manifest (init helper + public entry) avoids
;; needing `let' in a value context (= AOT `let' only supports
;; compile-time constant bindings).  The helper takes both `box-ptr'
;; and `initial' as parameters so the allocated pointer travels as a
;; function argument.
;;
;; `extern-call nl_sexp_clone_into initial box-ptr' calls the Rust
;; `nl_sexp_clone_into(src, dst)' helper which:
;;   (a) clones *initial (= variant-aware: bumps refcount on box-tagged
;;       variants, copies pod bits otherwise)
;;   (b) writes the clone into *box-ptr (= the value field at offset 0)
;;   The dst is treated as uninitialized — safe here because the box
;;   was just allocated by `alloc-bytes'.
;;
;; `ptr-write-u64 box-ptr 32 1' writes the u64 value 1 to the
;; AtomicUsize refcount trailer at offset 32.  On x86_64 an aligned
;; 8-byte store is inherently atomic at the hardware level; initialising
;; an AtomicUsize this way before handing the pointer to any concurrent
;; code is safe (matches the `AtomicUsize::new(1)' semantics in the
;; original Rust body).
;;
;; `extern-call nl_sexp_clone_into' returns Rust unit (= rax
;; undefined after the call).  The `(or ... 1)' wrapper guarantees the
;; `and' chain sees a non-zero value at that position, preventing
;; accidental short-circuit.  `ptr-write-u64' returns 1 sentinel.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature;
;; `build.rs' compiles the source into `nl_alloc_cell.o' and archives
;; it.  `build-tool/src/eval/nlcell.rs' keeps a thin `pub unsafe fn
;; nl_alloc_cell' wrapper that calls the C-symbol from the .o via an
;; `extern "C"' block; the integration test `nlcell_integration.rs'
;; imports it through `use nelisp_build_tool::eval::nlcell::*' and
;; continues to verify alloc + refcount-1 invariants.

;;; Code:

(defconst nelisp-cc-nlcell-alloc--source
  '(seq
    ;; Helper: given a freshly-allocated box pointer and the initial
    ;; Sexp pointer, clones the initial value into the value field and
    ;; writes refcount = 1.  Returns box-ptr.
    ;;
    ;; Layout (mirrors `sexp_abi_assert.rs' const_asserts):
    ;;   value    @ 0   (32 bytes — Sexp)
    ;;   refcount @ 32  (8 bytes  — AtomicUsize)
    (defun nl_alloc_cell_init (box-ptr initial)
      (and (or (extern-call nl_sexp_clone_into initial box-ptr) 1)
           (ptr-write-u64 box-ptr 32 1)
           box-ptr))

    ;; Public entry — allocate 40-byte NlCell with alignment 8,
    ;; initialise fields, return raw pointer as i64.
    (defun nl_alloc_cell (initial)
      (nl_alloc_cell_init (alloc-bytes 40 8) initial)))
  "AOT source for the `nl_alloc_cell' allocator swap.

Two-entry `(seq DEFUN ...)' manifest:
- `nl_alloc_cell_init (box-ptr initial) -> box-ptr' — initialises
  the NlCell's value field (= clone of *initial) and refcount (= 1).
- `nl_alloc_cell (initial) -> *mut NlCell' — public entry; calls
  `alloc-bytes(40, 8)' then delegates to the init helper.

AOT ops consumed:
  `alloc-bytes'           — 2-arg `nl_alloc_bytes(size, align)';
                            size=40 / align=8 compile-time immediates.
  `extern-call nl_sexp_clone_into' — clones *initial into box-ptr+0.
  `ptr-write-u64'         — writes u64 value 1 to offset 32 (refcount).

Net Rust delta: deletes the 9-LOC `nl_alloc_cell' body + safety
comment from `build-tool/src/eval/nlcell.rs', replacing it with a
5-LOC extern \"C\" block + thin pub wrapper so the integration test
`nlcell_integration::nl_alloc_cell_returns_initialised_cell_with_refcount_1'
continues to compile and pass.")

(provide 'nelisp-cc-nlcell-alloc)

;;; nelisp-cc-nlcell-alloc.el ends here
