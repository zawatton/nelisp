;;; nelisp-cc-rc-payload-ptr.el --- Doc 123 §123.D payload-ptr reader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 123 §123.D — pure-elisp `nelisp_rc_payload_ptr' kernel, the
;; box-pointer extractor of `bi_nl_rc_payload_ptr' at
;; `build-tool/src/eval/rc_primitives.rs:230-244'.  Replaces the
;; `Sexp::Cons(r) => NlConsBoxRef::as_ptr(r) as usize as i64' arm
;; with a single §122.E `ptr-read-u64' grammar op.
;;
;; Function contract:
;;   sexp-ptr: raw `*const Sexp' coerced to i64 (= the outer Sexp
;;             value's address; not the inner box payload).  Same
;;             input shape as §123.C `nelisp_rc_kind'.
;;   returns: the inner NlBox* payload pointer as i64.  For
;;            `Sexp::Cons(NlConsBoxRef)' this is the `NlConsBox*'
;;            stored at offset 8 (= `SEXP_PAYLOAD_OFFSET' per
;;            `build-tool/src/eval/sexp.rs:270') of the outer Sexp's
;;            `#[repr(C, u8)]' layout.  For atoms (Nil / T / Int /
;;            Float / Str / Symbol / etc.) this returns whatever
;;            bytes happen to live at offset 8 — the *caller* is
;;            responsible for tag-checking via §123.C `rc_kind'
;;            before treating the result as a meaningful pointer.
;;            The Rust `bi_nl_rc_payload_ptr' returns 0 for non-Cons
;;            via a tag-dispatch arm; that dispatch lands in §123.F
;;            (= the sweep stage's Rust shim around this kernel).
;;
;; The body is one composed value form:
;;
;;   (ptr-read-u64 sexp-ptr 8)
;;
;; The constant `8' is `SEXP_PAYLOAD_OFFSET' defined at
;; `build-tool/src/eval/sexp.rs:270'.  Layout-pinned by the
;; `#[repr(C, u8)]' attribute on the `Sexp' declaration at
;; `build-tool/src/eval/sexp.rs:57' (= the discriminant byte at
;; offset 0, then 7 bytes of padding, then the payload union
;; starting at offset 8).  Same offset every `Sexp` variant uses
;; for its boxed payload pointer — `NlConsBoxRef'
;; (`#[repr(transparent)]' over `NonNull<NlConsBox>') / `NlVector'
;; / `NlCell' / `NlRecord' / `NlStr' all share this layout slot.
;;
;; Why offset 8 and not the NlConsBox header layout?  The audit
;; cell in `docs/design/123-rc-primitives-elisp-rewrite.org §2.4'
;; calls out: this kernel reads the *inner box pointer* out of the
;; *outer Sexp enum*, not a field of the box itself.  The box's
;; per-type fields (= car / cdr / refcount / etc.) live at offsets
;; relative to the box base — those are reached by *follow-up*
;; ops (= §123.A's REFCOUNT_OFFSET = 64, §123.D's gc-walk-children
;; car/cdr at offsets 0/32).
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this
;; feature in its manifest; `build-tool/build.rs' compiles the
;; source into `nelisp_rc_payload_ptr.o' and archives it into the
;; static lib the crate links against.  The Rust side declares the
;; `extern "C"' signature in `build-tool/src/lib.rs::elisp_cc_spike'
;; and exposes a safe wrapper `elisp_cc_spike::rc_payload_ptr(sexp_ptr)'
;; for the integration test in
;; `tests/elisp_cc_rc_payload_ptr_probe.rs'.
;;
;; Stage scope: §123.D ships *only* the elisp kernel + probe.  The
;; Rust-side `bi_nl_rc_payload_ptr' primitive in `rc_primitives.rs:
;; 230-244' continues to dispatch on the variant directly; §123.F
;; (= a future sweep stage matching the §123.A-C pattern) will swap
;; the body to call this kernel via extern-C and re-check the tag.
;; Existing `rc_primitives.rs' unit tests remain green throughout
;; this stage.

;;; Code:

(defconst nelisp-cc-rc-payload-ptr--source
  '(defun nelisp_rc_payload_ptr (sexp-ptr)
     ;; NlBox payload pointer at offset 8 of the outer `Sexp' enum
     ;; (= `SEXP_PAYLOAD_OFFSET' per `build-tool/src/eval/sexp.rs:270').
     ;; Layout-pinned by `#[repr(C, u8)]' on the `Sexp' decl at
     ;; `sexp.rs:57' — discriminant byte at offset 0, 7 bytes of
     ;; padding, payload union starting at offset 8.
     ;;
     ;; Lowers to `mov rax, qword ptr [rdi + 8]' on x86_64 SysV via
     ;; the §122.E `ptr-read-u64' extern (= `nl_ptr_read_u64' in
     ;; `build-tool/src/eval/raw_mem.rs').  Tag-dispatch (= the
     ;; non-Cons-returns-0 branch of `bi_nl_rc_payload_ptr') is the
     ;; caller's responsibility; this kernel performs the raw load
     ;; unconditionally.
     (ptr-read-u64 sexp-ptr 8))
  "AOT source for the Doc 123 §123.D payload-ptr reader kernel.

Single-arg function — AOT's SysV AMD64 prologue spills the
first arg (`sexp-ptr' = raw `*const Sexp' as i64) into the rbp-
relative slot 0.  The body is one composed value form: pass
`sexp-ptr' + offset = 8 (= `SEXP_PAYLOAD_OFFSET', the payload-union
position inside the outer `#[repr(C, u8)]' enum) to the §122.E
`ptr-read-u64' extern.

`ptr-read-u64' lowers to `call nl_ptr_read_u64@PLT' (= the Rust
extern in `build-tool/src/eval/raw_mem.rs' that performs
`*(u64*)(ptr + offset)' and re-casts to i64).  rax on return
holds the inner NlBox* pointer (or garbage bytes for atom variants
that have no boxed payload).

No allocation, no relocation in `.text' beyond the single PLT
fixup for `nl_ptr_read_u64'.  Identical machine-code structure to
§123.C's `nelisp_rc_strong_count.o' modulo the offset literal
(8 vs. 64).")

(provide 'nelisp-cc-rc-payload-ptr)

;;; nelisp-cc-rc-payload-ptr.el ends here
