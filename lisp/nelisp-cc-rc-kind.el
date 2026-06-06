;;; nelisp-cc-rc-kind.el --- Doc 123 §123.C kind-tag reader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 123 §123.C — pure-elisp `nelisp_rc_kind' kernel, the tag-byte
;; reader twin of §123.A `nelisp_rc_inc'.  Replaces the body of
;; `bi_nl_rc_kind' (= `args[0].tag() as i64' which reads the
;; discriminant byte of the outer `Sexp' enum) at
;; `build-tool/src/eval/rc_primitives.rs:204-207' with a single
;; §122.E `ptr-read-u8' grammar op.
;;
;; Function contract:
;;   sexp-ptr: raw `*const Sexp' coerced to i64 (= NOT the inner box
;;             pointer; the *outer* Sexp value's address — the
;;             discriminant byte lives at offset 0 of the enum itself,
;;             not the heap-allocated `NlConsBox' payload).  Caller
;;             responsibility: the dispatch site at §123.F passes the
;;             `*const Sexp' from the eval frame's arg slot, not the
;;             unwrapped `NlConsBoxRef::as_ptr' result.
;;   returns: the discriminant byte as i64 (= zero-extended u8;
;;            matches one of `SEXP_TAG_NIL = 0' ... `SEXP_TAG_RECORD
;;            = 12' constants defined in `build-tool/src/eval/
;;            sexp.rs:217-229').
;;
;; The body is one composed value form:
;;
;;   (ptr-read-u8 sexp-ptr 0)
;;
;; The constant `0' is the byte offset of the `#[repr(C, u8)]'
;; discriminant inside a `Sexp' enum value.  Layout-pinned by the
;; `#[repr(C, u8)]' attribute on the `Sexp' declaration at
;; `build-tool/src/eval/sexp.rs:57' and asserted stable by the
;; `variant_tags_are_stable' unit test in the same file.  The Rust
;; `variant_tag(s: &Sexp) -> u8' helper at `sexp.rs:234-240' does the
;; same `*(s as *const Sexp as *const u8)' read; this elisp kernel
;; mirrors that read byte-for-byte.
;;
;; Why offset 0 and not the NlBox header layout?  rc_primitives.rs's
;; `bi_nl_rc_kind' body is `args[0].tag() as i64' — i.e. it reads
;; the Sexp's own discriminant, not a field of the heap box behind
;; the variant's payload pointer.  This works on *every* Sexp variant
;; (Nil / T / Int / Float / Symbol / Str / MutStr / Cons / Vector /
;; CharTable / BoolVector / Cell / Record) because the discriminant
;; is always at offset 0 of the enum regardless of which variant
;; payload follows.  In contrast, §123.A's REFCOUNT_OFFSET = 64
;; applies only to `NlConsBox' (= the `Sexp::Cons' payload) because
;; that's where the layout pin lives.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nelisp_rc_kind.o' and archives it into the static lib the crate
;; links against.  The Rust side declares the `extern "C"' signature
;; in `build-tool/src/lib.rs::elisp_cc_spike' and exposes a safe
;; wrapper `elisp_cc_spike::rc_kind(sexp_ptr)' for the integration
;; test in `tests/elisp_cc_rc_kind_probe.rs'.
;;
;; Stage scope: §123.C ships *only* the elisp kernel + probe.  The
;; Rust-side `bi_nl_rc_kind' primitive in `rc_primitives.rs:204-207'
;; continues to call `args[0].tag()' directly; §123.F (= the sweep
;; stage) will swap that dispatch to call `nelisp_rc_kind' via
;; extern-C.  Existing `rc_primitives.rs' unit tests remain green
;; throughout this stage.

;;; Code:

(defconst nelisp-cc-rc-kind--source
  '(defun nelisp_rc_kind (sexp-ptr)
     ;; Discriminant byte at offset 0 of the outer `Sexp' enum value
     ;; (= `#[repr(C, u8)]' guarantees the tag is a `u8' at offset 0
     ;; regardless of variant; matches one of `SEXP_TAG_*' constants
     ;; per `build-tool/src/eval/sexp.rs:217-229').
     ;;
     ;; Lowers to `movzx rax, byte ptr [rdi]' on x86_64 SysV via the
     ;; §122.E `ptr-read-u8' extern (= `nl_ptr_read_u8' performs the
     ;; zero-extended u8 load and returns the byte as i64).  Same
     ;; zero-extension contract as `variant_tag' in `sexp.rs:234' —
     ;; 0xFF returns 255, not -1.
     (ptr-read-u8 sexp-ptr 0))
  "AOT source for the Doc 123 §123.C kind-byte reader kernel.

Single-arg function — AOT's SysV AMD64 prologue spills the
first arg (`sexp-ptr' = raw `*const Sexp' as i64) into the rbp-
relative slot 0.  The body is one composed value form: pass
`sexp-ptr' + offset = 0 (= the `#[repr(C, u8)]' discriminant
position) to the §122.E `ptr-read-u8' extern.

`ptr-read-u8' lowers to `call nl_ptr_read_u8@PLT' (= the Rust
extern in `build-tool/src/eval/raw_mem.rs' that performs
`*(u8*)(ptr + offset)' and zero-extends to i64).  rax on return
holds the discriminant byte value (0-12 per `SEXP_TAG_*' constants
in `sexp.rs:217-229'), which is also this function's return.

Works on *every* `Sexp' variant — the discriminant byte is at
offset 0 of the enum regardless of which payload follows, so this
kernel does not branch on variant tag (the *caller* uses the
returned tag to dispatch).

No allocation, no relocation in `.text' beyond the single PLT
fixup for `nl_ptr_read_u8'.  Smallest substrate-stage `.o' shape
(= ~30 bytes of machine code, identical structure to §123.C's
`nelisp_rc_strong_count.o' modulo the differing PLT target).")

(provide 'nelisp-cc-rc-kind)

;;; nelisp-cc-rc-kind.el ends here
