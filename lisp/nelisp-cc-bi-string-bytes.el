;;; nelisp-cc-bi-string-bytes.el --- Doc 117 §117.A.2 bi_string_bytes swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.A.2 — moves the byte-length computation of the
;; `(string-bytes STR)' builtin into a Phase 47 elisp object linked
;; into the `nelisp' binary.  The Rust side keeps only the arity check,
;; the `Sexp::Str' / `Sexp::MutStr' tag dispatch, and the caller-owned
;; out-slot setup; the actual `len() -> Sexp::Int' step lives only in
;; the source below.
;;
;; Phase 47 ops consumed:
;;   §101.C  `str-len'        — read `String::len' (= byte count) at
;;                              offset 24 from a `Sexp::Str' /
;;                              `Sexp::Symbol' slot.
;;   §100    `sexp-int-make'  — write `Sexp::Int(n)' into a caller-
;;                              owned 32-byte slot.
;;
;; Function contract:
;;   arg0:        *const Sexp pointing at a `Sexp::Str' (or
;;                `Sexp::Symbol' — `str-len' reads the shared 24-byte
;;                `String::len' field for both).  `MutStr' is unwrapped
;;                in the Rust shim before this body runs, so the
;;                pointer always lands on a plain `Sexp::Str' view of
;;                the underlying bytes.
;;   result-slot: *mut Sexp pointing at a 32-byte caller-owned slot
;;                pre-initialised to `Sexp::Nil'.
;;   returns:     result-slot (= same pointer in rax, parity with the
;;                other Doc 100 / §101.B / §111.B `.o' helpers).
;;
;; Semantics match the pre-swap Rust body verbatim
;; (`build-tool/src/eval/builtins.rs::bi_string_bytes'): the function
;; returns `Sexp::Int(byte_count)' for the UTF-8 byte length of the
;; argument, matching all in-tree callers
;; (`lisp/nelisp-stdlib-os.el', `test/nelisp-runtime-module-test.el',
;; `packages/nelisp-secure-hash/').
;;
;; Per Doc 117 §4.3: pure migration — no new behaviour.  The Rust shim
;; preserves the existing arity check + `WrongType' error contract for
;; non-string inputs; only the per-string byte-count computation moves
;; into elisp.

;;; Code:

(defconst nelisp-cc-bi-string-bytes--source
  '(defun nelisp_bi_string_bytes (arg0 result-slot)
     (sexp-int-make result-slot (str-len arg0)))
  "Phase 47 source for the Doc 117 §117.A.2 `(string-bytes STR)' swap.

Two-argument function — Phase 47's SysV AMD64 prologue spills the
first arg (`arg0' = `*const Sexp' to a `Sexp::Str' / `Sexp::Symbol')
into the rbp-relative slot 0 and the second arg (`result-slot' =
`*mut Sexp') into slot 1.  The body is one composed value form:
read the `String::len' field of `*arg0' via `str-len' (= `mov rax,
qword ptr [rdi + 24]') and write a fresh `Sexp::Int' with that
i64 payload into `*result-slot' via `sexp-int-make'.

`sexp-int-make' returns the slot pointer in rax, which is also the
function's return value.  The Rust caller already holds the same
pointer (= the `&mut' into a stack-local `Sexp::Nil'), so the
returned value is discarded.

No allocation, no Rust helpers — every memory access is a single
`disp8' load / store emitted by Phase 47's `str-len' + `sexp-int-
make' grammar forms.  The emitted `.text' contains no relocations.

Cross-checks against the pre-swap Rust body (deleted from
`build-tool/src/eval/builtins.rs::bi_string_bytes'):

  * Both arms (`Sexp::Str' + `Sexp::MutStr') feed the same i64 byte
    length into `Sexp::Int' — handled by the Rust shim unifying the
    pointer it passes here, since the elisp body needs a single
    `*const Sexp' contract.
  * Non-string inputs signal `WrongType' with `expected = \"string\"'
    — kept in the Rust shim (= type discrimination cannot be expressed
    in Phase 47's current grammar without a `sexp-tag' branch, and
    Doc 117 §4.3 forbids any behaviour change in this swap).")

(provide 'nelisp-cc-bi-string-bytes)

;;; nelisp-cc-bi-string-bytes.el ends here
