;;; nelisp-cc-sexp-clone-into.el --- Phase 47 nl_sexp_clone_into swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for `nl_sexp_clone_into(src, dst)' which
;; reproduced the deleted Rust `core::ptr::write(dst, (*src).clone())'.
;;
;; `Sexp' is a 32-byte `#[repr(C, u8)]' slot:
;;   tag (u8) @ 0, payload @ 8.
;;
;; Tag constants (from `build-tool/src/eval/sexp.rs'):
;;   0 Nil          — inline atom, plain 32-byte copy.
;;   1 T            — inline atom, plain 32-byte copy.
;;   2 Int          — inline atom, plain 32-byte copy.
;;   3 Float        — inline atom, plain 32-byte copy.
;;   4 Symbol(String) — deep copy: read bytes-ptr@16 + len@24, nl_alloc_symbol.
;;   5 Str(String)  — deep copy: read bytes-ptr@16 + len@24, nl_alloc_str.
;;   6 MutStr(NlStrRef) — boxed, refcount bump via nelisp_nlstr_clone.
;;   7 Cons(NlConsBoxRef) — boxed, refcount bump via nelisp_nlconsbox_clone.
;;   8 Vector       — boxed, refcount bump via nelisp_nlvector_clone.
;;   9 CharTable    — boxed, refcount bump via nelisp_nlchartable_clone.
;;  10 BoolVector   — boxed, refcount bump via nelisp_nlboolvector_clone.
;;  11 Cell         — boxed, refcount bump via nelisp_nlcell_clone.
;;  12 Record       — boxed, refcount bump via nelisp_nlrecord_clone.
;;
;; Inline String layout (Str/Symbol variants — payload is inline String):
;;   Sexp offset 0:  tag (u8)
;;   Sexp offset 8:  String.cap  (u64)
;;   Sexp offset 16: String.ptr  (u64 — *const u8 char data)
;;   Sexp offset 24: String.len  (u64)
;; (Confirmed by `nl_alloc_str' writer in nelisp-cc-nlstr-direct-ops.el.)
;;
;; Phase 47 `let' is FOLD-ONLY (cannot bind a runtime value). The tag is
;; threaded as a function parameter via a helper chain to avoid `let'.
;;
;; Helper structure:
;;   nl_sci_prog2     — effect-sequencer (evaluate both, return 2nd).
;;   nl_sci_copy      — raw 32-byte (4×u64) slot copy, src→dst, return dst.
;;   nl_sci_bump      — refcount-bump dispatch for boxed variants (tags 6..12).
;;   nl_sci_rc        — bump rc then bit-copy the slot.
;;   nl_sci_dispatch  — 3-way dispatch: String deep-copy / inline / boxed.
;;   nl_sexp_clone_into — public C-ABI entry.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs' compiles the source into
;; `nl_sexp_clone_into.o' and archives it into `libnelisp_elisp_spike.a'
;; which the linker resolves against the `nl_sexp_clone_into' PLT
;; reference emitted by the Phase 47 grammar ops and callers.

;;; Code:

(defconst nelisp-cc-sexp-clone-into--source
  '(seq
    ;; effect-sequencer: evaluate both args (so a void effect can precede
    ;; a value), return the 2nd. Same idiom as nelisp_nlstr_clone_prog2.
    (defun nl_sci_prog2 (_eff val) val)

    ;; Copy the 32-byte Sexp slot (4 u64 words) src->dst, return dst.
    ;; ptr-write-u64 returns 1 (sentinel) so the `and' never short-circuits.
    (defun nl_sci_copy (src dst)
      (nl_sci_prog2
       (and (ptr-write-u64 dst 0  (ptr-read-u64 src 0))
            (ptr-write-u64 dst 8  (ptr-read-u64 src 8))
            (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
            (ptr-write-u64 dst 24 (ptr-read-u64 src 24)))
       dst))

    ;; Refcount-bump the box for a boxed variant (tags 6..12). 7-way tail
    ;; dispatch to the per-type clone (each bumps rc + returns box-ptr).
    ;; tag 6 MutStr holds NlStrRef -> nelisp_nlstr_clone (NOT a deep copy).
    (defun nl_sci_bump (tag box)
      (if (= tag 7)  (nelisp_nlconsbox_clone box)
        (if (= tag 6)  (nelisp_nlstr_clone box)
          (if (= tag 8)  (nelisp_nlvector_clone box)
            (if (= tag 9)  (nelisp_nlchartable_clone box)
              (if (= tag 10) (nelisp_nlboolvector_clone box)
                (if (= tag 11) (nelisp_nlcell_clone box)
                  (if (= tag 12) (nelisp_nlrecord_clone box)
                    0))))))))

    ;; Boxed path: bump rc, then bit-copy the slot.
    (defun nl_sci_rc (src dst tag)
      (nl_sci_prog2
       (nl_sci_bump tag (ptr-read-u64 src 8))
       (nl_sci_copy src dst)))

    ;; Dispatch on tag (threaded as a param to avoid Phase-47 `let').
    ;; tag 5 Str / 4 Symbol = deep String copy via nl_alloc_str/symbol
    ;; (read buffer ptr@16 + len@24 from src, alloc+copy into dst).
    ;; tag < 4 (0..3) = inline atom, plain copy. else (6..12) = boxed.
    (defun nl_sci_dispatch (src dst tag)
      (if (= tag 5) (nl_alloc_str    (ptr-read-u64 src 16) (ptr-read-u64 src 24) dst)
        (if (= tag 4) (nl_alloc_symbol (ptr-read-u64 src 16) (ptr-read-u64 src 24) dst)
          (if (< tag 4) (nl_sci_copy src dst)
            (nl_sci_rc src dst tag)))))

    ;; Public C-ABI entry: nl_sexp_clone_into(dst, src) = ptr::write(dst,(*src).clone()).
    ;; Doc 135 cutover fix: the param order is (DST SRC) to match the Rust
    ;; signature, the `(sys:extern ...)' decls, and EVERY caller (which all
    ;; pass dst first).  The prior `(src dst)' defun had params reversed vs.
    ;; all callers, so every clone wrote the SOURCE slot and read the DEST --
    ;; corrupting e.g. the bootstrap unbound-marker.  (Latent: the eval
    ;; driver never reached runtime before this cutover, so it was untested.)
    (defun nl_sexp_clone_into (src dst)
      (nl_sci_dispatch src dst (ptr-read-u8 src 0))))
  "Phase 47 source for nl_sexp_clone_into = ptr::write(dst,(*src).clone()).

Re-provides the deleted Rust `core::ptr::write(dst, (*src).clone())'
from `build-tool/src/eval/sexp.rs'.  Sexp is a 32-byte #[repr(C,u8)]
slot; the tag byte at offset 0 drives a 3-way dispatch:

  tag < 4  (Nil/T/Int/Float): plain 32-byte (4×u64) bit-copy.
  tag = 4  (Symbol): deep String copy via nl_alloc_symbol.
  tag = 5  (Str):    deep String copy via nl_alloc_str.
  tag 6..12 (MutStr/Cons/Vector/CharTable/BoolVector/Cell/Record):
             refcount bump via the per-type nelisp_nl*_clone helper,
             then plain 32-byte bit-copy.

Phase 47 `let' is compile-time only; the tag is threaded as an
extra function parameter (nl_sci_dispatch/nl_sci_rc/nl_sci_bump)
to avoid any runtime binding.

Six-entry `(seq DEFUN ...)' manifest:
- `nl_sci_prog2'      — effect-sequencer (evaluate both, return 2nd).
- `nl_sci_copy'       — 4×u64 raw slot copy, returns dst.
- `nl_sci_bump'       — 7-way rc-bump dispatch for boxed variants.
- `nl_sci_rc'         — bump rc then nl_sci_copy.
- `nl_sci_dispatch'   — 3-way tag dispatch (String / inline / boxed).
- `nl_sexp_clone_into' — public C-ABI entry point.")

(provide 'nelisp-cc-sexp-clone-into)

;;; nelisp-cc-sexp-clone-into.el ends here
