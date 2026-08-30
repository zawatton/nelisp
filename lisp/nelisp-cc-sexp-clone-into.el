;;; nelisp-cc-sexp-clone-into.el --- AOT nl_sexp_clone_into swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for `nl_sexp_clone_into(src, dst)' which started as the
;; deleted Rust `core::ptr::write(dst, (*src).clone())'.  Symbol/String values
;; now take the Doc 149 shallow-buffer-alias path by default; the legacy deep
;; string copy remains behind the compatibility flag in `nl_sci_dispatch'.
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
;;  13 Bignum       — Doc 190 Phase A, no Rust precursor (added post-Rust-
;;                   removal).  Inline pointer+len, same shape as Str
;;                   (offset 8/16/24 = sign/limb-ptr/limb-count instead of
;;                   cap/ptr/len); IMMUTABLE once constructed, so it is
;;                   cloned the same shallow-buffer-alias way Str/Symbol are
;;                   (a plain 32-byte bit-copy, no allocation, no refcount)
;;                   -- the tracing GC keeps the aliased limb buffer alive
;;                   via reachability, exactly as it already does for a
;;                   Str's char buffer (Doc 149).  No legacy deep-copy flag:
;;                   this is a new type with no back-compat behaviour to
;;                   preserve.
;;
;; Inline String layout (Str/Symbol variants — payload is inline String):
;;   Sexp offset 0:  tag (u8)
;;   Sexp offset 8:  String.cap  (u64)
;;   Sexp offset 16: String.ptr  (u64 — *const u8 char data)
;;   Sexp offset 24: String.len  (u64)
;;
;; Inline Bignum layout (tag 13 — mirrors the Str layout above exactly):
;;   Sexp offset 0:  tag (u8) = 13
;;   Sexp offset 8:  sign        (u64 — 0 non-negative, 1 negative)
;;   Sexp offset 16: limb-ptr    (u64 — *const u32, little-endian limbs)
;;   Sexp offset 24: limb-count  (u64 — canonical: no leading zero limb,
;;                   except a single zero limb for the value 0)
;; (Confirmed by `nl_alloc_str' writer in nelisp-cc-nlstr-direct-ops.el.)
;;
;; AOT `let' is FOLD-ONLY (cannot bind a runtime value). The tag is
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
;; reference emitted by the AOT grammar ops and callers.

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
        (if (or (= tag 6) (= tag 15)) (nelisp_nlstr_clone box)
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

    ;; Dispatch on tag (threaded as a param to avoid AOT `let').
    ;; tag 5 Str / 4 Symbol = String clone.  Doc 149: behind flag 268435648
    ;; (1 = ON, wired at reader boot init; 0 = legacy) the clone ALIASES the
    ;; char buffer (plain 32B slot copy, buffer shared) instead of deep
    ;; copying it.  Sound because tag-4/5 are immutable in the reader
    ;; (bf_aset only mutates tag-8 vectors; in-place string mutation is the
    ;; MutStr tag-6 path, which already rc-aliases) and the tracing GC keeps
    ;; shared buffers alive via reachability (rc is vestigial, Doc 146 §2).
    ;; Why: the deep copy made EVERY read/bind of a large string value cost
    ;; O(len) arena (measured 1.5MB/iter on a 528KB string) with no
    ;; intra-form GC -> the nemacs bridge OOM class.  Escape hatch: flag 0.
    ;; tag < 4 (0..3) = inline atom, plain copy. else (6..12) = boxed.
    ;; tag 13 (Bignum, Doc 190 Phase A): ALWAYS a shallow buffer alias, no
    ;; legacy-flag branch and no refcount bump -- a bignum is immutable from
    ;; the instant it is constructed (there is no in-place bignum mutation
    ;; path the way MutStr has one), so aliasing the limb buffer is sound
    ;; unconditionally, the same reachability argument Doc 149 already
    ;; established for Str/Symbol's flag=1 path above.  Routing it through
    ;; `nl_sci_rc' instead would silently do nothing useful (`nl_sci_bump'
    ;; has no tag-13 arm and falls through to its 0 default) while implying
    ;; a refcounted box that does not exist for this tag; this is an
    ;; explicit tag-13 arm instead, matching Str/Symbol's own explicitness.
    (defun nl_sci_dispatch (src dst tag)
      (if (= tag 14)
          ;; Immutable raw-byte strings always shallow-alias their buffer.
          (nl_sci_copy src dst)
        (if (= tag 5)
          (if (= (ptr-read-u64 268435648 0) 1)
              (nl_sci_copy src dst)
            (nl_alloc_str (ptr-read-u64 src 16) (ptr-read-u64 src 24) dst))
        (if (= tag 4)
            (if (= (ptr-read-u64 268435648 0) 1)
                (nl_sci_copy src dst)
              (nl_alloc_symbol (ptr-read-u64 src 16) (ptr-read-u64 src 24) dst))
          (if (= tag 13) (nl_sci_copy src dst)
          (if (< tag 4) (nl_sci_copy src dst)
            (nl_sci_rc src dst tag)))))))

    ;; Public C-ABI entry: nl_sexp_clone_into(dst, src) = ptr::write(dst,(*src).clone()).
    ;; Doc 135 cutover fix: the param order is (DST SRC) to match the Rust
    ;; signature, the `(sys:extern ...)' decls, and EVERY caller (which all
    ;; pass dst first).  The prior `(src dst)' defun had params reversed vs.
    ;; all callers, so every clone wrote the SOURCE slot and read the DEST --
    ;; corrupting e.g. the bootstrap unbound-marker.  (Latent: the eval
    ;; driver never reached runtime before this cutover, so it was untested.)
    (defun nl_sci_store_imm (word dst)
      ;; Doc 146 §3.0 step 6: materialise an immediate value WORD as a 32-byte
      ;; storage Sexp at DST.  (word&3)==1 Int -> tag2 + (sar word 2); word==3
      ;; Nil(tag0); word==7 T(tag1).
      (if (= (logand word 3) 1)
          (seq (ptr-write-u64 dst 0 2) (ptr-write-u64 dst 8 (sar word 2))
               (ptr-write-u64 dst 16 0) (ptr-write-u64 dst 24 0) dst)
        (if (= word 3)
            (seq (ptr-write-u64 dst 0 0) (ptr-write-u64 dst 8 0)
                 (ptr-write-u64 dst 16 0) (ptr-write-u64 dst 24 0) dst)
          (seq (ptr-write-u64 dst 0 1) (ptr-write-u64 dst 8 0)
               (ptr-write-u64 dst 16 0) (ptr-write-u64 dst 24 0) dst))))
    (defun nl_sexp_clone_into (src dst)
      ;; Doc 146 §3.0 step 6: SRC is an immediate value word (low bit 1) or an
      ;; 8-aligned slot pointer (low bit 0).  Immediates materialise into the
      ;; 32-byte storage Sexp at DST; slot pointers use the tag dispatch.
      ;; Behaviour-preserving until immediates are produced (SRC always a slot).
      (if (= (logand src 1) 0)
          (nl_sci_dispatch src dst (ptr-read-u8 src 0))
        (nl_sci_store_imm src dst))))
  "AOT source for nl_sexp_clone_into.

Re-provides the deleted Rust clone entry point from
`build-tool/src/eval/sexp.rs', with Doc 149's shallow-buffer-alias fast path
for immutable Symbol/String values enabled by default.  Sexp is a 32-byte
#[repr(C,u8)] slot; the tag byte at offset 0 drives a 3-way dispatch:

  tag < 4  (Nil/T/Int/Float): plain 32-byte (4×u64) bit-copy.
  tag = 4  (Symbol): shallow string-buffer alias by default; legacy deep
             copy via nl_alloc_symbol when the compatibility flag is off.
  tag = 5  (Str):    shallow string-buffer alias by default; legacy deep
             copy via nl_alloc_str when the compatibility flag is off.
  tag 6..12 (MutStr/Cons/Vector/CharTable/BoolVector/Cell/Record):
             refcount bump via the per-type nelisp_nl*_clone helper,
             then plain 32-byte bit-copy.
  tag = 13 (Bignum, Doc 190 Phase A): always a shallow limb-buffer alias
             (plain 32-byte bit-copy, unconditionally -- no legacy flag,
             no refcount; sound because a bignum is immutable from
             construction, matching the reachability argument tag 4/5's
             flag=1 path already relies on).

AOT `let' is compile-time only; the tag is threaded as an
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
