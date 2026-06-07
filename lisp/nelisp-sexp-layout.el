;;; nelisp-sexp-layout.el --- Doc 100 §100.B Sexp byte-layout constants  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 100 v2 §100.B frozen byte-layout constants for the Rust `Sexp'
;; enum.  The AOT compiler reads these when emitting direct-access
;; instructions against Sexp values held in caller-provided register
;; pointers.
;;
;; The canonical spec lives in `docs/arch/sexp-abi.md'.  Rust-side
;; assertions in `build-tool/src/eval/sexp_abi_assert.rs' fail
;; compilation on drift; `make sexp-abi-check' diffs the two sides at
;; CI time.  AOT is the ABI's third consumer — keep these
;; constants in lockstep with both the doc and the assertion module.

;;; Code:

;; ---------------------------------------------------------------------------
;; Variant tag bytes (= the `#[repr(C, u8)]' discriminant at offset 0).
;; ---------------------------------------------------------------------------

(defconst nelisp-sexp--tag-nil           0 "Sexp::Nil tag byte.")
(defconst nelisp-sexp--tag-t             1 "Sexp::T tag byte.")
(defconst nelisp-sexp--tag-int           2 "Sexp::Int(i64) tag byte.")
(defconst nelisp-sexp--tag-float         3 "Sexp::Float(f64) tag byte.")
(defconst nelisp-sexp--tag-symbol        4 "Sexp::Symbol(String) tag byte.")
(defconst nelisp-sexp--tag-str           5 "Sexp::Str(String) tag byte.")
(defconst nelisp-sexp--tag-mut-str       6 "Sexp::MutStr(NlStrRef) tag byte.")
(defconst nelisp-sexp--tag-cons          7 "Sexp::Cons(NlConsBoxRef) tag byte.")
(defconst nelisp-sexp--tag-vector        8 "Sexp::Vector(NlVectorRef) tag byte.")
(defconst nelisp-sexp--tag-char-table    9 "Sexp::CharTable(...) tag byte.")
(defconst nelisp-sexp--tag-bool-vector  10 "Sexp::BoolVector(...) tag byte.")
(defconst nelisp-sexp--tag-cell         11 "Sexp::Cell(...) tag byte.")
(defconst nelisp-sexp--tag-record       12 "Sexp::Record(...) tag byte.")

;; ---------------------------------------------------------------------------
;; Field offsets within a Sexp slot.
;; ---------------------------------------------------------------------------

(defconst nelisp-sexp--offset-tag         0
  "Byte offset of the variant tag inside a Sexp slot (= always 0).")

(defconst nelisp-sexp--offset-payload     8
  "Byte offset of the variant payload inside a Sexp slot.
Mirrors `SEXP_PAYLOAD_OFFSET' on the Rust side.  AOT emits
`mov rax, [rdi + 8]' / `mov [rdi + 8], rsi' using this offset.")

(defconst nelisp-sexp--offset-int-payload 8
  "Byte offset of the i64 payload of Sexp::Int(n).
Same as `nelisp-sexp--offset-payload' but kept as a separate
constant so future variants whose payload starts at a different
offset (= e.g. tagged unions inside a Cons payload) can be
expressed without overloading the generic offset.")

(defconst nelisp-sexp--slot-size         32
  "Total bytes occupied by one Sexp value, including tag and padding.
Mirrors `std::mem::size_of::<Sexp>()' on the Rust side.
NOTE (Doc 147 Phase 2): this is the CHILD-BOX / type_tag size and is
UNCHANGED — child Sexp boxes stay 32B.  Container SLOTS shrank to an
8-byte tagged WORD; use `nelisp-sexp--container-word-size' for the
per-slot stride of Vector / Record data buffers.")

(defconst nelisp-sexp--container-word-size 8
  "Bytes per CONTAINER SLOT (Vector / Record data-buffer element).
Doc 147 Phase 2: a container slot is now an 8-byte tagged WORD (low
bit 1 = immediate Int/Nil/T; low bit 0 = 8-aligned ptr to a 32B child
Sexp box), NOT a 32-byte inline Sexp.  The per-slot stride is 8;
`nelisp-sexp--slot-size' (= 32) is reserved for the still-32B child
boxes + the inline NlRecord type_tag.  Do NOT conflate the two.")

;; ---------------------------------------------------------------------------
;; Doc 101 §101.A — NlConsBox struct field offsets (= car / cdr / refcount
;; inside the heap-allocated cons box that Sexp::Cons points to).
;; The box is `#[repr(C)]` per `build-tool/src/eval/nlconsbox.rs:56-67'.
;; ---------------------------------------------------------------------------

(defconst nelisp-nlconsbox--offset-car   0
  "Byte offset of the `car' Sexp inside an NlConsBox.")

(defconst nelisp-nlconsbox--offset-cdr   32
  "Byte offset of the `cdr' Sexp inside an NlConsBox.
Equals one full Sexp slot (= 32) past the `car'.")

(defconst nelisp-nlconsbox--offset-refcount 64
  "Byte offset of the `refcount' AtomicUsize inside an NlConsBox.
Two full Sexp slots (= 64) past the start.")

(defconst nelisp-nlconsbox--size         72
  "Total size of an NlConsBox struct in bytes.
Mirrors `std::mem::size_of::<NlConsBox>()' on the Rust side.")

;; ---------------------------------------------------------------------------
;; Doc 101 §101.A — Rust `String' header field offsets within a Sexp::Symbol
;; or Sexp::Str slot.  The `String' header is laid out as
;; `(ptr, capacity, length)' at offsets `(8, 16, 24)' of the Sexp slot
;; (= 0, 8, 16 within the 24-byte `String' header itself).
;;
;; NOTE: Rust's `String' layout is stdlib-internal and not formally
;; frozen across compiler versions.  The repo pins the toolchain via
;; `rust-toolchain.toml'; `sexp_abi_assert.rs' adds `const_assert!' for
;; each offset so drift fails compilation.  See docs/arch/sexp-abi.md §7.
;; ---------------------------------------------------------------------------

(defconst nelisp-string--offset-capacity 8
  "Byte offset (within a Sexp::Symbol / Sexp::Str slot) of the
String's capacity field.  Equals `nelisp-sexp--offset-payload' + 0
because Rust's `String' header currently stores capacity first on the
repo toolchain.")

(defconst nelisp-string--offset-ptr      16
  "Byte offset (within a Sexp::Symbol / Sexp::Str slot) of the
String's data pointer.  Equals payload + 8 on the repo toolchain.")

(defconst nelisp-string--offset-length   24
  "Byte offset (within a Sexp::Symbol / Sexp::Str slot) of the
String's length field (= byte count, NOT char count).  Equals
payload + 16.")

(defconst nelisp-string--header-size     24
  "Total size of a Rust `String' header in bytes (= ptr + cap + len).")

;; ---------------------------------------------------------------------------
;; Doc 111 §111.A — boxed-collection layout constants for NlRecord /
;; NlVector / NlCell.  `NlRecord' and `NlVector' each carry an inline
;; `Vec<Sexp>' header whose inner fields are addressed by fixed +0/+8/+16
;; offsets from the header start.  See docs/arch/sexp-abi.md §10-§12.
;; ---------------------------------------------------------------------------

(defconst nelisp-nlrecord--offset-type-tag       0
  "Byte offset of the inline `type_tag' Sexp inside an NlRecord.")

(defconst nelisp-nlrecord--offset-slots-vec      32
  "Byte offset of the inline `slots' Vec<Sexp> header inside an NlRecord.")

(defconst nelisp-nlrecord--offset-slots-ptr      32
  "Byte offset of `slots.ptr' inside an NlRecord.
Alias for `nelisp-nlrecord--offset-slots-vec' kept for emit clarity.")

(defconst nelisp-nlrecord--offset-slots-capacity 40
  "Byte offset of `slots.capacity' inside an NlRecord.")

(defconst nelisp-nlrecord--offset-slots-length   48
  "Byte offset of `slots.length' inside an NlRecord.")

(defconst nelisp-nlrecord--offset-refcount       56
  "Byte offset of the `refcount' AtomicUsize inside an NlRecord.")

(defconst nelisp-nlrecord--size                  64
  "Total size of an NlRecord struct in bytes.")

(defconst nelisp-nlvector--offset-value-vec      0
  "Byte offset of the inline `value' Vec<Sexp> header inside an NlVector.")

(defconst nelisp-nlvector--offset-value-ptr      0
  "Byte offset of `value.ptr' inside an NlVector.
Alias for `nelisp-nlvector--offset-value-vec' kept for emit clarity.")

(defconst nelisp-nlvector--offset-value-capacity 8
  "Byte offset of `value.capacity' inside an NlVector.")

(defconst nelisp-nlvector--offset-value-length   16
  "Byte offset of `value.length' inside an NlVector.")

(defconst nelisp-nlvector--offset-refcount       24
  "Byte offset of the `refcount' AtomicUsize inside an NlVector.")

(defconst nelisp-nlvector--size                  32
  "Total size of an NlVector struct in bytes.")

(defconst nelisp-nlcell--offset-value            0
  "Byte offset of the inline `value' WORD @ box+0, rc @ box+8, size 16.
Doc 147 Phase 1: the NlCell `value' is now an 8-byte tagged WORD (low
bit 1 = immediate Int/Nil/T; low bit 0 = 8-aligned ptr to a 32B child
Sexp box), NOT a 32-byte inline Sexp slot.")

(defconst nelisp-nlcell--offset-refcount         8
  "Byte offset of the `refcount' AtomicUsize inside an NlCell.
Doc 147 Phase 1: immediately after the 8-byte value WORD (was 32).")

(defconst nelisp-nlcell--size                    16
  "Total size of an NlCell struct in bytes.
Doc 147 Phase 1: 8-byte value WORD + 8-byte refcount (was 40).")

;; ---------------------------------------------------------------------------
;; Self-export — list of (NAME . VALUE) pairs every consumer that
;; needs to diff against the Rust assertions can iterate over.
;; ---------------------------------------------------------------------------

(defconst nelisp-sexp--abi-export
  `((tag-nil          . ,nelisp-sexp--tag-nil)
    (tag-t            . ,nelisp-sexp--tag-t)
    (tag-int          . ,nelisp-sexp--tag-int)
    (tag-float        . ,nelisp-sexp--tag-float)
    (tag-symbol       . ,nelisp-sexp--tag-symbol)
    (tag-str          . ,nelisp-sexp--tag-str)
    (tag-mut-str      . ,nelisp-sexp--tag-mut-str)
    (tag-cons         . ,nelisp-sexp--tag-cons)
    (tag-vector       . ,nelisp-sexp--tag-vector)
    (tag-char-table   . ,nelisp-sexp--tag-char-table)
    (tag-bool-vector  . ,nelisp-sexp--tag-bool-vector)
    (tag-cell         . ,nelisp-sexp--tag-cell)
    (tag-record       . ,nelisp-sexp--tag-record)
    (offset-tag       . ,nelisp-sexp--offset-tag)
    (offset-payload   . ,nelisp-sexp--offset-payload)
    (slot-size        . ,nelisp-sexp--slot-size)
    (container-word-size . ,nelisp-sexp--container-word-size)
    ;; Doc 101 §101.A additions
    (nlconsbox-offset-car      . ,nelisp-nlconsbox--offset-car)
    (nlconsbox-offset-cdr      . ,nelisp-nlconsbox--offset-cdr)
    (nlconsbox-offset-refcount . ,nelisp-nlconsbox--offset-refcount)
    (nlconsbox-size            . ,nelisp-nlconsbox--size)
    (string-offset-capacity    . ,nelisp-string--offset-capacity)
    (string-offset-ptr         . ,nelisp-string--offset-ptr)
    (string-offset-length      . ,nelisp-string--offset-length)
    (string-header-size        . ,nelisp-string--header-size)
    ;; Doc 111 §111.A additions
    (nlrecord-offset-type-tag       . ,nelisp-nlrecord--offset-type-tag)
    (nlrecord-offset-slots-vec      . ,nelisp-nlrecord--offset-slots-vec)
    (nlrecord-offset-slots-ptr      . ,nelisp-nlrecord--offset-slots-ptr)
    (nlrecord-offset-slots-capacity . ,nelisp-nlrecord--offset-slots-capacity)
    (nlrecord-offset-slots-length   . ,nelisp-nlrecord--offset-slots-length)
    (nlrecord-offset-refcount       . ,nelisp-nlrecord--offset-refcount)
    (nlrecord-size                  . ,nelisp-nlrecord--size)
    (nlvector-offset-value-vec      . ,nelisp-nlvector--offset-value-vec)
    (nlvector-offset-value-ptr      . ,nelisp-nlvector--offset-value-ptr)
    (nlvector-offset-value-capacity . ,nelisp-nlvector--offset-value-capacity)
    (nlvector-offset-value-length   . ,nelisp-nlvector--offset-value-length)
    (nlvector-offset-refcount       . ,nelisp-nlvector--offset-refcount)
    (nlvector-size                  . ,nelisp-nlvector--size)
    (nlcell-offset-value            . ,nelisp-nlcell--offset-value)
    (nlcell-offset-refcount         . ,nelisp-nlcell--offset-refcount)
    (nlcell-size                    . ,nelisp-nlcell--size))
  "Layout constants flattened to (NAME . VALUE) for cross-side diffing.
`make sexp-abi-check' runs the Rust driver, prints the same set in
the same order, and asserts equality.  Order matters for the diff
output to be readable.")

(provide 'nelisp-sexp-layout)

;;; nelisp-sexp-layout.el ends here
