;;; nelisp-sexp-layout.el --- Doc 100 §100.B Sexp byte-layout constants  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 100 v2 §100.B frozen byte-layout constants for the Rust `Sexp'
;; enum.  The Phase 47 compiler reads these when emitting direct-access
;; instructions against Sexp values held in caller-provided register
;; pointers.
;;
;; The canonical spec lives in `docs/arch/sexp-abi.md'.  Rust-side
;; assertions in `build-tool/src/eval/sexp_abi_assert.rs' fail
;; compilation on drift; `make sexp-abi-check' diffs the two sides at
;; CI time.  Phase 47 is the ABI's third consumer — keep these
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
Mirrors `SEXP_PAYLOAD_OFFSET' on the Rust side.  Phase 47 emits
`mov rax, [rdi + 8]' / `mov [rdi + 8], rsi' using this offset.")

(defconst nelisp-sexp--offset-int-payload 8
  "Byte offset of the i64 payload of Sexp::Int(n).
Same as `nelisp-sexp--offset-payload' but kept as a separate
constant so future variants whose payload starts at a different
offset (= e.g. tagged unions inside a Cons payload) can be
expressed without overloading the generic offset.")

(defconst nelisp-sexp--slot-size         32
  "Total bytes occupied by one Sexp value, including tag and padding.
Mirrors `std::mem::size_of::<Sexp>()' on the Rust side.")

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

(defconst nelisp-string--offset-ptr      8
  "Byte offset (within a Sexp::Symbol / Sexp::Str slot) of the
String's data pointer.  Equals `nelisp-sexp--offset-payload' + 0
since `ptr' is the first field of the String header.")

(defconst nelisp-string--offset-capacity 16
  "Byte offset (within a Sexp::Symbol / Sexp::Str slot) of the
String's capacity field.  Equals payload + 8.")

(defconst nelisp-string--offset-length   24
  "Byte offset (within a Sexp::Symbol / Sexp::Str slot) of the
String's length field (= byte count, NOT char count).  Equals
payload + 16.")

(defconst nelisp-string--header-size     24
  "Total size of a Rust `String' header in bytes (= ptr + cap + len).")

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
    ;; Doc 101 §101.A additions
    (nlconsbox-offset-car      . ,nelisp-nlconsbox--offset-car)
    (nlconsbox-offset-cdr      . ,nelisp-nlconsbox--offset-cdr)
    (nlconsbox-offset-refcount . ,nelisp-nlconsbox--offset-refcount)
    (nlconsbox-size            . ,nelisp-nlconsbox--size)
    (string-offset-ptr         . ,nelisp-string--offset-ptr)
    (string-offset-capacity    . ,nelisp-string--offset-capacity)
    (string-offset-length      . ,nelisp-string--offset-length)
    (string-header-size        . ,nelisp-string--header-size))
  "Layout constants flattened to (NAME . VALUE) for cross-side diffing.
`make sexp-abi-check' runs the Rust driver, prints the same set in
the same order, and asserts equality.  Order matters for the diff
output to be readable.")

(provide 'nelisp-sexp-layout)

;;; nelisp-sexp-layout.el ends here
