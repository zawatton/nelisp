;;; nelisp-cc-sexp-fmt.el --- AOT Sexp formatter (write_sexp migration) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT migration of the `write_sexp' / `fmt_sexp' formatter chain
;; from `build-tool/src/eval/sexp.rs' (lines 181-351 at HEAD b1a22d9a).
;;
;; Migrated functions:
;;   `write_quoted_string'  → `nl_fmt_sexp_write_quoted_str'
;;   `write_sexp'           → `nl_fmt_sexp_dispatch'
;;   `write_reader_macro'   → `nl_fmt_sexp_try_reader_macro'
;;   `list_tag_and_arg'     (inlined into `nl_fmt_sexp_try_reader_macro')
;;   `write_list_body'      → `nl_fmt_sexp_write_list_body'
;;   top-level `fmt_sexp'   → `nelisp_fmt_sexp' (public AOT entry)
;;
;; Additional helpers (not in original but needed for completeness):
;;   BoolVector packing: `nl_fmt_sexp_build_bool_chunk' / `write_bool_chunks'
;;   CharTable: `nl_fmt_sexp_write_char_table'
;;   Vector: `nl_fmt_sexp_write_vec_items'
;;   Record: `nl_fmt_sexp_write_record' / `write_record_slots'
;;
;; Public entry point:
;;   `nelisp_fmt_sexp (s result-slot)' → i64 (always 0)
;;   Allocates Sexp::MutStr in result-slot, formats s, finalizes to Str.
;;   Rust wrapper: `build-tool/src/lib.rs' cc_wrap! entry.
;;
;; AOT grammar ops consumed:
;;   Sexp ops: sexp-tag, sexp-int-unwrap, sexp-payload-ptr
;;   String ops: str-len, str-bytes-ptr, mut-str-make-empty,
;;               mut-str-push-byte, mut-str-len, mut-str-finalize
;;   Record ops: record-slot-count, record-slot-ref-ptr
;;   Vector ops: vector-len, vector-ref-ptr
;;   Memory ops: ptr-read-u64, ptr-read-u8
;;   Arith: +, -, *, logior, logand, shl, sar
;;   Control: if, cond, and, or, =, <, >, <=, >=
;;   extern-call: nl_i64_append_to_mut_str, nl_f64_bits_append_to_mut_str
;;
;; Extern calls (Rust helpers in nlstr.rs):
;;   nl_i64_append_to_mut_str(n: i64, buf: *mut Sexp) -> i64
;;   nl_f64_bits_append_to_mut_str(bits: i64, buf: *mut Sexp) -> i64
;;
;; ABI tag constants (frozen by sexp_abi_assert.rs):
;;   SEXP_TAG_NIL=0, T=1, INT=2, FLOAT=3, SYMBOL=4, STR=5, MUT_STR=6,
;;   CONS=7, VECTOR=8, CHAR_TABLE=9, BOOL_VECTOR=10, CELL=11, RECORD=12
;;
;; Struct layout constants used:
;;   Sexp: payload-offset=8, slot-size=32
;;   NlConsBox: car=0, cdr=32
;;   NlVector: value.ptr=0, value.len=16
;;   NlBoolVector: value.ptr=0, value.len=16  (same layout as NlVector)
;;   NlRecord: type_tag=0, slots.ptr=32, slots.len=48
;;   NlCharTable/CharTableInner: subtype=0, default_val=32, entries.len=80
;;
;; Net Rust impact: deletes write_quoted_string+write_sexp+write_reader_macro+
;;   list_tag_and_arg+write_list_body from sexp.rs (~155 LOC) and simplifies
;;   fmt_sexp to a thin wrapper calling nelisp_fmt_sexp through cc_wrap.

;;; Code:

(defconst nelisp-cc-sexp-fmt--source
  '(seq

    ;; -----------------------------------------------------------------------
    ;; (1) Raw byte sequence writer: push bytes [i..n) from bytes-ptr to buf
    ;; -----------------------------------------------------------------------
    (defun nl_fmt_sexp_write_bytes (bytes-ptr i n buf)
      ;; bytes-ptr: *const u8, i: i64, n: i64, buf: *mut Sexp::MutStr
      (if (= i n)
          0
        (and (mut-str-push-byte buf (ptr-read-u8 bytes-ptr i))
             (nl_fmt_sexp_write_bytes bytes-ptr (+ i 1) n buf))))

    ;; -----------------------------------------------------------------------
    ;; (2) Single-byte quoted string helper: push byte b with escape
    ;; -----------------------------------------------------------------------
    (defun nl_fmt_sexp_write_quoted_byte (b buf)
      ;; b: i64 (byte value 0-255), buf: *mut Sexp::MutStr
      ;; Escape: " → \", \ → \\, \n → \n, \t → \t, \r → \r
      (cond
       ((= b 34)  (and (mut-str-push-byte buf 92) (mut-str-push-byte buf 34)))   ; \"
       ((= b 92)  (and (mut-str-push-byte buf 92) (mut-str-push-byte buf 92)))   ; \\
       ((= b 10)  (and (mut-str-push-byte buf 92) (mut-str-push-byte buf 110)))  ; \n
       ((= b 9)   (and (mut-str-push-byte buf 92) (mut-str-push-byte buf 116)))  ; \t
       ((= b 13)  (and (mut-str-push-byte buf 92) (mut-str-push-byte buf 114)))  ; \r
       (1 (mut-str-push-byte buf b))))

    ;; -----------------------------------------------------------------------
    ;; (3) Escaped byte sequence: push bytes [i..n) with escaping
    ;; -----------------------------------------------------------------------
    (defun nl_fmt_sexp_write_quoted_bytes (bytes-ptr i n buf)
      (if (= i n)
          0
        (and (nl_fmt_sexp_write_quoted_byte (ptr-read-u8 bytes-ptr i) buf)
             (nl_fmt_sexp_write_quoted_bytes bytes-ptr (+ i 1) n buf))))

    ;; -----------------------------------------------------------------------
    ;; (4) Write "..." quoted string from a Sexp::Str or Sexp::MutStr
    ;; -----------------------------------------------------------------------
    (defun nl_fmt_sexp_write_quoted_str (s buf)
      ;; s: *const Sexp (Str tag=5 or MutStr tag=6), buf: *mut Sexp::MutStr
      ;; Uses str-bytes-ptr for both Str and MutStr (handled by nl_str_bytes_ptr).
      ;; Length: Str uses str-len (inline String at offset 8), MutStr uses mut-str-len.
      (and (mut-str-push-byte buf 34)  ; opening "
           (nl_fmt_sexp_write_quoted_bytes
            (str-bytes-ptr s)
            0
            (if (= (sexp-tag s) 5) (str-len s) (mut-str-len s))
            buf)
           (mut-str-push-byte buf 34)  ; closing "
           1))

    ;; -----------------------------------------------------------------------
    ;; (5) Try reader macro prefix: write prefix if s is (SYMBOL ARG)
    ;;     Returns 1 if wrote a reader macro, 0 if not.
    ;; -----------------------------------------------------------------------
    (defun nl_fmt_sexp_try_reader_macro (s buf)
      ;; s: *const Sexp
      ;; Check structure: Cons(Symbol(name), Cons(arg, Nil))
      ;; car = sexp-payload-ptr(s) (NlConsBox* = &car, offset 0)
      ;; cdr = sexp-payload-ptr(s) + 32 (= &cdr in NlConsBox)
      ;; cdr.car = sexp-payload-ptr(cdr) = sexp-payload-ptr(+payload(s) 32)
      ;; cdr.cdr = sexp-payload-ptr(cdr) + 32
      (if (and (= (sexp-tag s) 7)
               (= (sexp-tag (sexp-payload-ptr s)) 4)
               (= (sexp-tag (+ (sexp-payload-ptr s) 32)) 7)
               (= (sexp-tag (+ (sexp-payload-ptr
                                (+ (sexp-payload-ptr s) 32)) 32)) 0))
          ;; Match symbol name and write prefix
          (if (symbol-name-eq (sexp-payload-ptr s) "quote")
              (and (mut-str-push-byte buf 39)  ; '
                   (nl_fmt_sexp_dispatch
                    (sexp-payload-ptr (+ (sexp-payload-ptr s) 32))
                    buf)
                   1)
            (if (symbol-name-eq (sexp-payload-ptr s) "backquote")
                (and (mut-str-push-byte buf 96)  ; `
                     (nl_fmt_sexp_dispatch
                      (sexp-payload-ptr (+ (sexp-payload-ptr s) 32))
                      buf)
                     1)
              (if (symbol-name-eq (sexp-payload-ptr s) "comma")
                  (and (mut-str-push-byte buf 44)  ; ,
                       (nl_fmt_sexp_dispatch
                        (sexp-payload-ptr (+ (sexp-payload-ptr s) 32))
                        buf)
                       1)
                (if (symbol-name-eq (sexp-payload-ptr s) "comma-at")
                    (and (mut-str-push-byte buf 44)  ; ,
                         (mut-str-push-byte buf 64)  ; @
                         (nl_fmt_sexp_dispatch
                          (sexp-payload-ptr (+ (sexp-payload-ptr s) 32))
                          buf)
                         1)
                  (if (symbol-name-eq (sexp-payload-ptr s) "function")
                      (and (mut-str-push-byte buf 35)  ; #
                           (mut-str-push-byte buf 39)  ; '
                           (nl_fmt_sexp_dispatch
                            (sexp-payload-ptr (+ (sexp-payload-ptr s) 32))
                            buf)
                           1)
                    0)))))
        0))

    ;; -----------------------------------------------------------------------
    ;; (6) Write list body: recursive list printer (proper + dotted)
    ;; -----------------------------------------------------------------------
    (defun nl_fmt_sexp_write_list_body (s buf is-first)
      ;; s: *const Sexp, buf: *mut Sexp::MutStr, is-first: i64 (1=first, 0=rest)
      ;; Cons: write [space] car, recurse on cdr
      ;; Nil: base case, return 0
      ;; Other: write " . " + value
      (if (= (sexp-tag s) 7)
          ;; Cons: write space (unless first), write car, recurse on cdr
          (and (if (= is-first 1) 1 (mut-str-push-byte buf 32))  ; space
               (nl_fmt_sexp_dispatch (sexp-payload-ptr s) buf)
               (nl_fmt_sexp_write_list_body (+ (sexp-payload-ptr s) 32) buf 0))
        (if (= (sexp-tag s) 0)
            0  ; Nil: proper list end
          ;; Dotted pair: " . " other
          (and (mut-str-push-byte buf 32)   ; space
               (mut-str-push-byte buf 46)   ; .
               (mut-str-push-byte buf 32)   ; space
               (nl_fmt_sexp_dispatch s buf)
               0))))

    ;; -----------------------------------------------------------------------
    ;; (7) Write Vector items: "[" already pushed, writes items and "]"
    ;; -----------------------------------------------------------------------
    (defun nl_fmt_sexp_write_vec_items (s i n buf)
      ;; s: *const Sexp::Vector, i: current index, n: length, buf: *mut Sexp::MutStr
      (if (= i n)
          0
        (and (if (= i 0) 1 (mut-str-push-byte buf 32))  ; space before items 1+
             (nl_fmt_sexp_dispatch (vector-ref-ptr s i) buf)
             (nl_fmt_sexp_write_vec_items s (+ i 1) n buf))))

    ;; -----------------------------------------------------------------------
    ;; (8) CharTable formatter
    ;; -----------------------------------------------------------------------
    (defun nl_fmt_sexp_write_char_table (ct-ptr buf)
      ;; ct-ptr: NlCharTable* (= sexp-payload-ptr of CharTable Sexp)
      ;; CharTableInner layout: subtype Sexp at offset 0, default_val at offset 32,
      ;;   entries Vec<(i64,Sexp)> at offset 64 (ptr=64,cap=72,len=80)
      ;; Write: "#<char-table" [" " subtype] " default=" default_val " entries=" N ">"
      (and
       ;; "#<char-table"
       (mut-str-push-byte buf 35)   ; #
       (mut-str-push-byte buf 60)   ; <
       (mut-str-push-byte buf 99)   ; c
       (mut-str-push-byte buf 104)  ; h
       (mut-str-push-byte buf 97)   ; a
       (mut-str-push-byte buf 114)  ; r
       (mut-str-push-byte buf 45)   ; -
       (mut-str-push-byte buf 116)  ; t
       (mut-str-push-byte buf 97)   ; a
       (mut-str-push-byte buf 98)   ; b
       (mut-str-push-byte buf 108)  ; l
       (mut-str-push-byte buf 101)  ; e
       ;; Conditional subtype: ct-ptr itself is &subtype (offset 0)
       (if (= (sexp-tag ct-ptr) 0)
           1  ; Nil subtype: skip
         (and (mut-str-push-byte buf 32)  ; space
              (nl_fmt_sexp_dispatch ct-ptr buf)))
       ;; " default="
       (mut-str-push-byte buf 32)   ; space
       (mut-str-push-byte buf 100)  ; d
       (mut-str-push-byte buf 101)  ; e
       (mut-str-push-byte buf 102)  ; f
       (mut-str-push-byte buf 97)   ; a
       (mut-str-push-byte buf 117)  ; u
       (mut-str-push-byte buf 108)  ; l
       (mut-str-push-byte buf 116)  ; t
       (mut-str-push-byte buf 61)   ; =
       ;; default_val at offset 32
       (nl_fmt_sexp_dispatch (+ ct-ptr 32) buf)
       ;; " entries="
       (mut-str-push-byte buf 32)   ; space
       (mut-str-push-byte buf 101)  ; e
       (mut-str-push-byte buf 110)  ; n
       (mut-str-push-byte buf 116)  ; t
       (mut-str-push-byte buf 114)  ; r
       (mut-str-push-byte buf 105)  ; i
       (mut-str-push-byte buf 101)  ; e
       (mut-str-push-byte buf 115)  ; s
       (mut-str-push-byte buf 61)   ; =
       ;; entries.len at offset 80 (Vec<(i64,Sexp)>: ptr=64, cap=72, len=80)
       (extern-call nl_i64_append_to_mut_str (ptr-read-u64 ct-ptr 80) buf)
       (mut-str-push-byte buf 62)   ; >
       1))

    ;; -----------------------------------------------------------------------
    ;; (9) BoolVector helpers
    ;; -----------------------------------------------------------------------

    ;; Write 3-digit octal escape: \NNN
    (defun nl_fmt_sexp_write_octal_byte (b buf)
      (and (mut-str-push-byte buf 92)  ; backslash
           (mut-str-push-byte buf (+ 48 (sar b 6)))               ; hundreds 0-3
           (mut-str-push-byte buf (+ 48 (logand (sar b 3) 7)))    ; tens 0-7
           (mut-str-push-byte buf (+ 48 (logand b 7)))))          ; ones 0-7

    ;; Write one byte: octal-escape if control (<32), non-ASCII (>=127), " or \
    (defun nl_fmt_sexp_write_bool_byte (b buf)
      (if (or (< b 32) (>= b 127) (= b 34) (= b 92))
          (nl_fmt_sexp_write_octal_byte b buf)
        (mut-str-push-byte buf b)))

    ;; Build packed byte from bools [base+bit..min(base+8,n))
    (defun nl_fmt_sexp_build_bool_chunk (data-ptr base n bit byte-val)
      ;; data-ptr: *const bool, base: chunk start, n: total len,
      ;; bit: 0..8, byte-val: accumulator
      (if (or (= bit 8) (= (+ base bit) n))
          byte-val
        (nl_fmt_sexp_build_bool_chunk
         data-ptr base n (+ bit 1)
         (if (= (ptr-read-u8 data-ptr (+ base bit)) 0)
             byte-val
           (logior byte-val (shl 1 bit))))))

    ;; Write packed bytes for bools [i..n) in chunks of 8
    (defun nl_fmt_sexp_write_bool_chunks (data-ptr i n buf)
      (if (>= i n)
          0
        (and (nl_fmt_sexp_write_bool_byte
              (nl_fmt_sexp_build_bool_chunk data-ptr i n 0 0)
              buf)
             (nl_fmt_sexp_write_bool_chunks data-ptr (+ i 8) n buf))))

    ;; Write BoolVector: #&N"..."
    (defun nl_fmt_sexp_write_bool_vector (bv-ptr buf)
      ;; bv-ptr: NlBoolVector* (= sexp-payload-ptr of BoolVector Sexp)
      ;; NlBoolVector.value is Vec<bool> at offset 0: {ptr=0, cap=8, len=16}
      (and
       (mut-str-push-byte buf 35)  ; #
       (mut-str-push-byte buf 38)  ; &
       (extern-call nl_i64_append_to_mut_str (ptr-read-u64 bv-ptr 16) buf)  ; len
       (mut-str-push-byte buf 34)  ; "
       (nl_fmt_sexp_write_bool_chunks
        (ptr-read-u64 bv-ptr 0)  ; data pointer
        0
        (ptr-read-u64 bv-ptr 16)  ; length
        buf)
       (mut-str-push-byte buf 34)  ; "
       1))

    ;; -----------------------------------------------------------------------
    ;; (10) Record writer
    ;; -----------------------------------------------------------------------
    (defun nl_fmt_sexp_write_record_slots (s i n buf)
      ;; s: *const Sexp::Record, i: slot index, n: slot count, buf: *mut Sexp::MutStr
      (if (= i n)
          0
        (and (mut-str-push-byte buf 32)  ; space
             (nl_fmt_sexp_dispatch (record-slot-ref-ptr s i) buf)
             (nl_fmt_sexp_write_record_slots s (+ i 1) n buf))))

    (defun nl_fmt_sexp_write_record (s buf)
      ;; s: *const Sexp::Record, buf: *mut Sexp::MutStr
      ;; NlRecord: type_tag at offset 0 = sexp-payload-ptr(s) itself (*const Sexp)
      ;; record-slot-count(s) reads NlRecord.slots.len
      (and
       (mut-str-push-byte buf 35)  ; #
       (mut-str-push-byte buf 115) ; s
       (mut-str-push-byte buf 40)  ; (
       (nl_fmt_sexp_dispatch (sexp-payload-ptr s) buf)  ; type_tag at offset 0
       (nl_fmt_sexp_write_record_slots s 0 (record-slot-count s) buf)
       (mut-str-push-byte buf 41)  ; )
       1))

    ;; -----------------------------------------------------------------------
    ;; (11) Main dispatch (equivalent to write_sexp)
    ;; -----------------------------------------------------------------------
    (defun nl_fmt_sexp_dispatch (s buf)
      ;; s: *const Sexp, buf: *mut Sexp::MutStr (accumulator)
      ;; Returns non-zero on success.
      ;; First check reader macros (quote/backquote/comma/comma-at/function)
      (if (= (nl_fmt_sexp_try_reader_macro s buf) 1)
          1
        (cond
         ;; Nil (tag=0) → "nil"
         ((= (sexp-tag s) 0)
          (and (mut-str-push-byte buf 110)  ; n
               (mut-str-push-byte buf 105)  ; i
               (mut-str-push-byte buf 108)  ; l
               1))
         ;; T (tag=1) → "t"
         ((= (sexp-tag s) 1)
          (and (mut-str-push-byte buf 116)  ; t
               1))
         ;; Int (tag=2) → decimal
         ((= (sexp-tag s) 2)
          (and (= (extern-call nl_i64_append_to_mut_str (sexp-int-unwrap s) buf) 0)
               1))
         ;; Float (tag=3) → read f64 bits at offset 8
         ((= (sexp-tag s) 3)
          (and (= (extern-call nl_f64_bits_append_to_mut_str
                               (ptr-read-u64 s 8) buf) 0)
               1))
         ;; Symbol (tag=4) → raw bytes (str-len works for inline String)
         ((= (sexp-tag s) 4)
          (and (nl_fmt_sexp_write_bytes (str-bytes-ptr s) 0 (str-len s) buf)
               1))
         ;; Str (tag=5) or MutStr (tag=6) → "..." with escaping
         ((or (= (sexp-tag s) 5) (= (sexp-tag s) 6))
          (nl_fmt_sexp_write_quoted_str s buf))
         ;; Cons (tag=7) → (body)
         ((= (sexp-tag s) 7)
          (and (mut-str-push-byte buf 40)  ; (
               (nl_fmt_sexp_write_list_body s buf 1)
               (mut-str-push-byte buf 41)  ; )
               1))
         ;; Vector (tag=8) → [items]
         ((= (sexp-tag s) 8)
          (and (mut-str-push-byte buf 91)  ; [
               (nl_fmt_sexp_write_vec_items s 0 (vector-len s) buf)
               (mut-str-push-byte buf 93)  ; ]
               1))
         ;; CharTable (tag=9)
         ((= (sexp-tag s) 9)
          (nl_fmt_sexp_write_char_table (sexp-payload-ptr s) buf))
         ;; BoolVector (tag=10)
         ((= (sexp-tag s) 10)
          (nl_fmt_sexp_write_bool_vector (sexp-payload-ptr s) buf))
         ;; Cell (tag=11) → delegate to cell's value
         ;;   NlCell.value Sexp is at offset 0 = sexp-payload-ptr(s) itself
         ((= (sexp-tag s) 11)
          (nl_fmt_sexp_dispatch (sexp-payload-ptr s) buf))
         ;; Record (tag=12)
         ((= (sexp-tag s) 12)
          (nl_fmt_sexp_write_record s buf))
         ;; Unknown tag → output nothing, return 1
         (1 1))))

    ;; -----------------------------------------------------------------------
    ;; (12) Public entry point: nelisp_fmt_sexp
    ;; -----------------------------------------------------------------------
    (defun nelisp_fmt_sexp (s result-slot)
      ;; s: *const Sexp, result-slot: *mut Sexp
      ;; Allocates Sexp::MutStr in result-slot with 64 byte capacity,
      ;; formats s into it, then finalizes to Sexp::Str.
      ;; Returns 0 (always succeeds — unknown tags produce empty output).
      (and (mut-str-make-empty result-slot 64)
           (nl_fmt_sexp_dispatch s result-slot)
           (mut-str-finalize result-slot result-slot)
           0)))

  "AOT source for the Doc sexp-fmt migration.

Public entry: `nelisp_fmt_sexp (s result-slot)' → i64.
  s: *const Sexp  — the Sexp value to format.
  result-slot: *mut Sexp — receives Sexp::Str with the formatted text.

All `nl_fmt_sexp_*' helpers are internal and not exported to the
manifest (only `nelisp_fmt_sexp' is linked as a cc_wrap entry).

Tag constants: NIL=0 T=1 INT=2 FLOAT=3 SYMBOL=4 STR=5 MUT_STR=6
CONS=7 VECTOR=8 CHAR_TABLE=9 BOOL_VECTOR=10 CELL=11 RECORD=12.

Struct offsets used:
  NlConsBox: car=0, cdr=32.
  NlVector: value.ptr=0, value.len=16.
  NlBoolVector: value.ptr=0, value.len=16.
  NlCharTable/CharTableInner: subtype=0, default_val=32, entries.len=80.
  NlRecord: type_tag=0 (= NlRecord* itself is &type_tag).

Extern calls to Rust helpers (nlstr.rs):
  nl_i64_append_to_mut_str(n, buf) — appends decimal i64 to MutStr.
  nl_f64_bits_append_to_mut_str(bits, buf) — appends float (bit pattern).

Net Rust削減: ~155 LOC deleted from sexp.rs (write_quoted_string,
write_sexp, write_reader_macro, list_tag_and_arg, write_list_body)
offset by +35 LOC (i64/f64 helpers in nlstr.rs + lib.rs wiring).")

(provide 'nelisp-cc-sexp-fmt)

;;; nelisp-cc-sexp-fmt.el ends here
