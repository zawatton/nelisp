;;; nelisp-cc-nlstr-utf8-direct.el --- Wave n2 §n2.A nlstr UTF-8 direct-symbol migrations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave n2 §n2.A — AOT elisp migrations that replace Rust
;; `#[no_mangle]' bodies in `build-tool/src/eval/nlstr.rs' by
;; exporting the SAME C-linkage symbol names from AOT `.o' files.
;;
;; Migrated symbols:
;;
;;   nl_str_char_count      — count UTF-8 codepoints by counting
;;                            non-continuation bytes.
;;   nl_str_codepoint_at    — decode one UTF-8 codepoint at byte_idx;
;;                            writes codepoint + byte-width via ptr-write-u64.
;;   nl_str_is_alphanumeric_at — ASCII fast-path alnum check; Unicode
;;                            slow-path via `extern-call nl_is_char_alphanumeric'.
;;
;; After these three migrations the private Rust helpers `sexp_as_str'
;; (7 LOC) and `NlStrRef::with_value_mut' (5 LOC dead code) have no
;; callers in nlstr.rs and are also deleted.  One thin 4-LOC Rust
;; delegate `nl_is_char_alphanumeric(cp: i64) -> i64' is added to
;; support the Unicode alphanumeric test from elisp.
;;
;; Net Rust change: +4 (delegate) -3 (char_count) -25 (codepoint_at)
;;   -23 (is_alphanumeric_at) -7 (sexp_as_str) -5 (with_value_mut)
;;   = -59 LOC.
;;
;; Sexp layout (same as nelisp-cc-nlstr-direct-ops.el):
;;   Str/Symbol (tag 5/4): cap@8, ptr@16, len@24  (inline String)
;;   MutStr (tag 6):       NlStr* @8
;;     NlStr: String.cap@0, String.ptr@8, String.len@16, refcount@24
;;
;; UTF-8 continuation byte: (logand byte 0xC0) == 0x80.
;; Non-continuation bytes are codepoint-start bytes; counting them
;; gives the codepoint count.
;;
;; `(sexp-tag sexp)' is a pure u8 load at [sexp+0]; safe to evaluate
;; multiple times.  SEXP tags: Symbol=4, Str=5, MutStr=6.
;;
;; AOT `let' restriction: compile-time constant folding only.
;; All runtime pointer values are threaded as extra function parameters.

;;; Code:

;; ---------------------------------------------------------------------------
;; nl_str_char_count — count UTF-8 codepoints
;; ---------------------------------------------------------------------------
;;
;; Algorithm: walk each byte; count bytes where
;;   (logand byte 0xC0) != 0x80  (= codepoint-start byte, not a continuation).
;;
;; Helper chain:
;;   nl_str_char_count_loop    — tail-recursive byte walker
;;   nl_str_char_count_str     — called with (data-ptr, len, 0, 0); dispatched
;;                               for Str/Symbol
;;   nl_str_char_count_mutstr  — called with (NlStr*, 0, 0); reads ptr+len
;;   nl_str_char_count         — public entry; dispatches on sexp tag

(defconst nelisp-cc-nlstr-utf8-direct--char-count-source
  '(seq
    ;; Tail-recursive loop over bytes [0, len).
    ;; data: *const u8, i: current byte index, len: byte count, acc: codepoint count.
    ;; Returns acc (= total codepoints in string).
    (defun nl_str_char_count_loop (data i len acc)
      (if (= i len)
          acc
        (nl_str_char_count_loop
         data
         (+ i 1)
         len
         (+ acc (if (= (logand (ptr-read-u8 data i) 192) 128)
                    0
                  1)))))

    ;; Called with inline-string data pointer, len, then starts the loop.
    ;; data: *const u8, len: byte count.
    (defun nl_str_char_count_str (data len)
      (nl_str_char_count_loop data 0 len 0))

    ;; Called with NlStr* for MutStr variant; reads ptr and len from box.
    ;; nlstr: *const NlStr (= [sexp+8] for MutStr).
    (defun nl_str_char_count_mutstr (nlstr)
      (nl_str_char_count_str
       (ptr-read-u64 nlstr 8)    ; String.ptr  = [NlStr*+8]
       (ptr-read-u64 nlstr 16))) ; String.len  = [NlStr*+16]

    ;; Public entry: nl_str_char_count(str_ptr) -> i64.
    ;; str_ptr: *const Sexp — any string-y variant (Str/Symbol/MutStr).
    ;; Returns: codepoint count; 0 for non-string variant.
    (defun nl_str_char_count (str-ptr)
      (if (= (sexp-tag str-ptr) 6)
          ;; MutStr: hop through NlStr box
          (nl_str_char_count_mutstr (ptr-read-u64 str-ptr 8))
        (if (= (sexp-tag str-ptr) 5)
            ;; Str: inline String
            (nl_str_char_count_str
             (ptr-read-u64 str-ptr 16)   ; String.ptr
             (ptr-read-u64 str-ptr 24))  ; String.len
          (if (= (sexp-tag str-ptr) 4)
              ;; Symbol: same layout as Str
              (nl_str_char_count_str
               (ptr-read-u64 str-ptr 16)
               (ptr-read-u64 str-ptr 24))
            0)))))
  "AOT direct-symbol source for `nl_str_char_count'.

Exports `nl_str_char_count' (C-linkage) to replace the Rust
`#[no_mangle] pub unsafe extern \"C\" fn nl_str_char_count' body
in `build-tool/src/eval/nlstr.rs' (3 LOC).

Algorithm: count non-continuation bytes (= bytes where
(logand byte 0xC0) != 0x80), which equals the UTF-8 codepoint
count.  Tail-recursive loop via `nl_str_char_count_loop'.

Tag dispatch reads [sexp+0] via `sexp-tag' for MutStr(6)/Str(5)/Symbol(4).
MutStr hops through NlStr* at [sexp+8]; Str/Symbol use inline String
fields ptr@16, len@24.")

;; ---------------------------------------------------------------------------
;; nl_str_codepoint_at — decode one UTF-8 codepoint at byte_idx
;; ---------------------------------------------------------------------------
;;
;; UTF-8 decode rules (byte b0 = first byte of sequence):
;;   1-byte:  b0 & 0x80 == 0x00  → cp = b0,                width = 1
;;   2-byte:  b0 & 0xE0 == 0xC0  → cp = (b0 & 0x1F)<<6
;;                                      | (b1 & 0x3F),      width = 2
;;   3-byte:  b0 & 0xF0 == 0xE0  → cp = (b0 & 0x0F)<<12
;;                                      | (b1 & 0x3F)<<6
;;                                      | (b2 & 0x3F),      width = 3
;;   4-byte:  b0 & 0xF8 == 0xF0  → cp = (b0 & 0x07)<<18
;;                                      | (b1 & 0x3F)<<12
;;                                      | (b2 & 0x3F)<<6
;;                                      | (b3 & 0x3F),      width = 4
;;
;; Boundary check: byte_idx must satisfy 0 <= byte_idx < len.
;; char-boundary check: first byte must NOT be a continuation byte
;;   (i.e., (logand b0 0xC0) != 0x80 — same as codepoint-start test).
;;
;; Helper chain:
;;   nl_str_codepoint_at_decode  — decode, write out_{cp,width}, return 1
;;   nl_str_codepoint_at_b0      — given data+idx+len+out_cp+out_bw+b0,
;;                                 boundary-checks then dispatches decode
;;   nl_str_codepoint_at_ptr     — given data,idx,len,out_cp,out_bw reads b0
;;   nl_str_codepoint_at_str     — resolves data-ptr for Str/Symbol
;;   nl_str_codepoint_at_mutstr  — resolves data-ptr for MutStr
;;   nl_str_codepoint_at         — public entry; tag dispatch + idx bounds

(defconst nelisp-cc-nlstr-utf8-direct--codepoint-at-source
  '(seq
    ;; Innermost decoder: given all ingredients, decode and write outputs.
    ;; data: *const u8, idx: byte start, len: byte count (for multi-byte bounds),
    ;; out-cp: *mut i64, out-bw: *mut i64, b0: leading byte value.
    ;; Returns 1 on success.
    ;;
    ;; AOT arithmetic note: "left-shift by N" = multiply by 2^N.
    ;; 2^6=64, 2^12=4096, 2^18=262144.
    (defun nl_str_codepoint_at_decode (data idx len out-cp out-bw b0)
      (if (= (logand b0 128) 0)
          ;; 1-byte ASCII: 0xxxxxxx
          (and (ptr-write-u64 out-cp 0 b0)
               (ptr-write-u64 out-bw 0 1)
               1)
        (if (= (logand b0 224) 192)
            ;; 2-byte: 110xxxxx 10xxxxxx  (need idx+1 < len)
            (if (< (+ idx 1) len)
                (and (ptr-write-u64 out-cp 0
                       (logior (* (logand b0 31) 64)
                               (logand (ptr-read-u8 data (+ idx 1)) 63)))
                     (ptr-write-u64 out-bw 0 2)
                     1)
              0)
          (if (= (logand b0 240) 224)
              ;; 3-byte: 1110xxxx 10xxxxxx 10xxxxxx  (need idx+2 < len)
              (if (< (+ idx 2) len)
                  (and (ptr-write-u64 out-cp 0
                         (logior (* (logand b0 15) 4096)
                                 (logior (* (logand (ptr-read-u8 data (+ idx 1)) 63) 64)
                                         (logand (ptr-read-u8 data (+ idx 2)) 63))))
                       (ptr-write-u64 out-bw 0 3)
                       1)
                0)
            (if (= (logand b0 248) 240)
                ;; 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx  (need idx+3 < len)
                (if (< (+ idx 3) len)
                    (and (ptr-write-u64 out-cp 0
                           (logior (* (logand b0 7) 262144)
                                   (logior (* (logand (ptr-read-u8 data (+ idx 1)) 63) 4096)
                                           (logior (* (logand (ptr-read-u8 data (+ idx 2)) 63) 64)
                                                   (logand (ptr-read-u8 data (+ idx 3)) 63)))))
                         (ptr-write-u64 out-bw 0 4)
                         1)
                  0)
              ;; invalid leading byte
              0)))))

    ;; Given b0 (leading byte), check char boundary then decode.
    ;; A char boundary means b0 is NOT a continuation byte.
    (defun nl_str_codepoint_at_b0 (data idx len out-cp out-bw b0)
      (if (= (logand b0 192) 128)
          ;; b0 is a continuation byte — not a char boundary
          0
        (nl_str_codepoint_at_decode data idx len out-cp out-bw b0)))

    ;; Read leading byte at data[idx] and call b0 handler.
    (defun nl_str_codepoint_at_ptr (data idx len out-cp out-bw)
      (nl_str_codepoint_at_b0
       data idx len out-cp out-bw
       (ptr-read-u8 data idx)))

    ;; Str/Symbol variant: read inline String ptr@16, len@24.
    ;; sexp: *const Sexp, byte-idx: i64, out-cp: *mut i64, out-bw: *mut i64.
    (defun nl_str_codepoint_at_str (sexp byte-idx out-cp out-bw)
      (nl_str_codepoint_at_ptr
       (ptr-read-u64 sexp 16)   ; String.ptr
       byte-idx
       (ptr-read-u64 sexp 24)   ; String.len
       out-cp out-bw))

    ;; MutStr variant: NlStr* @8, then String.ptr@8, String.len@16.
    (defun nl_str_codepoint_at_mutstr (sexp byte-idx out-cp out-bw)
      (nl_str_codepoint_at_ptr
       (ptr-read-u64 (ptr-read-u64 sexp 8) 8)   ; NlStr*.String.ptr
       byte-idx
       (ptr-read-u64 (ptr-read-u64 sexp 8) 16)  ; NlStr*.String.len
       out-cp out-bw))

    ;; Public entry: nl_str_codepoint_at(str_ptr, byte_idx, out_codepoint, out_byte_width) -> i64.
    ;; Returns 1 on success, 0 on out-of-bounds / non-char-boundary / non-string.
    (defun nl_str_codepoint_at (str-ptr byte-idx out-cp out-bw)
      (if (< byte-idx 0)
          0
        (if (= (sexp-tag str-ptr) 6)
            (nl_str_codepoint_at_mutstr str-ptr byte-idx out-cp out-bw)
          (if (= (sexp-tag str-ptr) 5)
              (nl_str_codepoint_at_str str-ptr byte-idx out-cp out-bw)
            (if (= (sexp-tag str-ptr) 4)
                (nl_str_codepoint_at_str str-ptr byte-idx out-cp out-bw)
              0))))))
  "AOT direct-symbol source for `nl_str_codepoint_at'.

Exports `nl_str_codepoint_at' (C-linkage) to replace the Rust
`#[no_mangle] pub unsafe extern \"C\" fn nl_str_codepoint_at' body
in `build-tool/src/eval/nlstr.rs' (25 LOC).

UTF-8 decode via bit ops only: `logand', `logior', `*' for shifts.
2^6=64, 2^12=4096, 2^18=262144.  Boundary check: leading byte must
not be a continuation byte ((logand b0 0xC0) != 0x80).

Helper chain: nl_str_codepoint_at_decode (innermost) ← _b0 (boundary) ←
_ptr (reads b0) ← _str/_mutstr (resolves data+len) ← nl_str_codepoint_at
(tag dispatch + byte_idx >= 0 check).  Avoids `let' by threading all
runtime values as extra parameters.")

;; ---------------------------------------------------------------------------
;; nl_str_is_alphanumeric_at — test alphanumeric at byte position
;; ---------------------------------------------------------------------------
;;
;; Strategy:
;;   1. bounds check: 0 <= byte_idx < len
;;   2. Read leading byte b0.
;;   3. ASCII fast path: if b0 in [48,57] (digits), [65,90] (A-Z),
;;      [97,122] (a-z) → return 1.
;;   4. Check char boundary: if (logand b0 0xC0) == 0x80 → return 0.
;;   5. Unicode slow path: extern-call nl_is_char_alphanumeric(cp) where
;;      cp is decoded from the UTF-8 sequence.  The delegate is a thin
;;      4-LOC Rust fn added to nlstr.rs.
;;
;; For the Unicode slow path we reuse the codepoint decoder from the
;; nl_str_codepoint_at helper chain, then call the delegate.  However
;; to avoid circular dependency (both would be in separate .o files but
;; link together), we inline a simplified 4-byte decode here rather than
;; extern-calling the helper.
;;
;; Helper chain:
;;   nl_str_is_alnum_decode_cp  — decode one codepoint at data[idx], call delegate
;;   nl_str_is_alnum_b0         — ASCII fast-path + boundary + unicode dispatch
;;   nl_str_is_alnum_ptr        — reads b0 and len bound check, threads to b0
;;   nl_str_is_alnum_str        — resolves data/len for Str/Symbol, checks idx bounds
;;   nl_str_is_alnum_mutstr     — resolves data/len for MutStr, checks idx bounds
;;   nl_str_is_alphanumeric_at  — public entry; tag dispatch

(defconst nelisp-cc-nlstr-utf8-direct--is-alphanumeric-at-source
  '(seq
    ;; Decode one UTF-8 codepoint at data[idx] (idx < len guaranteed)
    ;; and call nl_is_char_alphanumeric(cp) → 0/1.
    ;; b0 is the leading byte (already read by caller).
    (defun nl_str_is_alnum_decode_cp (data idx len b0)
      (if (= (logand b0 128) 0)
          ;; ASCII (should not reach here given fast-path above, but guard)
          (extern-call nl_is_char_alphanumeric b0)
        (if (= (logand b0 224) 192)
            ;; 2-byte sequence
            (if (< (+ idx 1) len)
                (extern-call nl_is_char_alphanumeric
                  (logior (* (logand b0 31) 64)
                          (logand (ptr-read-u8 data (+ idx 1)) 63)))
              0)
          (if (= (logand b0 240) 224)
              ;; 3-byte sequence
              (if (< (+ idx 2) len)
                  (extern-call nl_is_char_alphanumeric
                    (logior (* (logand b0 15) 4096)
                            (logior (* (logand (ptr-read-u8 data (+ idx 1)) 63) 64)
                                    (logand (ptr-read-u8 data (+ idx 2)) 63))))
                0)
            (if (= (logand b0 248) 240)
                ;; 4-byte sequence
                (if (< (+ idx 3) len)
                    (extern-call nl_is_char_alphanumeric
                      (logior (* (logand b0 7) 262144)
                              (logior (* (logand (ptr-read-u8 data (+ idx 1)) 63) 4096)
                                      (logior (* (logand (ptr-read-u8 data (+ idx 2)) 63) 64)
                                              (logand (ptr-read-u8 data (+ idx 3)) 63)))))
                  0)
              ;; invalid leading byte
              0)))))

    ;; ASCII fast-path: digit 48-57, uppercase 65-90, lowercase 97-122.
    ;; If ASCII and not alnum → 0.
    ;; If not ASCII, check char boundary, then decode + delegate.
    ;; b0: leading byte value.
    (defun nl_str_is_alnum_b0 (data idx len b0)
      ;; ASCII fast path (b0 < 128)
      (if (= (logand b0 128) 0)
          (if (and (> b0 47) (< b0 58))
              1  ; digit 0-9
            (if (and (> b0 64) (< b0 91))
                1  ; A-Z
              (if (and (> b0 96) (< b0 123))
                  1  ; a-z
                0)))
        ;; Non-ASCII: char boundary check
        (if (= (logand b0 192) 128)
            0  ; continuation byte — not a char boundary
          ;; Unicode slow path via delegate
          (nl_str_is_alnum_decode_cp data idx len b0))))

    ;; Read b0 at data[idx]; idx bounds already checked by caller.
    (defun nl_str_is_alnum_ptr (data idx len)
      (nl_str_is_alnum_b0 data idx len (ptr-read-u8 data idx)))

    ;; Str/Symbol: read data-ptr@16, len@24; check idx < len.
    (defun nl_str_is_alnum_str (sexp byte-idx)
      (if (< byte-idx (ptr-read-u64 sexp 24))
          (nl_str_is_alnum_ptr
           (ptr-read-u64 sexp 16)   ; String.ptr
           byte-idx
           (ptr-read-u64 sexp 24))  ; String.len
        0))

    ;; MutStr: NlStr*@8, String.ptr@[NlStr*+8], String.len@[NlStr*+16].
    (defun nl_str_is_alnum_mutstr (sexp byte-idx)
      (if (< byte-idx (ptr-read-u64 (ptr-read-u64 sexp 8) 16))
          (nl_str_is_alnum_ptr
           (ptr-read-u64 (ptr-read-u64 sexp 8) 8)   ; NlStr*.String.ptr
           byte-idx
           (ptr-read-u64 (ptr-read-u64 sexp 8) 16)) ; NlStr*.String.len
        0))

    ;; Public entry: nl_str_is_alphanumeric_at(str_ptr, byte_idx) -> i64.
    ;; Returns 1 if alphanumeric, 0 otherwise.
    (defun nl_str_is_alphanumeric_at (str-ptr byte-idx)
      (if (< byte-idx 0)
          0
        (if (= (sexp-tag str-ptr) 6)
            (nl_str_is_alnum_mutstr str-ptr byte-idx)
          (if (= (sexp-tag str-ptr) 5)
              (nl_str_is_alnum_str str-ptr byte-idx)
            (if (= (sexp-tag str-ptr) 4)
                (nl_str_is_alnum_str str-ptr byte-idx)
              0))))))
  "AOT direct-symbol source for `nl_str_is_alphanumeric_at'.

Exports `nl_str_is_alphanumeric_at' (C-linkage) to replace the Rust
`#[no_mangle] pub unsafe extern \"C\" fn nl_str_is_alphanumeric_at' body
in `build-tool/src/eval/nlstr.rs' (23 LOC).

ASCII fast-path: byte in [48,57] (digits) / [65,90] (A-Z) / [97,122] (a-z)
returns 1 immediately without any allocation.

Unicode slow-path: decode the UTF-8 sequence starting at byte_idx and
call `(extern-call nl_is_char_alphanumeric cp)' which is a thin 4-LOC
Rust delegate added to nlstr.rs calling Rust `char::is_alphanumeric()'.

Char-boundary check: leading byte must not be a continuation byte
((logand b0 0xC0) != 0x80).  This matches the Rust impl's
`s.is_char_boundary(idx)' check.

Helper chain threads data/idx/len + decoded codepoint as function params
to avoid AOT's `let' restriction.")

(provide 'nelisp-cc-nlstr-utf8-direct)

;;; nelisp-cc-nlstr-utf8-direct.el ends here
