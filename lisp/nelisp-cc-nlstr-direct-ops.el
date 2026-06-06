;;; nelisp-cc-nlstr-direct-ops.el --- Doc 128 §128.A nlstr.rs direct-symbol migrations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 128 §128.A — AOT elisp migrations that replace Rust
;; `#[no_mangle]' bodies in `build-tool/src/eval/nlstr.rs' by
;; exporting the SAME C-linkage symbol names from AOT `.o' files.
;;
;; Unlike the §122.B / §122.D probe defconsts (which export
;; `nelisp_*' symbols that call the grammar ops and thus call the Rust
;; externs), these defconsts export the original `nl_*' symbol names
;; that the grammar-op PLT stubs resolve against.  Once a `.o'
;; exporting `nl_foo' lands in the static archive the Rust
;; `#[no_mangle] pub unsafe extern "C" fn nl_foo' body can be deleted
;; — the linker routes all callers (grammar-op PLT stubs, other Phase
;; 47 `.o' PLT stubs) directly to the new elisp implementation.
;;
;; Migrated symbols (AOT source → Rust LOC deleted):
;;
;;   nl_mut_str_len    — MutStr byte length via ptr-read-u64 chain.
;;                       NlStr* = [sexp+8], String::len = [NlStr*+16].
;;   nl_str_bytes_ptr  — tag-dispatch data-pointer extract:
;;                         Str/Symbol → [sexp+16] (inline String.ptr)
;;                         MutStr     → [NlStr*+8] where NlStr* = [sexp+8]
;;   nl_alloc_str      — allocate char buf + write Sexp::Str fields.
;;   nl_alloc_symbol   — same as nl_alloc_str but Sexp tag = 4.
;;   nl_alloc_mut_str  — allocate NlStr box (32b/8b) + char buf,
;;                       write NlStr header, write Sexp::MutStr fields.
;;   nl_mut_str_finalize — clone MutStr's String to a fresh Sexp::Str.
;;
;; AOT `let' constraint: the `let' grammar form is COMPILE-TIME
;; only (= constant-folding of integer literals).  Runtime pointer
;; values from `alloc-bytes' or `ptr-read-u64' cannot be bound in `let'.
;; The standard pattern is to thread runtime values as extra function
;; parameters — each helper adds one new value as a parameter so the
;; parent can pass the computed value to the child that uses it.
;;
;; NlStr layout (`#[repr(C)]' in `build-tool/src/eval/nlstr.rs'):
;;   offset 0:  value: String   — cap@0, ptr@8, len@16  (24 bytes)
;;   offset 24: refcount: AtomicUsize                   ( 8 bytes)
;;   total = 32 bytes, align = 8
;;
;; Sexp header layout (inline String variants — Str/Symbol):
;;   offset 0:  tag (u8 via ptr-write-u8)
;;   offset 8:  String.cap  (u64)
;;   offset 16: String.ptr  (u64 — *const u8 char data)
;;   offset 24: String.len  (u64)
;;
;; Sexp::MutStr layout:
;;   offset 0:  tag = 6 (u8)
;;   offset 8:  NlStr* (u64 pointer to heap-allocated NlStr box)
;;
;; SEXP tag constants: Symbol=4, Str=5, MutStr=6.
;;
;; Alloc safety: `alloc-bytes' maps to `std::alloc::alloc' (= same
;; global allocator that Rust's String/Vec uses).  When the NlStr box
;; is freed via `nl_str_drop_inner' + `dealloc-bytes(box, 32, 8)'
;; the interior `drop_in_place::<NlStr>' call frees the char buf via
;; `std::alloc::dealloc(char_buf, Layout::from_size_align(cap, 1))'
;; which matches the `alloc-bytes(cap, 1)' AOT allocation.
;;
;; `build_string' private helper in nlstr.rs is used only by
;; `nl_alloc_str' and `nl_alloc_symbol'; once both are migrated the
;; helper can be deleted from Rust.
;;
;; `mut_str_value' private helper in nlstr.rs is used only by
;; `nl_mut_str_len' and `nl_mut_str_finalize'; once both are migrated
;; the helper can be deleted from Rust.

;;; Code:

;; ---------------------------------------------------------------------------
;; nl_mut_str_len — MutStr byte-length
;; ---------------------------------------------------------------------------

(defconst nelisp-cc-nlstr-direct-ops--mut-str-len-source
  '(defun nl_mut_str_len (ptr)
     ;; ptr: *const Sexp — must carry Sexp::MutStr (tag=6).
     ;;
     ;; Layout:
     ;;   [ptr+8]     = NlStr* (heap box pointer)
     ;;   [NlStr*+16] = String::len (= byte count, u64)
     ;;
     ;; Returns: i64 byte length.
     (ptr-read-u64 (ptr-read-u64 ptr 8) 16))
  "AOT direct-symbol source for `nl_mut_str_len'.

Replaces the Rust `#[no_mangle] pub unsafe extern \"C\" fn nl_mut_str_len'
body in `build-tool/src/eval/nlstr.rs' (lines 147-150, 4 LOC).
Follows the two-pointer-hop chain: Sexp slot -> NlStr* -> String.len
offset 16.  No `let' needed — the two-hop expression is a direct
value form using the `ptr-read-u64' grammar op in nested position.")

;; ---------------------------------------------------------------------------
;; nl_str_bytes_ptr — tag-dispatch data-pointer extract
;; ---------------------------------------------------------------------------

(defconst nelisp-cc-nlstr-direct-ops--str-bytes-ptr-source
  '(defun nl_str_bytes_ptr (sexp)
     ;; sexp: *const Sexp — any string-y variant (Str/Symbol/MutStr).
     ;;
     ;; Sexp::Str (tag=5) / Sexp::Symbol (tag=4):
     ;;   inline String.ptr = [sexp+16]  (= ptr field of inline String)
     ;;
     ;; Sexp::MutStr (tag=6):
     ;;   NlStr* = [sexp+8]
     ;;   String.ptr = [NlStr*+8]  (= ptr field of NlStr.value)
     ;;
     ;; Returns: *const u8 data pointer (i64); 0 on non-string variant.
     ;;
     ;; Note: `sexp-tag' is evaluated at most 3 times; it is a pure
     ;; grammar op (single load from [sexp+0]) so repeated evaluation
     ;; is safe and avoids the `let' restriction.
     (if (= (sexp-tag sexp) 6)
         (ptr-read-u64 (ptr-read-u64 sexp 8) 8)
       (if (= (sexp-tag sexp) 5)
           (ptr-read-u64 sexp 16)
         (if (= (sexp-tag sexp) 4)
             (ptr-read-u64 sexp 16)
           0))))
  "AOT direct-symbol source for `nl_str_bytes_ptr'.

Replaces the Rust `#[no_mangle] pub unsafe extern \"C\" fn nl_str_bytes_ptr'
body in `build-tool/src/eval/nlstr.rs' (lines 218-225, 8 LOC).
Implements the same 3-arm match: MutStr -> NlStr box hop; Str/Symbol
-> inline String.ptr; other -> null (0).

The `sexp-tag' grammar op (= single u8 load at [sexp+0]) is evaluated
up to 3 times; this is safe since the tag field is immutable for the
lifetime of a Sexp value.  No `let' binding needed.")

;; ---------------------------------------------------------------------------
;; nl_alloc_str / nl_alloc_symbol — allocate immutable string/symbol Sexp
;; ---------------------------------------------------------------------------
;;
;; The AOT `let' form only supports compile-time constant folding.
;; To thread the runtime `alloc-bytes' result (= the char-buf pointer)
;; through the write sequence, we use a helper function pattern:
;;
;;   nl_alloc_str_copy_loop  — tail-recursive byte copier
;;   nl_alloc_str_write      — inner writer for Sexp::Str; char-buf is param 5
;;   nl_alloc_str_pos        — normalises n >= 0; calls alloc+write
;;   nl_alloc_str            — entry; clamps len < 0 to 0
;;   nl_alloc_symbol_write   — inner writer for Sexp::Symbol (tag=4)
;;   nl_alloc_symbol_pos     — normalises n >= 0; calls alloc+write
;;   nl_alloc_symbol         — entry; clamps len < 0 to 0
;;
;; The `(seq DEFUN ...)' form compiles all seven into one `.o' file.
;; Both `nl_alloc_str' and `nl_alloc_symbol' live in the same archive
;; member so the copy-loop helper is shared at link time.

(defconst nelisp-cc-nlstr-direct-ops--alloc-str-source
  '(seq
    ;; Tail-recursive byte copier: copies N bytes from SRC to DST.
    (defun nl_alloc_str_copy_loop (src dst i n)
      (if (= i n)
          1
        (and
         (ptr-write-u8 dst i (ptr-read-u8 src i))
         (nl_alloc_str_copy_loop src dst (+ i 1) n))))

    ;; Inner Sexp::Str writer.
    ;; bytes-ptr: source, n: logical length, alloc-n: allocated cap,
    ;; result-slot: output, char-buf: newly-allocated char buffer.
    (defun nl_alloc_str_write (bytes-ptr n alloc-n result-slot char-buf)
      (and
       (nl_alloc_str_copy_loop bytes-ptr char-buf 0 n)
       (ptr-write-u8  result-slot 0  5)
       (ptr-write-u64 result-slot 8  alloc-n)
       (ptr-write-u64 result-slot 16 char-buf)
       (ptr-write-u64 result-slot 24 n)
       result-slot))

    ;; Alloc + write with n >= 0 already normalised.
    (defun nl_alloc_str_pos (bytes-ptr n result-slot)
      (nl_alloc_str_write
       bytes-ptr n (if (= n 0) 1 n) result-slot
       (alloc-bytes (if (= n 0) 1 n) 1)))

    ;; Public entry: nl_alloc_str(bytes_ptr, len, result_slot).
    ;; bytes-ptr: *const u8 — source bytes (may be null when len <= 0).
    ;; len:       i64        — byte count; treated as 0 if negative.
    ;; result-slot: *mut Sexp — receives Sexp::Str.
    ;; Returns result-slot.
    (defun nl_alloc_str (bytes-ptr len result-slot)
      (nl_alloc_str_pos bytes-ptr (if (< len 0) 0 len) result-slot))

    ;; Inner Sexp::Symbol writer (tag=4, identical to Str write modulo tag).
    (defun nl_alloc_symbol_write (bytes-ptr n alloc-n result-slot char-buf)
      (and
       (nl_alloc_str_copy_loop bytes-ptr char-buf 0 n)
       (ptr-write-u8  result-slot 0  4)
       (ptr-write-u64 result-slot 8  alloc-n)
       (ptr-write-u64 result-slot 16 char-buf)
       (ptr-write-u64 result-slot 24 n)
       result-slot))

    ;; ---- symbol-name interning (Doc 08 §8.16) ----
    ;; Dedup the immutable name buffer across all occurrences of a symbol.
    ;; eq-safe: `bf_eq2' compares Symbols by NAME (symbol-eq), so a shared
    ;; buffer never changes eq.  The table + buffers live in a separate mmap
    ;; region (set up by `nl_intern_region_init'; base @ slot +832, bump @
    ;; +840) that the GC never walks (not a chunk) -> interned buffers are
    ;; permanent and invisible to mark/sweep.  Open-addressing table of 2^20
    ;; slots, each (len+1 @+0, buf @+8); 0 @+0 = empty.  When the region is not
    ;; set up (base 0) `nl_alloc_symbol_pos' falls back to the plain allocator.
    ;; No-let style (AOT alloc/ptr-read values can't be `let'-bound): values
    ;; thread through tail-recursive helper args, like the copy loop.
    (defun nl_intern_hash (p i n h)
      (if (< i n)
          (nl_intern_hash p (+ i 1) n
                          (logand (* (logxor h (ptr-read-u8 p i)) 16777619) 4294967295))
        h))
    (defun nl_intern_eq (a b i n)
      (if (= i n)
          1
        (if (= (ptr-read-u8 a i) (ptr-read-u8 b i))
            (nl_intern_eq a b (+ i 1) n)
          0)))
    (defun nl_intern_slotmatch (slot p n)
      (if (= (ptr-read-u64 slot 0) (+ n 1))
          (nl_intern_eq p (ptr-read-u64 slot 8) 0 n)
        0))
    (defun nl_intern_probe (table idx p n)
      (if (= (ptr-read-u64 (+ table (* idx 16)) 0) 0)
          (+ table (* idx 16))
        (if (= (nl_intern_slotmatch (+ table (* idx 16)) p n) 1)
            (+ table (* idx 16))
          (nl_intern_probe table (logand (+ idx 1) 1048575) p n))))
    (defun nl_intern_bump (n)
      (and (ptr-write-u64 268436296 0
                          (+ (ptr-read-u64 268436296 0) (if (= n 0) 1 n)))
           (- (ptr-read-u64 268436296 0) (if (= n 0) 1 n))))
    (defun nl_intern_write_sexp (result-slot buf n)
      (and (ptr-write-u8  result-slot 0  4)
           (ptr-write-u64 result-slot 8  (if (= n 0) 1 n))
           (ptr-write-u64 result-slot 16 buf)
           (ptr-write-u64 result-slot 24 n)
           result-slot))
    (defun nl_intern_insert (slot p n result-slot buf)
      (and (nl_alloc_str_copy_loop p buf 0 n)
           (ptr-write-u64 slot 0 (+ n 1))
           (ptr-write-u64 slot 8 buf)
           (nl_intern_write_sexp result-slot buf n)))
    (defun nl_intern_finish (slot p n result-slot)
      (if (= (ptr-read-u64 slot 0) 0)
          (nl_intern_insert slot p n result-slot (nl_intern_bump n))
        (nl_intern_write_sexp result-slot (ptr-read-u64 slot 8) n)))

    ;; Alloc + write with n >= 0.  Intern via the region when set up (+832 !=
    ;; 0); otherwise fall back to a fresh per-occurrence buffer.
    (defun nl_alloc_symbol_pos (bytes-ptr n result-slot)
      (if (= (ptr-read-u64 268436288 0) 0)
          (nl_alloc_symbol_write
           bytes-ptr n (if (= n 0) 1 n) result-slot
           (alloc-bytes (if (= n 0) 1 n) 1))
        (nl_intern_finish
         (nl_intern_probe (ptr-read-u64 268436288 0)
                          (logand (nl_intern_hash bytes-ptr 0 n 2166136261) 1048575)
                          bytes-ptr n)
         bytes-ptr n result-slot)))

    ;; Public entry: nl_alloc_symbol(bytes_ptr, len, result_slot).
    (defun nl_alloc_symbol (bytes-ptr len result-slot)
      (nl_alloc_symbol_pos bytes-ptr (if (< len 0) 0 len) result-slot)))
  "AOT direct-symbol source for `nl_alloc_str' + `nl_alloc_symbol'.

Single `(seq DEFUN ...)' manifest exporting seven symbols:
- `nl_alloc_str_copy_loop'  — private tail-recursive byte copier.
- `nl_alloc_str_write'      — Sexp::Str (tag=5) inner writer.
- `nl_alloc_str_pos'        — normalisation + alloc bridge.
- `nl_alloc_str'            — public entry point.
- `nl_alloc_symbol_write'   — Sexp::Symbol (tag=4) inner writer.
- `nl_alloc_symbol_pos'     — normalisation + alloc bridge.
- `nl_alloc_symbol'         — public entry point.

Replaces the Rust `#[no_mangle]' bodies for both `nl_alloc_str' and
`nl_alloc_symbol' in nlstr.rs (lines 102-118, 17 LOC across both) plus
the private `build_string' helper (lines 79-87, 9 LOC) which had no
other callers.  Net Rust reduction: ~26 LOC.

The helper-chain pattern avoids AOT's `let' restriction (= `let'
is compile-time constant folding only).  `alloc-bytes' returns its
pointer in rax; passing it as the final argument to a 5-param write
helper threads the runtime value through without any `let' binding.")

;; ---------------------------------------------------------------------------
;; nl_alloc_mut_str — allocate NlStr box + Sexp::MutStr slot
;; ---------------------------------------------------------------------------
;;
;; Requires two `alloc-bytes' calls: one for the 32-byte NlStr box
;; (align 8) and one for the char buffer (align 1).  The helper chain:
;;
;;   nl_alloc_mut_str_write  — given alloc-n + result-slot + nlstr-box
;;                             + char-buf, writes all fields.
;;   nl_alloc_mut_str_inner  — given alloc-n + result-slot + nlstr-box,
;;                             allocates char-buf and calls write.
;;   nl_alloc_mut_str_pos    — given n + result-slot (n >= 0),
;;                             allocates nlstr-box and calls inner.
;;   nl_alloc_mut_str        — public entry; clamps cap < 0 to 0.

(defconst nelisp-cc-nlstr-direct-ops--alloc-mut-str-source
  '(seq
    ;; Innermost writer: all four NlStr fields + Sexp header.
    ;; alloc-n:     allocated char-buffer capacity (u64)
    ;; result-slot: *mut Sexp output
    ;; nlstr-box:   fresh 32-byte NlStr allocation
    ;; char-buf:    fresh alloc-n-byte char buffer
    (defun nl_alloc_mut_str_write (alloc-n result-slot nlstr-box char-buf)
      (and
       (ptr-write-u64 nlstr-box 0  alloc-n)    ; String.cap
       (ptr-write-u64 nlstr-box 8  char-buf)   ; String.ptr
       (ptr-write-u64 nlstr-box 16 0)          ; String.len = 0
       (ptr-write-u64 nlstr-box 24 1)          ; refcount = 1
       (ptr-write-u8  result-slot 0 6)         ; Sexp tag = MutStr
       (ptr-write-u64 result-slot 8 nlstr-box) ; NlStr*
       result-slot))

    ;; Given allocated nlstr-box, allocate char-buf and finish.
    (defun nl_alloc_mut_str_inner (alloc-n result-slot nlstr-box)
      (nl_alloc_mut_str_write
       alloc-n result-slot nlstr-box
       (alloc-bytes alloc-n 1)))

    ;; Normalised n (>= 0); compute alloc-n = max(n, 1) and allocate box.
    (defun nl_alloc_mut_str_pos (n result-slot)
      (nl_alloc_mut_str_inner
       (if (= n 0) 1 n)
       result-slot
       (alloc-bytes 32 8)))

    ;; Public entry: nl_alloc_mut_str(cap, result_slot).
    ;; cap:         i64      — requested capacity; clamped to 0 if negative.
    ;; result-slot: *mut Sexp — receives Sexp::MutStr.
    ;; Returns result-slot.
    (defun nl_alloc_mut_str (cap result-slot)
      (nl_alloc_mut_str_pos (if (< cap 0) 0 cap) result-slot)))
  "AOT direct-symbol source for `nl_alloc_mut_str'.

Single `(seq DEFUN ...)' manifest exporting four symbols:
- `nl_alloc_mut_str_write'  — innermost writer (all fields).
- `nl_alloc_mut_str_inner'  — allocates char-buf; forwards to write.
- `nl_alloc_mut_str_pos'    — allocates NlStr box; forwards to inner.
- `nl_alloc_mut_str'        — public entry point.

Replaces the Rust `#[no_mangle] pub unsafe extern \"C\" fn nl_alloc_mut_str'
body in `build-tool/src/eval/nlstr.rs' (lines 120-129, 10 LOC).
Two-level helper chain threads the two `alloc-bytes' results (nlstr-box
and char-buf) through as extra parameters, avoiding the `let' restriction.

NlStr box alloc: `alloc-bytes(32, 8)' matching `Layout::new::<NlStr>()'
(= 32-byte size, 8-byte align per compile-time asserts in nlstr.rs).
Char-buf alloc: `alloc-bytes(max(cap,1), 1)' matching `String' drop
expectations.  Both use the global allocator, compatible with
`nl_str_drop_inner' + `dealloc-bytes(box, 32, 8)' + String::drop.")

;; ---------------------------------------------------------------------------
;; nl_mut_str_finalize — clone MutStr's String into a fresh Sexp::Str
;; ---------------------------------------------------------------------------
;;
;; Helper chain to thread two ptr-read-u64 results + one alloc-bytes
;; result through the write sequence:
;;
;;   nl_mut_str_finalize_copy_loop — tail-recursive byte copier
;;   nl_mut_str_finalize_write     — given str-ptr + str-len + alloc-n
;;                                   + result-slot + new-buf, writes Sexp::Str
;;   nl_mut_str_finalize_alloc     — given str-ptr + str-len + alloc-n
;;                                   + result-slot, allocates new-buf
;;   nl_mut_str_finalize_inner     — given nlstr + result-slot + str-len,
;;                                   reads str-ptr and calls alloc
;;   nl_mut_str_finalize_nlstr     — given ptr + result-slot + nlstr,
;;                                   reads str-len and calls inner
;;   nl_mut_str_finalize           — public entry; reads nlstr from sexp

(defconst nelisp-cc-nlstr-direct-ops--mut-str-finalize-source
  '(seq
    ;; Tail-recursive byte copier.
    (defun nl_mut_str_finalize_copy_loop (src dst i n)
      (if (= i n)
          1
        (and
         (ptr-write-u8 dst i (ptr-read-u8 src i))
         (nl_mut_str_finalize_copy_loop src dst (+ i 1) n))))

    ;; Innermost writer: fills in Sexp::Str fields.
    ;; str-ptr: *const u8 source data, str-len: byte count,
    ;; alloc-n: allocated cap, result-slot: output, new-buf: fresh allocation.
    (defun nl_mut_str_finalize_write (str-ptr str-len alloc-n result-slot new-buf)
      (and
       (nl_mut_str_finalize_copy_loop str-ptr new-buf 0 str-len)
       (ptr-write-u8  result-slot 0  5)
       (ptr-write-u64 result-slot 8  alloc-n)
       (ptr-write-u64 result-slot 16 new-buf)
       (ptr-write-u64 result-slot 24 str-len)
       result-slot))

    ;; Given str-ptr + str-len + alloc-n + result-slot, allocate new-buf.
    (defun nl_mut_str_finalize_alloc (str-ptr str-len alloc-n result-slot)
      (nl_mut_str_finalize_write
       str-ptr str-len alloc-n result-slot
       (alloc-bytes alloc-n 1)))

    ;; Given nlstr* + result-slot + str-len (already read), read str-ptr.
    (defun nl_mut_str_finalize_inner (nlstr result-slot str-len)
      (nl_mut_str_finalize_alloc
       (ptr-read-u64 nlstr 8)               ; str-ptr = NlStr.value.ptr
       str-len
       (if (= str-len 0) 1 str-len)         ; alloc-n = max(str-len, 1)
       result-slot))

    ;; Given ptr (Sexp*) + result-slot + nlstr (already read from ptr+8).
    (defun nl_mut_str_finalize_nlstr (ptr result-slot nlstr)
      (nl_mut_str_finalize_inner
       nlstr result-slot
       (ptr-read-u64 nlstr 16)))             ; str-len = NlStr.value.len

    ;; Public entry: nl_mut_str_finalize(ptr, result_slot).
    ;; ptr:         *const Sexp — source MutStr slot (tag=6).
    ;; result-slot: *mut Sexp   — destination slot to receive Sexp::Str.
    ;; Returns result-slot.  Source MutStr remains live (clone semantics).
    (defun nl_mut_str_finalize (ptr result-slot)
      (nl_mut_str_finalize_nlstr
       ptr result-slot
       (ptr-read-u64 ptr 8))))               ; nlstr = NlStr* at [sexp+8]
  "AOT direct-symbol source for `nl_mut_str_finalize'.

Six-entry `(seq DEFUN ...)' manifest:
- `nl_mut_str_finalize_copy_loop'  — tail-recursive byte copier.
- `nl_mut_str_finalize_write'      — Sexp::Str field writer.
- `nl_mut_str_finalize_alloc'      — allocates new char buf.
- `nl_mut_str_finalize_inner'      — reads str-ptr; threads to alloc.
- `nl_mut_str_finalize_nlstr'      — reads str-len; threads to inner.
- `nl_mut_str_finalize'            — public entry.

Replaces the Rust `#[no_mangle]' body for `nl_mut_str_finalize' in
nlstr.rs (lines 152-159, 8 LOC).  After migration, the private
`mut_str_value' helper (lines 98-100, 3 LOC; used only by the now-
migrated `nl_mut_str_len' and `nl_mut_str_finalize') has no more
callers and is deleted.  Net Rust reduction: ~11 LOC (8 + 3).

The five-level helper chain threads the three runtime pointer values
(`nlstr' from ptr-read-u64(ptr,8), `str-len' from ptr-read-u64(nlstr,16),
`new-buf' from alloc-bytes) without any `let' bindings, respecting
AOT's compile-time-only `let' restriction.")

(provide 'nelisp-cc-nlstr-direct-ops)

;;; nelisp-cc-nlstr-direct-ops.el ends here
