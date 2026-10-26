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
;;   nl_intern_lookup  — Doc 163 Phase C, no Rust precursor: probe-only
;;                       ("lookup without insert") counterpart of
;;                       nl_alloc_symbol, backing elisp `intern-soft''s
;;                       real soft-fail path.
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
;;              Reader immutable tag-5 strings reserve bit 62 for an explicit
;;              multibyte representation; low 62 bits are the allocated capacity.
;;              Buffer GC uses pointer/length, not this representation bit.
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
     (heap-read-u64 (heap-read-u64 ptr 8) 16))
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
  '(seq
    (defun nl_str_bytes_ptr (sexp)
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
     ;; Cache the semantic tag once. Every branch keeps its existing
     ;; physical field ABI; unsupported kinds still return null.
     (let* ((kind (sexp-tag sexp)))
       (if (or (= kind 6) (= kind 15))
           (heap-read-u64 (heap-sexp-payload sexp) 8)
         (if (or (= kind 5) (= kind 14))
             (heap-sexp-data sexp)
           ;; Doc 226 D9 stage 3.2: an interned symbol may be its object's WORD.
           (if (= kind 4) (heap-sexp-data sexp) 0)))))
    ;; Raw-pointer byte access shared by AOT consumers whose source files are
    ;; deliberately kept out of the raw-memory kernel inventory.
    (defun nl_bytes_byte_at (bytes i)
      (heap-read-u8 bytes i))
    ;; Raw memory transport (clause 3), not a Lisp-visible native. Callers
    ;; establish that four bytes remain, including for unaligned buffers.
    (defun nl_bytes_u32_at (bytes i)
      (heap-read-u32 bytes i))
    ;; The reader's scratch builder is already an owning MutStr.  Numeric
    ;; escapes only change its representation flag; the boxed NlStr layout and
    ;; byte buffer remain untouched.
    (defun nl_mut_str_mark_unibyte (sexp)
      (and (ptr-write-u8 sexp 0 15) sexp)))
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
    (defun nl_alloc_str_copy_loop (src dst_ptr i n)
      ;; ITERATIVE 8-byte bulk + byte tail.  The old per-byte tail recursion
      ;; recursed once per byte, so a large string drove the native stack to
      ;; ~768MB -- the dominant peak-RSS contributor + deep-stack SIGSEGV source.
      (let ((k i))
        (while (<= (+ k 8) n)
          (and (ptr-write-u64 dst_ptr k (heap-read-u64 src k)) (setq k (+ k 8))))
        (while (< k n)
          (and (ptr-write-u8 dst_ptr k (heap-read-u8 src k)) (setq k (+ k 1))))
        1))

    ;; Shared inline-string writer.  TAG is 5 for UTF-8 Str and 14 for
    ;; raw-byte UnibyteStr; the remaining fields have the same layout.
    ;; bytes-ptr: source, n: logical length, alloc-n: allocated cap,
    ;; result-slot: output, char-buf: newly-allocated char buffer.
    (defun nl_alloc_str_write_tag
        (bytes-ptr n _alloc-n result-slot _char-buf tag)
      (let* ((uni (if (= tag 14) 1
                     (if (= (nl_string_char_count bytes-ptr n) n) 1 0)))
             (word (nl_string_new bytes-ptr n uni)))
        (if (= word 0) 0 (nl_string_store_view word result-slot))))
    (defun nl_alloc_str_write (bytes-ptr n alloc-n dest-slot char-buf)
      (nl_alloc_str_write_tag bytes-ptr n alloc-n dest-slot char-buf 5))
    ;; Alloc + write with n >= 0 already normalised.
    (defun nl_alloc_str_pos (bytes-ptr n dest-slot)
      (let* ((chars (nl_string_char_count bytes-ptr n))
             (word (nl_string_new bytes-ptr n (if (= chars n) 1 0))))
        (if (= word 0) 0 (nl_string_store_view word dest-slot))))
    (defun nl_alloc_str (bytes-ptr len result-slot)
      (nl_alloc_str_pos bytes-ptr (if (< len 0) 0 len) result-slot))
    (defun nl_alloc_str_w (bytes-ptr len _pad)
      (let* ((n (if (< len 0) 0 len)) (chars (nl_string_char_count bytes-ptr n)))
        (nl_string_new bytes-ptr n (if (= chars n) 1 0))))
    (defun nl_alloc_unibyte_str_pos (bytes-ptr n result-slot)
      (let ((word (nl_string_new bytes-ptr n 1)))
        (if (= word 0) 0 (nl_string_store_view word result-slot))))
    (defun nl_alloc_unibyte_str (bytes-ptr len result-slot)
      (nl_alloc_unibyte_str_pos bytes-ptr (if (< len 0) 0 len) result-slot))
    (defun nl_alloc_unibyte_str_w (bytes-ptr len _pad)
      (nl_string_new bytes-ptr (if (< len 0) 0 len) 1))

    ;; Inner Sexp::Symbol writer (tag=4, identical to Str write modulo tag).
    (defun nl_alloc_symbol_write (bytes-ptr n alloc-n dest-slot char-buf)
      (and
       (nl_alloc_str_copy_loop bytes-ptr char-buf 0 n)
       (ptr-write-u8  dest-slot 0  4)
       (ptr-write-u64 dest-slot 8  alloc-n)
       (ptr-write-u64 dest-slot 16 char-buf)
       (ptr-write-u64 dest-slot 24 n)
       dest-slot))

    ;; Canonical mutable name objects are kept in an external open-addressed
    ;; table.  It is outside the moving Lisp heap; values at entry+40 are
    ;; traced explicitly by the collector. Entry layout (88 bytes): state,
    ;; key-kind, key, byte-length, hash, name-Sexp, last-mark-epoch, canonical record-type WORD.
    ;; Key-kind 1 is an interned/tag-4 name buffer; 2 is a tag-16 identity.
    (defun nl_symname_copy32 (src to off)
      (if (= (logand src 7) 4)
          (seq (nl_string_store_view src to) 1)
        (if (>= off 32) 1
          (seq (ptr-write-u64 to off (heap-read-u64 src off))
               (nl_symname_copy32 src to (+ off 8))))))
    (defun nl_symname_copy88 (src dst_ptr off)
      (if (>= off 88) 1
        (seq (ptr-write-u64 dst_ptr off (heap-read-u64 src off))
             (nl_symname_copy88 src dst_ptr (+ off 8)))))
    (defun nl_symname_entry_match (entry kind key len hash)
      (if (or (/= (heap-read-u64 entry 8) kind)
              (/= (heap-read-u64 entry 32) hash)) 0
        (if (= kind 2)
            (if (= (heap-read-u64 entry 16) key) 1 0)
          (if (/= (heap-read-u64 entry 24) len) 0
            ;; Kind-1 keys are immutable name buffers. Pointer identity is
            ;; sufficient after the kind/hash/length guards above.
            (if (= (heap-read-u64 entry 16) key) 1
              (nl_intern_eq (heap-read-u64 entry 16) key 0 len))))))
    (defun nl_symname_probe (table cap kind key len hash step first-tomb)
      (if (>= step cap) first-tomb
        (let* ((entry (+ table (* (logand (+ hash step) (- cap 1)) 88)))
               (state (heap-read-u64 entry 0)))
          (if (= state 0)
              (if (= first-tomb 0) entry first-tomb)
            (if (= state 2)
                (nl_symname_probe table cap kind key len hash (+ step 1)
                                  (if (= first-tomb 0) entry first-tomb))
              (if (= (nl_symname_entry_match entry kind key len hash) 1)
                  entry
                (nl_symname_probe table cap kind key len hash (+ step 1)
                                  first-tomb)))))))
    (defun nl_symname_hash (kind key len)
      (if (= kind 2)
          (logand (* (logxor key (sar key 32)) 2654435761) 4294967295)
        (nl_intern_hash key 0 len 2166136261)))
    ;; Interned raw name buffers occupy the dedicated name arena, beginning
    ;; 16 MiB after its base. Other kind-1 keys are ordinary heap buffers and
    ;; remain live only while a reachable Symbol points at them.
    (defun nl_symbol_name_key_permanent (key len)
      ;; A buffer merely landing in the intern mapping is insufficient: only
      ;; a currently occupied intern-table row grants permanent lifetime.
      (let* ((base (heap-read-u64 268436288 0)))
        (if (= base 0) 0
          (let* ((slot (nl_intern_probe
                        base (logand (nl_intern_hash key 0 len 2166136261) 1048575)
                        key len 0)))
            (if (= slot 0) 0
              (if (= (heap-read-u64 slot 0) (+ len 1))
                  (if (= (heap-read-u64 slot 8) key) 1 0)
                0))))))
    (defun nl_symname_reinsert (new-table new-cap old-entry)
      (let* ((hash (heap-read-u64 old-entry 32)) (step 0) (done 0))
        (while (and (< step new-cap) (= done 0))
          (let* ((entry (+ new-table (* (logand (+ hash step) (- new-cap 1)) 88))))
            (if (= (heap-read-u64 entry 0) 0)
                (seq (nl_symname_copy88 old-entry entry 0) (setq done 1))
              (setq step (+ step 1)))))
        done))
    (defun nl_symname_rehash_walk (old-table old-cap new-table new-cap)
      (let* ((i 0) (valid 1))
        (while (and (< i old-cap) (= valid 1))
          (let* ((entry (+ old-table (* i 88))))
            (if (and (= (heap-read-u64 entry 0) 1)
                     (= (nl_symname_reinsert new-table new-cap entry) 0))
                (setq valid 0) 0)
            (setq i (+ i 1))))
        valid))
    (defun nl_symname_grow ()
      (let* ((cap (heap-read-u64 (data-addr nl_symbol_name_state) 8)))
        (nl_symname_rehash_to (if (= cap 0) 256 (* cap 2)))))
    ;; Rebuild the table at NEW-CAP (a power of two) from its live entries.
    ;; Tombstones are not copied, so this is also how the collector drops
    ;; them once sweeping has filled the table with them.
    (defun nl_symname_rehash_to (new-cap)
      (let* ((state (data-addr nl_symbol_name_state))
             (old (heap-read-u64 state 0))
             (cap (heap-read-u64 state 8)))
        ;; Power-of-two capacity and 88-byte entries must remain in signed
        ;; i64 range. Allocation failure leaves the old table untouched.
        (if (or (< cap 0) (>= new-cap 72057594037927936)) 0
          (let* ((bytes (* new-cap 88)) (new (nl_os_alloc_chunk bytes)))
            (if (< new 4096) 0
              (if (= (nl_os_commit_range new 0 bytes) 0)
                  (seq (nl_os_free_chunk new bytes) 0)
                (if (= (nl_symname_rehash_walk old cap new new-cap) 0)
                    (seq (nl_os_free_chunk new bytes) 0)
                  (seq (ptr-write-u64 state 0 new)
                       (ptr-write-u64 state 8 new-cap)
                       ;; Publication is complete before reclaim. If the OS
                       ;; refuses this valid unmap, the old table leaks but
                       ;; the new live table remains fully usable.
                       (if (> old 0) (nl_os_free_chunk old (* cap 88)) 0)
                       1))))))))
    (defun nl_symname_entry (kind key len hash)
      (let* ((state (data-addr nl_symbol_name_state))
             (base (heap-read-u64 state 0))
             (cap (heap-read-u64 state 8)))
        (if (= base 0)
            (if (= (nl_symname_grow) 0) 0
              (nl_symname_entry kind key len hash))
          (let* ((entry (nl_symname_probe base cap kind key len hash 0 0))
                 (used (heap-read-u64 state 16)))
            (if (= entry 0) 0
              (if (= (heap-read-u64 entry 0) 1) entry
                (if (>= (* (+ used 1) 4) (* cap 3))
                    (if (= (nl_symname_grow) 0) 0
                      (nl_symname_entry kind key len hash))
                  entry)))))))
    ;; Return the entry address without allocating. Used by the GC hooks.
    (defun nl_symbol_name_entry_for (kind key len)
      (let* ((state (data-addr nl_symbol_name_state))
             (base (heap-read-u64 state 0)) (cap (heap-read-u64 state 8)))
        (if (= base 0) 0
          (let* ((hash (nl_symname_hash kind key len))
                 (entry (nl_symname_probe base cap kind key len hash 0 0)))
            ;; Probe already validated every occupied row it returns.
            ;; Empty/tombstone insertion positions are never lookup hits.
            (if (= entry 0) 0
              (if (= (heap-read-u64 entry 0) 1) entry 0))))))
    ;; First insertion wins: an intern hit must retain the original name Sexp.
    (defun nl_symbol_name_register (kind key len name-slot)
      (let* ((hash (nl_symname_hash kind key len))
             (entry (nl_symname_entry kind key len hash)))
        (if (= entry 0) 0
          (if (= (heap-read-u64 entry 0) 1) 1
            (seq (ptr-write-u64 entry 8 kind)
                 (ptr-write-u64 entry 16 key)
                 (ptr-write-u64 entry 24 len)
                 (ptr-write-u64 entry 32 hash)
                 (nl_symname_copy32 name-slot (+ entry 40) 0)
                 (ptr-write-u64 entry 72 (heap-read-u64 (data-addr nl_symbol_name_state) 24))
                 (ptr-write-u64 entry 80 0)
                 (ptr-write-u64 entry 0 1)
                 (ptr-write-u64 (data-addr nl_symbol_name_state) 16
                                (+ (heap-read-u64 (data-addr nl_symbol_name_state) 16) 1))
                 1)))))
    (defun nl_symbol_name_copy_out (entry out)
      (if (= entry 0) 0
        (if (= (heap-read-u64 entry 0) 1)
            (seq (nl_symname_copy32 (+ entry 40) out 0) 1)
          0)))
    ;; Doc 215 word ABI: the result WORD, or 0 after a stashed generic error when the legacy form answered 0.
    ;; `nl_symbol_name_copy_out' keeps its C-ABI (nonzero/0) and is unchanged.
    (defun nl_symbol_name_copy_out_w (entry _pad)
      (let* ((out (alloc-bytes 32 8)))
        (if (/= (nl_symbol_name_copy_out entry out) 0)
            out
          (nl_abi_fail 1 0))))

    ;; A record must own a transient type view. Interned symbols share one
    ;; owned Sexp per name in the existing sidecar, rather than a fresh 32B
    ;; alias in every record. Identity-bearing symbols never use this cache.
    (defun nl_record_type_intern_own (word)
      (let* ((key (heap-read-u64 word 16))
             (len (heap-read-u64 word 24))
             (entry (nl_symbol_name_entry_for 1 key len)))
        (if (if (= entry 0) 0 (/= (heap-read-u64 entry 80) 0))
            (heap-read-u64 entry 80)
          (let* ((mark (nl_root_mark 0))
                 (symbol-root (nl_root_reserve_checked 0))
                 (name-root (nl_root_reserve_checked 0))
                 (result 0))
            (if (or (= symbol-root 0) (= name-root 0))
                (seq (nl_root_release 0 mark) 0)
              (seq
               (nl_symname_copy32 word symbol-root 0)
               ;; Use the ordinary name constructor: its independently owned
               ;; mutable string must not alias the immutable intern key.
               (if (= (nl_symbol_name_value 0 symbol-root name-root) 0) 0
                 (let* ((owned (alloc-bytes 32 8)))
                   ;; The root is reread after allocation/possible compaction.
                   ;; Symbol identity is its interned name; this shallow owned
                   ;; view retains that name without another byte allocation.
                   (nl_symname_copy32 symbol-root owned 0)
                   (let* ((row (nl_symbol_name_entry_for
                                1 (heap-read-u64 owned 16)
                                (heap-read-u64 owned 24))))
                     (if (= row 0) 0
                       (seq (if (= (heap-read-u64 row 80) 0)
                                (ptr-write-u64 row 80 owned) 0)
                            (setq result (heap-read-u64 row 80)))))))
               (nl_root_release 0 mark)
               result))))))

    (defun nl_record_type_own (word)
      (if (/= (logand word 7) 0) word
        (if (= (nl_val_tag word) 4)
            (nl_record_type_intern_own word)
          (let* ((storage (alloc-bytes 8 8)))
            (nl_val_clone_into word storage)))))

    (defun nl_next_symbol_identity ()
      ;; Shared with the reader and C-ABI producer. The mutation epoch is
      ;; monotonic and survives image restoration; callers reject overflow.
      (+ (atomic-fetch-add 268435544 1) 1))

    ;; Internal tag-16 symbols separate identity from the public name. The
    ;; caller owns unique positive IDs; this constructor never interns names.
    (defun nl_uninterned_symbol_write (bytes-ptr n identity result-slot char-buf)
      (and
       (nl_alloc_str_copy_loop bytes-ptr char-buf 0 n)
       (ptr-write-u64 result-slot 0 16)
       (ptr-write-u64 result-slot 8 identity)
       (ptr-write-u64 result-slot 16 char-buf)
       (ptr-write-u64 result-slot 24 n)
       result-slot))
    ;; Doc 215 word ABI: the result WORD, or 0 after a stashed generic error when the legacy form answered 0.
    ;; `nl_uninterned_symbol_write' keeps its C-ABI (nonzero/0) and is unchanged.
    (defun nl_uninterned_symbol_write_w (bytes-ptr n identity _pad char-buf)
      (let* ((result-slot (alloc-bytes 32 8)))
        (if (/= (nl_uninterned_symbol_write bytes-ptr n identity result-slot char-buf) 0)
            result-slot
          (nl_abi_fail 1 0))))
    ;; Doc 226 D9: uninterned symbols are direct objects (GNU Fmake_symbol,
    ;; alloc.c:3675).  The C-ABI writers below build the object and write
    ;; its view into RESULT-SLOT; IDENTITY is no longer used (the object's
    ;; address is the identity).
    (defun nl_alloc_uninterned_symbol (bytes-ptr n identity result-slot)
      (let* ((name (alloc-bytes 32 8)))
        (seq (nl_alloc_str bytes-ptr n name)
             (let* ((w (nl_sym_make_uninterned name 0)))
               (if (= w 0) 0 (seq (nl_sym_view_into w result-slot) 1))))))
    (defun nl_uninterned_symbol_equal (left right)
      (if (= (sexp-tag left) 16)
          (if (= (sexp-tag right) 16)
              (if (= (heap-sexp-payload left) (heap-sexp-payload right)) 1 0)
            0)
        0))

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
    ;; The two byte loops below are `while' loops over `let*' locals instead
    ;; (the AOT compiler makes a real call of a self tail call: 40 instructions
    ;; a byte, 19% of a new name's reader cost): FNV-1a is the same function.
    (defun nl_intern_hash (p i n h)
      (let* ((hh h) (ii i))
        (seq (while (< ii n)
               (seq (setq hh (logand (* (logxor hh (heap-read-u8 p ii)) 16777619) 4294967295))
                    (setq ii (+ ii 1))))
             hh)))
    (defun nl_intern_eq (a b i n)
      (let* ((ii i) (same 1))
        (seq (while (if (= same 1) (< ii n) 0)
               (seq (if (= (heap-read-u8 a ii) (heap-read-u8 b ii)) 0 (setq same 0))
                    (setq ii (+ ii 1))))
             same)))
    (defun nl_intern_slotmatch (slot p n)
      (if (= (heap-read-u64 slot 0) (+ n 1))
          (nl_intern_eq p (heap-read-u64 slot 8) 0 n)
        0))
    ;; K counts probe steps.  After 2^20 of them every slot has been
    ;; visited and the table is full: answer 0 (no slot) so the callers
    ;; degrade to a plain per-occurrence buffer instead of spinning here
    ;; forever.  Capacity is fixed at 2^20 names (16 MiB of slots) plus a
    ;; 48 MiB name arena; the region's layout is what the flat-arena image
    ;; dumper relocates, so growing it is a dumper change, not a table one.
    (defun nl_intern_probe (table idx p n k)
      (if (< 1048575 k)
          0
        (if (= (heap-read-u64 (+ table (* idx 16)) 0) 0)
            (+ table (* idx 16))
          (if (= (nl_intern_slotmatch (+ table (* idx 16)) p n) 1)
              (+ table (* idx 16))
            (nl_intern_probe table (logand (+ idx 1) 1048575) p n (+ k 1))))))
    ;; 1 when N more bytes (1 for an empty name) still fit in the name
    ;; arena [base+16MiB, base+64MiB).  The bump pointer used to advance
    ;; past the end of the mapping unchecked.  The 24 extra bytes cover the
    ;; one-time cell-index counter, the header alignment and the header.
    (defun nl_intern_arena_fits (n)
      (if (<= (+ (heap-read-u64 268436296 0) (+ 24 (if (= n 0) 1 n)))
              (+ (heap-read-u64 268436288 0) 67104768))
          1
        0))
    ;; GNU lisp.h:786-822: a symbol owns its value and function cells.  A
    ;; NeLisp symbol box is a per-occurrence view of one canonical interned
    ;; name buffer, so that buffer stands for the symbol: an 8-aligned
    ;; 8-byte header just before its bytes holds the symbol's cell index
    ;; (0 = none yet; see `nl_symcell_assign').  The arena's first word is
    ;; the dense index counter.  Both live in the dumped [base, bump) span.
    (defun nl_intern_bump (n)
      (let* ((arena (+ (heap-read-u64 268436288 0) 16777216))
             (b0 (heap-read-u64 268436296 0))
             (b1 (if (= b0 arena) (+ arena 8) b0))
             (hdr (logand (+ b1 7) -8)))
        (seq (ptr-write-u64 hdr 0 0)
             (ptr-write-u64 268436296 0 (+ (+ hdr 8) (if (= n 0) 1 n)))
             (+ hdr 8))))
    (defun nl_intern_write_sexp (dest-slot buf n)
      (and (ptr-write-u8  dest-slot 0  4)
           (ptr-write-u64 dest-slot 8  (if (= n 0) 1 n))
           (ptr-write-u64 dest-slot 16 buf)
           (ptr-write-u64 dest-slot 24 n)
           dest-slot))
    (defun nl_intern_insert (slot p n dest-slot buf)
      (and (nl_alloc_str_copy_loop p buf 0 n)
           (ptr-write-u64 slot 0 (+ n 1))
           (ptr-write-u64 slot 8 buf)
           (nl_intern_write_sexp dest-slot buf n)))
    ;; Plain per-occurrence buffer: used when the region is disabled and
    ;; when it is exhausted (SLOT 0 = table full, or the arena cannot take
    ;; the name).  The symbol is still correct -- `eq' compares names -- it
    ;; is just not deduplicated and `intern-soft' cannot see it.
    (defun nl_alloc_symbol_plain (bytes-ptr n dest-slot)
      (nl_alloc_symbol_write
       bytes-ptr n (if (= n 0) 1 n) dest-slot
       (alloc-bytes (if (= n 0) 1 n) 1)))
    (defun nl_intern_finish (slot p n dest-slot)
      (if (= slot 0)
          (nl_alloc_symbol_plain p n dest-slot)
        (if (= (heap-read-u64 slot 0) 0)
            (if (= (nl_intern_arena_fits n) 1)
                (nl_intern_insert slot p n dest-slot (nl_intern_bump n))
              (nl_alloc_symbol_plain p n dest-slot))
          (nl_intern_write_sexp dest-slot (heap-read-u64 slot 8) n))))

    ;; ---- nil/t name canonicalization ----
    ;; `nil' and `t' are immediate Sexp values (tag=0 / tag=1 -- see
    ;; `symbol-name''s reverse mapping above, which already turns those two
    ;; tags back into the strings "nil"/"t"); they never occupy a slot in
    ;; this intern-region table.  Before this fix `nl_alloc_symbol_pos' and
    ;; `nl_intern_lookup_pos' had no inverse special case, so `(intern
    ;; "nil")' built and returned a genuine tag=4 Symbol object NAMED "nil"
    ;; -- printing exactly like nil (both print as `nil') but failing `eq',
    ;; `null' and `listp' against it -- and `(intern-soft "nil")' probed a
    ;; table that had never had "nil" inserted into it (the reader bypasses
    ;; `intern' for these two tokens entirely, see nelisp-read.el) and
    ;; reported a miss, i.e. nil-that-prints-as-nil-but-is-a-miss.  Measured
    ;; 2026-08-21.  Any caller of `nl_alloc_symbol' / `nl_intern_lookup' --
    ;; not just `intern' / `intern-soft' -- inherits the fix from here,
    ;; since both public entries route through these two `_pos' helpers.
    (defun nl_name_is_nil (p n)
      (if (= n 3)
          (if (= (heap-read-u8 p 0) 110)
              (if (= (heap-read-u8 p 1) 105)
                  (if (= (heap-read-u8 p 2) 108) 1 0)
                0)
            0)
        0))
    (defun nl_name_is_t (p n)
      (if (= n 1)
          (if (= (heap-read-u8 p 0) 116) 1 0)
        0))
    (defun nl_write_canonical_nil (dest-slot)
      (and (ptr-write-u64 dest-slot 0 0) (ptr-write-u64 dest-slot 8 0)
           (ptr-write-u64 dest-slot 16 0) (ptr-write-u64 dest-slot 24 0)
           dest-slot))
    (defun nl_write_canonical_t (dest-slot)
      (and (ptr-write-u64 dest-slot 0 1) (ptr-write-u64 dest-slot 8 0)
           (ptr-write-u64 dest-slot 16 0) (ptr-write-u64 dest-slot 24 0)
           dest-slot))

    ;; Alloc + write with n >= 0.  "nil"/"t" short-circuit to the canonical
    ;; immediate value first (see above); otherwise intern via the region
    ;; when set up (+832 != 0), or fall back to a fresh per-occurrence
    ;; buffer.
    (defun nl_alloc_symbol_pos (bytes-ptr n dest-slot)
      (if (= (nl_name_is_nil bytes-ptr n) 1)
          (nl_write_canonical_nil dest-slot)
        (if (= (nl_name_is_t bytes-ptr n) 1)
            (nl_write_canonical_t dest-slot)
          (if (= (heap-read-u64 268436288 0) 0)
              (nl_alloc_symbol_plain bytes-ptr n dest-slot)
            (nl_intern_finish
             (nl_intern_probe (heap-read-u64 268436288 0)
                              (logand (nl_intern_hash bytes-ptr 0 n 2166136261) 1048575)
                              bytes-ptr n 0)
             bytes-ptr n dest-slot)))))

    ;; Public entry: nl_alloc_symbol(bytes_ptr, len, result_slot).
    (defun nl_alloc_symbol (bytes-ptr len result-slot)
      ;; Construction has no canonical name Sexp yet. `symbol-name' creates
      ;; it lazily in a checked root slot; this keeps intern hits allocation
      ;; free and avoids retaining an unrooted 32-byte scratch box here.
      (nl_alloc_symbol_pos bytes-ptr (if (< len 0) 0 len) result-slot))
    ;; Doc 215 builtin word ABI: nl_alloc_symbol_w returns the result WORD; nl_alloc_symbol keeps the direct slot-writing form (hot leaf).
    (defun nl_alloc_symbol_w (bytes-ptr len _pad)
      (if (= (nl_name_is_nil bytes-ptr len) 1) 3
        (if (= (nl_name_is_t bytes-ptr len) 1) 7
          (let ((object (alloc-bytes 32 8)))
            (seq (nl_alloc_symbol bytes-ptr len object) object)))))

    ;; A front cache for `nl_alloc_symbol': callers that intern the same
    ;; fixed name on every call (the bytecode VM's opcode primitives) skip the
    ;; FNV hash and the table probe.  GNU's Bcall reads the callee symbol out
    ;; of the constants vector and never interns; these callers have no
    ;; constant, so the interned name buffer (permanent, outside the GC) is
    ;; remembered instead.  256 entries of (buffer, length) live in the last
    ;; 4096 bytes of the intern region's mapping, which `nl_intern_arena_fits'
    ;; keeps out of the name arena; a fresh mapping is zero, and the dump
    ;; covers only [base, bump), so the cache starts empty in every process.
    ;; An entry is used only when its length matches, its buffer lies inside
    ;; the name arena, and the bytes compare equal, so a stale or torn entry
    ;; falls back to `nl_alloc_symbol'.  The symbol written is the one
    ;; `nl_alloc_symbol' writes for the same name: the same interned buffer.
    (defun nl_symcache_index (p n)
      (logand (+ (+ (* n 31) (* (heap-read-u8 p 0) 7))
                 (+ (* (heap-read-u8 p (- n 1)) 13) (heap-read-u8 p (sar n 1))))
              255))
    ;; N bytes of P against BUF, a word at a time: both buffers hold at
    ;; least N bytes, so no word read reaches past either.
    (defun nl_symcache_eq (p buf n)
      (let* ((i 0) (same 1))
        (seq (while (if (= same 1) (<= (+ i 8) n) 0)
               (seq (if (= (heap-read-u64 p i) (heap-read-u64 buf i)) 0 (setq same 0))
                    (setq i (+ i 8))))
             (while (if (= same 1) (< i n) 0)
               (seq (if (= (heap-read-u8 p i) (heap-read-u8 buf i)) 0 (setq same 0))
                    (setq i (+ i 1))))
             same)))
    (defun nl_alloc_symbol_cached (bytes-ptr len result-slot)
      (let* ((base (heap-read-u64 268436288 0)))
        (if (= base 0) (nl_alloc_symbol bytes-ptr len result-slot)
          (if (< len 1) (nl_alloc_symbol bytes-ptr len result-slot)
            (let* ((ent (+ (+ base 67104768) (* (nl_symcache_index bytes-ptr len) 16)))
                   (buf (heap-read-u64 ent 0)))
              (if (and (= (heap-read-u64 ent 8) len)
                       (and (>= buf (+ base 16777224))
                            (and (< buf (heap-read-u64 268436296 0))
                                 (= (nl_symcache_eq bytes-ptr buf len) 1))))
                  (nl_intern_write_sexp result-slot buf len)
                (seq
                 (nl_alloc_symbol bytes-ptr len result-slot)
                 (let* ((nb (heap-read-u64 result-slot 16)))
                   (if (and (>= nb (+ base 16777224))
                            (< nb (heap-read-u64 268436296 0)))
                       (seq (ptr-write-u64 ent 0 nb) (ptr-write-u64 ent 8 len))
                     0))
                 result-slot)))))))
        ;; The miss-only heap fallback allocates before rereading the rooted input
    ;; string. A GC during `alloc-bytes' may move its byte buffer.
    (defun nl_alloc_symbol_plain_named (name-slot n result-slot)
      (let* ((alloc-n (if (= n 0) 1 n))
             (char-buf (alloc-bytes alloc-n 1)))
        (nl_alloc_symbol_write (str-bytes-ptr name-slot) n alloc-n
                               result-slot char-buf)))
    ;; Doc 215 word ABI: the result WORD, or 0 after a stashed generic error when the legacy form answered 0.
    ;; `nl_alloc_symbol_plain_named' keeps its C-ABI (nonzero/0) and is unchanged.
    (defun nl_alloc_symbol_plain_named_w (name-slot n _pad)
      (let* ((result-slot (alloc-bytes 32 8)))
        (if (/= (nl_alloc_symbol_plain_named name-slot n result-slot) 0)
            result-slot
          (nl_abi_fail 1 0))))
    (defun nl_intern_finish_named (slot n name-slot result-slot)
      (if (= slot 0)
          (nl_alloc_symbol_plain_named name-slot n result-slot)
        (if (= (heap-read-u64 slot 0) 0)
            (if (= (nl_intern_arena_fits n) 1)
                (nl_intern_insert slot (str-bytes-ptr name-slot) n result-slot
                                  (nl_intern_bump n))
              (nl_alloc_symbol_plain_named name-slot n result-slot))
          (nl_intern_write_sexp result-slot (heap-read-u64 slot 8) n))))
    ;; Doc 215 word ABI: the result WORD, or 0 after a stashed generic error when the legacy form answered 0.
    ;; `nl_intern_finish_named' keeps its C-ABI (nonzero/0) and is unchanged.
    (defun nl_intern_finish_named_w (slot n name-slot _pad)
      (let* ((result-slot (alloc-bytes 32 8)))
        (if (/= (nl_intern_finish_named slot n name-slot result-slot) 0)
            result-slot
          (nl_abi_fail 1 0))))
    ;; NAME-SLOT is a borrowed, caller-rooted string Sexp for the whole call.
    ;; Re-read its byte pointer after any allocating fallback. Existing
    ;; intern rows retain their first canonical string.
    (defun nl_alloc_symbol_named (bytes-ptr len name-slot result-slot)
      (let* ((n (if (< len 0) 0 len))
             (probe (if (or (= (nl_name_is_nil bytes-ptr n) 1)
                            (= (nl_name_is_t bytes-ptr n) 1)
                            (= (heap-read-u64 268436288 0) 0)) 0
                      (nl_intern_probe
                       (heap-read-u64 268436288 0)
                       (logand (nl_intern_hash bytes-ptr 0 n 2166136261) 1048575)
                       bytes-ptr n 0)))
             (was-present (if (= probe 0) 0
                            (if (= (heap-read-u64 probe 0) (+ n 1)) 1 0))))
        (if (= (nl_name_is_nil bytes-ptr n) 1)
            (nl_write_canonical_nil result-slot)
          (if (= (nl_name_is_t bytes-ptr n) 1)
              (nl_write_canonical_t result-slot)
            (let* ((slot (if (= (heap-read-u64 268436288 0) 0) 0
                           (nl_intern_probe
                            (heap-read-u64 268436288 0)
                            (logand (nl_intern_hash bytes-ptr 0 n 2166136261) 1048575)
                            bytes-ptr n 0))))
              (if (= (nl_intern_finish_named slot n name-slot result-slot) 0) 0
                (if (= was-present 1) 1
                  (nl_symbol_name_register
                   1 (heap-read-u64 result-slot 16) n name-slot))))))))
    ;; Doc 215 word ABI: the result WORD, or 0 after a stashed generic error when the legacy form answered 0.
    ;; `nl_alloc_symbol_named' keeps its C-ABI (nonzero/0) and is unchanged.
    (defun nl_alloc_symbol_named_w (bytes-ptr len name-slot _pad)
      (if (= (nl_name_is_nil bytes-ptr len) 1) 3
        (if (= (nl_name_is_t bytes-ptr len) 1) 7
          (let ((object (alloc-bytes 32 8)))
            (if (/= (nl_alloc_symbol_named bytes-ptr len name-slot object) 0)
                object (nl_abi_fail 1 0))))))

    ;; NAME-SLOT is caller-rooted through allocation; bytes are reread from
    ;; that updated Sexp only after the destination buffer has been allocated.
    (defun nl_alloc_uninterned_symbol_named (bytes-ptr n identity name-slot result-slot)
      (let* ((w (nl_sym_make_uninterned name-slot 0)))
        (if (= w 0) 0 (seq (nl_sym_view_into w result-slot) 1))))
    ;; Doc 215 word ABI: the result WORD, or 0 after a stashed generic error when the legacy form answered 0.
    ;; `nl_alloc_uninterned_symbol_named' keeps its C-ABI (nonzero/0) and is unchanged.
    (defun nl_alloc_uninterned_symbol_named_w (bytes-ptr n identity name-slot _pad)
      (let* ((w (nl_sym_make_uninterned name-slot 0)))
        (if (= w 0) (nl_abi_fail 1 0) w)))

    ;; Return the canonical original name Sexp for a symbol, lazily creating
    ;; it for symbols produced by the reader's byte-only construction path.
    (defun nl_symbol_name_value (env symbol-slot out)
      ;; Doc 226 D9: a symbol object (or the view of one) holds its name.
      ;; Answers 1 after writing OUT, as the C-ABI forms below do.
      (if (= (logand symbol-slot 7) 6)
          (seq (nl_sexp_clone_into (heap-read-u64 (- symbol-slot 6) 8) out) 1)
      (if (if (= (heap-read-u8 symbol-slot 0) 16)
              (> (heap-read-u64 symbol-slot 8) 65535) 0)
          (seq (nl_sexp_clone_into (heap-read-u64 (heap-read-u64 symbol-slot 8) 8) out) 1)
      (let* ((tag (heap-read-u64 symbol-slot 0)))
        (if (= tag 4)
            (let* ((key (heap-read-u64 symbol-slot 16))
                   (len (heap-read-u64 symbol-slot 24))
                   (entry (nl_symbol_name_entry_for 1 key len)))
              (if (/= entry 0)
                  (nl_symbol_name_copy_out entry out)
                (nl_symbol_name_value_create env symbol-slot out)))
          (if (= tag 16)
              (let* ((identity (heap-read-u64 symbol-slot 8))
                     (key (heap-read-u64 symbol-slot 16))
                     (len (heap-read-u64 symbol-slot 24))
                     (entry (nl_symbol_name_entry_for 2 identity len)))
                (if (/= entry 0)
                    (nl_symbol_name_copy_out entry out)
                  (nl_symbol_name_value_create env symbol-slot out)))
            0))))))
    ;; The value `symbol-name' answers for the Sexp slot OUT it filled: the WORD of
    ;; the canonical String when OUT holds a view of one, else the slot itself (a
    ;; legacy name box).  The evaluator keeps a builtin's result as it is in the
    ;; argument list of its caller, so `(list (symbol-name S) 1)' held a 32-byte
    ;; box whose edge points at a String header in the pool; an image dump has no
    ;; relocation for that edge (a view is not a WORD field), and the restored
    ;; image's first collection followed it into the dead pool.  The String WORD is
    ;; what the string builtins answer, and what the dump relocates.
    (defun nl_symbol_name_result (out)
      (let* ((h (nl_string_view_header out)))
        (if (= h 0) out (+ h 4))))
    ;; GNU's Fsymbol_name answers XSYMBOL (w)->u.s.name itself.  Decision item A: the
    ;; name cell of a symbol object W (interned or not) holds that String's WORD, which
    ;; is the answer, with no copy of the cell into a slot; 0 when W is not a symbol
    ;; object whose name cell holds a String WORD (an object not yet named, a view), and
    ;; the general path answers.
    (defun nl_symbol_name_fast (w)
      (if (= (logand w 7) 6)
          (let* ((k (logand (heap-read-u64 (- w 6) 0) 63))
                 (nm (heap-read-u64 (- w 6) 8)))
            (if (= (logand nm 7) 4)
                (if (= k 4) nm (if (= k 16) nm 0))
              0))
        0))
    ;; Doc 215 word ABI: the result WORD, or 0 after a stashed generic error when the legacy form answered 0.
    ;; `nl_symbol_name_value' keeps its C-ABI (nonzero/0) and is unchanged.
    (defun nl_symbol_name_value_w (env symbol-slot _pad)
      (let* ((out (alloc-bytes 32 8)))
        (if (/= (nl_symbol_name_value env symbol-slot out) 0)
            out
          (nl_abi_fail 1 0))))

    (defun nl_symbol_name_value_create (env symbol-slot out)
      (let* ((marker (nl_root_mark env))
             (symbol-root (nl_root_reserve_checked env)))
        (if (= symbol-root 0) 0
          (let* ((tmp (nl_root_reserve_checked env)))
            (if (= tmp 0)
                (seq (nl_root_release env marker) 0)
              (seq
               (nl_symname_copy32 symbol-slot symbol-root 0)
               ;; Allocate destination first. This may collect/compact; the
               ;; rooted symbol slot is reread afterwards for its moved key.
               (let* ((len (heap-read-u64 symbol-root 24))
                      (alloc-n (if (= len 0) 1 len))
                      (buf (alloc-bytes alloc-n 1)))
                 (let* ((tag (heap-read-u64 symbol-root 0))
                        (kind (if (= tag 4) 1 2))
                        (name-key (if (= tag 4) (heap-read-u64 symbol-root 16)
                                    (heap-read-u64 symbol-root 8)))
                        (key (heap-read-u64 symbol-root 16))
                        (name-tag 14)
                        (i 0))
                   ;; Reader-created symbols have no original string Sexp.
                   ;; Establish ASCII byte kind before canonical registration
                   ;; so every returned alias uses the same representation.
                   (while (< i len)
                     (if (>= (heap-read-u8 key i) 128) (setq name-tag 5) 0)
                     (setq i (+ i 1)))
                   (if (= (nl_alloc_str_write_tag key len alloc-n tmp buf name-tag) 0)
                       (seq (nl_root_release env marker) 0)
                     (if (= (nl_symbol_name_register kind name-key len tmp) 1)
                         (seq (nl_symname_copy32 tmp out 0)
                              (nl_root_release env marker) 1)
                       (seq (nl_root_release env marker) 0)))))))))))
    ;; Doc 215 word ABI: the result WORD, or 0 after a stashed generic error when the legacy form answered 0.
    ;; `nl_symbol_name_value_create' keeps its C-ABI (nonzero/0) and is unchanged.
    (defun nl_symbol_name_value_create_w (env symbol-slot _pad)
      (let* ((out (alloc-bytes 32 8)))
        (if (/= (nl_symbol_name_value_create env symbol-slot out) 0)
            out
          (nl_abi_fail 1 0))))

    (defun nl_symbol_name_immediate (env tag out)
      (let* ((state (if (= tag 0) (data-addr nl_symbol_name_nil)
                      (data-addr nl_symbol_name_t))))
        (if (= (heap-read-u64 state 0) 14)
            (seq (nl_symname_copy32 state out 0) 1)
          (let* ((marker (nl_root_mark env))
                 (tmp (nl_root_reserve_checked env))
                 (len (if (= tag 0) 3 1)))
            (if (= tmp 0) 0
              (let* ((buf (if (= tag 0) (data-addr nl_symbol_name_nil_bytes)
                            (data-addr nl_symbol_name_t_bytes))))
                (seq
                 (if (= tag 0)
                     (seq (ptr-write-u8 buf 0 110) (ptr-write-u8 buf 1 105)
                          (ptr-write-u8 buf 2 108))
                   (ptr-write-u8 buf 0 116))
               (if (= (nl_alloc_str buf len tmp) 0) 0
                 (seq (ptr-write-u8 tmp 0 14)
                      (nl_symname_copy32 tmp state 0)
                      (nl_symname_copy32 tmp out 0)
                      1))
               (nl_root_release env marker)
               (if (= (heap-read-u64 state 0) 14) 1 0))))))))
    ;; Doc 215 word ABI: the result WORD, or 0 after a stashed generic error when the legacy form answered 0.
    ;; `nl_symbol_name_immediate' keeps its C-ABI (nonzero/0) and is unchanged.
    (defun nl_symbol_name_immediate_w (env tag _pad)
      (let* ((out (alloc-bytes 32 8)))
        (if (/= (nl_symbol_name_immediate env tag out) 0)
            out
          (nl_abi_fail 1 0))))

    ;; ---- symbol-name LOOKUP WITHOUT INSERT (Doc 163 Phase C) ----
    ;; `nl_alloc_symbol' always inserts on a miss (= `intern' semantics: the
    ;; probed slot is either already occupied by a matching name, or is the
    ;; first empty slot found by `nl_intern_probe', and `nl_intern_finish'
    ;; unconditionally fills it in via `nl_intern_insert').  There was no
    ;; entry point that probes the SAME open-addressing table and reports a
    ;; miss WITHOUT writing anything -- so the elisp `intern-soft' (see
    ;; lisp/nelisp-stdlib-misc.el) had no primitive to dispatch to and fell
    ;; back to calling `intern' itself, i.e. it could never soft-fail (Doc
    ;; 163: this hung the Gnus message.el `cited-text-face' probe loop into
    ;; an unbounded interned-name allocation loop, exhausting the `ulimit -v'
    ;; address space after ~40s / ~2.6GB and exiting rc=88).
    ;;
    ;; `nl_intern_lookup_finish' mirrors `nl_intern_finish' but on a miss
    ;; (slot's len-word still 0) returns the sentinel 0 INSTEAD of calling
    ;; `nl_intern_insert' -- no buffer copy, no slot write, no bump-pointer
    ;; advance, no Sexp written to RESULT-SLOT.  On a hit it is byte-for-byte
    ;; the same as `nl_intern_finish''s hit arm (writes the EXISTING interned
    ;; buffer's Sexp via `nl_intern_write_sexp' and returns RESULT-SLOT, a
    ;; non-zero address).  0 vs. a non-zero pointer is therefore an
    ;; unambiguous miss/hit signal for the caller, following the same
    ;; 0-sentinel convention `nl_os_alloc_chunk' / `nl_intern_region_init'
    ;; already use elsewhere in this codebase.
    (defun nl_intern_lookup_finish (slot n result-slot)
      (if (= slot 0)
          0
        (if (= (heap-read-u64 slot 0) 0)
            0
          (nl_intern_write_sexp result-slot (heap-read-u64 slot 8) n))))
    ;; Doc 215 word ABI: the result WORD, or 3 (nil) when the legacy form answered 0 (a miss, no signal).
    ;; `nl_intern_lookup_finish' keeps its C-ABI (nonzero/0) and is unchanged.
    (defun nl_intern_lookup_finish_w (slot n _pad)
      (let* ((result-slot (alloc-bytes 32 8)))
        (if (/= (nl_intern_lookup_finish slot n result-slot) 0)
            result-slot
          3)))

    ;; Probe-only counterpart of `nl_alloc_symbol_pos'.  "nil"/"t" short-
    ;; circuit to a HIT on the canonical immediate value first: they are
    ;; always considered interned (matching host Emacs, where `intern-soft'
    ;; on either never fails) even though the table itself never holds an
    ;; entry for them -- `nl_alloc_symbol_pos' above never inserts one.
    ;; Otherwise, when the intern region has not been mapped (+832 == 0)
    ;; there is no table to probe and therefore nothing can ever have been
    ;; recorded as "interned" through it -- report a miss (0) rather than
    ;; falling back to an allocator (a lookup must never allocate or have a
    ;; side effect, unlike `nl_alloc_symbol_pos''s region-disabled fallback,
    ;; which intentionally allocates a fresh per-occurrence buffer for
    ;; `intern').
    (defun nl_intern_lookup_pos (bytes-ptr n result-slot)
      (if (= (nl_name_is_nil bytes-ptr n) 1)
          (nl_write_canonical_nil result-slot)
        (if (= (nl_name_is_t bytes-ptr n) 1)
            (nl_write_canonical_t result-slot)
          (if (= (heap-read-u64 268436288 0) 0)
              0
            (nl_intern_lookup_finish
             (nl_intern_probe (heap-read-u64 268436288 0)
                              (logand (nl_intern_hash bytes-ptr 0 n 2166136261) 1048575)
                              bytes-ptr n 0)
             n result-slot)))))
    ;; Doc 215 word ABI: the result WORD, or 3 (nil) when the legacy form answered 0 (a miss, no signal).
    ;; `nl_intern_lookup_pos' keeps its C-ABI (nonzero/0) and is unchanged.
    (defun nl_intern_lookup_pos_w (bytes-ptr n _pad)
      (if (= (nl_name_is_nil bytes-ptr n) 1) 3
        (if (= (nl_name_is_t bytes-ptr n) 1) 7
          (let* ((table (heap-read-u64 268436288 0))
                 (slot (if (= table 0) 0
                         (nl_intern_probe table
                                          (logand (nl_intern_hash bytes-ptr 0 n 2166136261) 1048575)
                                          bytes-ptr n 0))))
            (if (= slot 0) 3
              (if (= (heap-read-u64 slot 0) 0) 3
                ;; A miss creates no view. On a hit, allocation may run callbacks that
                ;; unintern or replace the probed name, so repeat the original lookup.
                (let ((object (alloc-bytes 32 8)))
                  (if (/= (nl_intern_lookup_pos bytes-ptr n object) 0) object 3))))))))

    ;; Public entry: nl_intern_lookup(bytes_ptr, len, result_slot).
    ;; Returns 0 (RESULT-SLOT left untouched) when NAME has never been
    ;; interned; otherwise writes the existing interned Symbol Sexp into
    ;; RESULT-SLOT and returns RESULT-SLOT.  Performs no allocation, no
    ;; table insert, and no bump-pointer advance on either path -- a pure
    ;; read.  This is the primitive the elisp `intern-soft' dispatches to
    ;; for its string-argument case (see `bf_intern_soft' in
    ;; scripts/nelisp-standalone-build.el and `intern-soft' in
    ;; lisp/nelisp-stdlib-misc.el).
    (defun nl_intern_lookup (bytes-ptr len result-slot)
      (nl_intern_lookup_pos bytes-ptr (if (< len 0) 0 len) result-slot))
    ;; Doc 215 word ABI: the result WORD, or 3 (nil) when the legacy form answered 0 (a miss, no signal).
    ;; `nl_intern_lookup' keeps its C-ABI (nonzero/0) and is unchanged.
    (defun nl_intern_lookup_w (bytes-ptr len _pad)
      (nl_intern_lookup_pos_w bytes-ptr (if (< len 0) 0 len) 0))

    ;; The canonical interned name buffer of BYTES-PTR/N, 0 when the name
    ;; is not in the intern table (region disabled, plain overflow name).
    (defun nl_intern_canonical_buf (bytes-ptr n)
      (if (= (heap-read-u64 268436288 0) 0)
          0
        (let* ((slot (nl_intern_probe
                      (heap-read-u64 268436288 0)
                      (logand (nl_intern_hash bytes-ptr 0 n 2166136261) 1048575)
                      bytes-ptr n 0)))
          (if (= slot 0) 0
            (if (= (heap-read-u64 slot 0) 0) 0 (heap-read-u64 slot 8))))))
    ;; The cell index of the interned name BYTES-PTR/N, assigned from the
    ;; arena counter on first use (see `nl_intern_bump').  0 when the name
    ;; has no canonical buffer: such keys keep the hashed mirror path, like
    ;; GNU's non-SYMBOL_PLAINVAL redirects falling back to Fsymbol_value.
    ;; Only the mirror's entry writers call this, never a reader.
    ;; The header's low 32 bits hold the cell index, its high 32 bits the
    ;; static symbol id (`nl_sym_set_sid').
    ;; Doc 226 D6: a key whose bytes are a canonical buffer (a symbol view
    ;; or a key string sharing it) is its own symbol: use that buffer's
    ;; header, not the name's current table entry, which after a global
    ;; `unintern' names a different symbol.  Other keys map by name.
    (defun nl_symcell_assign (bytes-ptr n)
      (let* ((base (heap-read-u64 268436288 0))
             (buf (if (if (= base 0) 0
                        (if (> bytes-ptr (+ base 16777224))
                            (< bytes-ptr (heap-read-u64 268436296 0))
                          0))
                      bytes-ptr
                    (nl_intern_canonical_buf bytes-ptr n))))
        (if (= buf 0) 0
          (let* ((hdr (heap-read-u64 (- buf 8) 0))
                 (idx (logand hdr 4294967295)))
            (if (/= idx 0) idx
              (let* ((counter (+ (heap-read-u64 268436288 0) 16777216))
                     (next (+ (heap-read-u64 counter 0) 1)))
                (seq (ptr-write-u64 counter 0 next)
                     (ptr-write-u64 (- buf 8) 0 (+ hdr next))
                     next)))))))
    ;; GNU lisp.h DEFSYM / lispsym: a builtin symbol has a fixed index, and
    ;; EQ (x, Qcar) is one compare.  NeLisp gives the interned names the
    ;; build lists (`nelisp-standalone--static-symbols') a static symbol id
    ;; in the high 32 bits of their name header; generated code compares
    ;; ids where it compared name bytes, and keeps the byte compare for a
    ;; symbol without an id (uninterned, plain overflow name, or a string).
    (defun nl_sym_set_sid (bytes-ptr n sid)
      (let* ((buf (nl_intern_canonical_buf bytes-ptr n)))
        (if (= buf 0) 0
          (let* ((hdr (heap-read-u64 (- buf 8) 0)))
            (seq (ptr-write-u64 (- buf 8) 0
                                (+ (logand hdr 4294967295) (shl sid 32)))
                 1)))))
    ;; Intern BYTES-PTR/N (SCRATCH receives a symbol view) and give it SID.
    (defun nl_sym_init_one (bytes-ptr n sid scratch)
      (seq (nl_alloc_symbol bytes-ptr n scratch)
           (nl_sym_set_sid bytes-ptr n sid)))

    ;; Internal read-only cursor used by native diagnostics and future
    ;; obarray work.  CURSOR is a slot index in [0, 2^20].  A hit writes the
    ;; current symbol value and returns its slot index + 1; -1 means end or
    ;; an invalid cursor, and leaves RESULT-SLOT untouched.  This deliberately
    ;; does not call Lisp or allocate: interned name bytes live in the
    ;; permanent mmap region and the caller owns RESULT-SLOT.
    (defun nl_intern_next_scan (table cursor dest-slot)
      (let* ((scan cursor) (slot 0) (word 0))
        (while (and (< scan 1048576) (= word 0))
          (setq slot (+ table (* scan 16)))
          (setq word (heap-read-u64 slot 0))
          ;; A removed name (`nl_intern_remove') is not enumerated.
          (if (/= (logand word 4611686018427387904) 0) (setq word 0) 0)
          (if (= word 0) (setq scan (+ scan 1)) 0))
        (if (= word 0)
            -1
          (seq
           (nl_intern_write_sexp
            dest-slot (heap-read-u64 slot 8) (- word 1))
           (+ scan 1)))))
    ;; GNU lread.c:4858 Funintern on the initial obarray (Doc 226 D6).  The
    ;; name's slot becomes a tombstone: its length word gains bit 62, which
    ;; no probe matches and which keeps probe chains intact.  A later intern
    ;; of the name makes a new canonical buffer, a new symbol, while the
    ;; removed symbol keeps its buffer, its object and its cells.  BUF is
    ;; the symbol's canonical buffer; 1 when it was the interned one.
    (defun nl_intern_remove (buf n)
      (if (= (heap-read-u64 268436288 0) 0) 0
        (let* ((slot (nl_intern_probe
                      (heap-read-u64 268436288 0)
                      (logand (nl_intern_hash buf 0 n 2166136261) 1048575)
                      buf n 0)))
          (if (= slot 0) 0
            (if (= (heap-read-u64 slot 0) 0) 0
              (if (/= (heap-read-u64 slot 8) buf) 0
                (seq (ptr-write-u64 slot 0 (logior (heap-read-u64 slot 0)
                                                   4611686018427387904))
                     1)))))))
    (defun nl_intern_next (cursor dest-slot)
      (if (or (< cursor 0) (>= cursor 1048576)
              (= (heap-read-u64 268436288 0) 0))
          -1
        (nl_intern_next_scan (heap-read-u64 268436288 0) cursor dest-slot)))
  )
  "AOT direct-symbol source for `nl_alloc_str' + `nl_alloc_symbol' +
`nl_intern_lookup'.

Single `(seq DEFUN ...)' manifest.  Native symbols exported by this object:
- `nl_alloc_str'      — allocate + intern-independent Sexp::Str (tag=5).
- `nl_alloc_symbol'   — allocate-or-intern Sexp::Symbol (tag=4); always
  succeeds, inserting into the intern region's open-addressing table on
  a miss (= `intern' semantics).
- `nl_intern_lookup'  — PROBE-ONLY counterpart of `nl_alloc_symbol' (Doc
  163 Phase C): returns 0 on a miss without inserting; on a hit, writes
  the existing interned Sexp::Symbol into RESULT-SLOT and returns
  RESULT-SLOT.  Backs `intern-soft''s real soft-fail path.
- `nl_intern_next'    — internal native read-only cursor over occupied
  slots.  It is not registered as a Lisp builtin; a hit writes one symbol
  and returns the next cursor, while end/invalid cursors return -1 without
  touching RESULT-SLOT.

Private helpers (shared within this `.o'):
- `nl_alloc_str_copy_loop' / `nl_alloc_str_write' / `nl_alloc_str_pos'
  — Sexp::Str allocation chain.
- `nl_name_is_nil' / `nl_name_is_t' / `nl_write_canonical_nil' /
  `nl_write_canonical_t' — nil/t name canonicalization: `nl_alloc_symbol_pos'
  and `nl_intern_lookup_pos' both check these FIRST so every caller of
  `nl_alloc_symbol' / `nl_intern_lookup' (not just `intern' / `intern-soft')
  gets the canonical tag=0/tag=1 immediate value for the names nil and t
  rather than a tag=4 Symbol object that merely prints the same way.
- `nl_alloc_symbol_write' / `nl_alloc_symbol_pos'
  — Sexp::Symbol allocate-or-intern chain (calls `nl_intern_finish' on
  a set-up region).
- `nl_intern_hash' / `nl_intern_eq' / `nl_intern_slotmatch' /
  `nl_intern_probe' / `nl_intern_arena_fits' / `nl_intern_bump' /
  `nl_intern_write_sexp' / `nl_intern_insert' / `nl_alloc_symbol_plain' /
  `nl_intern_finish'
  — open-addressing intern-region table ops (Doc 08 §8.16); `nl_intern_probe'
  returns the address of either the first empty slot or a matching slot --
  or 0 once it has stepped through all 2^20 slots (table full) -- and is
  shared, read-only, by BOTH the insert path (`nl_intern_finish') and the
  lookup-only path (`nl_intern_lookup_finish') below.  A full table or a
  full name arena degrades to `nl_alloc_symbol_plain' (2026-09-06); before
  that the probe spun forever and the bump pointer ran off the mapping.
- `nl_intern_lookup_finish' / `nl_intern_lookup_pos'
  — lookup-only chain backing `nl_intern_lookup' (Doc 163 Phase C).
- `nl_intern_next_scan'
  — iterative table scan backing the internal enumeration cursor; it calls
  no allocator or Lisp callback and only reads table slots / writes the
  caller-owned Sexp value.

Originally replaced the Rust `#[no_mangle]' bodies for both `nl_alloc_str'
and `nl_alloc_symbol' in nlstr.rs (lines 102-118, 17 LOC across both) plus
the private `build_string' helper (lines 79-87, 9 LOC) which had no other
callers.  `nl_intern_lookup' and its helpers are new elisp-only additions
(Doc 163) with no Rust precursor.

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
    ;; Shared boxed-string writer.  TAG is 6 for UTF-8 MutStr and 15 for
    ;; raw-byte UnibyteMutStr; their NlStr layouts are identical.
    ;; alloc-n:     allocated char-buffer capacity (u64)
    ;; result-slot: *mut Sexp output
    ;; nlstr-box:   fresh 32-byte NlStr allocation
    ;; char-buf:    fresh alloc-n-byte char buffer
    (defun nl_alloc_mut_str_write_tag
        (alloc-n dest-slot nlstr-box char-buf tag)
      (and
       (ptr-write-u64 nlstr-box 0  alloc-n)    ; String.cap
       (ptr-write-u64 nlstr-box 8  char-buf)   ; String.ptr
       (ptr-write-u64 nlstr-box 16 0)          ; String.len = 0
       (ptr-write-u64 nlstr-box 24 1)          ; refcount = 1
       (ptr-write-u8  dest-slot 0 tag)
       (ptr-write-u64 dest-slot 8 nlstr-box) ; NlStr*
       dest-slot))

    (defun nl_alloc_mut_str_write (alloc-n result-slot nlstr-box char-buf)
      (and
       (ptr-write-u64 nlstr-box 0  alloc-n)
       (ptr-write-u64 nlstr-box 8  char-buf)
       (ptr-write-u64 nlstr-box 16 0)
       (ptr-write-u64 nlstr-box 24 1)
       (ptr-write-u8  result-slot 0 6)
       (ptr-write-u64 result-slot 8 nlstr-box)
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
      (nl_alloc_mut_str_pos (if (< cap 0) 0 cap) result-slot))

    (defun nl_alloc_unibyte_mut_str_inner
        (alloc-n dest-slot nlstr-box)
      (nl_alloc_mut_str_write_tag
       alloc-n dest-slot nlstr-box (alloc-bytes alloc-n 1) 15))

    (defun nl_alloc_unibyte_mut_str_pos (n dest-slot)
      (nl_alloc_unibyte_mut_str_inner
       (if (= n 0) 1 n) dest-slot (alloc-bytes 32 8)))

    (defun nl_alloc_unibyte_mut_str (cap result-slot)
      (nl_alloc_unibyte_mut_str_pos
       (if (< cap 0) 0 cap) result-slot))
    ;; Doc 215 builtin word ABI: nl_alloc_unibyte_mut_str_w returns the result WORD; nl_alloc_unibyte_mut_str keeps the direct slot-writing form (hot leaf).
    (defun nl_alloc_unibyte_mut_str_w (cap _pad)
      (let* ((s (alloc-bytes 32 8)))
        (seq (nl_alloc_unibyte_mut_str cap s) s))))
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
      (let ((k i))
        (while (<= (+ k 8) n)
          (and (ptr-write-u64 dst k (heap-read-u64 src k)) (setq k (+ k 8))))
        (while (< k n)
          (and (ptr-write-u8 dst k (heap-read-u8 src k)) (setq k (+ k 1))))
        1))

    ;; Innermost writers: the tag-6 builder finalizes through the historical
    ;; tag-5 producer, while a tag-15 builder uses the layout-identical tag-14
    ;; writer below.
    ;; str-ptr: *const u8 source data, str-len: byte count,
    ;; alloc-n: allocated cap, result-slot: output, new-buf: fresh allocation.
    (defun nl_mut_str_finalize_write
        (str-ptr str-len alloc-n result-slot new-buf)
      (and
       (nl_mut_str_finalize_copy_loop str-ptr new-buf 0 str-len)
       (ptr-write-u8  result-slot 0  5)
       (ptr-write-u64 result-slot 8  alloc-n)
       (ptr-write-u64 result-slot 16 new-buf)
       (ptr-write-u64 result-slot 24 str-len)
       result-slot))

    (defun nl_mut_str_finalize_unibyte_write
        (str-ptr str-len alloc-n result-slot new-buf)
      (and
       (nl_mut_str_finalize_copy_loop str-ptr new-buf 0 str-len)
       (ptr-write-u8  result-slot 0  14)
       (ptr-write-u64 result-slot 8  alloc-n)
       (ptr-write-u64 result-slot 16 new-buf)
       (ptr-write-u64 result-slot 24 str-len)
       result-slot))

    ;; Given str-ptr + str-len + alloc-n + result-slot, allocate new-buf.
    (defun nl_mut_str_finalize_alloc
        (str-ptr str-len alloc-n result-slot out-tag)
      (if (= out-tag 14)
          (nl_alloc_unibyte_str str-ptr str-len result-slot)
        (nl_alloc_str str-ptr str-len result-slot)))

    ;; Given nlstr* + result-slot + str-len (already read), read str-ptr.
    (defun nl_mut_str_finalize_inner
        (nlstr result-slot str-len out-tag)
      (nl_mut_str_finalize_alloc
       (heap-read-u64 nlstr 8)               ; str-ptr = NlStr.value.ptr
       str-len
       (if (= str-len 0) 1 str-len)         ; alloc-n = max(str-len, 1)
       result-slot out-tag))

    ;; Given ptr (Sexp*) + result-slot + nlstr (already read from ptr+8).
    (defun nl_mut_str_finalize_nlstr (ptr result-slot nlstr out-tag)
      (nl_mut_str_finalize_inner
       nlstr result-slot
       (heap-read-u64 nlstr 16) out-tag))     ; str-len = NlStr.value.len

    ;; Public entry: nl_mut_str_finalize(ptr, result_slot).
    ;; ptr:         *const Sexp — source MutStr slot (tag=6).
    ;; result-slot: *mut Sexp   — destination slot to receive Sexp::Str.
    ;; Returns result-slot.  Source MutStr remains live (clone semantics).
    (defun nl_mut_str_finalize (ptr result-slot)
      (nl_mut_str_finalize_nlstr
       ptr result-slot
       (heap-read-u64 ptr 8)
       (if (= (heap-read-u8 ptr 0) 15) 14 5))))
  "AOT direct-symbol source for `nl_mut_str_finalize'.

Seven-entry `(seq DEFUN ...)' manifest:
- `nl_mut_str_finalize_copy_loop'  — tail-recursive byte copier.
- `nl_mut_str_finalize_write'      — tag-5 Sexp::Str field writer.
- `nl_mut_str_finalize_unibyte_write' — tag-14 Sexp::UnibyteStr writer.
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
