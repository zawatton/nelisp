;; Doc 135 Stage — nl_char_table_get_raw + nl_char_table_set_raw
;; STAGED (unwired -- do NOT touch manifest/build.rs)
;; Type-checked + object-compiled.  Not linked.
;;
;; Replaces deleted Rust #[no_mangle] bodies (nlchartable.rs, removed in
;; commit 60123723 "LOC compression -30 via module merges and line packing"):
;;
;;   nl_char_table_get_raw(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64
;;   nl_char_table_set_raw(arg: *const Sexp, idx: i64,
;;                         val: *const Sexp, out: *mut Sexp) -> i64
;;
;; These are the callees that nl_jit_char_table_aref / nl_jit_char_table_aset
;; (compiled from lisp/nelisp-cc-jit-char-table-aref.el / -aset.el) delegate to
;; after the CharTable tag check.
;;
;; Deleted Rust logic:
;;   fn ct_get(rc: &NlCharTableRef, c: i64) -> Sexp {
;;     rc.inner.entries.iter().find(|(k,_)| *k==c)
;;       .map(|(_,v)| v.clone())
;;       .or_else(|| rc.inner.parent.as_ref().map(|p| ct_get(p,c)))
;;       .unwrap_or_else(|| rc.inner.default_val.clone())
;;   }
;;   fn ct_set(i: &mut CharTableInner, c: i64, v: Sexp) {
;;     match i.entries.iter_mut().find(|(k,_)| *k==c) {
;;       Some(e) => e.1 = v,
;;       None    => i.entries.push((c, v))
;;     }
;;   }
;;
;; CharTableInner layout (#[repr(C)], from build-tool/src/eval/sexp.rs):
;;   subtype      Sexp (32 bytes): box+0
;;   default_val  Sexp (32 bytes): box+32
;;   entries      Vec<(i64,Sexp)>:
;;     .ptr  (usize): box+64    — pointer to heap entry array
;;     .cap  (usize): box+72    — allocated capacity (# of entries)
;;     .len  (usize): box+80    — used length (# of entries)
;;   parent       Option<NlCharTableRef> (8 bytes): box+88  (0 = None)
;;   extra        Vec<Sexp> (24 bytes): box+96..119
;;
;;   Vec<(i64,Sexp)> entry layout (40 bytes each, align 8):
;;     key  i64  (8 bytes): entry+0
;;     val  Sexp (32 bytes): entry+8
;;
;;   Sexp::CharTable tag = 9, payload (box ptr) at sexp+8.
;;
;; Tag / OK / ERR constants:
;;   SEXP_TAG_CHAR_TABLE  = 9
;;   TRAMPOLINE_OK        = 0
;;   TRAMPOLINE_ERR       = 1
;;
;; nm verification (libnelisp_elisp_spike.a, 2761033c43e4fcda build):
;;   nelisp_ptr_read_u8     T 0x0000
;;   nelisp_ptr_write_u8    T 0x0000
;;   nelisp_alloc_bytes     T 0x0000
;;   nelisp_dealloc_bytes   T 0x0000
;;   nl_sexp_clone_into     T 0x0472  (Phase 47 compiled; takes (dst, src))
;;
;; nl_jit_char_table_aref / nl_jit_char_table_aset status in same archive:
;;   T (defined) — call THROUGH to nl_char_table_get_raw / nl_char_table_set_raw
;;   which appear as U (undefined) until this .nl is wired in.
;;
;; Semantic note on update path (ct_set update branch):
;;   nl_sexp_clone_into(dst, src) performs ptr::write(dst, clone(src)) without
;;   dropping the old value at dst.  The in-place update path therefore has a
;;   refcount-leak on the overwritten Sexp.  This is an accepted limitation of
;;   the staged implementation; a proper nl_sexp_replace_into (drop-then-write)
;;   C-ABI helper would fix it and is deferred to Stage linkage.
;;
;; NO *.rs files edited.  Not committed.  Not wired.

;; ---------------------------------------------------------------------------
;; sys:extern declarations — byte-level I/O + allocator + clone
;; ---------------------------------------------------------------------------

;; nelisp_ptr_read_u8(base_ptr, offset) -> i64 (zero-extended byte)
;; MOVZX RAX, BYTE PTR [rdi + rsi].
(sys:extern nelisp_ptr_read_u8
  (:symbol "nelisp_ptr_read_u8" :abi c :unsafe t)
  ((base_ptr usize) (offset i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_ptr_write_u8(base_ptr, offset, val) -> i64 (sentinel = 1)
(sys:extern nelisp_ptr_write_u8
  (:symbol "nelisp_ptr_write_u8" :abi c :unsafe t)
  ((base_ptr usize) (offset i64) (val i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_sexp_clone_into(dst, src) -> i64
;; Phase 47 compiled symbol: refcount-aware clone of *src into *dst.
;; Convention in eval-port: first arg = dst, second arg = src.
(sys:extern nl_sexp_clone_into
  (:symbol "nl_sexp_clone_into" :abi c :unsafe t)
  ((dst usize) (src usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nelisp_alloc_bytes(size, align) -> usize (raw pointer, 0 on oom)
;; Maps to std::alloc::alloc — same global allocator as Rust Vec.
(sys:extern nelisp_alloc_bytes
  (:symbol "nelisp_alloc_bytes" :abi c :unsafe t)
  ((size i64) (align i64))
  usize
  (:alloc may :ffi may :unsafe may))

;; nelisp_dealloc_bytes(ptr, size, align) -> i64
(sys:extern nelisp_dealloc_bytes
  (:symbol "nelisp_dealloc_bytes" :abi c :unsafe t)
  ((ptr usize) (size i64) (align i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; HELPER: nl_ct_copy_u64s(src, dst, i, n) -> i64
;;
;; Tail-recursive u64-word copier.  Copies n u64 words (8 bytes each) from
;; src to dst.  Used to duplicate a 40-byte entry (5 words) during Vec grow.
;; ---------------------------------------------------------------------------
(sys:defun nl_ct_copy_u64s
    ((src usize) (dst usize) (i i64) (n i64))
  i64
  (:alloc none :ffi may :unsafe may)
  (if (= i n)
      (sys:cast i64 1)
    (let ((word_off i64 (* i 8))
          (val usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ src (sys:cast usize word_off)))))))
      (sys:unsafe
       (sys:poke-u64 (+ dst (sys:cast usize word_off)) (sys:cast i64 val)))
      (nl_ct_copy_u64s src dst (+ i 1) n))))

;; ---------------------------------------------------------------------------
;; HELPER: nl_ct_copy_entries(src_ptr, dst_ptr, i, n) -> i64
;;
;; Copies n entries of 40 bytes each from src_ptr to dst_ptr.
;; Each entry = 5 u64 words (key: i64 + val: Sexp[32 bytes=4 u64 words]).
;; Does NOT update refcounts — used only for the grow-buffer migration where
;; the old buffer is about to be freed (so the copied data inherits the old
;; refcount references from the soon-to-be-freed source).
;; ---------------------------------------------------------------------------
(sys:defun nl_ct_copy_entries
    ((src_ptr usize) (dst_ptr usize) (i i64) (n i64))
  i64
  (:alloc none :ffi may :unsafe may)
  (if (= i n)
      (sys:cast i64 1)
    (let ((entry_off usize (sys:cast usize (* i 40))))
      (nl_ct_copy_u64s (+ src_ptr entry_off) (+ dst_ptr entry_off) 0 5)
      (nl_ct_copy_entries src_ptr dst_ptr (+ i 1) n))))

;; ---------------------------------------------------------------------------
;; HELPER: nl_ct_grow_entries(box, old_ptr, old_cap, old_len) -> usize
;;
;; Grows the entries Vec when len == cap.  Allocates a buffer with new_cap =
;; max(old_cap * 2, 4) entries of 40 bytes each, copies old entries, frees
;; the old buffer, and updates the Vec header fields in the box:
;;   box+64 (entries.ptr), box+72 (entries.cap), box+80 (entries.len).
;; Returns the new_ptr (usize) so the caller can immediately write a new entry.
;; ---------------------------------------------------------------------------
(sys:defun nl_ct_grow_entries
    ((box usize) (old_ptr usize) (old_cap i64) (old_len i64))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((new_cap i64 (if (< (* old_cap 2) 4) 4 (* old_cap 2)))
        (new_ptr usize (sys:unsafe (sys:cast usize (nelisp_alloc_bytes (* new_cap 40) 8)))))
    ;; Copy existing entries.
    (nl_ct_copy_entries old_ptr new_ptr 0 old_len)
    ;; Free old buffer (only if it was previously allocated, i.e., old_cap > 0).
    (if (> old_cap 0)
        (sys:unsafe (nelisp_dealloc_bytes old_ptr (* old_cap 40) 8))
      (sys:cast i64 1))
    ;; Update Vec header: ptr, cap (len stays at old_len; caller bumps it).
    (sys:unsafe
     (sys:poke-u64 (+ box 64) (sys:cast i64 new_ptr))
     (sys:poke-u64 (+ box 72) (sys:cast i64 new_cap)))
    new_ptr))

;; ---------------------------------------------------------------------------
;; ENTRY POINT: nl_char_table_get_raw(arg, idx, out) -> i64
;;
;; Implements the deleted Rust nl_char_table_get_raw:
;;   1. Tag check: sexp-tag(arg) must be 9 (Sexp::CharTable), else ERR.
;;   2. Get NlCharTable box pointer from arg+8.
;;   3. Linear search through entries Vec for key == idx.
;;      If found: nl_sexp_clone_into(out, &entry.val) + return OK.
;;   4. If not found: check parent at box+88.
;;      If parent != 0: build a synthetic CharTable Sexp + recurse.
;;      If parent == 0: clone default_val (box+32) into out + return OK.
;;
;; ---------------------------------------------------------------------------
(sys:defun nl_char_table_get_raw
    ((arg usize) (idx i64) (out usize))
  i64
  (:export "nl_char_table_get_raw" :abi c :alloc may :ffi may :unsafe may)
  ;; Step 1: tag check — byte at arg+0 must be 9.
  (let ((tag i64 (sys:unsafe (nelisp_ptr_read_u8 arg 0))))
    (if (= tag 9)
        ;; Step 2: get box pointer from payload field (arg+8).
        (let ((box usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ arg 8))))))
          ;; Step 3: linear search in entries Vec.
          ;; entries.ptr at box+64, entries.len at box+80.
          (let ((entries_ptr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ box 64)))))
                (entries_len i64   (sys:cast i64   (sys:unsafe (sys:peek-u64 (+ box 80))))))
            ;; Search loop — tail-recursive via a helper.
            (nl_ct_get_search box entries_ptr entries_len idx out 0)))
      ;; Tag != 9: ERR.
      (sys:cast i64 1))))

;; ---------------------------------------------------------------------------
;; HELPER: nl_ct_get_search(box, eptr, elen, idx, out, i) -> i64
;;
;; Searches entries[i..elen] for key == idx.  If found, clones the value.
;; If exhausted, falls back to parent or default_val.
;; ---------------------------------------------------------------------------
(sys:defun nl_ct_get_search
    ((box usize) (eptr usize) (elen i64) (idx i64) (out usize) (i i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (if (= i elen)
      ;; Exhausted entries — check parent at box+88.
      (let ((parent_ptr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ box 88))))))
        (if (= (sys:cast i64 parent_ptr) 0)
            ;; No parent: clone default_val (box+32) into out.
            (sys:unsafe
             (nl_sexp_clone_into (+ box 32) out))
          ;; Parent exists: build a synthetic CharTable Sexp on the stack,
          ;; write tag=9 + payload=parent_ptr, then recurse.
          (let ((tmp usize (sys:alloc 32 8)))
            ;; tag byte at tmp+0 = 9; write as u64 so byte[0]=9, bytes[1-7]=0.
            (sys:unsafe
             (sys:poke-u64 tmp 9))
            ;; payload (box ptr) at tmp+8.
            (sys:unsafe
             (sys:poke-u64 (+ tmp 8) (sys:cast i64 parent_ptr)))
            ;; zero pad bytes [16..31].
            (sys:unsafe
             (sys:poke-u64 (+ tmp 16) 0)
             (sys:poke-u64 (+ tmp 24) 0))
            ;; Recurse into the parent CharTable Sexp.
            (nl_char_table_get_raw tmp idx out))))
    ;; Check entries[i]: key at eptr + i*40 + 0.
    (let ((entry_base usize (+ eptr (sys:cast usize (* i 40)))))
      (let ((key i64 (sys:cast i64 (sys:unsafe (sys:peek-u64 entry_base)))))
        (if (= key idx)
            ;; Found: clone entry value (entry_base+8) into out.
            (sys:unsafe
             (nl_sexp_clone_into (+ entry_base 8) out))
          ;; Not found: continue search.
          (nl_ct_get_search box eptr elen idx out (+ i 1)))))))

;; ---------------------------------------------------------------------------
;; ENTRY POINT: nl_char_table_set_raw(arg, idx, val, out) -> i64
;;
;; Implements the deleted Rust nl_char_table_set_raw:
;;   1. Tag check: sexp-tag(arg) must be 9, else ERR.
;;   2. Get box pointer from arg+8.
;;   3. Linear search entries for key == idx:
;;      - If found (update): overwrite entry.val with clone of *val.
;;        NOTE: the old Sexp at entry.val is NOT dropped (staged limitation).
;;      - If not found (push): grow Vec if needed, append new (idx, val) entry.
;;   4. Clone *val into *out.
;;   5. Return OK.
;;
;; ---------------------------------------------------------------------------
(sys:defun nl_char_table_set_raw
    ((arg usize) (idx i64) (val usize) (out usize))
  i64
  (:export "nl_char_table_set_raw" :abi c :alloc may :ffi may :unsafe may)
  ;; Step 1: tag check.
  (let ((tag i64 (sys:unsafe (nelisp_ptr_read_u8 arg 0))))
    (if (= tag 9)
        ;; Step 2: get box pointer.
        (let ((box usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ arg 8))))))
          ;; Step 3: search + update/push.  Pass box + search args.
          ;; box re-read for eptr/ecap/elen inside the search helper to
          ;; keep param count <= 6 (backend C-ABI register limit).
          (nl_ct_set_step box idx val out 0))
      ;; Tag != 9: ERR.
      (sys:cast i64 1))))

;; ---------------------------------------------------------------------------
;; HELPER: nl_ct_set_step(box, idx, val, out, i) -> i64
;;
;; One iteration of the ct_set linear scan.  Reads entries.ptr/cap/len fresh
;; from box each call (cheap — already in L1 cache), avoiding >6-param limit.
;;
;; If i < elen and entries[i].key == idx: update in-place + finish.
;; If i == elen: push (grow if needed) + finish.
;; Else: recurse with i+1.
;;
;; Backend constraint: <= 6 parameters (registers rdi/rsi/rdx/rcx/r8/r9).
;; ---------------------------------------------------------------------------
(sys:defun nl_ct_set_step
    ((box usize) (idx i64) (val usize) (out usize) (i i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((elen i64 (sys:cast i64 (sys:unsafe (sys:peek-u64 (+ box 80))))))
    (if (= i elen)
        ;; Not found — push new entry.
        (nl_ct_push_new box idx val out elen)
      ;; Check entries[i].
      (let ((eptr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ box 64)))))
            (entry_base usize (+ eptr (sys:cast usize (* i 40)))))
        (let ((key i64 (sys:cast i64 (sys:unsafe (sys:peek-u64 entry_base)))))
          (if (= key idx)
              ;; Found: update in-place (no drop of old — staged limitation).
              (sys:unsafe
               (nl_sexp_clone_into val (+ entry_base 8))
               (nl_sexp_clone_into val out))
            ;; Continue.
            (nl_ct_set_step box idx val out (+ i 1))))))))

;; ---------------------------------------------------------------------------
;; HELPER: nl_ct_push_new(box, idx, val, out, elen) -> i64
;;
;; Appends a new (idx, val) entry to the entries Vec, growing if needed.
;; elen = current len (= insertion index).  Updates box+80 (len bump).
;; Clones val into *out.  Returns OK.
;;
;; Parameters: 5 (within 6-param backend limit).
;; ---------------------------------------------------------------------------
(sys:defun nl_ct_push_new
    ((box usize) (idx i64) (val usize) (out usize) (elen i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((eptr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ box 64)))))
        (ecap i64   (sys:cast i64   (sys:unsafe (sys:peek-u64 (+ box 72))))))
    (let ((new_eptr usize
           ;; Grow if len == cap.
           (if (= elen ecap)
               (nl_ct_grow_entries box eptr ecap elen)
             eptr)))
      (let ((new_entry usize (+ new_eptr (sys:cast usize (* elen 40)))))
        ;; Write key at new_entry+0.
        (sys:unsafe
         (sys:poke-u64 new_entry (sys:cast i64 idx)))
        ;; Clone val into value slot at new_entry+8.
        (sys:unsafe
         (nl_sexp_clone_into val (+ new_entry 8)))
        ;; Bump len in Vec header.
        (sys:unsafe
         (sys:poke-u64 (+ box 80) (sys:cast i64 (+ elen 1))))
        ;; Clone val into *out + return OK.
        (sys:unsafe
         (nl_sexp_clone_into val out))))))

;; End of Doc 135 Stage — nl_char_table_get_raw + nl_char_table_set_raw
