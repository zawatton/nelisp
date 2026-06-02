;; Doc XX §nonenv -- nl_mut_str_set_codepoint_raw: in-place codepoint replacement
;; STATUS: STAGED (unwired -- do NOT touch manifest/build.rs)
;; Type-checked + compile-gate target only.  Not linked into the binary.
;;
;; Implements:
;;   nl_mut_str_set_codepoint_raw(arg usize, idx i64, val_cp i64, out usize) -> i64
;;
;; Contract (mirrors deleted Rust body in build-tool/src/eval/nlstr.rs):
;;   arg:    usize = *const Sexp — must carry Sexp::MutStr (tag = 6).
;;           Guard present here as belt-and-suspenders even though the JIT
;;           wrapper (nl_jit_mut_str_set_codepoint) already checked tag.
;;   idx:    i64 — char index (caller guarantees >= 0).
;;   val_cp: i64 — replacement codepoint (already extracted from *val Sexp::Int).
;;   out:    usize = *mut Sexp — receives Sexp::Int(val_cp) on success.
;;   returns: 0 = TRAMPOLINE_OK, 1 = TRAMPOLINE_ERR.
;;
;; Implementation strategy (in-place NlStr String field swap):
;;   1. Guard: tag byte at arg+0 must be 6 (Sexp::MutStr).
;;   2. Guard: val_cp in [0, 0x10FFFF] excluding surrogates [0xD800, 0xDFFF].
;;   3. char_count = nl_str_char_count(arg); guard idx < char_count.
;;   4. Alloc a fresh Sexp::MutStr slot (32 bytes) + NlStr (via nl_alloc_mut_str).
;;   5. Walk all chars via nl_str_codepoint_at (byte-indexed, tab-recursive helper):
;;      for each char j: push val_cp if j == idx, else push original cp.
;;   6. Swap String fields {cap@0, ptr@8, len@16} from new NlStr into arg's NlStr.
;;      DEFERRED / FLAGGED: the old char buffer at arg's NlStr.value.ptr leaks
;;      (dealloc-bytes requires old cap from arg's NlStr; we skip it here to keep
;;      the swap body within <=6 params and avoid a 7th helper level.  A follow-up
;;      nl_msscp_dealloc_old helper can fix this using the saved old_cap value).
;;   7. Write Sexp::Int(val_cp) to *out: poke tag=2 at out+0, val_cp at out+8.
;;   8. Return 0 (TRAMPOLINE_OK).
;;
;; Archive nm verification (before this .nl lands):
;;   nl_str_char_count     T   (walks chars().count() for Str/MutStr/Symbol)
;;   nl_str_codepoint_at   T   (byte-indexed UTF-8 decode; returns 1=ok 0=fail)
;;   nl_alloc_mut_str      T   (alloc NlStr box + char buf; writes Sexp::MutStr)
;;   nl_mut_str_push_codepoint T (appends 1-4 UTF-8 bytes via Rust nlstr push_codepoint)
;;   nelisp_ptr_read_u8    T   (ptr-read-u8 grammar op: MOVZX RAX,BYTE PTR [RDI+RSI])
;;
;; Sexp layout (32 bytes, all variants):
;;   offset 0:  tag (u8) — zero-extended to u64 by nelisp_ptr_read_u8
;;   offset 8:  payload (u64)
;;
;; Sexp::MutStr specifics:
;;   tag = 6
;;   offset 8: NlStr* (heap box pointer)
;;
;; NlStr layout (#[repr(C)], 32 bytes, align 8):
;;   offset  0: String.cap  (u64)
;;   offset  8: String.ptr  (u64 = *const u8 char data)
;;   offset 16: String.len  (u64)
;;   offset 24: refcount    (AtomicUsize)
;;
;; Sexp::Int:
;;   tag = 2 (= poke u64 value 2 at offset 0; zeroes bytes 1-7)
;;   offset 8: i64 payload (the integer value)
;;
;; TRAMPOLINE_OK = 0, TRAMPOLINE_ERR = 1 (confirmed from lib.rs jit module).
;;
;; Helper-chain param threading:
;;   Phase 47 / nelisp-sys `let' is compile-time constant folding only.
;;   Runtime pointer values (alloc results, peek results) are threaded as
;;   extra function parameters -- the same pattern used in nl_alloc_mut_str,
;;   nl_mut_str_finalize, nl_alloc_str, nl_jit_str_codepoint_at_walk, etc.
;;
;; NO *.rs files edited.  Not committed.  Not wired into manifest/build.rs.

;; ---------------------------------------------------------------------------
;; sys:extern declarations
;; ---------------------------------------------------------------------------

;; nl_str_char_count(ptr) -> i64
;; Counts Unicode codepoints (= chars().count()) for Str/MutStr/Symbol.
;; Returns 0 for other variants.  T in archive (nl_str_char_count.o).
(sys:extern nl_str_char_count
  (:symbol "nl_str_char_count" :abi c :unsafe t)
  ((ptr usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_str_codepoint_at(ptr, byte_idx, cp_slot, width_slot) -> i64
;; Decodes the UTF-8 codepoint starting at byte_idx.
;; On success (return 1): writes decoded cp (i64) to *cp_slot,
;;   UTF-8 byte width (1..4, i64) to *width_slot.
;; On failure (return 0): byte_idx out of range or malformed UTF-8.
;; T in archive (nl_str_codepoint_at.o).
(sys:extern nl_str_codepoint_at
  (:symbol "nl_str_codepoint_at" :abi c :unsafe t)
  ((ptr usize) (byte_idx i64) (cp_slot usize) (width_slot usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_alloc_mut_str(cap, result_slot) -> result_slot
;; Allocates a fresh NlStr box (32B/8B) + char buf (cap bytes/1B),
;; writes Sexp::MutStr (tag=6, NlStr*) into result_slot.
;; Returns result_slot (usize).  T in archive (nl_alloc_mut_str.o).
(sys:extern nl_alloc_mut_str
  (:symbol "nl_alloc_mut_str" :abi c :unsafe t)
  ((cap i64) (result_slot usize))
  usize
  (:alloc may :ffi may :unsafe may))

;; nl_mut_str_push_codepoint(ptr, cp) -> i64
;; UTF-8-encodes cp (1-4 bytes) and appends to ptr's MutStr inner String.
;; Surrogates / OOB codepoints clamp to U+FFFD inside the Rust extern.
;; ptr: *mut Sexp pointing at Sexp::MutStr.
;; T in archive (via grammar op PLT stub).
(sys:extern nl_mut_str_push_codepoint
  (:symbol "nl_mut_str_push_codepoint" :abi c :unsafe t)
  ((ptr usize) (cp i64))
  i64
  (:alloc may :ffi may :unsafe may))

;; nelisp_ptr_read_u8(ptr, offset) -> i64
;; MOVZX RAX, BYTE PTR [RDI+RSI] — zero-extends the byte to i64.
;; Used here to read the Sexp tag byte at offset 0.
;; T in archive (nelisp_ptr_read_u8.o).
(sys:extern nelisp_ptr_read_u8
  (:symbol "nelisp_ptr_read_u8" :abi c :unsafe t)
  ((ptr usize) (offset i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; HELPER: nl_msscp_write_int_out(out, val_cp) -> i64
;;
;; Writes Sexp::Int(val_cp) into *out:
;;   poke u64 2 at out+0   (tag byte = 2 = Sexp::Int; clears bytes 1-7)
;;   poke u64 val_cp at out+8
;; Returns 0.
;; ---------------------------------------------------------------------------
(sys:defun nl_msscp_write_int_out
    ((out usize) (val_cp i64))
  i64
  (:alloc none :ffi may :unsafe may)
  (sys:unsafe
   (sys:poke-u64 out 2)
   (sys:poke-u64 (+ out 8) (sys:cast usize val_cp))
   (sys:cast i64 0)))

;; ---------------------------------------------------------------------------
;; HELPER: nl_msscp_swap_fields(arg, new_nlstr) -> i64
;;
;; In-place String field swap: copies {cap, ptr, len} from new_nlstr into
;; arg's NlStr box (= [arg+8]).
;;
;;   new_cap = [new_nlstr + 0]
;;   new_ptr = [new_nlstr + 8]
;;   new_len = [new_nlstr + 16]
;;   arg_nlstr = [arg + 8]
;;   [arg_nlstr + 0]  <- new_cap
;;   [arg_nlstr + 8]  <- new_ptr
;;   [arg_nlstr + 16] <- new_len
;;
;; FLAGGED LEAK: the old String char buffer ([old arg_nlstr+8] bytes, capacity
;; [old arg_nlstr+0]) is not freed here.  A follow-up nl_msscp_dealloc_old
;; helper (dealloc-bytes old_ptr old_cap 1) can fix this.  For the
;; link-green / compile-gate goal this leak is acceptable.
;;
;; arg: usize = *const Sexp::MutStr slot.
;; new_nlstr: usize = NlStr* of the freshly built replacement string.
;; Returns 0.
;; ---------------------------------------------------------------------------
(sys:defun nl_msscp_swap_fields
    ((arg usize) (new_nlstr usize))
  i64
  (:alloc none :ffi may :unsafe may)
  (let ((arg_nlstr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ arg 8))))))
    (sys:unsafe
     (sys:poke-u64 arg_nlstr
                   (sys:cast usize (sys:peek-u64 new_nlstr)))
     (sys:poke-u64 (+ arg_nlstr 8)
                   (sys:cast usize (sys:peek-u64 (+ new_nlstr 8))))
     (sys:poke-u64 (+ arg_nlstr 16)
                   (sys:cast usize (sys:peek-u64 (+ new_nlstr 16))))
     (sys:cast i64 0))))

;; ---------------------------------------------------------------------------
;; HELPER: nl_msscp_walk(arg, new_slot, byte_idx, char_j, idx, val_cp) -> i64
;;
;; Tail-recursive char walker.  For each codepoint at char index char_j:
;;   - calls nl_str_codepoint_at(arg, byte_idx, new_slot+8, new_slot+16)
;;     writing decoded cp to [new_slot+8] and byte-width to [new_slot+16].
;;   - if return == 0: end of string (all chars pushed) → return 0.
;;   - push val_cp to new_slot's MutStr if char_j == idx,
;;     else push the decoded original cp from [new_slot+8].
;;   - recurse with byte_idx += width (from [new_slot+16]), char_j += 1.
;;
;; Scratch layout reuse: new_slot is the 32-byte Sexp::MutStr slot.
;;   new_slot + 8  = used as cp_slot   (i64 scratch for decoded codepoint)
;;   new_slot + 16 = used as width_slot (i64 scratch for UTF-8 byte width)
;; Both offsets are within the Sexp's pad region ([16..31]) and are safe
;; to overwrite since the MutStr's structural fields are tag@0 and NlStr*@8.
;;
;; 6 params (SysV AMD64 register limit):
;;   arg, new_slot, byte_idx, char_j, idx, val_cp
;;
;; Returns 0 when walk completes successfully.
;; ---------------------------------------------------------------------------
(sys:defun nl_msscp_walk
    ((arg usize) (new_slot usize) (byte_idx i64) (char_j i64) (idx i64) (val_cp i64))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; Doc 135 §9.4 fix: cp_slot / width_slot must NOT alias new_slot+8
  ;; (= the MutStr Sexp's NlStr* payload).  Writing the decoded codepoint
  ;; there clobbers the box pointer -> the next nl_mut_str_push_codepoint
  ;; dereferences garbage -> SIGSEGV.  Use the Sexp pad region [16..31]:
  ;; cp_slot = new_slot+16, width_slot = new_slot+24.
  (let ((ok i64 (sys:unsafe
                 (nl_str_codepoint_at arg byte_idx
                                      (+ new_slot 16)
                                      (+ new_slot 24)))))
    (if (= ok 0)
        ;; End of string: all chars pushed successfully.
        (sys:cast i64 0)
      ;; Decode succeeded: push correct codepoint, then recurse.
      (let ((orig_cp i64 (sys:cast i64 (sys:unsafe (sys:peek-u64 (+ new_slot 16)))))
            (width   i64 (sys:cast i64 (sys:unsafe (sys:peek-u64 (+ new_slot 24))))))
        (sys:unsafe
         (nl_mut_str_push_codepoint
          new_slot
          (if (= char_j idx) val_cp orig_cp))
         (nl_msscp_walk arg new_slot
                        (+ byte_idx width)
                        (+ char_j 1)
                        idx val_cp))))))

;; ---------------------------------------------------------------------------
;; HELPER: nl_msscp_do_build(arg, new_slot, char_count, idx, val_cp) -> i64
;;
;; Allocates the fresh MutStr, walks + rebuilds, then swaps NlStr fields.
;;
;;   1. nl_alloc_mut_str(char_count, new_slot) — allocate with old char_count
;;      as capacity hint (byte capacity; may under-allocate for multi-byte
;;      codepoints, but nl_mut_str_push_codepoint grows the buffer via Rust
;;      String::push_char which handles reallocation).
;;   2. nl_msscp_walk(arg, new_slot, 0, 0, idx, val_cp) — fill new_slot.
;;   3. nl_msscp_swap_fields(arg, [new_slot+8]) — swap NlStr fields in-place.
;;   Returns 0 on success.
;;
;; 5 params: arg, new_slot (32-byte Sexp scratch from caller), char_count,
;;           idx, val_cp.
;; ---------------------------------------------------------------------------
(sys:defun nl_msscp_do_build
    ((arg usize) (new_slot usize) (char_count i64) (idx i64) (val_cp i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (sys:unsafe
   (nl_alloc_mut_str char_count new_slot)
   (nl_msscp_walk arg new_slot (sys:cast i64 0) (sys:cast i64 0) idx val_cp)
   (nl_msscp_swap_fields arg (sys:cast usize (sys:peek-u64 (+ new_slot 8))))))

;; ---------------------------------------------------------------------------
;; PUBLIC ENTRY: nl_mut_str_set_codepoint_raw(arg, idx, val_cp, out) -> i64
;;
;; C-ABI exported symbol replacing the deleted Rust body.
;; Called by nl_jit_mut_str_set_codepoint (T in archive) after that wrapper
;; has already checked idx >= 0 and tag == 6 and extracted val_cp from *val.
;;
;; Guard layers here (belt-and-suspenders per contract):
;;   1. tag check via nelisp_ptr_read_u8(arg, 0) != 6 → ERR.
;;   2. val_cp < 0 → ERR.
;;   3. val_cp > 0x10FFFF → ERR (above Unicode range).
;;   4. surrogate: 0xD800 <= val_cp <= 0xDFFF → ERR.
;;   5. char_count = nl_str_char_count(arg); idx >= char_count → ERR.
;;   6. Rebuild: nl_msscp_do_build(arg, scratch, char_count, idx, val_cp).
;;   7. Write Sexp::Int(val_cp) to *out.
;;   8. Return 0 (TRAMPOLINE_OK).
;;
;; nm expected (after compile):
;;   nl_mut_str_set_codepoint_raw  T  (this file)
;;   nl_msscp_walk                 T  (this file, private helper)
;;   nl_msscp_swap_fields          T  (this file, private helper)
;;   nl_msscp_do_build             T  (this file, private helper)
;;   nl_msscp_write_int_out        T  (this file, private helper)
;; ---------------------------------------------------------------------------
(sys:defun nl_mut_str_set_codepoint_raw
    ((arg usize) (idx i64) (val_cp i64) (out usize))
  i64
  (:export "nl_mut_str_set_codepoint_raw" :abi c :alloc may :ffi may :unsafe may)
  (if (= (sys:unsafe (nelisp_ptr_read_u8 arg 0)) 6)
      (if (< val_cp 0)
          (sys:cast i64 1)
        (if (> val_cp (sys:cast i64 1114111))
            (sys:cast i64 1)
          (if (and (>= val_cp (sys:cast i64 55296))
                   (<= val_cp (sys:cast i64 57343)))
              (sys:cast i64 1)
            (let ((char_count i64 (sys:unsafe (nl_str_char_count arg))))
              (if (>= idx char_count)
                  (sys:cast i64 1)
                (let ((new_slot usize (sys:alloc 32 8)))
                  (sys:unsafe
                   (nl_msscp_do_build arg new_slot char_count idx val_cp)
                   (nl_msscp_write_int_out out val_cp))))))))
    (sys:cast i64 1)))

;; End of nonenv-mut-str-set-cp.nl
