;; Doc 128 §128.B — nl_mut_str_push_byte + nl_mut_str_push_codepoint
;; STAGED (unwired -- do NOT touch manifest/build.rs)
;; Type-checked + object-compiled.  Not linked.
;;
;; Replaces Rust #[no_mangle] bodies (deleted):
;;   nl_mut_str_push_byte(mut_str_ptr: usize, byte: i64)
;;   nl_mut_str_push_codepoint(mut_str_ptr: usize, codepoint: i64)
;;
;; ---------------------------------------------------------------------------
;; NlStr layout (#[repr(C)], verified from nelisp-cc-nlstr-direct-ops.el):
;;   Sexp::MutStr:  tag@0 (u8=6), NlStr*@8 (u64 box pointer)
;;   NlStr:         String.cap@0  String.ptr@8  String.len@16  refcount@24
;;
;; Box pointer: nlstr = peek-u64(sexp+8)
;;   cap:       peek-u64(nlstr+0)
;;   data_ptr:  peek-u64(nlstr+8)
;;   len:       peek-u64(nlstr+16)
;;
;; Allocator safety: nelisp_alloc_bytes maps to std::alloc::alloc (same
;; global allocator as Rust String/Vec).  The char-buf allocated here is
;; freed by the same allocator when the MutStr drops, matching the
;; nl_alloc_mut_str pattern: alloc-bytes(max(cap,1), 1).
;;
;; ---------------------------------------------------------------------------
;; nm verification (libnelisp_elisp_spike.a, 2761033c43e4fcda build):
;;   nelisp_ptr_write_u8    T 0x0000
;;   nelisp_ptr_read_u8     T 0x0000
;;   nelisp_alloc_bytes     T 0x0000
;;   nelisp_dealloc_bytes   T 0x0000
;;   nl_mut_str_len         T 0x0000  (reuse for layout reference)
;;
;; ---------------------------------------------------------------------------
;; GROW PATH FLAG:
;;   When len == cap the buffer must be reallocated.  The grow path
;;   implemented here:
;;     new_cap = max(cap*2, 8)
;;     new_buf = nelisp_alloc_bytes(new_cap, 1)
;;     memcopy old bytes (nl_msp_copy_loop)
;;     write byte at new_buf[len]
;;     nelisp_dealloc_bytes(old_data_ptr, cap, 1)
;;     poke new_buf  → nlstr+8  (String.ptr)
;;     poke new_cap  → nlstr+0  (String.cap)
;;     poke len+1    → nlstr+16 (String.len)
;;   The nelisp_alloc_bytes / nelisp_dealloc_bytes pair matches Rust's
;;   String grow strategy; they use the same global allocator.
;;   NOTE: this is correct for cap > 0.  For cap == 0 the initial
;;   nl_alloc_mut_str always allocates >= 1 byte (alloc-bytes(max(cap,1),1)),
;;   so cap >= 1 is invariant on entry; the cap*2 path never sees cap==0.
;;
;; ---------------------------------------------------------------------------
;; NO *.rs files edited. Not committed. Not wired.

;; ---------------------------------------------------------------------------
;; sys:extern declarations — byte-level I/O + allocator
;; ---------------------------------------------------------------------------

;; nelisp_ptr_write_u8(base_ptr, offset, val) — MOV BYTE [base+offset], val_low8
;; Returns i64 = 1 sentinel.
(sys:extern nelisp_ptr_write_u8
  (:symbol "nelisp_ptr_write_u8" :abi c :unsafe t)
  ((base_ptr usize) (offset i64) (val i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_ptr_read_u8(base_ptr, offset) — MOVZX RAX, BYTE [base+offset]
;; Returns i64 = zero-extended byte.
(sys:extern nelisp_ptr_read_u8
  (:symbol "nelisp_ptr_read_u8" :abi c :unsafe t)
  ((base_ptr usize) (offset i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_alloc_bytes(size, align) -> usize (raw pointer or 0 on oom)
;; Maps to std::alloc::alloc — same global allocator as Rust String.
(sys:extern nelisp_alloc_bytes
  (:symbol "nelisp_alloc_bytes" :abi c :unsafe t)
  ((size i64) (align i64))
  usize
  (:alloc may :ffi may :unsafe may))

;; nelisp_dealloc_bytes(ptr, size, align) -> i64
;; Maps to std::alloc::dealloc — frees a nelisp_alloc_bytes block.
(sys:extern nelisp_dealloc_bytes
  (:symbol "nelisp_dealloc_bytes" :abi c :unsafe t)
  ((ptr usize) (size i64) (align i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; HELPER: nl_msp_copy_loop(src, dst, i, n) -> i64
;;
;; Tail-recursive byte copier; copies n bytes from src[0..n) into dst[0..n).
;; Pattern mirrors nl_alloc_str_copy_loop / nl_mut_str_finalize_copy_loop.
;; ---------------------------------------------------------------------------
(sys:defun nl_msp_copy_loop
    ((src usize) (dst usize) (i i64) (n i64))
  i64
  (:alloc none :ffi may :unsafe may)
  (if (= i n)
      (sys:cast i64 1)
    (sys:unsafe
     (nelisp_ptr_write_u8 dst i (nelisp_ptr_read_u8 src i))
     (nl_msp_copy_loop src dst (+ i 1) n))))

;; ---------------------------------------------------------------------------
;; HELPER: nl_msp_grow_and_push(nlstr, cap, data_ptr, len, byte) -> void (i64=1)
;;
;; Called when len == cap: allocate a new buffer of new_cap = max(cap*2, 8),
;; copy old bytes, write the new byte at position len, free the old buffer,
;; update nlstr String fields.
;;
;; Signature carries pre-read values to avoid multiple peek-u64 calls.
;; ---------------------------------------------------------------------------
(sys:defun nl_msp_grow_and_push
    ((nlstr usize) (cap i64) (data_ptr usize) (len i64) (byte i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((new_cap i64 (if (< (* cap 2) 8) 8 (* cap 2)))
        (new_buf usize (sys:unsafe (sys:cast usize (nelisp_alloc_bytes new_cap 1)))))
    ;; Copy existing bytes from old buffer into new buffer.
    (nl_msp_copy_loop data_ptr new_buf 0 len)
    ;; Write the new byte at position len.
    (sys:unsafe
     (nelisp_ptr_write_u8 new_buf len (sys:cast i64 (sys:cast i64 byte))))
    ;; Free the old char buffer: dealloc_bytes(old_ptr, old_cap, 1).
    (sys:unsafe
     (nelisp_dealloc_bytes data_ptr cap 1))
    ;; Update NlStr String fields:
    ;;   String.cap  = new_cap  → nlstr+0
    ;;   String.ptr  = new_buf  → nlstr+8
    ;;   String.len  = len+1    → nlstr+16
    (sys:unsafe
     (sys:poke-u64 nlstr                    (sys:cast i64 new_cap))
     (sys:poke-u64 (+ nlstr 8)  (sys:cast i64 new_buf))
     (sys:poke-u64 (+ nlstr 16) (sys:cast i64 (+ len 1))))))

;; ---------------------------------------------------------------------------
;; nl_mut_str_push_byte(mut_str_ptr, byte) — exported C-ABI symbol
;;
;; Deleted Rust body:
;;   mut_str_val_mut!(ptr).as_mut_vec().push((byte & 0xFF) as u8)
;;
;; Layout hop chain:
;;   nlstr    = peek-u64(mut_str_ptr + 8)   — NlStr* box pointer
;;   cap      = peek-u64(nlstr + 0)         — String.cap
;;   data_ptr = peek-u64(nlstr + 8)         — String.ptr
;;   len      = peek-u64(nlstr + 16)        — String.len
;;
;; Fast path (len < cap): write byte at data_ptr[len]; poke len+1.
;; Grow path (len == cap): delegate to nl_msp_grow_and_push.
;; ---------------------------------------------------------------------------
(sys:defun nl_mut_str_push_byte
    ((mut_str_ptr usize) (byte i64))
  i64
  (:export "nl_mut_str_push_byte" :abi c :alloc may :ffi may :unsafe may)
  (let ((nlstr    usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ mut_str_ptr 8)))))
        (cap      i64   (sys:cast i64   (sys:unsafe (sys:peek-u64 nlstr))))
        (data_ptr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ nlstr 8)))))
        (len      i64   (sys:cast i64   (sys:unsafe (sys:peek-u64 (+ nlstr 16))))))
    (if (< len cap)
        ;; Fast path: room available — write byte, bump len.
        (sys:unsafe
         (nelisp_ptr_write_u8 data_ptr len (sys:cast i64 (sys:cast i64 byte)))
         (sys:poke-u64 (+ nlstr 16) (sys:cast i64 (+ len 1))))
      ;; Grow path: buffer full — reallocate.
      (nl_msp_grow_and_push nlstr cap data_ptr len byte))))

;; ---------------------------------------------------------------------------
;; HELPER: nl_msp_push_bytes_2(ptr, b0, b1) — push two UTF-8 continuation bytes
;; Helper for 2-byte UTF-8 encoding.
;; ---------------------------------------------------------------------------
(sys:defun nl_msp_push_bytes_2
    ((ptr usize) (b0 i64) (b1 i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (nl_mut_str_push_byte ptr b0)
  (nl_mut_str_push_byte ptr b1))

;; ---------------------------------------------------------------------------
;; HELPER: nl_msp_push_bytes_3(ptr, b0, b1, b2) — push three UTF-8 bytes
;; ---------------------------------------------------------------------------
(sys:defun nl_msp_push_bytes_3
    ((ptr usize) (b0 i64) (b1 i64) (b2 i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (nl_mut_str_push_byte ptr b0)
  (nl_mut_str_push_byte ptr b1)
  (nl_mut_str_push_byte ptr b2))

;; ---------------------------------------------------------------------------
;; HELPER: nl_msp_push_bytes_4(ptr, b0, b1, b2, b3) — push four UTF-8 bytes
;; ---------------------------------------------------------------------------
(sys:defun nl_msp_push_bytes_4
    ((ptr usize) (b0 i64) (b1 i64) (b2 i64) (b3 i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (nl_mut_str_push_byte ptr b0)
  (nl_mut_str_push_byte ptr b1)
  (nl_mut_str_push_byte ptr b2)
  (nl_mut_str_push_byte ptr b3))

;; ---------------------------------------------------------------------------
;; nl_mut_str_push_codepoint(mut_str_ptr, codepoint) — exported C-ABI symbol
;;
;; Deleted Rust body:
;;   clamp to 0..=0x10FFFF else 0xFFFD
;;   String::push(char::from_u32(codepoint).unwrap())
;;   = UTF-8 encode (1-4 bytes) + append
;;
;; UTF-8 encoding table:
;;   U+0000..U+007F   1 byte:  0xxxxxxx
;;   U+0080..U+07FF   2 bytes: 110xxxxx 10xxxxxx
;;   U+0800..U+FFFF   3 bytes: 1110xxxx 10xxxxxx 10xxxxxx
;;   U+10000..U+10FFFF 4 bytes: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
;;
;; Surrogate range U+D800..U+DFFF: clamp to U+FFFD (= 3-byte sequence).
;; ---------------------------------------------------------------------------
(sys:defun nl_mut_str_push_codepoint
    ((mut_str_ptr usize) (codepoint i64))
  i64
  (:export "nl_mut_str_push_codepoint" :abi c :alloc may :ffi may :unsafe may)
  ;; Clamp out-of-range codepoints to U+FFFD (REPLACEMENT CHARACTER).
  ;; Surrogate range U+D800..U+DFFF: (cp >= 55296 && cp < 57344).
  ;; Expressed with nested if: cp < 55296 → ok; else cp < 57344 → replace; else ok.
  (let ((cp i64 (if (< codepoint 0)
                    65533
                  (if (> codepoint 1114111)
                      65533
                    ;; Surrogate range U+D800..U+DFFF → clamp to U+FFFD
                    (if (< codepoint 55296)
                        codepoint
                      (if (< codepoint 57344)
                          65533
                        codepoint))))))
    (if (< cp 128)
        ;; 1-byte ASCII: 0xxxxxxx
        (nl_mut_str_push_byte mut_str_ptr cp)
      (if (< cp 2048)
          ;; 2-byte: 110xxxxx 10xxxxxx
          (nl_msp_push_bytes_2
           mut_str_ptr
           ;; byte 0: 110xxxxx = 0xC0 | (cp >> 6)
           (sys:cast i64 (+ 192 (sys:cast i64 (/ cp 64))))
           ;; byte 1: 10xxxxxx = 0x80 | (cp & 0x3F)
           (sys:cast i64 (+ 128 (sys:cast i64 (sys:cast i64 (- cp (* (/ cp 64) 64)))))))
        (if (< cp 65536)
            ;; 3-byte: 1110xxxx 10xxxxxx 10xxxxxx
            (nl_msp_push_bytes_3
             mut_str_ptr
             ;; byte 0: 1110xxxx = 0xE0 | (cp >> 12)
             (sys:cast i64 (+ 224 (sys:cast i64 (/ cp 4096))))
             ;; byte 1: 10xxxxxx = 0x80 | ((cp >> 6) & 0x3F)
             (sys:cast i64 (+ 128 (sys:cast i64 (- (/ cp 64) (* (/ cp 4096) 64)))))
             ;; byte 2: 10xxxxxx = 0x80 | (cp & 0x3F)
             (sys:cast i64 (+ 128 (sys:cast i64 (- cp (* (/ cp 64) 64))))))
          ;; 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx  (U+10000..U+10FFFF)
          (nl_msp_push_bytes_4
           mut_str_ptr
           ;; byte 0: 11110xxx = 0xF0 | (cp >> 18)
           (sys:cast i64 (+ 240 (sys:cast i64 (/ cp 262144))))
           ;; byte 1: 10xxxxxx = 0x80 | ((cp >> 12) & 0x3F)
           (sys:cast i64 (+ 128 (sys:cast i64 (- (/ cp 4096) (* (/ cp 262144) 64)))))
           ;; byte 2: 10xxxxxx = 0x80 | ((cp >> 6) & 0x3F)
           (sys:cast i64 (+ 128 (sys:cast i64 (- (/ cp 64) (* (/ cp 4096) 64)))))
           ;; byte 3: 10xxxxxx = 0x80 | (cp & 0x3F)
           (sys:cast i64 (+ 128 (sys:cast i64 (- cp (* (/ cp 64) 64)))))))))))

;; End of Doc 128 §128.B — nl_mut_str_push_byte + nl_mut_str_push_codepoint
