;; Doc §nonenv -- nl_tty_memcpy_to_saved(src usize) -> void
;; STATUS: STAGED (unwired -- do NOT touch manifest/build.rs)
;; Type-checked + compile-gate target only.  Not linked into the binary.
;;
;; Implements:
;;   nl_tty_memcpy_to_saved(src usize) -> void
;;
;; Contract (replaces deleted Rust body):
;;   Deleted Rust:
;;     std::ptr::copy_nonoverlapping(src as *const u8,
;;         std::ptr::addr_of_mut!(SAVED_TERMIOS) as *mut u8, 60)
;;   Elisp replacement:
;;     1. Obtain dst = nl_tty_saved_termios_ptr() — the C-ABI getter that
;;        returns addr_of_mut!(SAVED_TERMIOS) as *mut u8 (T in rlib).
;;     2. Copy 60 bytes src → dst via:
;;          7 × nelisp_ptr_write_u64 / nelisp_ptr_read_u64  (bytes 0-55)
;;          4 × nelisp_ptr_write_u8  / nelisp_ptr_read_u8   (bytes 56-59)
;;
;; SAVED_TERMIOS address obtained via:
;;   #[no_mangle] pub extern "C" fn nl_tty_saved_termios_ptr() -> *mut u8
;;   defined in build-tool/src/eval/mod.rs (tty module).
;;   Confirmed T in target/debug/libnelisp_build_tool.rlib.
;;   NOT in the spike archive (libnelisp_elisp_spike.a) — it is in the
;;   main rlib, which is the linker input during the full nelisp binary
;;   link.  For the compile-gate (.o only) target this symbol is U
;;   (undefined, will be resolved at link time).
;;
;; Archive nm verification (libnelisp_elisp_spike.a) of callees used here:
;;   nelisp_ptr_read_u64    T  — 8-byte load: MOV RAX,[RDI+RSI*8] style
;;   nelisp_ptr_write_u64   T  — 8-byte store
;;   nelisp_ptr_read_u8     T  — byte load: MOVZX RAX,BYTE [RDI+RSI]
;;   nelisp_ptr_write_u8    T  — byte store
;;
;; nl_tty_saved_termios_ptr is T in libnelisp_build_tool.rlib (not in
;; spike archive), so it appears as U in the produced .o — correct for
;; a staged / compile-gate file.
;;
;; NO *.rs files edited.  Not committed.  Not wired into manifest/build.rs.

;; ---------------------------------------------------------------------------
;; sys:extern declarations
;; ---------------------------------------------------------------------------

;; nl_tty_saved_termios_ptr() -> usize
;; C-ABI getter — returns addr_of_mut!(SAVED_TERMIOS) as *mut u8.
;; Defined: build-tool/src/eval/mod.rs (tty module), #[no_mangle].
;; T in libnelisp_build_tool.rlib; U in produced .o (resolved at link time).
(sys:extern nl_tty_saved_termios_ptr
  (:symbol "nl_tty_saved_termios_ptr" :abi c :unsafe t)
  ()
  usize
  (:alloc none :ffi may :unsafe may))

;; nelisp_ptr_read_u64(ptr, offset) -> i64
;; 8-byte load: value = *(ptr + offset * sizeof(u64))  (offset is byte offset)
;; T in spike archive (nelisp_ptr_read_u64.o).
(sys:extern nelisp_ptr_read_u64
  (:symbol "nelisp_ptr_read_u64" :abi c :unsafe t)
  ((ptr usize) (offset i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_ptr_write_u64(ptr, val) -> i64
;; 8-byte store: *(u64*)ptr = val.
;; T in spike archive (nelisp_ptr_write_u64.o).
(sys:extern nelisp_ptr_write_u64
  (:symbol "nelisp_ptr_write_u64" :abi c :unsafe t)
  ((ptr usize) (val i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_ptr_read_u8(ptr, offset) -> i64
;; Byte load: MOVZX RAX, BYTE [ptr+offset].
;; T in spike archive (nelisp_ptr_read_u8.o).
(sys:extern nelisp_ptr_read_u8
  (:symbol "nelisp_ptr_read_u8" :abi c :unsafe t)
  ((ptr usize) (offset i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_ptr_write_u8(ptr, offset, val) -> i64
;; Byte store: BYTE [ptr+offset] = val & 0xFF.
;; T in spike archive (nelisp_ptr_write_u8.o).
(sys:extern nelisp_ptr_write_u8
  (:symbol "nelisp_ptr_write_u8" :abi c :unsafe t)
  ((ptr usize) (offset i64) (val i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; HELPER: nl_tmcs_copy8(src, dst, off) -> i64
;;
;; Copy one u64 word (8 bytes) from src+off to dst+off.
;; nelisp_ptr_read_u64 / write_u64 take byte-offset as i64.
;; ---------------------------------------------------------------------------
(sys:defun nl_tmcs_copy8
    ((src usize) (dst usize) (off i64))
  i64
  (:alloc none :ffi may :unsafe may)
  (sys:unsafe
   (nelisp_ptr_write_u64 (sys:cast usize (+ (sys:cast i64 dst) off))
                         (nelisp_ptr_read_u64 src off))))

;; ---------------------------------------------------------------------------
;; HELPER: nl_tmcs_copy1(src, dst, off) -> i64
;;
;; Copy one byte from src+off to dst+off.
;; Uses nelisp_ptr_read_u8 / write_u8 (ptr + byte-offset).
;; ---------------------------------------------------------------------------
(sys:defun nl_tmcs_copy1
    ((src usize) (dst usize) (off i64))
  i64
  (:alloc none :ffi may :unsafe may)
  (sys:unsafe
   (nelisp_ptr_write_u8 (sys:cast usize (+ (sys:cast i64 dst) off))
                        0
                        (nelisp_ptr_read_u8 src off))))

;; ---------------------------------------------------------------------------
;; PUBLIC ENTRY: nl_tty_memcpy_to_saved(src usize) -> void
;;
;; Copies 60 bytes from src into SAVED_TERMIOS (obtained via C-ABI getter).
;;
;; Layout:
;;   bytes  0- 7 : word 0   (8 bytes, u64)
;;   bytes  8-15 : word 1
;;   bytes 16-23 : word 2
;;   bytes 24-31 : word 3
;;   bytes 32-39 : word 4
;;   bytes 40-47 : word 5
;;   bytes 48-55 : word 6   (7 × 8 = 56 bytes)
;;   bytes 56-59 : tail (4 bytes, copied as 4 individual bytes)
;;
;; Deleted Rust:
;;   std::ptr::copy_nonoverlapping(src, addr_of_mut!(SAVED_TERMIOS) as *mut u8, 60)
;; ---------------------------------------------------------------------------
(sys:defun nl_tty_memcpy_to_saved
    ((src usize))
  void
  (:export "nl_tty_memcpy_to_saved" :abi c :alloc none :ffi may :unsafe may)
  (let ((dst usize (sys:cast usize (sys:unsafe (nl_tty_saved_termios_ptr)))))
    ;; Copy 56 bytes as 7 u64 words (byte offsets 0, 8, 16, 24, 32, 40, 48).
    (nl_tmcs_copy8 src dst (sys:cast i64 0))
    (nl_tmcs_copy8 src dst (sys:cast i64 8))
    (nl_tmcs_copy8 src dst (sys:cast i64 16))
    (nl_tmcs_copy8 src dst (sys:cast i64 24))
    (nl_tmcs_copy8 src dst (sys:cast i64 32))
    (nl_tmcs_copy8 src dst (sys:cast i64 40))
    (nl_tmcs_copy8 src dst (sys:cast i64 48))
    ;; Copy 4-byte tail (bytes 56, 57, 58, 59) individually.
    (nl_tmcs_copy1 src dst (sys:cast i64 56))
    (nl_tmcs_copy1 src dst (sys:cast i64 57))
    (nl_tmcs_copy1 src dst (sys:cast i64 58))
    (nl_tmcs_copy1 src dst (sys:cast i64 59))))

;; End of nonenv-tty-memcpy.nl
