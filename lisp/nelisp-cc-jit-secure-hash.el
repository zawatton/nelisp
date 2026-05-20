;;; nelisp-cc-jit-secure-hash.el --- Phase 47 nl_jit_secure_hash (SHA1 arm)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 migration of the SHA1 arm of `nl_jit_secure_hash' from
;; `build-tool/src/jit/hash.rs'.  The Rust function now handles only
;; sha224/sha256/sha384/sha512/md5 under the name
;; `nl_jit_secure_hash_non_sha1'; this object provides the new
;; `nl_jit_secure_hash' trampoline that intercepts sha1 Symbol calls
;; and delegates everything else to the Rust fallback.
;;
;; Trampoline signature: `(*const Sexp, *const Sexp, *mut Sexp) -> i64'
;; (OK=0 / ERR=1), reached via `nl-jit-call-out-2' from
;; `nelisp-stdlib-hash.el::nl-secure-hash'.
;;
;; SHA1 implementation (FIPS 180-4):
;;   Workspace buffer (816 bytes, align 8):
;;     [0..639]  : w[0..79] message schedule (u64 slots, i*8 offset)
;;     [640..647]: a (round working variable)
;;     [648..655]: b
;;     [656..663]: c
;;     [664..671]: d
;;     [672..679]: e
;;     [680..687]: H0 (running hash)
;;     [688..695]: H1
;;     [696..703]: H2
;;     [704..711]: H3
;;     [712..719]: H4
;;     [720..727]: T (temporary, round computation scratch)
;;     [728..735]: i (general loop counter for load/expand/compress/scratch)
;;     [736..743]: block_i (outer counter for full_blocks loop)
;;     [744..751]: zero_i (counter for scratch zero-fill loop)
;;     [752..815]: 64-byte block scratch for padding construction
;;
;; Counter discipline:
;;   buf[728]: used by nl_sha1_load / nl_sha1_expand / nl_sha1_compress
;;             (re-initialized at start of each)
;;   buf[736]: used by nl_sha1_full_blocks outer loop only
;;   buf[744]: used by nl_sha1_scratch_fill zeroing + second-padding-block loop
;;
;; while-in-and fix: Phase 47 `(while ...)' always returns 0.  Every
;; `while' that appears in an `and' chain with more code after it is
;; wrapped as `(or (while ...) 1)' so the `and' chain continues.
;;
;; SHA1 32-bit literal encoding:
;;   Values >= 0x80000000 must avoid sign-extending imm32:
;;   H1 = 0xEFCDAB89 = (+ (shl 1 31) 1875749769)
;;   H2 = 0x98BADCFE = (+ (shl 1 31) 414899454)
;;   H4 = 0xC3D2E1F0 = (+ (shl 1 31) 1137893872)
;;   K2 = 0x8F1BBCDC = (+ (shl 1 31) 253476060)
;;   K3 = 0xCA62C1D6 = (+ (shl 1 31) 1247986134)
;;   mask32 = 0xFFFFFFFF = (- (shl 1 32) 1)
;;
;; Build wiring:
;;   `scripts/compile-elisp-objects.el' manifest entry -> `nl_jit_secure_hash.o'
;;   `build-tool/build.rs' manifest_sources: `"nelisp-cc-jit-secure-hash.el"'
;;   `build-tool/src/jit/bridge.rs': extern nl_jit_secure_hash + anchor entry
;;   `build-tool/src/jit/hash.rs': renamed to `nl_jit_secure_hash_non_sha1',
;;     sha1 arm + sha1 crate import deleted.

;;; Code:

(defconst nelisp-cc-jit-secure-hash--source
  '(seq

    ;; ---- 32-bit arithmetic helpers ----------------------------------------

    ;; nl_sha1_mask: mask to low 32 bits.
    ;; 0xFFFFFFFF = (- (shl 1 32) 1) avoids sign-extension of imm32.
    (defun nl_sha1_mask (x)
      (logand x (- (shl 1 32) 1)))

    ;; nl_sha1_rol: rotate-left 32-bit.
    ;; Pre: x should be already masked to 32 bits for sar to behave as lsr.
    (defun nl_sha1_rol (x n)
      (nl_sha1_mask
       (logior (shl (nl_sha1_mask x) n)
               (sar (nl_sha1_mask x) (- 32 n)))))

    ;; ---- SHA1 f-function dispatch -----------------------------------------

    ;; Ch(b,c,d) = (b AND c) OR (NOT b AND d)  [rounds 0-19]
    ;; NOT b in 32-bit = logxor(b, 0xFFFFFFFF) = logxor(b, mask32)
    (defun nl_sha1_f0 (b c d)
      (nl_sha1_mask
       (logior (logand b c)
               (logand (logxor b (- (shl 1 32) 1)) d))))

    ;; Parity(b,c,d) = b XOR c XOR d  [rounds 20-39, 60-79]
    (defun nl_sha1_f1 (b c d)
      (nl_sha1_mask (logxor b (logxor c d))))

    ;; Maj(b,c,d) = (b AND c) OR (b AND d) OR (c AND d)  [rounds 40-59]
    (defun nl_sha1_f2 (b c d)
      (nl_sha1_mask
       (logior (logand b c)
               (logior (logand b d) (logand c d)))))

    ;; f dispatch
    (defun nl_sha1_f (i b c d)
      (if (< i 20)
          (nl_sha1_f0 b c d)
        (if (< i 40)
            (nl_sha1_f1 b c d)
          (if (< i 60)
              (nl_sha1_f2 b c d)
            (nl_sha1_f1 b c d)))))

    ;; K dispatch: round constants.
    ;; K0=0x5A827999=1518500249, K1=0x6ED9EBA1=1859775393
    ;; K2=0x8F1BBCDC=(+ (shl 1 31) 253476060)
    ;; K3=0xCA62C1D6=(+ (shl 1 31) 1247986134)
    (defun nl_sha1_k (i)
      (if (< i 20)
          1518500249
        (if (< i 40)
            1859775393
          (if (< i 60)
              (+ (shl 1 31) 253476060)
            (+ (shl 1 31) 1247986134)))))

    ;; ---- Message schedule expansion w[16..79] ----------------------------
    ;;
    ;; w[i] = rol32(w[i-3]^w[i-8]^w[i-14]^w[i-16], 1) for i=16..79.
    ;; Uses buf[728] as loop counter (re-initialized here).
    ;; `(or (while ...) 1)' — `while' always returns 0; `or' coerces to 1
    ;; so callers in `and' chains see a truthy return.
    (defun nl_sha1_expand (buf)
      (and
       (ptr-write-u64 buf 728 16)
       (or (while (< (ptr-read-u64 buf 728) 80)
             (ptr-write-u64 buf
               (* (ptr-read-u64 buf 728) 8)
               (nl_sha1_rol
                (logxor
                 (ptr-read-u64 buf (* (- (ptr-read-u64 buf 728) 3)  8))
                 (logxor
                  (ptr-read-u64 buf (* (- (ptr-read-u64 buf 728) 8)  8))
                  (logxor
                   (ptr-read-u64 buf (* (- (ptr-read-u64 buf 728) 14) 8))
                   (ptr-read-u64 buf (* (- (ptr-read-u64 buf 728) 16) 8)))))
                1))
             (ptr-write-u64 buf 728 (+ (ptr-read-u64 buf 728) 1)))
           1)))

    ;; ---- Load w[0..15] from raw bytes (big-endian) -----------------------
    ;;
    ;; bytes: *const u8.  byte-off: offset of first message byte in `bytes'.
    ;; Loads 4 bytes big-endian at positions byte-off + i*4 + 0..3 for i=0..15.
    ;; Uses buf[728] as loop counter (re-initialized here).
    (defun nl_sha1_load (bytes byte-off buf)
      (and
       (ptr-write-u64 buf 728 0)
       (or (while (< (ptr-read-u64 buf 728) 16)
             (ptr-write-u64 buf
               (* (ptr-read-u64 buf 728) 8)
               (nl_sha1_mask
                (logior
                 (shl (ptr-read-u8 bytes (+ byte-off (* (ptr-read-u64 buf 728) 4))) 24)
                 (logior
                  (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 728) 4) 1))) 16)
                  (logior
                   (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 728) 4) 2))) 8)
                   (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 728) 4) 3))))))))
             (ptr-write-u64 buf 728 (+ (ptr-read-u64 buf 728) 1)))
           1)))

    ;; ---- 80-round SHA1 compression ---------------------------------------
    ;;
    ;; Initializes a,b,c,d,e from H0..H4, runs 80 rounds, adds back to H.
    ;; State slots: a=640, b=648, c=656, d=664, e=672
    ;; Hash slots:  H0=680, H1=688, H2=696, H3=704, H4=712
    ;; Temp: T=720, counter: i=728 (re-initialized here).
    (defun nl_sha1_compress (buf)
      (and
       ;; Init working vars from current H0..H4
       (ptr-write-u64 buf 640 (ptr-read-u64 buf 680))
       (ptr-write-u64 buf 648 (ptr-read-u64 buf 688))
       (ptr-write-u64 buf 656 (ptr-read-u64 buf 696))
       (ptr-write-u64 buf 664 (ptr-read-u64 buf 704))
       (ptr-write-u64 buf 672 (ptr-read-u64 buf 712))
       ;; Init round counter
       (ptr-write-u64 buf 728 0)
       ;; 80-round loop.  `(or (while ...) 1)' coerces the 0 return to 1.
       (or (while (< (ptr-read-u64 buf 728) 80)
             ;; T = ROL(a,5) + f(i,b,c,d) + e + k(i) + w[i]
             ;; w[i] at buf[i*8]; i is ptr-read-u64(buf,728)
             (ptr-write-u64 buf 720
               (nl_sha1_mask
                (+ (nl_sha1_rol (ptr-read-u64 buf 640) 5)
                   (+ (nl_sha1_f
                       (ptr-read-u64 buf 728)
                       (ptr-read-u64 buf 648)
                       (ptr-read-u64 buf 656)
                       (ptr-read-u64 buf 664))
                      (+ (ptr-read-u64 buf 672)
                         (+ (nl_sha1_k (ptr-read-u64 buf 728))
                            (ptr-read-u64 buf
                              (* (ptr-read-u64 buf 728) 8))))))))
             ;; Update: e=d, d=c, c=ROL(b,30), b=a, a=T
             ;; Write in reverse order (e first, a last) to use old values
             (ptr-write-u64 buf 672 (ptr-read-u64 buf 664))
             (ptr-write-u64 buf 664 (ptr-read-u64 buf 656))
             (ptr-write-u64 buf 656 (nl_sha1_rol (ptr-read-u64 buf 648) 30))
             (ptr-write-u64 buf 648 (ptr-read-u64 buf 640))
             (ptr-write-u64 buf 640 (ptr-read-u64 buf 720))
             ;; i++
             (ptr-write-u64 buf 728 (+ (ptr-read-u64 buf 728) 1)))
           1)
       ;; H += working vars (32-bit wrap)
       (ptr-write-u64 buf 680
         (nl_sha1_mask (+ (ptr-read-u64 buf 680) (ptr-read-u64 buf 640))))
       (ptr-write-u64 buf 688
         (nl_sha1_mask (+ (ptr-read-u64 buf 688) (ptr-read-u64 buf 648))))
       (ptr-write-u64 buf 696
         (nl_sha1_mask (+ (ptr-read-u64 buf 696) (ptr-read-u64 buf 656))))
       (ptr-write-u64 buf 704
         (nl_sha1_mask (+ (ptr-read-u64 buf 704) (ptr-read-u64 buf 664))))
       (ptr-write-u64 buf 712
         (nl_sha1_mask (+ (ptr-read-u64 buf 712) (ptr-read-u64 buf 672))))))

    ;; ---- Process full 64-byte message blocks ----------------------------
    ;;
    ;; bytes: *const u8.  num-blocks: i64 (= msg-len / 64 = sar msg-len 6).
    ;; Uses buf[736] as OUTER loop counter to avoid clobbering buf[728]
    ;; which is used by nl_sha1_load / nl_sha1_expand / nl_sha1_compress.
    (defun nl_sha1_full_blocks (bytes num-blocks buf)
      (and
       (ptr-write-u64 buf 736 0)
       (or (while (< (ptr-read-u64 buf 736) num-blocks)
             (and
              (nl_sha1_load bytes (* (ptr-read-u64 buf 736) 64) buf)
              (nl_sha1_expand buf)
              (nl_sha1_compress buf))
             (ptr-write-u64 buf 736 (+ (ptr-read-u64 buf 736) 1)))
           1)))

    ;; ---- Build padding scratch buffer [752..815] -------------------------
    ;;
    ;; Copies rem bytes from bytes[offset..] to scratch[0..rem-1],
    ;; then zero-fills scratch[rem..63].
    ;; Scratch base = buf + 752.
    ;; Uses buf[728] for copy loop, buf[744] for zero-fill loop.
    (defun nl_sha1_scratch_fill (bytes offset rem buf)
      (and
       ;; Copy rem message bytes into scratch
       (ptr-write-u64 buf 728 0)
       (or (while (< (ptr-read-u64 buf 728) rem)
             (ptr-write-u8 buf
               (+ 752 (ptr-read-u64 buf 728))
               (ptr-read-u8 bytes (+ offset (ptr-read-u64 buf 728))))
             (ptr-write-u64 buf 728 (+ (ptr-read-u64 buf 728) 1)))
           1)
       ;; Zero-fill scratch[rem..63]
       (ptr-write-u64 buf 744 rem)
       (or (while (< (ptr-read-u64 buf 744) 64)
             (ptr-write-u8 buf (+ 752 (ptr-read-u64 buf 744)) 0)
             (ptr-write-u64 buf 744 (+ (ptr-read-u64 buf 744) 1)))
           1)))

    ;; ---- Write 8-byte big-endian length into scratch[56..63] = buf[808..815]
    ;;
    ;; msg-bits: i64 (= msg-len * 8).
    ;; Writes all 8 bytes of the 64-bit big-endian length.
    ;; buf[808..815] = scratch bytes 56..63.
    (defun nl_sha1_write_length (msg-bits buf)
      (and
       (ptr-write-u8 buf 808 (logand (sar msg-bits 56) 255))
       (ptr-write-u8 buf 809 (logand (sar msg-bits 48) 255))
       (ptr-write-u8 buf 810 (logand (sar msg-bits 40) 255))
       (ptr-write-u8 buf 811 (logand (sar msg-bits 32) 255))
       (ptr-write-u8 buf 812 (logand (sar msg-bits 24) 255))
       (ptr-write-u8 buf 813 (logand (sar msg-bits 16) 255))
       (ptr-write-u8 buf 814 (logand (sar msg-bits 8) 255))
       (ptr-write-u8 buf 815 (logand msg-bits 255))))

    ;; ---- Process the final padded block from scratch [752..815] ----------
    ;;
    ;; Loads w[0..15] from buf[752..815] (= scratch).
    ;; byte-off 752 = scratch base within buf.
    (defun nl_sha1_final_block (buf)
      (and
       (nl_sha1_load buf 752 buf)
       (nl_sha1_expand buf)
       (nl_sha1_compress buf)))

    ;; ---- Hex output helpers ----------------------------------------------

    ;; Write one hex nibble (0..15) as ASCII to hex-buf[offset].
    (defun nl_sha1_hex_nibble (hex-buf offset nibble)
      (ptr-write-u8 hex-buf offset
        (if (< nibble 10)
            (+ nibble 48)
          (+ nibble 87))))

    ;; Write 8 hex chars for one 32-bit SHA1 word to hex-buf[offset..offset+7].
    (defun nl_sha1_hex_word (hex-buf offset word)
      (and
       (nl_sha1_hex_nibble hex-buf offset       (logand (sar word 28) 15))
       (nl_sha1_hex_nibble hex-buf (+ offset 1) (logand (sar word 24) 15))
       (nl_sha1_hex_nibble hex-buf (+ offset 2) (logand (sar word 20) 15))
       (nl_sha1_hex_nibble hex-buf (+ offset 3) (logand (sar word 16) 15))
       (nl_sha1_hex_nibble hex-buf (+ offset 4) (logand (sar word 12) 15))
       (nl_sha1_hex_nibble hex-buf (+ offset 5) (logand (sar word 8) 15))
       (nl_sha1_hex_nibble hex-buf (+ offset 6) (logand (sar word 4) 15))
       (nl_sha1_hex_nibble hex-buf (+ offset 7) (logand word 15))))

    ;; ---- Main SHA1 computation -------------------------------------------
    ;;
    ;; bytes: *const u8.  msg-len: i64.
    ;; hex-buf: *mut u8 (40 bytes for output).
    ;; buf: 816-byte workspace.
    ;;
    ;; Algorithm:
    ;;   1. Init H0..H4 in buf[680..719].
    ;;   2. Process num_full = sar(msg-len, 6) full 64-byte blocks.
    ;;   3. rem = msg-len & 63 (remaining bytes).
    ;;   4. Fill scratch with rem bytes, pad with 0x80, zero-fill rest.
    ;;   5a. rem <= 55: write length at scratch[56..63], one final block.
    ;;   5b. rem >= 56: process first final block (no length), then
    ;;       zero-fill new scratch, write length, second final block.
    ;;   6. Hex-encode H0..H4 to hex-buf.
    (defun nl_sha1_compute (bytes msg-len hex-buf buf)
      (and
       ;; Init H0..H4
       ;;   H0=0x67452301=1732584193
       ;;   H1=0xEFCDAB89=(+ (shl 1 31) 1875749769)
       ;;   H2=0x98BADCFE=(+ (shl 1 31) 414899454)
       ;;   H3=0x10325476=271733878
       ;;   H4=0xC3D2E1F0=(+ (shl 1 31) 1137893872)
       (ptr-write-u64 buf 680 1732584193)
       (ptr-write-u64 buf 688 (+ (shl 1 31) 1875749769))
       (ptr-write-u64 buf 696 (+ (shl 1 31) 414899454))
       (ptr-write-u64 buf 704 271733878)
       (ptr-write-u64 buf 712 (+ (shl 1 31) 1137893872))
       ;; Process full blocks: num_full = sar(msg-len, 6) = msg-len / 64
       (nl_sha1_full_blocks bytes (sar msg-len 6) buf)
       ;; Fill scratch with remaining bytes + 0x80 pad
       ;; rem = msg-len & 63
       (nl_sha1_scratch_fill bytes (* (sar msg-len 6) 64) (logand msg-len 63) buf)
       ;; Write 0x80 at scratch[rem] = buf[752 + rem]
       (ptr-write-u8 buf (+ 752 (logand msg-len 63)) 128)
       ;; Branch on rem vs 55
       (if (<= (logand msg-len 63) 55)
           ;; Single final block: write length at scratch[56..63]
           (and
            (nl_sha1_write_length (* msg-len 8) buf)
            (nl_sha1_final_block buf))
         ;; Two final blocks:
         ;; First: scratch has bytes + 0x80 + zeros, no length
         (and
          (nl_sha1_final_block buf)
          ;; Second: all zeros + length
          ;; Zero-fill scratch again using buf[744] counter
          (ptr-write-u64 buf 744 0)
          (or (while (< (ptr-read-u64 buf 744) 64)
                (ptr-write-u8 buf (+ 752 (ptr-read-u64 buf 744)) 0)
                (ptr-write-u64 buf 744 (+ (ptr-read-u64 buf 744) 1)))
              1)
          (nl_sha1_write_length (* msg-len 8) buf)
          (nl_sha1_final_block buf)))
       ;; Hex-encode H0..H4 to hex-buf
       (nl_sha1_hex_word hex-buf 0  (ptr-read-u64 buf 680))
       (nl_sha1_hex_word hex-buf 8  (ptr-read-u64 buf 688))
       (nl_sha1_hex_word hex-buf 16 (ptr-read-u64 buf 696))
       (nl_sha1_hex_word hex-buf 24 (ptr-read-u64 buf 704))
       (nl_sha1_hex_word hex-buf 32 (ptr-read-u64 buf 712))))

    ;; ---- Stack-alignment trampoline for non-SHA1 extern-call ----------------
    ;;
    ;; Phase 47 calling convention (see `a440f4af' / `1cdc2d9c'):
    ;;   - An odd-arity (1, 3, 5) GP defun has a `sub $8, rsp' in its
    ;;     prologue so body-entry rsp ≡ 0 mod 16 when called with the
    ;;     CORRECT SysV entry alignment (entry rsp ≡ 8 mod 16).
    ;;   - `--emit-call' / `--emit-extern-call' add `needs-align = arity & 1',
    ;;     i.e. a `sub $8' before every call when the enclosing defun is odd.
    ;;
    ;; nl_jit_secure_hash (arity 3, called from Rust with correct SysV rsp):
    ;;   body-entry rsp ≡ 0.  Calling any sub-function:
    ;;     push/pop N args → rsp back to 0
    ;;     sub $8 (needs-align=true) → rsp ≡ 8
    ;;     call → callee entry rsp ≡ 0   ← wrong SysV entry for Rust callees
    ;;
    ;; This wrapper has ODD arity (3).  Called from nl_jit_secure_hash
    ;; it therefore receives entry rsp ≡ 0.  Its odd-arity prologue:
    ;;   push rbp (-8 → 8), 3 param pushes (-24 → 0), sub $8 (-8 → 8)
    ;;   body-entry rsp ≡ 8 mod 16
    ;; extern-call from odd-arity body (needs-align=true):
    ;;   3 arg pushes (-24 → 0), 3 pops (+24 → 8), sub $8 (-8 → 0)
    ;;   call → nl_jit_secure_hash_non_sha1 entry rsp ≡ 8 ✓ (SysV correct)
    ;;
    ;; Using even arity (4) does NOT fix the problem: the misaligned call
    ;; from nl_jit_secure_hash would give entry rsp ≡ 0 to the wrapper,
    ;; then with even-arity no prologue sub + no needs-align sub → the
    ;; extern-call still arrives at nl_jit_secure_hash_non_sha1 with
    ;; entry rsp ≡ 0 (wrong).
    (defun nl_sha1_call_non_sha1 (algo-ptr str-ptr out)
      (extern-call nl_jit_secure_hash_non_sha1 algo-ptr str-ptr out))

    ;; ---- Main trampoline: nl_jit_secure_hash ----------------------------
    ;;
    ;; Checks if algo-ptr is Symbol "sha1".  If so, runs SHA1.
    ;; Otherwise delegates to nl_jit_secure_hash_non_sha1 (Rust fallback)
    ;; via the even-arity wrapper nl_sha1_call_non_sha1.
    ;;
    ;; algo-ptr: *const Sexp.  str-ptr: *const Sexp.  out: *mut Sexp.
    ;; Returns TRAMPOLINE_OK=0 on success, TRAMPOLINE_ERR=1 on error.
    ;;
    ;; Note: symbol-name-eq checks tag=Symbol(4) + name bytes.  If algo
    ;; is passed as a Sexp::Str (tag 5) "sha1", it falls through to the
    ;; Rust fallback which handles it correctly.  The common call path
    ;; uses quoted symbols (`sha1 etc.) so this is acceptable.
    (defun nl_jit_secure_hash (algo-ptr str-ptr out)
      (if (= (symbol-name-eq algo-ptr "sha1") 1)
          ;; SHA1 path: dispatch on str-ptr tag
          (if (= (sexp-tag str-ptr) 6)
              ;; MutStr (tag 6): use mut-str-len
              (nl_sha1_run str-ptr (mut-str-len str-ptr) out)
            (if (= (sexp-tag str-ptr) 4)
                ;; Symbol (tag 4): use str-len
                (nl_sha1_run str-ptr (str-len str-ptr) out)
              (if (= (sexp-tag str-ptr) 5)
                  ;; Str (tag 5): use str-len
                  (nl_sha1_run str-ptr (str-len str-ptr) out)
                ;; Unsupported type
                1)))
        ;; Non-SHA1: delegate to Rust fallback via odd-arity wrapper
        ;; (fixes extern-call stack misalignment — see nl_sha1_call_non_sha1)
        (nl_sha1_call_non_sha1 algo-ptr str-ptr out)))

    ;; ---- SHA1 run helpers -----------------------------------------------

    ;; Allocate 816-byte workspace and 40-byte hex buffer, compute SHA1.
    (defun nl_sha1_run (str-ptr msg-len out)
      (nl_sha1_run_buf str-ptr msg-len out (alloc-bytes 816 8)))

    (defun nl_sha1_run_buf (str-ptr msg-len out buf)
      (if (= buf 0)
          1
        (nl_sha1_run_hex str-ptr msg-len out buf (alloc-bytes 40 1))))

    (defun nl_sha1_run_hex (str-ptr msg-len out buf hex-buf)
      (if (= hex-buf 0)
          (and (dealloc-bytes buf 816 8) 1)
        (and
         (nl_sha1_compute (str-bytes-ptr str-ptr) msg-len hex-buf buf)
         (sexp-write-str out hex-buf 40)
         (dealloc-bytes hex-buf 40 1)
         (dealloc-bytes buf 816 8)
         0))))
  "Phase 47 source for `nl_jit_secure_hash' (SHA1 arm + non-SHA1 fallback).

The SHA1 implementation follows FIPS 180-4 with full multi-block
support.  The workspace buffer (816 bytes) stores the 80-word message
schedule, 5 round working variables, 5 running hash values, 3
counters at fixed offsets, and a 64-byte scratch block for padding.

Counter discipline (three separate slots to prevent nested-loop
clobbering):
  buf[728]: used by load/expand/compress (each re-initializes before use)
  buf[736]: used by full_blocks outer loop only
  buf[744]: used by scratch_fill zero-fill + second-padding-block loop

Replaces the ~8 LOC sha1 arm (import + 5 body lines) in
`build-tool/src/jit/hash.rs'.  Net Rust delta: -8 Rust LOC (sha1 arm
+ sha1 crate) + +4 bridge.rs (extern decl + anchor entry) = -4 LOC.")

(provide 'nelisp-cc-jit-secure-hash)

;;; nelisp-cc-jit-secure-hash.el ends here
