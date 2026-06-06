;;; nelisp-cc-jit-secure-hash-ext.el --- AOT SHA256/SHA224/SHA512/SHA384/MD5  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT migration of the non-SHA1 arm of `nl_jit_secure_hash_non_sha1'
;; from `build-tool/src/jit/hash.rs' (sha2 + md5 crates).
;;
;; Provides:
;;   nl_sha256_compute   sha224/sha256 in 32-bit arithmetic
;;   nl_sha512_compute   sha384/sha512 in 64-bit arithmetic
;;   nl_md5_compute      md5 in 32-bit little-endian arithmetic
;;
;; All follow the same workspace-buffer pattern as nelisp-cc-jit-secure-hash.el
;; (SHA1 arm).  K and T constants are written into the workspace at the start
;; of each hash computation to avoid giant nested-if dispatch trees.
;;
;; SHA256 workspace layout (buf, 1856 bytes, align 8):
;;   [0..511]    w[0..63]   message schedule  (64 × u64 slots)
;;   [512..575]  a..h       round working vars (8 × u64)
;;   [576..639]  H[0..7]    running hash words (8 × u64)
;;   [640..647]  T1         scratch
;;   [648..655]  T2         scratch
;;   [656..663]  ctr_i      inner loop counter (re-init each sub-function)
;;   [664..671]  ctr_blk    outer block loop counter
;;   [672..679]  ctr_z      zero-fill counter
;;   [680..743]  scratch64  64-byte padding scratch block
;;   [744..1255] K[0..63]   SHA256 round constants (written once at start)
;;
;; SHA512 workspace layout (buf, 1856 bytes, align 8) — same offsets:
;;   [0..639]    w[0..79]   message schedule  (80 × u64 slots)
;;   [640..703]  a..h       round working vars (8 × u64)
;;   [704..767]  H[0..7]    running hash       (8 × u64)
;;   [768..775]  T1 scratch
;;   [776..783]  T2 scratch
;;   [784..791]  ctr_i
;;   [792..799]  ctr_blk
;;   [800..807]  ctr_z
;;   [808..935]  scratch128  128-byte padding scratch block
;;   [936..1575] K[0..79]   SHA512 round constants (written once at start)
;;
;; MD5 workspace layout (buf, 784 bytes, align 8):
;;   [0..127]    M[0..15]   message words (16 × u64, little-endian u32 in low half)
;;   [128..135]  a
;;   [136..143]  b
;;   [144..151]  c
;;   [152..159]  d
;;   [160..167]  AA (saved a)
;;   [168..175]  BB (saved b)
;;   [176..183]  CC (saved c)
;;   [184..191]  DD (saved d)
;;   [192..199]  T scratch
;;   [200..207]  ctr_i
;;   [208..271]  scratch64
;;   [272..783]  T_MD5[0..63] round constants (written once at start)
;;
;; while-in-and fix: same as SHA1 — `(or (while ...) 1)'.
;;
;; 32-bit literals >= 0x80000000 encoded as (+ (shl 1 31) low31).
;; 64-bit K512 constants encoded as (+ (shl hi32 32) lo32).

;;; Code:

(defconst nelisp-cc-jit-secure-hash-ext--source
  '(seq

    ;; ============================================================
    ;; Shared helpers (used by SHA256/SHA224)
    ;; ============================================================

    ;; 32-bit mask helper
    (defun nl_h32_mask (x)
      (logand x (- (shl 1 32) 1)))

    ;; 32-bit rotate-right: rotr32(x, n) = (x >> n) | (x << (32-n))
    ;; x must be pre-masked to 32 bits.
    (defun nl_h32_rotr (x n)
      (nl_h32_mask
       (logior (logand (sar x n) (- (shl 1 (- 32 n)) 1))
               (shl (nl_h32_mask x) (- 32 n)))))

    ;; 32-bit shift-right (logical, unsigned): shr32(x, n)
    (defun nl_h32_shr (x n)
      (logand (sar x n) (- (shl 1 (- 32 n)) 1)))

    ;; SHA256 sigma0: rotr(x,7) ^ rotr(x,18) ^ shr(x,3)
    (defun nl_sha256_sigma0 (x)
      (logxor (nl_h32_rotr x 7)
              (logxor (nl_h32_rotr x 18)
                      (nl_h32_shr x 3))))

    ;; SHA256 sigma1: rotr(x,17) ^ rotr(x,19) ^ shr(x,10)
    (defun nl_sha256_sigma1 (x)
      (logxor (nl_h32_rotr x 17)
              (logxor (nl_h32_rotr x 19)
                      (nl_h32_shr x 10))))

    ;; SHA256 Sigma0: rotr(x,2) ^ rotr(x,13) ^ rotr(x,22)
    (defun nl_sha256_Sigma0 (x)
      (logxor (nl_h32_rotr x 2)
              (logxor (nl_h32_rotr x 13)
                      (nl_h32_rotr x 22))))

    ;; SHA256 Sigma1: rotr(x,6) ^ rotr(x,11) ^ rotr(x,25)
    (defun nl_sha256_Sigma1 (x)
      (logxor (nl_h32_rotr x 6)
              (logxor (nl_h32_rotr x 11)
                      (nl_h32_rotr x 25))))

    ;; Ch(x,y,z) = (x AND y) XOR (NOT x AND z)
    (defun nl_sha256_ch (x y z)
      (nl_h32_mask
       (logxor (logand x y)
               (logand (logxor x (- (shl 1 32) 1)) z))))

    ;; Maj(x,y,z) = (x AND y) XOR (x AND z) XOR (y AND z)
    (defun nl_sha256_maj (x y z)
      (nl_h32_mask
       (logxor (logand x y)
               (logxor (logand x z)
                       (logand y z)))))

    ;; ---- Write SHA256 K constants into buf[744..1255] --------------------
    ;; 64 constants × u64 slots = 512 bytes.  Offset = 744 + i*8.
    (defun nl_sha256_init_k (buf)
      (and
       (ptr-write-u64 buf 744 1116352408)
       (ptr-write-u64 buf 752 1899447441)
       (ptr-write-u64 buf 760 (+ (shl 1 31) 901839823))
       (ptr-write-u64 buf 768 (+ (shl 1 31) 1773525925))
       (ptr-write-u64 buf 776 961987163)
       (ptr-write-u64 buf 784 1508970993)
       (ptr-write-u64 buf 792 (+ (shl 1 31) 306152100))
       (ptr-write-u64 buf 800 (+ (shl 1 31) 723279573))
       (ptr-write-u64 buf 808 (+ (shl 1 31) 1476897432))
       (ptr-write-u64 buf 816 310598401)
       (ptr-write-u64 buf 824 607225278)
       (ptr-write-u64 buf 832 1426881987)
       (ptr-write-u64 buf 840 1925078388)
       (ptr-write-u64 buf 848 (+ (shl 1 31) 14594558))
       (ptr-write-u64 buf 856 (+ (shl 1 31) 467404455))
       (ptr-write-u64 buf 864 (+ (shl 1 31) 1100738932))
       (ptr-write-u64 buf 872 (+ (shl 1 31) 1687906753))
       (ptr-write-u64 buf 880 (+ (shl 1 31) 1874741126))
       (ptr-write-u64 buf 888 264347078)
       (ptr-write-u64 buf 896 604807628)
       (ptr-write-u64 buf 904 770255983)
       (ptr-write-u64 buf 912 1249150122)
       (ptr-write-u64 buf 920 1555081692)
       (ptr-write-u64 buf 928 1996064986)
       (ptr-write-u64 buf 936 (+ (shl 1 31) 406737234))
       (ptr-write-u64 buf 944 (+ (shl 1 31) 674350701))
       (ptr-write-u64 buf 952 (+ (shl 1 31) 805513160))
       (ptr-write-u64 buf 960 (+ (shl 1 31) 1062830023))
       (ptr-write-u64 buf 968 (+ (shl 1 31) 1189088243))
       (ptr-write-u64 buf 976 (+ (shl 1 31) 1437045063))
       (ptr-write-u64 buf 984 113926993)
       (ptr-write-u64 buf 992 338241895)
       (ptr-write-u64 buf 1000 666307205)
       (ptr-write-u64 buf 1008 773529912)
       (ptr-write-u64 buf 1016 1294757372)
       (ptr-write-u64 buf 1024 1396182291)
       (ptr-write-u64 buf 1032 1695183700)
       (ptr-write-u64 buf 1040 1986661051)
       (ptr-write-u64 buf 1048 (+ (shl 1 31) 29542702))
       (ptr-write-u64 buf 1056 (+ (shl 1 31) 309472389))
       (ptr-write-u64 buf 1064 (+ (shl 1 31) 583002273))
       (ptr-write-u64 buf 1072 (+ (shl 1 31) 672818763))
       (ptr-write-u64 buf 1080 (+ (shl 1 31) 1112247152))
       (ptr-write-u64 buf 1088 (+ (shl 1 31) 1198281123))
       (ptr-write-u64 buf 1096 (+ (shl 1 31) 1368582169))
       (ptr-write-u64 buf 1104 (+ (shl 1 31) 1452869156))
       (ptr-write-u64 buf 1112 (+ (shl 1 31) 1947088261))
       (ptr-write-u64 buf 1120 275423344)
       (ptr-write-u64 buf 1128 430227734)
       (ptr-write-u64 buf 1136 506948616)
       (ptr-write-u64 buf 1144 659060556)
       (ptr-write-u64 buf 1152 883997877)
       (ptr-write-u64 buf 1160 958139571)
       (ptr-write-u64 buf 1168 1322822218)
       (ptr-write-u64 buf 1176 1537002063)
       (ptr-write-u64 buf 1184 1747873779)
       (ptr-write-u64 buf 1192 1955562222)
       (ptr-write-u64 buf 1200 2024104815)
       (ptr-write-u64 buf 1208 (+ (shl 1 31) 80246804))
       (ptr-write-u64 buf 1216 (+ (shl 1 31) 214368776))
       (ptr-write-u64 buf 1224 (+ (shl 1 31) 280952826))
       (ptr-write-u64 buf 1232 (+ (shl 1 31) 609250539))
       (ptr-write-u64 buf 1240 (+ (shl 1 31) 1056547831))
       (ptr-write-u64 buf 1248 (+ (shl 1 31) 1181841650))))

    ;; ---- Load w[0..15] from big-endian 32-bit words ----------------------
    ;; SHA256: 16 words × 4 bytes = 64-byte block.
    ;; buf[656] = ctr_i (re-init here).
    (defun nl_sha256_load (bytes byte-off buf)
      (and
       (ptr-write-u64 buf 656 0)
       (or (while (< (ptr-read-u64 buf 656) 16)
             (ptr-write-u64 buf
               (* (ptr-read-u64 buf 656) 8)
               (nl_h32_mask
                (logior
                 (shl (ptr-read-u8 bytes (+ byte-off (* (ptr-read-u64 buf 656) 4))) 24)
                 (logior
                  (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 656) 4) 1))) 16)
                  (logior
                   (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 656) 4) 2))) 8)
                   (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 656) 4) 3))))))))
             (ptr-write-u64 buf 656 (+ (ptr-read-u64 buf 656) 1)))
           1)))

    ;; ---- SHA256 message schedule expansion w[16..63] --------------------
    ;; w[i] = sigma1(w[i-2]) + w[i-7] + sigma0(w[i-15]) + w[i-16]
    ;; buf[656] = ctr_i (re-init here).
    (defun nl_sha256_expand (buf)
      (and
       (ptr-write-u64 buf 656 16)
       (or (while (< (ptr-read-u64 buf 656) 64)
             (ptr-write-u64 buf
               (* (ptr-read-u64 buf 656) 8)
               (nl_h32_mask
                (+ (nl_sha256_sigma1
                    (ptr-read-u64 buf (* (- (ptr-read-u64 buf 656) 2) 8)))
                   (+ (ptr-read-u64 buf (* (- (ptr-read-u64 buf 656) 7) 8))
                      (+ (nl_sha256_sigma0
                          (ptr-read-u64 buf (* (- (ptr-read-u64 buf 656) 15) 8)))
                         (ptr-read-u64 buf (* (- (ptr-read-u64 buf 656) 16) 8)))))))
             (ptr-write-u64 buf 656 (+ (ptr-read-u64 buf 656) 1)))
           1)))

    ;; ---- SHA256 64-round compression ------------------------------------
    ;; Working vars: a=512, b=520, c=528, d=536, e=544, f=552, g=560, h=568
    ;; Hash:         H0=576, H1=584, H2=592, H3=600, H4=608, H5=616, H6=624, H7=632
    ;; T1=640, T2=648, ctr_i=656.  K[i] at buf[744 + i*8].
    (defun nl_sha256_compress (buf)
      (and
       ;; Init working vars from H
       (ptr-write-u64 buf 512 (ptr-read-u64 buf 576))
       (ptr-write-u64 buf 520 (ptr-read-u64 buf 584))
       (ptr-write-u64 buf 528 (ptr-read-u64 buf 592))
       (ptr-write-u64 buf 536 (ptr-read-u64 buf 600))
       (ptr-write-u64 buf 544 (ptr-read-u64 buf 608))
       (ptr-write-u64 buf 552 (ptr-read-u64 buf 616))
       (ptr-write-u64 buf 560 (ptr-read-u64 buf 624))
       (ptr-write-u64 buf 568 (ptr-read-u64 buf 632))
       ;; Init round counter
       (ptr-write-u64 buf 656 0)
       ;; 64-round loop
       (or (while (< (ptr-read-u64 buf 656) 64)
             ;; T1 = h + Sigma1(e) + Ch(e,f,g) + K[i] + w[i]
             (ptr-write-u64 buf 640
               (nl_h32_mask
                (+ (ptr-read-u64 buf 568)
                   (+ (nl_sha256_Sigma1 (ptr-read-u64 buf 544))
                      (+ (nl_sha256_ch
                          (ptr-read-u64 buf 544)
                          (ptr-read-u64 buf 552)
                          (ptr-read-u64 buf 560))
                         (+ (ptr-read-u64 buf (+ 744 (* (ptr-read-u64 buf 656) 8)))
                            (ptr-read-u64 buf (* (ptr-read-u64 buf 656) 8))))))))
             ;; T2 = Sigma0(a) + Maj(a,b,c)
             (ptr-write-u64 buf 648
               (nl_h32_mask
                (+ (nl_sha256_Sigma0 (ptr-read-u64 buf 512))
                   (nl_sha256_maj
                    (ptr-read-u64 buf 512)
                    (ptr-read-u64 buf 520)
                    (ptr-read-u64 buf 528)))))
             ;; Rotate: h=g, g=f, f=e, e=d+T1, d=c, c=b, b=a, a=T1+T2
             (ptr-write-u64 buf 568 (ptr-read-u64 buf 560))
             (ptr-write-u64 buf 560 (ptr-read-u64 buf 552))
             (ptr-write-u64 buf 552 (ptr-read-u64 buf 544))
             (ptr-write-u64 buf 544 (nl_h32_mask (+ (ptr-read-u64 buf 536) (ptr-read-u64 buf 640))))
             (ptr-write-u64 buf 536 (ptr-read-u64 buf 528))
             (ptr-write-u64 buf 528 (ptr-read-u64 buf 520))
             (ptr-write-u64 buf 520 (ptr-read-u64 buf 512))
             (ptr-write-u64 buf 512 (nl_h32_mask (+ (ptr-read-u64 buf 640) (ptr-read-u64 buf 648))))
             ;; i++
             (ptr-write-u64 buf 656 (+ (ptr-read-u64 buf 656) 1)))
           1)
       ;; H += working vars
       (ptr-write-u64 buf 576 (nl_h32_mask (+ (ptr-read-u64 buf 576) (ptr-read-u64 buf 512))))
       (ptr-write-u64 buf 584 (nl_h32_mask (+ (ptr-read-u64 buf 584) (ptr-read-u64 buf 520))))
       (ptr-write-u64 buf 592 (nl_h32_mask (+ (ptr-read-u64 buf 592) (ptr-read-u64 buf 528))))
       (ptr-write-u64 buf 600 (nl_h32_mask (+ (ptr-read-u64 buf 600) (ptr-read-u64 buf 536))))
       (ptr-write-u64 buf 608 (nl_h32_mask (+ (ptr-read-u64 buf 608) (ptr-read-u64 buf 544))))
       (ptr-write-u64 buf 616 (nl_h32_mask (+ (ptr-read-u64 buf 616) (ptr-read-u64 buf 552))))
       (ptr-write-u64 buf 624 (nl_h32_mask (+ (ptr-read-u64 buf 624) (ptr-read-u64 buf 560))))
       (ptr-write-u64 buf 632 (nl_h32_mask (+ (ptr-read-u64 buf 632) (ptr-read-u64 buf 568))))))

    ;; ---- Process full 64-byte blocks (SHA256) ----------------------------
    ;; buf[664] = ctr_blk outer counter (avoids clobbering buf[656]).
    (defun nl_sha256_full_blocks (bytes num-blocks buf)
      (and
       (ptr-write-u64 buf 664 0)
       (or (while (< (ptr-read-u64 buf 664) num-blocks)
             (and
              (nl_sha256_load bytes (* (ptr-read-u64 buf 664) 64) buf)
              (nl_sha256_expand buf)
              (nl_sha256_compress buf))
             (ptr-write-u64 buf 664 (+ (ptr-read-u64 buf 664) 1)))
           1)))

    ;; ---- Copy rem bytes + zero-fill into scratch[680..743] ---------------
    (defun nl_sha256_scratch_fill (bytes offset rem buf)
      (and
       (ptr-write-u64 buf 656 0)
       (or (while (< (ptr-read-u64 buf 656) rem)
             (ptr-write-u8 buf
               (+ 680 (ptr-read-u64 buf 656))
               (ptr-read-u8 bytes (+ offset (ptr-read-u64 buf 656))))
             (ptr-write-u64 buf 656 (+ (ptr-read-u64 buf 656) 1)))
           1)
       (ptr-write-u64 buf 672 rem)
       (or (while (< (ptr-read-u64 buf 672) 64)
             (ptr-write-u8 buf (+ 680 (ptr-read-u64 buf 672)) 0)
             (ptr-write-u64 buf 672 (+ (ptr-read-u64 buf 672) 1)))
           1)))

    ;; ---- Write 8-byte big-endian bit-length into scratch[56..63] --------
    (defun nl_sha256_write_length (msg-bits buf)
      (and
       (ptr-write-u8 buf 736 (logand (sar msg-bits 56) 255))
       (ptr-write-u8 buf 737 (logand (sar msg-bits 48) 255))
       (ptr-write-u8 buf 738 (logand (sar msg-bits 40) 255))
       (ptr-write-u8 buf 739 (logand (sar msg-bits 32) 255))
       (ptr-write-u8 buf 740 (logand (sar msg-bits 24) 255))
       (ptr-write-u8 buf 741 (logand (sar msg-bits 16) 255))
       (ptr-write-u8 buf 742 (logand (sar msg-bits 8) 255))
       (ptr-write-u8 buf 743 (logand msg-bits 255))))

    ;; ---- Process final block(s) from scratch[680..743] -------------------
    (defun nl_sha256_final_block (buf)
      (and
       (nl_sha256_load buf 680 buf)
       (nl_sha256_expand buf)
       (nl_sha256_compress buf)))

    ;; ---- Write hex for 32-bit word at hex-buf[offset..offset+7] ---------
    ;; Reuses nl_sha1_hex_nibble from the SHA1 object (already loaded).
    ;; Define local equivalent to avoid cross-object dependency.
    (defun nl_sha256_hex_nibble (hex-buf offset nibble)
      (ptr-write-u8 hex-buf offset
        (if (< nibble 10)
            (+ nibble 48)
          (+ nibble 87))))

    (defun nl_sha256_hex_word (hex-buf offset word)
      (and
       (nl_sha256_hex_nibble hex-buf offset       (logand (sar word 28) 15))
       (nl_sha256_hex_nibble hex-buf (+ offset 1) (logand (sar word 24) 15))
       (nl_sha256_hex_nibble hex-buf (+ offset 2) (logand (sar word 20) 15))
       (nl_sha256_hex_nibble hex-buf (+ offset 3) (logand (sar word 16) 15))
       (nl_sha256_hex_nibble hex-buf (+ offset 4) (logand (sar word 12) 15))
       (nl_sha256_hex_nibble hex-buf (+ offset 5) (logand (sar word  8) 15))
       (nl_sha256_hex_nibble hex-buf (+ offset 6) (logand (sar word  4) 15))
       (nl_sha256_hex_nibble hex-buf (+ offset 7) (logand word          15))))

    ;; ---- SHA256 H-init for sha256 ----------------------------------------
    ;; H0=0x6a09e667=1779033703, H1=0xbb67ae85=(+ (shl 1 31) 996650629)
    ;; H2=0x3c6ef372=1013904242, H3=0xa54ff53a=(+ (shl 1 31) 625997114)
    ;; H4=0x510e527f=1359893119, H5=0x9b05688c=(+ (shl 1 31) 453339276)
    ;; H6=0x1f83d9ab=528734635,  H7=0x5be0cd19=1541459225
    (defun nl_sha256_init_h256 (buf)
      (and
       (ptr-write-u64 buf 576 1779033703)
       (ptr-write-u64 buf 584 (+ (shl 1 31) 996650629))
       (ptr-write-u64 buf 592 1013904242)
       (ptr-write-u64 buf 600 (+ (shl 1 31) 625997114))
       (ptr-write-u64 buf 608 1359893119)
       (ptr-write-u64 buf 616 (+ (shl 1 31) 453339276))
       (ptr-write-u64 buf 624 528734635)
       (ptr-write-u64 buf 632 1541459225)))

    ;; ---- SHA224 H-init ---------------------------------------------------
    ;; Different IVs; shares SHA256 compression + K constants.
    ;; H0=0xc1059ed8=(+ (shl 1 31) 1090887384)
    ;; H1=0x367cd507=914150663
    ;; H2=0x3070dd17=812702999
    ;; H3=0xf70e5939=(+ (shl 1 31) 1997429049)
    ;; H4=0xffc00b31=(+ (shl 1 31) 2143292209) — note: 0xffc00b31 = 4291411761
    ;; H5=0x68581511=1750603025
    ;; H6=0x64f98fa7=1694076839
    ;; H7=0xbefa4fa4=(+ (shl 1 31) 1056591780)
    (defun nl_sha224_init_h (buf)
      (and
       (ptr-write-u64 buf 576 (+ (shl 1 31) 1090887384))
       (ptr-write-u64 buf 584 914150663)
       (ptr-write-u64 buf 592 812702999)
       (ptr-write-u64 buf 600 (+ (shl 1 31) 1997429049))
       (ptr-write-u64 buf 608 (+ (shl 1 31) 2143292209))
       (ptr-write-u64 buf 616 1750603025)
       (ptr-write-u64 buf 624 1694076839)
       (ptr-write-u64 buf 632 (+ (shl 1 31) 1056591780))))

    ;; ---- Main SHA256/SHA224 computation ----------------------------------
    ;; bytes: *const u8.  msg-len: i64.  hex-buf: *mut u8.
    ;; buf: 1256-byte workspace.
    ;; is-sha224: 0 for sha256, 1 for sha224.
    (defun nl_sha256_compute (bytes msg-len hex-buf buf is-sha224)
      (and
       ;; Init K constants into buf[744..1255]
       (nl_sha256_init_k buf)
       ;; Init H
       (if (= is-sha224 1)
           (nl_sha224_init_h buf)
         (nl_sha256_init_h256 buf))
       ;; Full blocks
       (nl_sha256_full_blocks bytes (sar msg-len 6) buf)
       ;; Fill scratch with remaining bytes + 0x80 pad
       (nl_sha256_scratch_fill bytes (* (sar msg-len 6) 64) (logand msg-len 63) buf)
       ;; Write 0x80 at scratch[rem] = buf[680 + rem]
       (ptr-write-u8 buf (+ 680 (logand msg-len 63)) 128)
       ;; Branch on rem vs 55
       (if (<= (logand msg-len 63) 55)
           (and
            (nl_sha256_write_length (* msg-len 8) buf)
            (nl_sha256_final_block buf))
         (and
          (nl_sha256_final_block buf)
          ;; Zero-fill scratch again
          (ptr-write-u64 buf 672 0)
          (or (while (< (ptr-read-u64 buf 672) 64)
                (ptr-write-u8 buf (+ 680 (ptr-read-u64 buf 672)) 0)
                (ptr-write-u64 buf 672 (+ (ptr-read-u64 buf 672) 1)))
              1)
          (nl_sha256_write_length (* msg-len 8) buf)
          (nl_sha256_final_block buf)))
       ;; Hex-encode H[0..7] (sha256 = 64 chars; sha224 = 56 chars = 7 words)
       (nl_sha256_hex_word hex-buf 0  (ptr-read-u64 buf 576))
       (nl_sha256_hex_word hex-buf 8  (ptr-read-u64 buf 584))
       (nl_sha256_hex_word hex-buf 16 (ptr-read-u64 buf 592))
       (nl_sha256_hex_word hex-buf 24 (ptr-read-u64 buf 600))
       (nl_sha256_hex_word hex-buf 32 (ptr-read-u64 buf 608))
       (nl_sha256_hex_word hex-buf 40 (ptr-read-u64 buf 616))
       (nl_sha256_hex_word hex-buf 48 (ptr-read-u64 buf 624))
       (if (= is-sha224 0)
           (nl_sha256_hex_word hex-buf 56 (ptr-read-u64 buf 632))
         0)))

    ;; ---- SHA256/SHA224 run helpers ---------------------------------------
    ;; hex-buf size: sha256=64 bytes, sha224=56 bytes.
    (defun nl_sha256_run (str-ptr msg-len out is-sha224)
      (nl_sha256_run_buf str-ptr msg-len out is-sha224 (alloc-bytes 1256 8)))

    (defun nl_sha256_run_buf (str-ptr msg-len out is-sha224 buf)
      (if (= buf 0)
          1
        (nl_sha256_run_hex str-ptr msg-len out is-sha224 buf
                           (alloc-bytes (if (= is-sha224 1) 56 64) 1))))

    (defun nl_sha256_run_hex (str-ptr msg-len out is-sha224 buf hex-buf)
      (if (= hex-buf 0)
          (and (dealloc-bytes buf 1256 8) 1)
        (and
         (nl_sha256_compute (str-bytes-ptr str-ptr) msg-len hex-buf buf is-sha224)
         (sexp-write-str out hex-buf (if (= is-sha224 1) 56 64))
         (dealloc-bytes hex-buf (if (= is-sha224 1) 56 64) 1)
         (dealloc-bytes buf 1256 8)
         0)))

    ;; ============================================================
    ;; SHA512 / SHA384
    ;; ============================================================

    ;; 64-bit mask: AOT i64 is already 64-bit signed,
    ;; but we mask to ensure logical (not arithmetic) behavior.
    ;; mask64 = all bits set = -1 in two's complement.
    ;; We use (logand x -1) which is identity — just use x directly.
    ;; For rotr64 we need to handle the sign bit carefully.

    ;; rotr64(x, n): rotate-right 64-bit.
    ;; (sar x n) is arithmetic right shift — for logical shift we mask.
    ;; Logical right shift: (logand (sar x n) (>> (shl 1 (64-n)) 0))
    ;; But 64-bit mask: (- (shl 1 63) 1) gives 0x7FFF... which is wrong.
    ;; Use: lsr64(x, n) = (logand (sar x n) lsr_mask(n))
    ;; where lsr_mask(n) = all bits except the top n = -1 >> n (logical)
    ;; We can compute: lsr_mask(n) = (- (shl 1 (- 64 n)) 1) ... only works for n<63
    ;; Actually for 64-bit: AOT i64 is signed, sar is arithmetic (sign-extends).
    ;; Logical right shift of 64-bit: (logand (sar x n) (sar -1 n))
    ;; Because sar(-1, n) fills with 1s in the top: 0xFFFF...FFFF >> n = 0x00...0111...1
    ;; Wait: sar(-1, n) = -1 (all ones) for any n (arithmetic shift of all-ones is all-ones).
    ;; Actually: sar of a negative value fills with 1-bits. sar(-1, n) = -1.
    ;; So for logical shift: we need to mask off the top bits manually.
    ;; lsr64(x, n) = (logand (sar x n) (- (shl 1 (- 64 n)) 1))
    ;; But (shl 1 64) overflows! Max safe shl is 62.
    ;; For n=1: mask = (- (shl 1 63) 1) = 0x7FFF...
    ;; For n=63: mask = 1
    ;; We encode the mask per rotation amount in rotr64 — but n is a constant in SHA512.

    ;; SHA512 uses rotr64 with n=1,8,14,18,19,28,34,39,41,61.
    ;; Each is a compile-time constant so we inline the mask.

    ;; lsr64_n: logical right shift by constant n (n < 64).
    ;; Inline helper: (logand (sar x n) mask) where mask = 2^(64-n) - 1
    ;; For n=6:  mask = 2^58 - 1 = (- (shl 1 58) 1)  -- fits in i64
    ;; For n=7:  mask = 2^57 - 1 = (- (shl 1 57) 1)
    ;; For n=8:  mask = 2^56 - 1
    ;; We define generic versions using the AOT shl/sar:

    ;; Generic lsr64 for n <= 62:
    (defun nl_h64_lsr (x n)
      (logand (sar x n) (- (shl 1 (- 64 n)) 1)))

    ;; Special lsr for n=63: result is 0 or 1 (top bit)
    (defun nl_h64_lsr63 (x)
      (logand (sar x 63) 1))

    ;; rotr64(x, n) = lsr(x, n) | shl(x, 64-n)
    ;; But shl by 64-n: if 64-n > 63 that's invalid; for rotr64(x,1), shift left by 63.
    ;; AOT shl of negative values: (shl x 63) shifts the LSB to position 63.
    ;; We need to handle the upper bits wrapping around.
    ;; For rotr64(x, n):
    ;;   right part: lsr64(x, n)  -- zeros in top n bits
    ;;   left part:  (shl x (- 64 n))  -- takes bottom (64-n) bits and puts them at top
    ;; But (shl x k) for k < 64 just shifts left; AOT uses i64 so top bits overflow.
    ;; Actually for the left part: only the bottom (64-n) bits of x contribute,
    ;; and we need them at positions n..63.
    ;; (shl x (- 64 n)) puts bit 0 at position (64-n), bit 1 at (65-n), etc.
    ;; But positions >= 64 are lost. For n>=1, (64-n)<=63, so bit 0 goes to bit (64-n).
    ;; The top n bits of x after shl(x, 64-n) have overflowed. That's fine — those
    ;; are the bits we're rotating into the low positions via lsr. The OR combines them.
    ;; This is correct because i64 arithmetic truncates at 64 bits on overflow.
    ;; AOT shl is 64-bit (mod 2^64 behavior? Let's verify by checking SHA1 uses 32-bit).
    ;; For safety, mask x to 64 bits before shl: x & 0xFFFF... = x (already 64-bit).
    ;; So: rotr64(x, n) = lsr64(x, n) | shl(x, 64-n)

    (defun nl_h64_rotr1 (x)
      (logior (nl_h64_lsr x 1) (shl x 63)))

    (defun nl_h64_rotr8 (x)
      (logior (nl_h64_lsr x 8) (shl x 56)))

    (defun nl_h64_rotr14 (x)
      (logior (nl_h64_lsr x 14) (shl x 50)))

    (defun nl_h64_rotr18 (x)
      (logior (nl_h64_lsr x 18) (shl x 46)))

    (defun nl_h64_rotr19 (x)
      (logior (nl_h64_lsr x 19) (shl x 45)))

    (defun nl_h64_rotr28 (x)
      (logior (nl_h64_lsr x 28) (shl x 36)))

    (defun nl_h64_rotr34 (x)
      (logior (nl_h64_lsr x 34) (shl x 30)))

    (defun nl_h64_rotr39 (x)
      (logior (nl_h64_lsr x 39) (shl x 25)))

    (defun nl_h64_rotr41 (x)
      (logior (nl_h64_lsr x 41) (shl x 23)))

    (defun nl_h64_rotr61 (x)
      (logior (nl_h64_lsr x 61) (shl x 3)))

    ;; SHA512 sigma0: rotr(x,1) ^ rotr(x,8) ^ shr(x,7)
    (defun nl_sha512_sigma0 (x)
      (logxor (nl_h64_rotr1 x)
              (logxor (nl_h64_rotr8 x)
                      (nl_h64_lsr x 7))))

    ;; SHA512 sigma1: rotr(x,19) ^ rotr(x,61) ^ shr(x,6)
    (defun nl_sha512_sigma1 (x)
      (logxor (nl_h64_rotr19 x)
              (logxor (nl_h64_rotr61 x)
                      (nl_h64_lsr x 6))))

    ;; SHA512 Sigma0: rotr(x,28) ^ rotr(x,34) ^ rotr(x,39)
    (defun nl_sha512_Sigma0 (x)
      (logxor (nl_h64_rotr28 x)
              (logxor (nl_h64_rotr34 x)
                      (nl_h64_rotr39 x))))

    ;; SHA512 Sigma1: rotr(x,14) ^ rotr(x,18) ^ rotr(x,41)
    (defun nl_sha512_Sigma1 (x)
      (logxor (nl_h64_rotr14 x)
              (logxor (nl_h64_rotr18 x)
                      (nl_h64_rotr41 x))))

    ;; Ch64 and Maj64 (same formula as SHA256 but 64-bit)
    (defun nl_sha512_ch (x y z)
      (logxor (logand x y)
              (logand (logxor x -1) z)))

    (defun nl_sha512_maj (x y z)
      (logxor (logand x y)
              (logxor (logand x z)
                      (logand y z))))

    ;; ---- Write SHA512 K constants into buf[936..1575] -------------------
    ;; 80 constants × 8 bytes = 640 bytes.  Offset = 936 + i*8.
    ;; Base 936 = end of scratch128 (808 + 128 = 936).
    (defun nl_sha512_init_k (buf)
      (and
       (ptr-write-u64 buf 936 (+ (shl 1116352408 32) (+ (shl 1 31) 1462283810)))
       (ptr-write-u64 buf 944 (+ (shl 1899447441 32) 602891725))
       (ptr-write-u64 buf 952 (+ (shl (+ (shl 1 31) 901839823) 32) (+ (shl 1 31) 1817000751)))
       (ptr-write-u64 buf 960 (+ (shl (+ (shl 1 31) 1773525925) 32) (+ (shl 1 31) 25811900)))
       (ptr-write-u64 buf 968 (+ (shl 961987163 32) (+ (shl 1 31) 1934144824)))
       (ptr-write-u64 buf 976 (+ (shl 1508970993 32) (+ (shl 1 31) 906350617)))
       (ptr-write-u64 buf 984 (+ (shl (+ (shl 1 31) 306152100) 32) (+ (shl 1 31) 790187931)))
       (ptr-write-u64 buf 992 (+ (shl (+ (shl 1 31) 723279573) 32) (+ (shl 1 31) 1517125912)))
       (ptr-write-u64 buf 1000 (+ (shl (+ (shl 1 31) 1476897432) 32) (+ (shl 1 31) 587399746)))
       (ptr-write-u64 buf 1008 (+ (shl 310598401 32) 1164996542))
       (ptr-write-u64 buf 1016 (+ (shl 607225278 32) 1323610764))
       (ptr-write-u64 buf 1024 (+ (shl 1426881987 32) (+ (shl 1 31) 1442821346)))
       (ptr-write-u64 buf 1032 (+ (shl 1925078388 32) (+ (shl 1 31) 1920698735)))
       (ptr-write-u64 buf 1040 (+ (shl (+ (shl 1 31) 14594558) 32) 991336113))
       (ptr-write-u64 buf 1048 (+ (shl (+ (shl 1 31) 467404455) 32) 633803317))
       (ptr-write-u64 buf 1056 (+ (shl (+ (shl 1 31) 1100738932) 32) (+ (shl 1 31) 1332291220)))
       (ptr-write-u64 buf 1064 (+ (shl (+ (shl 1 31) 1687906753) 32) (+ (shl 1 31) 519129810)))
       (ptr-write-u64 buf 1072 (+ (shl (+ (shl 1 31) 1874741126) 32) 944711139))
       (ptr-write-u64 buf 1080 (+ (shl 264347078 32) (+ (shl 1 31) 193779125)))
       (ptr-write-u64 buf 1088 (+ (shl 604807628 32) 2007800933))
       (ptr-write-u64 buf 1096 (+ (shl 770255983 32) 1495990901))
       (ptr-write-u64 buf 1104 (+ (shl 1249150122 32) 1856431235))
       (ptr-write-u64 buf 1112 (+ (shl 1555081692 32) (+ (shl 1 31) 1027734484)))
       (ptr-write-u64 buf 1120 (+ (shl 1996064986 32) (+ (shl 1 31) 51467189)))
       (ptr-write-u64 buf 1128 (+ (shl (+ (shl 1 31) 406737234) 32) (+ (shl 1 31) 1852235691)))
       (ptr-write-u64 buf 1136 (+ (shl (+ (shl 1 31) 674350701) 32) 766784016))
       (ptr-write-u64 buf 1144 (+ (shl (+ (shl 1 31) 805513160) 32) (+ (shl 1 31) 419111231)))
       (ptr-write-u64 buf 1152 (+ (shl (+ (shl 1 31) 1062830023) 32) (+ (shl 1 31) 1055854308)))
       (ptr-write-u64 buf 1160 (+ (shl (+ (shl 1 31) 1189088243) 32) 1034457026))
       (ptr-write-u64 buf 1168 (+ (shl (+ (shl 1 31) 1437045063) 32) (+ (shl 1 31) 319465253)))
       (ptr-write-u64 buf 1176 (+ (shl 113926993 32) (+ (shl 1 31) 1610842735)))
       (ptr-write-u64 buf 1184 (+ (shl 338241895 32) 168717936))
       (ptr-write-u64 buf 1192 (+ (shl 666307205 32) 1188179964))
       (ptr-write-u64 buf 1200 (+ (shl 773529912 32) 1546045734))
       (ptr-write-u64 buf 1208 (+ (shl 1294757372 32) 1522805485))
       (ptr-write-u64 buf 1216 (+ (shl 1396182291 32) (+ (shl 1 31) 496350175)))
       (ptr-write-u64 buf 1224 (+ (shl 1695183700 32) (+ (shl 1 31) 196043742)))
       (ptr-write-u64 buf 1232 (+ (shl 1986661051 32) 1014477480))
       (ptr-write-u64 buf 1240 (+ (shl (+ (shl 1 31) 29542702) 32) 1206759142))
       (ptr-write-u64 buf 1248 (+ (shl (+ (shl 1 31) 309472389) 32) 344077627))
       (ptr-write-u64 buf 1256 (+ (shl (+ (shl 1 31) 583002273) 32) 1290863460))
       (ptr-write-u64 buf 1264 (+ (shl (+ (shl 1 31) 672818763) 32) (+ (shl 1 31) 1010970625)))
       (ptr-write-u64 buf 1272 (+ (shl (+ (shl 1 31) 1112247152) 32) (+ (shl 1 31) 1358469009)))
       (ptr-write-u64 buf 1280 (+ (shl (+ (shl 1 31) 1198281123) 32) 106217008))
       (ptr-write-u64 buf 1288 (+ (shl (+ (shl 1 31) 1368582169) 32) (+ (shl 1 31) 1458524696)))
       (ptr-write-u64 buf 1296 (+ (shl (+ (shl 1 31) 1452869156) 32) 1432725776))
       (ptr-write-u64 buf 1304 (+ (shl (+ (shl 1 31) 1947088261) 32) 1467031594))
       (ptr-write-u64 buf 1312 (+ (shl 275423344 32) 851169720))
       (ptr-write-u64 buf 1320 (+ (shl 430227734 32) (+ (shl 1 31) 953340104)))
       (ptr-write-u64 buf 1328 (+ (shl 506948616 32) 1363258195))
       (ptr-write-u64 buf 1336 (+ (shl 659060556 32) (+ (shl 1 31) 1603201945)))
       (ptr-write-u64 buf 1344 (+ (shl 883997877 32) (+ (shl 1 31) 1637566632)))
       (ptr-write-u64 buf 1352 (+ (shl 958139571 32) (+ (shl 1 31) 1170823779)))
       (ptr-write-u64 buf 1360 (+ (shl 1322822218 32) (+ (shl 1 31) 1665239755)))
       (ptr-write-u64 buf 1368 (+ (shl 1537002063 32) 2003034995))
       (ptr-write-u64 buf 1376 (+ (shl 1747873779 32) (+ (shl 1 31) 1454553251)))
       (ptr-write-u64 buf 1384 (+ (shl 1955562222 32) 1575990012))
       (ptr-write-u64 buf 1392 (+ (shl 2024104815 32) 1125592928))
       (ptr-write-u64 buf 1400 (+ (shl (+ (shl 1 31) 80246804) 32) (+ (shl 1 31) 569420658)))
       (ptr-write-u64 buf 1408 (+ (shl (+ (shl 1 31) 214368776) 32) 442776044))
       (ptr-write-u64 buf 1416 (+ (shl (+ (shl 1 31) 280952826) 32) 593698344))
       (ptr-write-u64 buf 1424 (+ (shl (+ (shl 1 31) 609250539) 32) (+ (shl 1 31) 1585626601)))
       (ptr-write-u64 buf 1432 (+ (shl (+ (shl 1 31) 1056547831) 32) (+ (shl 1 31) 851867925)))
       (ptr-write-u64 buf 1440 (+ (shl (+ (shl 1 31) 1181841650) 32) (+ (shl 1 31) 1668436779)))
       (ptr-write-u64 buf 1448 (+ (shl (+ (shl 1 31) 1244085966) 32) (+ (shl 1 31) 1780900252)))
       (ptr-write-u64 buf 1456 (+ (shl (+ (shl 1 31) 1367783623) 32) 566280711))
       (ptr-write-u64 buf 1464 (+ (shl (+ (shl 1 31) 1792703958) 32) (+ (shl 1 31) 1306585886)))
       (ptr-write-u64 buf 1472 (+ (shl (+ (shl 1 31) 1971146623) 32) (+ (shl 1 31) 1852756344)))
       (ptr-write-u64 buf 1480 (+ (shl 116418474 32) 1914138554))
       (ptr-write-u64 buf 1488 (+ (shl 174292421 32) (+ (shl 1 31) 583571622)))
       (ptr-write-u64 buf 1496 (+ (shl 289380356 32) (+ (shl 1 31) 1056509358)))
       (ptr-write-u64 buf 1504 (+ (shl 460393269 32) 320620315))
       (ptr-write-u64 buf 1512 (+ (shl 685471733 32) 587496836))
       (ptr-write-u64 buf 1520 (+ (shl 852142971 32) 1086792851))
       (ptr-write-u64 buf 1528 (+ (shl 1017036298 32) 365543100))
       (ptr-write-u64 buf 1536 (+ (shl 1126000580 32) (+ (shl 1 31) 470814028)))
       (ptr-write-u64 buf 1544 (+ (shl 1288033470 32) (+ (shl 1 31) 1262371510)))
       (ptr-write-u64 buf 1552 (+ (shl 1501505948 32) (+ (shl 1 31) 2087026218)))
       (ptr-write-u64 buf 1560 (+ (shl 1607167915 32) 987167468))
       (ptr-write-u64 buf 1568 (+ (shl 1816402316 32) 1246189591))))

    ;; ---- Load w[0..15] from big-endian 64-bit words (SHA512) ------------
    ;; 16 words × 8 bytes = 128 bytes.
    ;; buf[784] = ctr_i.
    (defun nl_sha512_load (bytes byte-off buf)
      (and
       (ptr-write-u64 buf 784 0)
       (or (while (< (ptr-read-u64 buf 784) 16)
             (ptr-write-u64 buf
               (* (ptr-read-u64 buf 784) 8)
               (logior
                (shl (ptr-read-u8 bytes (+ byte-off (* (ptr-read-u64 buf 784) 8))) 56)
                (logior
                 (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 784) 8) 1))) 48)
                 (logior
                  (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 784) 8) 2))) 40)
                  (logior
                   (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 784) 8) 3))) 32)
                   (logior
                    (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 784) 8) 4))) 24)
                    (logior
                     (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 784) 8) 5))) 16)
                     (logior
                      (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 784) 8) 6))) 8)
                      (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 784) 8) 7)))))))))))
             (ptr-write-u64 buf 784 (+ (ptr-read-u64 buf 784) 1)))
           1)))

    ;; ---- SHA512 message schedule expansion w[16..79] --------------------
    (defun nl_sha512_expand (buf)
      (and
       (ptr-write-u64 buf 784 16)
       (or (while (< (ptr-read-u64 buf 784) 80)
             (ptr-write-u64 buf
               (* (ptr-read-u64 buf 784) 8)
               (+ (nl_sha512_sigma1
                   (ptr-read-u64 buf (* (- (ptr-read-u64 buf 784) 2) 8)))
                  (+ (ptr-read-u64 buf (* (- (ptr-read-u64 buf 784) 7) 8))
                     (+ (nl_sha512_sigma0
                         (ptr-read-u64 buf (* (- (ptr-read-u64 buf 784) 15) 8)))
                        (ptr-read-u64 buf (* (- (ptr-read-u64 buf 784) 16) 8))))))
             (ptr-write-u64 buf 784 (+ (ptr-read-u64 buf 784) 1)))
           1)))

    ;; ---- SHA512 80-round compression ------------------------------------
    ;; Working vars: a=640,b=648,c=656,d=664,e=672,f=680,g=688,h=696
    ;; Hash: H[0..7] at 704,712,720,728,736,744,752,760
    ;; T1=768, T2=776, ctr_i=784. K[i] at buf[936 + i*8].
    (defun nl_sha512_compress (buf)
      (and
       (ptr-write-u64 buf 640 (ptr-read-u64 buf 704))
       (ptr-write-u64 buf 648 (ptr-read-u64 buf 712))
       (ptr-write-u64 buf 656 (ptr-read-u64 buf 720))
       (ptr-write-u64 buf 664 (ptr-read-u64 buf 728))
       (ptr-write-u64 buf 672 (ptr-read-u64 buf 736))
       (ptr-write-u64 buf 680 (ptr-read-u64 buf 744))
       (ptr-write-u64 buf 688 (ptr-read-u64 buf 752))
       (ptr-write-u64 buf 696 (ptr-read-u64 buf 760))
       (ptr-write-u64 buf 784 0)
       (or (while (< (ptr-read-u64 buf 784) 80)
             ;; T1 = h + Sigma1(e) + Ch(e,f,g) + K[i] + w[i]
             (ptr-write-u64 buf 768
               (+ (ptr-read-u64 buf 696)
                  (+ (nl_sha512_Sigma1 (ptr-read-u64 buf 672))
                     (+ (nl_sha512_ch
                         (ptr-read-u64 buf 672)
                         (ptr-read-u64 buf 680)
                         (ptr-read-u64 buf 688))
                        (+ (ptr-read-u64 buf (+ 936 (* (ptr-read-u64 buf 784) 8)))
                           (ptr-read-u64 buf (* (ptr-read-u64 buf 784) 8)))))))
             ;; T2 = Sigma0(a) + Maj(a,b,c)
             (ptr-write-u64 buf 776
               (+ (nl_sha512_Sigma0 (ptr-read-u64 buf 640))
                  (nl_sha512_maj
                   (ptr-read-u64 buf 640)
                   (ptr-read-u64 buf 648)
                   (ptr-read-u64 buf 656))))
             ;; Rotate
             (ptr-write-u64 buf 696 (ptr-read-u64 buf 688))
             (ptr-write-u64 buf 688 (ptr-read-u64 buf 680))
             (ptr-write-u64 buf 680 (ptr-read-u64 buf 672))
             (ptr-write-u64 buf 672 (+ (ptr-read-u64 buf 664) (ptr-read-u64 buf 768)))
             (ptr-write-u64 buf 664 (ptr-read-u64 buf 656))
             (ptr-write-u64 buf 656 (ptr-read-u64 buf 648))
             (ptr-write-u64 buf 648 (ptr-read-u64 buf 640))
             (ptr-write-u64 buf 640 (+ (ptr-read-u64 buf 768) (ptr-read-u64 buf 776)))
             (ptr-write-u64 buf 784 (+ (ptr-read-u64 buf 784) 1)))
           1)
       ;; H += working vars (64-bit wrap — i64 wraps naturally)
       (ptr-write-u64 buf 704 (+ (ptr-read-u64 buf 704) (ptr-read-u64 buf 640)))
       (ptr-write-u64 buf 712 (+ (ptr-read-u64 buf 712) (ptr-read-u64 buf 648)))
       (ptr-write-u64 buf 720 (+ (ptr-read-u64 buf 720) (ptr-read-u64 buf 656)))
       (ptr-write-u64 buf 728 (+ (ptr-read-u64 buf 728) (ptr-read-u64 buf 664)))
       (ptr-write-u64 buf 736 (+ (ptr-read-u64 buf 736) (ptr-read-u64 buf 672)))
       (ptr-write-u64 buf 744 (+ (ptr-read-u64 buf 744) (ptr-read-u64 buf 680)))
       (ptr-write-u64 buf 752 (+ (ptr-read-u64 buf 752) (ptr-read-u64 buf 688)))
       (ptr-write-u64 buf 760 (+ (ptr-read-u64 buf 760) (ptr-read-u64 buf 696)))))

    ;; ---- SHA512 process full 128-byte blocks ----------------------------
    ;; buf[792] = ctr_blk outer counter.
    (defun nl_sha512_full_blocks (bytes num-blocks buf)
      (and
       (ptr-write-u64 buf 792 0)
       (or (while (< (ptr-read-u64 buf 792) num-blocks)
             (and
              (nl_sha512_load bytes (* (ptr-read-u64 buf 792) 128) buf)
              (nl_sha512_expand buf)
              (nl_sha512_compress buf))
             (ptr-write-u64 buf 792 (+ (ptr-read-u64 buf 792) 1)))
           1)))

    ;; ---- SHA512 scratch fill [808..935] (128 bytes) ----------------------
    (defun nl_sha512_scratch_fill (bytes offset rem buf)
      (and
       (ptr-write-u64 buf 784 0)
       (or (while (< (ptr-read-u64 buf 784) rem)
             (ptr-write-u8 buf
               (+ 808 (ptr-read-u64 buf 784))
               (ptr-read-u8 bytes (+ offset (ptr-read-u64 buf 784))))
             (ptr-write-u64 buf 784 (+ (ptr-read-u64 buf 784) 1)))
           1)
       (ptr-write-u64 buf 800 rem)
       (or (while (< (ptr-read-u64 buf 800) 128)
             (ptr-write-u8 buf (+ 808 (ptr-read-u64 buf 800)) 0)
             (ptr-write-u64 buf 800 (+ (ptr-read-u64 buf 800) 1)))
           1)))

    ;; ---- SHA512 write 128-bit (big-endian) bit length at scratch[112..127]
    ;; = buf[920..935].  Upper 64 bits are always 0 (msg < 2^64 bits).
    (defun nl_sha512_write_length (msg-bits buf)
      (and
       (ptr-write-u8 buf 920 0)
       (ptr-write-u8 buf 921 0)
       (ptr-write-u8 buf 922 0)
       (ptr-write-u8 buf 923 0)
       (ptr-write-u8 buf 924 0)
       (ptr-write-u8 buf 925 0)
       (ptr-write-u8 buf 926 0)
       (ptr-write-u8 buf 927 0)
       (ptr-write-u8 buf 928 (logand (sar msg-bits 56) 255))
       (ptr-write-u8 buf 929 (logand (sar msg-bits 48) 255))
       (ptr-write-u8 buf 930 (logand (sar msg-bits 40) 255))
       (ptr-write-u8 buf 931 (logand (sar msg-bits 32) 255))
       (ptr-write-u8 buf 932 (logand (sar msg-bits 24) 255))
       (ptr-write-u8 buf 933 (logand (sar msg-bits 16) 255))
       (ptr-write-u8 buf 934 (logand (sar msg-bits 8) 255))
       (ptr-write-u8 buf 935 (logand msg-bits 255))))

    ;; ---- SHA512 final block from scratch[808..935] ----------------------
    (defun nl_sha512_final_block (buf)
      (and
       (nl_sha512_load buf 808 buf)
       (nl_sha512_expand buf)
       (nl_sha512_compress buf)))

    ;; ---- Hex encode 64-bit word as 16 hex chars -------------------------
    (defun nl_sha512_hex_nibble (hex-buf offset nibble)
      (ptr-write-u8 hex-buf offset
        (if (< nibble 10)
            (+ nibble 48)
          (+ nibble 87))))

    (defun nl_sha512_hex_word64 (hex-buf offset word)
      (and
       (nl_sha512_hex_nibble hex-buf offset       (logand (nl_h64_lsr word 60) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 1) (logand (nl_h64_lsr word 56) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 2) (logand (nl_h64_lsr word 52) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 3) (logand (nl_h64_lsr word 48) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 4) (logand (nl_h64_lsr word 44) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 5) (logand (nl_h64_lsr word 40) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 6) (logand (nl_h64_lsr word 36) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 7) (logand (nl_h64_lsr word 32) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 8) (logand (nl_h64_lsr word 28) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 9) (logand (nl_h64_lsr word 24) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 10) (logand (nl_h64_lsr word 20) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 11) (logand (nl_h64_lsr word 16) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 12) (logand (nl_h64_lsr word 12) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 13) (logand (nl_h64_lsr word 8) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 14) (logand (nl_h64_lsr word 4) 15))
       (nl_sha512_hex_nibble hex-buf (+ offset 15) (logand word 15))))

    ;; ---- SHA512 H init ---------------------------------------------------
    ;; H[0..7] for sha512 (FIPS 180-4).
    ;; 0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1
    ;; 0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179
    (defun nl_sha512_init_h512 (buf)
      (and
       (ptr-write-u64 buf 704 (+ (shl 1779033703 32) (+ (shl 1 31) 1941752072)))
       (ptr-write-u64 buf 712 (+ (shl (+ (shl 1 31) 996650629) 32) (+ (shl 1 31) 80389947)))
       (ptr-write-u64 buf 720 (+ (shl 1013904242 32) (+ (shl 1 31) 2123692075)))
       (ptr-write-u64 buf 728 (+ (shl (+ (shl 1 31) 625997114) 32) 1595750129))
       (ptr-write-u64 buf 736 (+ (shl 1359893119 32) (+ (shl 1 31) 770081489)))
       (ptr-write-u64 buf 744 (+ (shl (+ (shl 1 31) 453339276) 32) 725511199))
       (ptr-write-u64 buf 752 (+ (shl 528734635 32) (+ (shl 1 31) 2067905899)))
       (ptr-write-u64 buf 760 (+ (shl 1541459225 32) 327033209))))

    ;; ---- SHA384 H init ---------------------------------------------------
    ;; H[0..7] for sha384 (FIPS 180-4).
    ;; 0xcbbb9d5dc1059ed8, 0x629a292a367cd507, 0x9159015a3070dd17, 0x152fecd8f70e5939
    ;; 0x67332667ffc00b31, 0x8eb44a8768581511, 0xdb0c2e0d64f98fa7, 0x47b5481dbefa4fa4
    (defun nl_sha384_init_h (buf)
      (and
       (ptr-write-u64 buf 704 (+ (shl (+ (shl 1 31) 1270586717) 32) (+ (shl 1 31) 1090887384)))
       (ptr-write-u64 buf 712 (+ (shl 1654270250 32) 914150663))
       (ptr-write-u64 buf 720 (+ (shl (+ (shl 1 31) 291045722) 32) 812702999))
       (ptr-write-u64 buf 728 (+ (shl 355462360 32) (+ (shl 1 31) 1997429049)))
       (ptr-write-u64 buf 736 (+ (shl 1731405415 32) (+ (shl 1 31) 2143292209)))
       (ptr-write-u64 buf 744 (+ (shl (+ (shl 1 31) 246696583) 32) 1750603025))
       (ptr-write-u64 buf 752 (+ (shl (+ (shl 1 31) 1527524877) 32) 1694076839))
       (ptr-write-u64 buf 760 (+ (shl 1203062813 32) (+ (shl 1 31) 1056591780)))))

    ;; ---- SHA512/384 main computation ------------------------------------
    ;; SHA512: 128-byte blocks, 112-byte threshold, 128-bit length field.
    ;; rem = msg-len & 127.  Threshold for single final block: rem <= 111.
    ;; is-sha384: 0 for sha512, 1 for sha384.
    (defun nl_sha512_compute (bytes msg-len hex-buf buf is-sha384)
      (and
       (nl_sha512_init_k buf)
       (if (= is-sha384 1)
           (nl_sha384_init_h buf)
         (nl_sha512_init_h512 buf))
       ;; Full blocks: msg-len / 128 = sar(msg-len, 7)
       (nl_sha512_full_blocks bytes (sar msg-len 7) buf)
       ;; rem = msg-len & 127
       (nl_sha512_scratch_fill bytes (* (sar msg-len 7) 128) (logand msg-len 127) buf)
       ;; 0x80 pad at scratch[rem] = buf[808 + rem]
       (ptr-write-u8 buf (+ 808 (logand msg-len 127)) 128)
       ;; Branch on rem vs 111
       (if (<= (logand msg-len 127) 111)
           (and
            (nl_sha512_write_length (* msg-len 8) buf)
            (nl_sha512_final_block buf))
         (and
          (nl_sha512_final_block buf)
          (ptr-write-u64 buf 800 0)
          (or (while (< (ptr-read-u64 buf 800) 128)
                (ptr-write-u8 buf (+ 808 (ptr-read-u64 buf 800)) 0)
                (ptr-write-u64 buf 800 (+ (ptr-read-u64 buf 800) 1)))
              1)
          (nl_sha512_write_length (* msg-len 8) buf)
          (nl_sha512_final_block buf)))
       ;; Hex-encode H[0..7] (sha512=128 chars; sha384=96 chars=6 words)
       (nl_sha512_hex_word64 hex-buf 0   (ptr-read-u64 buf 704))
       (nl_sha512_hex_word64 hex-buf 16  (ptr-read-u64 buf 712))
       (nl_sha512_hex_word64 hex-buf 32  (ptr-read-u64 buf 720))
       (nl_sha512_hex_word64 hex-buf 48  (ptr-read-u64 buf 728))
       (nl_sha512_hex_word64 hex-buf 64  (ptr-read-u64 buf 736))
       (nl_sha512_hex_word64 hex-buf 80  (ptr-read-u64 buf 744))
       (if (= is-sha384 0)
           (and
            (nl_sha512_hex_word64 hex-buf 96  (ptr-read-u64 buf 752))
            (nl_sha512_hex_word64 hex-buf 112 (ptr-read-u64 buf 760)))
         0)))

    ;; ---- SHA512/384 run helpers -----------------------------------------
    (defun nl_sha512_run (str-ptr msg-len out is-sha384)
      (nl_sha512_run_buf str-ptr msg-len out is-sha384 (alloc-bytes 1576 8)))

    (defun nl_sha512_run_buf (str-ptr msg-len out is-sha384 buf)
      (if (= buf 0)
          1
        (nl_sha512_run_hex str-ptr msg-len out is-sha384 buf
                           (alloc-bytes (if (= is-sha384 1) 96 128) 1))))

    (defun nl_sha512_run_hex (str-ptr msg-len out is-sha384 buf hex-buf)
      (if (= hex-buf 0)
          (and (dealloc-bytes buf 1576 8) 1)
        (and
         (nl_sha512_compute (str-bytes-ptr str-ptr) msg-len hex-buf buf is-sha384)
         (sexp-write-str out hex-buf (if (= is-sha384 1) 96 128))
         (dealloc-bytes hex-buf (if (= is-sha384 1) 96 128) 1)
         (dealloc-bytes buf 1576 8)
         0)))

    ;; ============================================================
    ;; MD5
    ;; ============================================================

    ;; MD5 uses 32-bit little-endian words, ROTL32, and specific s-shifts.
    ;; rotl32(x, n) = (x << n) | (x >> (32-n))
    (defun nl_md5_rotl (x n)
      (nl_h32_mask
       (logior (shl (nl_h32_mask x) n)
               (nl_h32_shr x (- 32 n)))))

    ;; F(b,c,d) = (b AND c) OR (NOT b AND d)
    (defun nl_md5_F (b c d)
      (nl_h32_mask
       (logior (logand b c)
               (logand (logxor b (- (shl 1 32) 1)) d))))

    ;; G(b,c,d) = (b AND d) OR (c AND NOT d)
    (defun nl_md5_G (b c d)
      (nl_h32_mask
       (logior (logand b d)
               (logand c (logxor d (- (shl 1 32) 1))))))

    ;; H(b,c,d) = b XOR c XOR d
    (defun nl_md5_H (b c d)
      (nl_h32_mask (logxor b (logxor c d))))

    ;; I(b,c,d) = c XOR (b OR NOT d)
    (defun nl_md5_I (b c d)
      (nl_h32_mask
       (logxor c
               (logior b (logxor d (- (shl 1 32) 1))))))

    ;; ---- Write MD5 T constants into buf[272..783] ----------------------
    ;; 64 constants × u64 slots = 512 bytes.  Offset = 272 + i*8.
    (defun nl_md5_init_t (buf)
      (and
       (ptr-write-u64 buf 272 (+ (shl 1 31) 1466606712))
       (ptr-write-u64 buf 280 (+ (shl 1 31) 1757919062))
       (ptr-write-u64 buf 288 606105819)
       (ptr-write-u64 buf 296 (+ (shl 1 31) 1102958318))
       (ptr-write-u64 buf 304 (+ (shl 1 31) 1971064751))
       (ptr-write-u64 buf 312 1200080426)
       (ptr-write-u64 buf 320 (+ (shl 1 31) 674252307))
       (ptr-write-u64 buf 328 (+ (shl 1 31) 2101777665))
       (ptr-write-u64 buf 336 1770035416)
       (ptr-write-u64 buf 344 (+ (shl 1 31) 189069231))
       (ptr-write-u64 buf 352 (+ (shl 1 31) 2147441585))
       (ptr-write-u64 buf 360 (+ (shl 1 31) 157079486))
       (ptr-write-u64 buf 368 1804603682)
       (ptr-write-u64 buf 376 (+ (shl 1 31) 2107142547))
       (ptr-write-u64 buf 384 (+ (shl 1 31) 645481358))
       (ptr-write-u64 buf 392 1236535329)
       (ptr-write-u64 buf 400 (+ (shl 1 31) 1981687138))
       (ptr-write-u64 buf 408 (+ (shl 1 31) 1077982016))
       (ptr-write-u64 buf 416 643717713)
       (ptr-write-u64 buf 424 (+ (shl 1 31) 1773586346))
       (ptr-write-u64 buf 432 (+ (shl 1 31) 1445924957))
       (ptr-write-u64 buf 440 38016083)
       (ptr-write-u64 buf 448 (+ (shl 1 31) 1487005313))
       (ptr-write-u64 buf 456 (+ (shl 1 31) 1741945800))
       (ptr-write-u64 buf 464 568446438)
       (ptr-write-u64 buf 472 (+ (shl 1 31) 1127679958))
       (ptr-write-u64 buf 480 (+ (shl 1 31) 1960119687))
       (ptr-write-u64 buf 488 1163531501)
       (ptr-write-u64 buf 496 (+ (shl 1 31) 702802181))
       (ptr-write-u64 buf 504 (+ (shl 1 31) 2096079864))
       (ptr-write-u64 buf 512 1735328473)
       (ptr-write-u64 buf 520 (+ (shl 1 31) 220875914))
       (ptr-write-u64 buf 528 (+ (shl 1 31) 2147105090))
       (ptr-write-u64 buf 536 (+ (shl 1 31) 124909185))
       (ptr-write-u64 buf 544 1839030562)
       (ptr-write-u64 buf 552 (+ (shl 1 31) 2112174092))
       (ptr-write-u64 buf 560 (+ (shl 1 31) 616491588))
       (ptr-write-u64 buf 568 1272893353)
       (ptr-write-u64 buf 576 (+ (shl 1 31) 1991986016))
       (ptr-write-u64 buf 584 (+ (shl 1 31) 1052753008))
       (ptr-write-u64 buf 592 681279174)
       (ptr-write-u64 buf 600 (+ (shl 1 31) 1788946426))
       (ptr-write-u64 buf 608 (+ (shl 1 31) 1424961669))
       (ptr-write-u64 buf 616 76029189)
       (ptr-write-u64 buf 624 (+ (shl 1 31) 1507119161))
       (ptr-write-u64 buf 632 (+ (shl 1 31) 1725667813))
       (ptr-write-u64 buf 640 530742520)
       (ptr-write-u64 buf 648 (+ (shl 1 31) 1152144997))
       (ptr-write-u64 buf 656 (+ (shl 1 31) 1948852804))
       (ptr-write-u64 buf 664 1126891415)
       (ptr-write-u64 buf 672 (+ (shl 1 31) 731128743))
       (ptr-write-u64 buf 680 (+ (shl 1 31) 2090049593))
       (ptr-write-u64 buf 688 1700485571)
       (ptr-write-u64 buf 696 (+ (shl 1 31) 252497042))
       (ptr-write-u64 buf 704 (+ (shl 1 31) 2146432125))
       (ptr-write-u64 buf 712 (+ (shl 1 31) 92560849))
       (ptr-write-u64 buf 720 1873313359)
       (ptr-write-u64 buf 728 (+ (shl 1 31) 2116871904))
       (ptr-write-u64 buf 736 (+ (shl 1 31) 587285268))
       (ptr-write-u64 buf 744 1309151649)
       (ptr-write-u64 buf 752 (+ (shl 1 31) 2001960578))
       (ptr-write-u64 buf 760 (+ (shl 1 31) 1027273269))
       (ptr-write-u64 buf 768 718787259)
       (ptr-write-u64 buf 776 (+ (shl 1 31) 1803998097))))

    ;; ---- Load MD5 message block: 16 little-endian 32-bit words ----------
    ;; M[i] at buf[i*8], i=0..15.  buf[200] = ctr_i.
    (defun nl_md5_load (bytes byte-off buf)
      (and
       (ptr-write-u64 buf 200 0)
       (or (while (< (ptr-read-u64 buf 200) 16)
             (ptr-write-u64 buf
               (* (ptr-read-u64 buf 200) 8)
               (nl_h32_mask
                (logior
                 (ptr-read-u8 bytes (+ byte-off (* (ptr-read-u64 buf 200) 4)))
                 (logior
                  (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 200) 4) 1))) 8)
                  (logior
                   (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 200) 4) 2))) 16)
                   (shl (ptr-read-u8 bytes (+ byte-off (+ (* (ptr-read-u64 buf 200) 4) 3))) 24))))))
             (ptr-write-u64 buf 200 (+ (ptr-read-u64 buf 200) 1)))
           1)))

    ;; ---- MD5 compression function ---------------------------------------
    ;; a=128, b=136, c=144, d=152, T scratch=192, ctr_i=200.
    ;; T[i] at buf[272 + i*8].
    ;; MD5 per-step shifts s[0..63]:
    ;;   Round 1: 7,12,17,22 (×4)
    ;;   Round 2: 5,9,14,20 (×4)
    ;;   Round 3: 4,11,16,23 (×4)
    ;;   Round 4: 6,10,15,21 (×4)
    ;; M index for each step also varies by round.
    ;; We inline all 64 steps explicitly to avoid needing a shifts array.
    ;; This is verbose but avoids another 64-slot constant buffer.
    ;;
    ;; Helper: one MD5 step.
    ;;   a = b + rotl(a + func(b,c,d) + M[k] + T[i], s)
    ;; After each step: (a,b,c,d) <- (d,a,b,c)
    ;; buf: a=128,b=136,c=144,d=152
    (defun nl_md5_step_F (buf k s)
      (and
       (ptr-write-u64 buf 192
         (nl_h32_mask
          (+ (ptr-read-u64 buf 136)
             (nl_md5_rotl
              (nl_h32_mask
               (+ (ptr-read-u64 buf 128)
                  (+ (nl_md5_F (ptr-read-u64 buf 136)
                                (ptr-read-u64 buf 144)
                                (ptr-read-u64 buf 152))
                     (+ (ptr-read-u64 buf (* k 8))
                        (ptr-read-u64 buf (+ 272 (* (ptr-read-u64 buf 200) 8)))))))
              s))))
       (ptr-write-u64 buf 128 (ptr-read-u64 buf 152))
       (ptr-write-u64 buf 152 (ptr-read-u64 buf 144))
       (ptr-write-u64 buf 144 (ptr-read-u64 buf 136))
       (ptr-write-u64 buf 136 (ptr-read-u64 buf 192))
       (ptr-write-u64 buf 200 (+ (ptr-read-u64 buf 200) 1))))

    (defun nl_md5_step_G (buf k s)
      (and
       (ptr-write-u64 buf 192
         (nl_h32_mask
          (+ (ptr-read-u64 buf 136)
             (nl_md5_rotl
              (nl_h32_mask
               (+ (ptr-read-u64 buf 128)
                  (+ (nl_md5_G (ptr-read-u64 buf 136)
                                (ptr-read-u64 buf 144)
                                (ptr-read-u64 buf 152))
                     (+ (ptr-read-u64 buf (* k 8))
                        (ptr-read-u64 buf (+ 272 (* (ptr-read-u64 buf 200) 8)))))))
              s))))
       (ptr-write-u64 buf 128 (ptr-read-u64 buf 152))
       (ptr-write-u64 buf 152 (ptr-read-u64 buf 144))
       (ptr-write-u64 buf 144 (ptr-read-u64 buf 136))
       (ptr-write-u64 buf 136 (ptr-read-u64 buf 192))
       (ptr-write-u64 buf 200 (+ (ptr-read-u64 buf 200) 1))))

    (defun nl_md5_step_H (buf k s)
      (and
       (ptr-write-u64 buf 192
         (nl_h32_mask
          (+ (ptr-read-u64 buf 136)
             (nl_md5_rotl
              (nl_h32_mask
               (+ (ptr-read-u64 buf 128)
                  (+ (nl_md5_H (ptr-read-u64 buf 136)
                                (ptr-read-u64 buf 144)
                                (ptr-read-u64 buf 152))
                     (+ (ptr-read-u64 buf (* k 8))
                        (ptr-read-u64 buf (+ 272 (* (ptr-read-u64 buf 200) 8)))))))
              s))))
       (ptr-write-u64 buf 128 (ptr-read-u64 buf 152))
       (ptr-write-u64 buf 152 (ptr-read-u64 buf 144))
       (ptr-write-u64 buf 144 (ptr-read-u64 buf 136))
       (ptr-write-u64 buf 136 (ptr-read-u64 buf 192))
       (ptr-write-u64 buf 200 (+ (ptr-read-u64 buf 200) 1))))

    (defun nl_md5_step_I (buf k s)
      (and
       (ptr-write-u64 buf 192
         (nl_h32_mask
          (+ (ptr-read-u64 buf 136)
             (nl_md5_rotl
              (nl_h32_mask
               (+ (ptr-read-u64 buf 128)
                  (+ (nl_md5_I (ptr-read-u64 buf 136)
                                (ptr-read-u64 buf 144)
                                (ptr-read-u64 buf 152))
                     (+ (ptr-read-u64 buf (* k 8))
                        (ptr-read-u64 buf (+ 272 (* (ptr-read-u64 buf 200) 8)))))))
              s))))
       (ptr-write-u64 buf 128 (ptr-read-u64 buf 152))
       (ptr-write-u64 buf 152 (ptr-read-u64 buf 144))
       (ptr-write-u64 buf 144 (ptr-read-u64 buf 136))
       (ptr-write-u64 buf 136 (ptr-read-u64 buf 192))
       (ptr-write-u64 buf 200 (+ (ptr-read-u64 buf 200) 1))))

    ;; ---- MD5 compress one 64-byte block ----------------------------------
    ;; AA=160,BB=168,CC=176,DD=184. buf[200] reset to 0 before steps.
    (defun nl_md5_compress (buf)
      (and
       ;; Save a,b,c,d
       (ptr-write-u64 buf 160 (ptr-read-u64 buf 128))
       (ptr-write-u64 buf 168 (ptr-read-u64 buf 136))
       (ptr-write-u64 buf 176 (ptr-read-u64 buf 144))
       (ptr-write-u64 buf 184 (ptr-read-u64 buf 152))
       ;; Reset step counter
       (ptr-write-u64 buf 200 0)
       ;; Round 1: F, k=i, s=7,12,17,22 (×4)
       (nl_md5_step_F buf 0  7)
       (nl_md5_step_F buf 1  12)
       (nl_md5_step_F buf 2  17)
       (nl_md5_step_F buf 3  22)
       (nl_md5_step_F buf 4  7)
       (nl_md5_step_F buf 5  12)
       (nl_md5_step_F buf 6  17)
       (nl_md5_step_F buf 7  22)
       (nl_md5_step_F buf 8  7)
       (nl_md5_step_F buf 9  12)
       (nl_md5_step_F buf 10 17)
       (nl_md5_step_F buf 11 22)
       (nl_md5_step_F buf 12 7)
       (nl_md5_step_F buf 13 12)
       (nl_md5_step_F buf 14 17)
       (nl_md5_step_F buf 15 22)
       ;; Round 2: G, k=(5i+1)mod16, s=5,9,14,20 (×4)
       (nl_md5_step_G buf 1  5)
       (nl_md5_step_G buf 6  9)
       (nl_md5_step_G buf 11 14)
       (nl_md5_step_G buf 0  20)
       (nl_md5_step_G buf 5  5)
       (nl_md5_step_G buf 10 9)
       (nl_md5_step_G buf 15 14)
       (nl_md5_step_G buf 4  20)
       (nl_md5_step_G buf 9  5)
       (nl_md5_step_G buf 14 9)
       (nl_md5_step_G buf 3  14)
       (nl_md5_step_G buf 8  20)
       (nl_md5_step_G buf 13 5)
       (nl_md5_step_G buf 2  9)
       (nl_md5_step_G buf 7  14)
       (nl_md5_step_G buf 12 20)
       ;; Round 3: H, k=(3i+5)mod16, s=4,11,16,23 (×4)
       (nl_md5_step_H buf 5  4)
       (nl_md5_step_H buf 8  11)
       (nl_md5_step_H buf 11 16)
       (nl_md5_step_H buf 14 23)
       (nl_md5_step_H buf 1  4)
       (nl_md5_step_H buf 4  11)
       (nl_md5_step_H buf 7  16)
       (nl_md5_step_H buf 10 23)
       (nl_md5_step_H buf 13 4)
       (nl_md5_step_H buf 0  11)
       (nl_md5_step_H buf 3  16)
       (nl_md5_step_H buf 6  23)
       (nl_md5_step_H buf 9  4)
       (nl_md5_step_H buf 12 11)
       (nl_md5_step_H buf 15 16)
       (nl_md5_step_H buf 2  23)
       ;; Round 4: I, k=(7i)mod16, s=6,10,15,21 (×4)
       (nl_md5_step_I buf 0  6)
       (nl_md5_step_I buf 7  10)
       (nl_md5_step_I buf 14 15)
       (nl_md5_step_I buf 5  21)
       (nl_md5_step_I buf 12 6)
       (nl_md5_step_I buf 3  10)
       (nl_md5_step_I buf 10 15)
       (nl_md5_step_I buf 1  21)
       (nl_md5_step_I buf 8  6)
       (nl_md5_step_I buf 15 10)
       (nl_md5_step_I buf 6  15)
       (nl_md5_step_I buf 13 21)
       (nl_md5_step_I buf 4  6)
       (nl_md5_step_I buf 11 10)
       (nl_md5_step_I buf 2  15)
       (nl_md5_step_I buf 9  21)
       ;; a += AA, b += BB, c += CC, d += DD
       (ptr-write-u64 buf 128 (nl_h32_mask (+ (ptr-read-u64 buf 128) (ptr-read-u64 buf 160))))
       (ptr-write-u64 buf 136 (nl_h32_mask (+ (ptr-read-u64 buf 136) (ptr-read-u64 buf 168))))
       (ptr-write-u64 buf 144 (nl_h32_mask (+ (ptr-read-u64 buf 144) (ptr-read-u64 buf 176))))
       (ptr-write-u64 buf 152 (nl_h32_mask (+ (ptr-read-u64 buf 152) (ptr-read-u64 buf 184))))))

    ;; ---- Process full 64-byte MD5 blocks --------------------------------
    ;; Uses buf[208] as outer block counter (scratch64 base, safe before padding).
    ;; nl_md5_load and nl_md5_compress use buf[200] as inner counter, so
    ;; we cannot reuse buf[200] here without clobbering the outer loop.
    (defun nl_md5_full_blocks (bytes num-blocks buf)
      (and
       (ptr-write-u64 buf 208 0)
       (or (while (< (ptr-read-u64 buf 208) num-blocks)
             (and
              (nl_md5_load bytes (* (ptr-read-u64 buf 208) 64) buf)
              (nl_md5_compress buf))
             (ptr-write-u64 buf 208 (+ (ptr-read-u64 buf 208) 1)))
           1)))

    ;; ---- MD5 scratch fill [scratch base=208..271] ----------------------
    ;; Copy rem bytes + zero-fill scratch[0..63].
    ;; Use buf[200] as copy counter, buf[200] again for zero loop (sequential).
    (defun nl_md5_scratch_fill (bytes offset rem buf)
      (and
       (ptr-write-u64 buf 200 0)
       (or (while (< (ptr-read-u64 buf 200) rem)
             (ptr-write-u8 buf
               (+ 208 (ptr-read-u64 buf 200))
               (ptr-read-u8 bytes (+ offset (ptr-read-u64 buf 200))))
             (ptr-write-u64 buf 200 (+ (ptr-read-u64 buf 200) 1)))
           1)
       (or (while (< (ptr-read-u64 buf 200) 64)
             (ptr-write-u8 buf (+ 208 (ptr-read-u64 buf 200)) 0)
             (ptr-write-u64 buf 200 (+ (ptr-read-u64 buf 200) 1)))
           1)))

    ;; ---- MD5 write 64-bit little-endian bit-length at scratch[56..63] --
    ;; = buf[264..271]
    (defun nl_md5_write_length (msg-bits buf)
      (and
       (ptr-write-u8 buf 264 (logand msg-bits 255))
       (ptr-write-u8 buf 265 (logand (sar msg-bits 8) 255))
       (ptr-write-u8 buf 266 (logand (sar msg-bits 16) 255))
       (ptr-write-u8 buf 267 (logand (sar msg-bits 24) 255))
       (ptr-write-u8 buf 268 (logand (sar msg-bits 32) 255))
       (ptr-write-u8 buf 269 (logand (sar msg-bits 40) 255))
       (ptr-write-u8 buf 270 (logand (sar msg-bits 48) 255))
       (ptr-write-u8 buf 271 (logand (sar msg-bits 56) 255))))

    ;; ---- MD5 final block from scratch[208..271] -------------------------
    (defun nl_md5_final_block (buf)
      (and
       (nl_md5_load buf 208 buf)
       (nl_md5_compress buf)))

    ;; ---- Hex helpers for MD5 little-endian output -----------------------
    ;; MD5 outputs 4 words as little-endian hex.
    (defun nl_md5_hex_nibble (hex-buf offset nibble)
      (ptr-write-u8 hex-buf offset
        (if (< nibble 10)
            (+ nibble 48)
          (+ nibble 87))))

    ;; One 32-bit word in little-endian hex order (LSB byte first, each byte hi-nib first).
    ;; byte 0 = bits 0..7, byte 1 = bits 8..15, etc.
    ;; hex for byte = high nibble (bits 4..7) then low nibble (bits 0..3).
    (defun nl_md5_hex_word_le (hex-buf offset word)
      (and
       (nl_md5_hex_nibble hex-buf offset       (logand (sar word 4)  15))
       (nl_md5_hex_nibble hex-buf (+ offset 1) (logand word          15))
       (nl_md5_hex_nibble hex-buf (+ offset 2) (logand (sar word 12) 15))
       (nl_md5_hex_nibble hex-buf (+ offset 3) (logand (sar word 8)  15))
       (nl_md5_hex_nibble hex-buf (+ offset 4) (logand (sar word 20) 15))
       (nl_md5_hex_nibble hex-buf (+ offset 5) (logand (sar word 16) 15))
       (nl_md5_hex_nibble hex-buf (+ offset 6) (logand (sar word 28) 15))
       (nl_md5_hex_nibble hex-buf (+ offset 7) (logand (sar word 24) 15))))

    ;; ---- MD5 main computation -------------------------------------------
    ;; H init: a=0x67452301=1732584193, b=0xefcdab89=(+ (shl 1 31) 1875749769)
    ;;         c=0x98badcfe=(+ (shl 1 31) 414899454), d=0x10325476=271733878
    (defun nl_md5_compute (bytes msg-len hex-buf buf)
      (and
       (nl_md5_init_t buf)
       (ptr-write-u64 buf 128 1732584193)
       (ptr-write-u64 buf 136 (+ (shl 1 31) 1875749769))
       (ptr-write-u64 buf 144 (+ (shl 1 31) 414899454))
       (ptr-write-u64 buf 152 271733878)
       ;; Full blocks
       (nl_md5_full_blocks bytes (sar msg-len 6) buf)
       ;; Fill scratch
       (nl_md5_scratch_fill bytes (* (sar msg-len 6) 64) (logand msg-len 63) buf)
       ;; 0x80 pad at scratch[rem] = buf[208 + rem]
       (ptr-write-u8 buf (+ 208 (logand msg-len 63)) 128)
       ;; Branch on rem vs 55
       (if (<= (logand msg-len 63) 55)
           (and
            (nl_md5_write_length (* msg-len 8) buf)
            (nl_md5_final_block buf))
         (and
          (nl_md5_final_block buf)
          ;; Zero-fill scratch again
          (ptr-write-u64 buf 200 0)
          (or (while (< (ptr-read-u64 buf 200) 64)
                (ptr-write-u8 buf (+ 208 (ptr-read-u64 buf 200)) 0)
                (ptr-write-u64 buf 200 (+ (ptr-read-u64 buf 200) 1)))
              1)
          (nl_md5_write_length (* msg-len 8) buf)
          (nl_md5_final_block buf)))
       ;; Little-endian hex output
       (nl_md5_hex_word_le hex-buf 0  (ptr-read-u64 buf 128))
       (nl_md5_hex_word_le hex-buf 8  (ptr-read-u64 buf 136))
       (nl_md5_hex_word_le hex-buf 16 (ptr-read-u64 buf 144))
       (nl_md5_hex_word_le hex-buf 24 (ptr-read-u64 buf 152))))

    ;; ---- MD5 run helpers ------------------------------------------------
    (defun nl_md5_run (str-ptr msg-len out)
      (nl_md5_run_buf str-ptr msg-len out (alloc-bytes 784 8)))

    (defun nl_md5_run_buf (str-ptr msg-len out buf)
      (if (= buf 0)
          1
        (nl_md5_run_hex str-ptr msg-len out buf (alloc-bytes 32 1))))

    (defun nl_md5_run_hex (str-ptr msg-len out buf hex-buf)
      (if (= hex-buf 0)
          (and (dealloc-bytes buf 784 8) 1)
        (and
         (nl_md5_compute (str-bytes-ptr str-ptr) msg-len hex-buf buf)
         (sexp-write-str out hex-buf 32)
         (dealloc-bytes hex-buf 32 1)
         (dealloc-bytes buf 784 8)
         0)))

    ;; ============================================================
    ;; Main entry point: nl_jit_secure_hash_non_sha1_ext
    ;; Replaces the Rust nl_jit_secure_hash_non_sha1 function.
    ;; Same signature: (algo-ptr str-ptr out) -> i64  (0=OK, 1=ERR)
    ;; ============================================================

    (defun nl_jit_secure_hash_non_sha1_ext (algo-ptr str-ptr out)
      (if (= (symbol-name-eq algo-ptr "sha256") 1)
          ;; sha256: Symbol path
          (if (= (sexp-tag str-ptr) 6)
              (nl_sha256_run str-ptr (mut-str-len str-ptr) out 0)
            (if (= (sexp-tag str-ptr) 4)
                (nl_sha256_run str-ptr (str-len str-ptr) out 0)
              (if (= (sexp-tag str-ptr) 5)
                  (nl_sha256_run str-ptr (str-len str-ptr) out 0)
                1)))
        (if (= (symbol-name-eq algo-ptr "sha224") 1)
            (if (= (sexp-tag str-ptr) 6)
                (nl_sha256_run str-ptr (mut-str-len str-ptr) out 1)
              (if (= (sexp-tag str-ptr) 4)
                  (nl_sha256_run str-ptr (str-len str-ptr) out 1)
                (if (= (sexp-tag str-ptr) 5)
                    (nl_sha256_run str-ptr (str-len str-ptr) out 1)
                  1)))
          (if (= (symbol-name-eq algo-ptr "sha512") 1)
              (if (= (sexp-tag str-ptr) 6)
                  (nl_sha512_run str-ptr (mut-str-len str-ptr) out 0)
                (if (= (sexp-tag str-ptr) 4)
                    (nl_sha512_run str-ptr (str-len str-ptr) out 0)
                  (if (= (sexp-tag str-ptr) 5)
                      (nl_sha512_run str-ptr (str-len str-ptr) out 0)
                    1)))
            (if (= (symbol-name-eq algo-ptr "sha384") 1)
                (if (= (sexp-tag str-ptr) 6)
                    (nl_sha512_run str-ptr (mut-str-len str-ptr) out 1)
                  (if (= (sexp-tag str-ptr) 4)
                      (nl_sha512_run str-ptr (str-len str-ptr) out 1)
                    (if (= (sexp-tag str-ptr) 5)
                        (nl_sha512_run str-ptr (str-len str-ptr) out 1)
                      1)))
              (if (= (symbol-name-eq algo-ptr "md5") 1)
                  (if (= (sexp-tag str-ptr) 6)
                      (nl_md5_run str-ptr (mut-str-len str-ptr) out)
                    (if (= (sexp-tag str-ptr) 4)
                        (nl_md5_run str-ptr (str-len str-ptr) out)
                      (if (= (sexp-tag str-ptr) 5)
                          (nl_md5_run str-ptr (str-len str-ptr) out)
                        1)))
                1)))))))

  "AOT source for SHA256/SHA224/SHA512/SHA384/MD5 arms.

Replaces `nl_jit_secure_hash_non_sha1' Rust function (~70 LOC) plus
sha2 and md5 crate dependencies from `build-tool/Cargo.toml'.

SHA256/SHA224: 32-bit bitwise arithmetic, 64-round compression, K
constants stored in workspace.  SHA224 shares all SHA256 code with
different H init and 7-word (56-byte) hex output.

SHA512/SHA384: 64-bit arithmetic with unsigned right shift via
(logand (sar x n) mask).  rotr64 variants inlined per constant n.
K constants 80×u64 stored in workspace.  SHA384 truncates to 6 words.

MD5: 32-bit little-endian, ROTL, 4-round 64-step expansion.  T
constants stored in workspace.  Output in little-endian hex byte order.

Build wiring (same pattern as nelisp-cc-jit-secure-hash.el):
  `scripts/compile-elisp-objects.el': manifest entry -> `nl_jit_secure_hash_ext.o'
  `build-tool/build.rs': manifest_sources entry
  `build-tool/src/jit/bridge.rs': extern + anchor entry
  `nelisp-cc-jit-secure-hash.el': nl_sha1_call_non_sha1 updated to call
    nl_jit_secure_hash_non_sha1_ext instead of nl_jit_secure_hash_non_sha1")

(provide 'nelisp-cc-jit-secure-hash-ext)

;;; nelisp-cc-jit-secure-hash-ext.el ends here
