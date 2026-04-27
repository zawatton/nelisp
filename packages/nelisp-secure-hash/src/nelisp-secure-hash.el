;;; nelisp-secure-hash.el --- Pure NeLisp MD5/SHA-1/SHA-256  -*- lexical-binding: t; -*-

;; T62 / T54 Wave 1 agent E deliverable.
;; anvil-disk / anvil-server / anvil-data secure-hash + base64 unblock for
;; the standalone path without host `secure-hash' / `md5' dependency.

;;; Commentary:

;; Pure Elisp secure-hash implementation for the NeLisp standalone path.
;; Supports MD5 (RFC 1321), SHA-1 (RFC 3174), and SHA-256 (RFC 6234).
;; Multibyte string input is converted to UTF-8 bytes through
;; `nelisp-coding-utf8-encode' before hashing.

;;; Code:

(require 'cl-lib)
(require 'nelisp-coding)

(defconst nelisp-hash--mask-32 #xFFFFFFFF
  "Unsigned 32-bit mask.")

(defconst nelisp-hash--md5-shifts
  [7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22
   5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20
   4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23
   6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21]
  "MD5 rotation schedule.")

(defconst nelisp-hash--md5-k
  [#xd76aa478 #xe8c7b756 #x242070db #xc1bdceee
   #xf57c0faf #x4787c62a #xa8304613 #xfd469501
   #x698098d8 #x8b44f7af #xffff5bb1 #x895cd7be
   #x6b901122 #xfd987193 #xa679438e #x49b40821
   #xf61e2562 #xc040b340 #x265e5a51 #xe9b6c7aa
   #xd62f105d #x02441453 #xd8a1e681 #xe7d3fbc8
   #x21e1cde6 #xc33707d6 #xf4d50d87 #x455a14ed
   #xa9e3e905 #xfcefa3f8 #x676f02d9 #x8d2a4c8a
   #xfffa3942 #x8771f681 #x6d9d6122 #xfde5380c
   #xa4beea44 #x4bdecfa9 #xf6bb4b60 #xbebfbc70
   #x289b7ec6 #xeaa127fa #xd4ef3085 #x04881d05
   #xd9d4d039 #xe6db99e5 #x1fa27cf8 #xc4ac5665
   #xf4292244 #x432aff97 #xab9423a7 #xfc93a039
   #x655b59c3 #x8f0ccc92 #xffeff47d #x85845dd1
   #x6fa87e4f #xfe2ce6e0 #xa3014314 #x4e0811a1
   #xf7537e82 #xbd3af235 #x2ad7d2bb #xeb86d391]
  "MD5 table constants.")

(defconst nelisp-hash--sha256-k
  [#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
   #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
   #xd807aa98 #x12835b01 #x243185be #x550c7dc3
   #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
   #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
   #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
   #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
   #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
   #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
   #x650a7354 #x766a0abb #x81c2c92e #x92722c85
   #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
   #xd192e819 #xd6990624 #xf40e3585 #x106aa070
   #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
   #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
   #x748f82ee #x78a5636f #x84c87814 #x8cc70208
   #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2]
  "SHA-256 round constants.")

(defun nelisp-hash--u32 (value)
  "Return VALUE wrapped to 32-bit unsigned range."
  (logand value nelisp-hash--mask-32))

(defun nelisp-hash--add32 (&rest values)
  "Return VALUES summed modulo 2^32."
  (let ((sum 0))
    (dolist (value values)
      (setq sum (nelisp-hash--u32 (+ sum value))))
    sum))

(defun nelisp-hash--rol32 (value count)
  "Rotate VALUE left by COUNT bits within 32 bits."
  (let ((count (mod count 32))
        (value (nelisp-hash--u32 value)))
    (nelisp-hash--u32
     (logior (ash value count)
             (ash value (- count 32))))))

(defun nelisp-hash--ror32 (value count)
  "Rotate VALUE right by COUNT bits within 32 bits."
  (nelisp-hash--rol32 value (- 32 (mod count 32))))

(defun nelisp-hash--shr32 (value count)
  "Logical right shift VALUE by COUNT bits."
  (ash (nelisp-hash--u32 value) (- count)))

(defun nelisp-hash--string-bytes (string)
  "Return STRING as a list of bytes for hashing."
  (if (multibyte-string-p string)
      (nelisp-coding-utf8-encode string)
    (mapcar (lambda (byte) (logand byte #xFF)) (append string nil))))

(defun nelisp-hash--pad-message (bytes length-endian)
  "Pad BYTES for Merkle-Damgard hashing.
LENGTH-ENDIAN is either `little' or `big' for the 64-bit bit length."
  (let* ((message (copy-sequence bytes))
         (bit-length (* 8 (length bytes))))
    (setq message (append message (list #x80)))
    (while (/= (mod (length message) 64) 56)
      (setq message (append message '(0))))
    (if (eq length-endian 'little)
        (dotimes (index 8)
          (setq message
                (append message
                        (list (logand (ash bit-length (- (* index 8))) #xFF)))))
      (dotimes (index 8)
        (let ((shift (* (- 7 index) 8)))
          (setq message
                (append message
                        (list (logand (ash bit-length (- shift)) #xFF)))))))
    message))

(defun nelisp-hash--bytes-to-words-le (block)
  "Decode 64-byte BLOCK into 16 little-endian 32-bit words."
  (let ((words (make-vector 16 0))
        (index 0))
    (while (< index 16)
      (let ((offset (* index 4)))
        (aset words index
              (logior (nth offset block)
                      (ash (nth (1+ offset) block) 8)
                      (ash (nth (+ offset 2) block) 16)
                      (ash (nth (+ offset 3) block) 24))))
      (setq index (1+ index)))
    words))

(defun nelisp-hash--bytes-to-words-be (block)
  "Decode 64-byte BLOCK into 16 big-endian 32-bit words."
  (let ((words (make-vector 16 0))
        (index 0))
    (while (< index 16)
      (let ((offset (* index 4)))
        (aset words index
              (logior (ash (nth offset block) 24)
                      (ash (nth (1+ offset) block) 16)
                      (ash (nth (+ offset 2) block) 8)
                      (nth (+ offset 3) block))))
      (setq index (1+ index)))
    words))

(defun nelisp-hash--u32-to-hex-be (value)
  "Return VALUE as 8 lowercase hex chars in big-endian word order."
  (format "%08x" (nelisp-hash--u32 value)))

(defun nelisp-hash--u32-to-hex-le (value)
  "Return VALUE as 8 lowercase hex chars from little-endian bytes."
  (format "%02x%02x%02x%02x"
          (logand value #xFF)
          (logand (ash value -8) #xFF)
          (logand (ash value -16) #xFF)
          (logand (ash value -24) #xFF)))

(defun nelisp-hash--md5-digest (bytes)
  "Return MD5 hex digest for BYTES."
  (let ((message (nelisp-hash--pad-message bytes 'little))
        (a0 #x67452301)
        (b0 #xefcdab89)
        (c0 #x98badcfe)
        (d0 #x10325476))
    (while message
      (let* ((block (cl-subseq message 0 64))
             (m (nelisp-hash--bytes-to-words-le block))
             (a a0)
             (b b0)
             (c c0)
             (d d0)
             f g)
        (dotimes (i 64)
          (cond
           ((< i 16)
            (setq f (logior (logand b c) (logand (lognot b) d))
                  g i))
           ((< i 32)
            (setq f (logior (logand d b) (logand (lognot d) c))
                  g (mod (+ (* 5 i) 1) 16)))
           ((< i 48)
            (setq f (logxor b c d)
                  g (mod (+ (* 3 i) 5) 16)))
           (t
            (setq f (logxor c (logior b (lognot d)))
                  g (mod (* 7 i) 16))))
          (let ((tmp d))
            (setq d c
                  c b
                  b (nelisp-hash--add32
                     b
                     (nelisp-hash--rol32
                      (nelisp-hash--add32 a f
                                          (aref nelisp-hash--md5-k i)
                                          (aref m g))
                      (aref nelisp-hash--md5-shifts i)))
                  a tmp)))
        (setq a0 (nelisp-hash--add32 a0 a)
              b0 (nelisp-hash--add32 b0 b)
              c0 (nelisp-hash--add32 c0 c)
              d0 (nelisp-hash--add32 d0 d)
              message (nthcdr 64 message))))
    (concat (nelisp-hash--u32-to-hex-le a0)
            (nelisp-hash--u32-to-hex-le b0)
            (nelisp-hash--u32-to-hex-le c0)
            (nelisp-hash--u32-to-hex-le d0))))

(defun nelisp-hash--sha1-digest (bytes)
  "Return SHA-1 hex digest for BYTES."
  (let ((message (nelisp-hash--pad-message bytes 'big))
        (h0 #x67452301)
        (h1 #xEFCDAB89)
        (h2 #x98BADCFE)
        (h3 #x10325476)
        (h4 #xC3D2E1F0))
    (while message
      (let* ((block (cl-subseq message 0 64))
             (w (make-vector 80 0))
             (base (nelisp-hash--bytes-to-words-be block))
             (a h0)
             (b h1)
             (c h2)
             (d h3)
             (e h4))
        (dotimes (i 16)
          (aset w i (aref base i)))
        (dotimes (i 64)
          (aset w (+ i 16)
                (nelisp-hash--rol32
                 (logxor (aref w (+ i 13))
                         (aref w (+ i 8))
                         (aref w (+ i 2))
                         (aref w i))
                 1)))
        (dotimes (i 80)
          (let* ((f (cond
                     ((< i 20) (logior (logand b c) (logand (lognot b) d)))
                     ((< i 40) (logxor b c d))
                     ((< i 60) (logior (logand b c) (logand b d) (logand c d)))
                     (t (logxor b c d))))
                 (k (cond
                     ((< i 20) #x5A827999)
                     ((< i 40) #x6ED9EBA1)
                     ((< i 60) #x8F1BBCDC)
                     (t #xCA62C1D6)))
                 (temp (nelisp-hash--add32
                        (nelisp-hash--rol32 a 5) f e k (aref w i))))
            (setq e d
                  d c
                  c (nelisp-hash--rol32 b 30)
                  b a
                  a temp)))
        (setq h0 (nelisp-hash--add32 h0 a)
              h1 (nelisp-hash--add32 h1 b)
              h2 (nelisp-hash--add32 h2 c)
              h3 (nelisp-hash--add32 h3 d)
              h4 (nelisp-hash--add32 h4 e)
              message (nthcdr 64 message))))
    (concat (nelisp-hash--u32-to-hex-be h0)
            (nelisp-hash--u32-to-hex-be h1)
            (nelisp-hash--u32-to-hex-be h2)
            (nelisp-hash--u32-to-hex-be h3)
            (nelisp-hash--u32-to-hex-be h4))))

(defun nelisp-hash--sha256-small-sigma0 (x)
  "SHA-256 sigma0 function."
  (logxor (nelisp-hash--ror32 x 7)
          (nelisp-hash--ror32 x 18)
          (nelisp-hash--shr32 x 3)))

(defun nelisp-hash--sha256-small-sigma1 (x)
  "SHA-256 sigma1 function."
  (logxor (nelisp-hash--ror32 x 17)
          (nelisp-hash--ror32 x 19)
          (nelisp-hash--shr32 x 10)))

(defun nelisp-hash--sha256-big-sigma0 (x)
  "SHA-256 Sigma0 function."
  (logxor (nelisp-hash--ror32 x 2)
          (nelisp-hash--ror32 x 13)
          (nelisp-hash--ror32 x 22)))

(defun nelisp-hash--sha256-big-sigma1 (x)
  "SHA-256 Sigma1 function."
  (logxor (nelisp-hash--ror32 x 6)
          (nelisp-hash--ror32 x 11)
          (nelisp-hash--ror32 x 25)))

(defun nelisp-hash--sha256-choice (x y z)
  "SHA-256 choice function."
  (logxor (logand x y) (logand (lognot x) z)))

(defun nelisp-hash--sha256-majority (x y z)
  "SHA-256 majority function."
  (logxor (logand x y) (logand x z) (logand y z)))

(defun nelisp-hash--sha256-digest (bytes)
  "Return SHA-256 hex digest for BYTES."
  (let ((message (nelisp-hash--pad-message bytes 'big))
        (h0 #x6a09e667)
        (h1 #xbb67ae85)
        (h2 #x3c6ef372)
        (h3 #xa54ff53a)
        (h4 #x510e527f)
        (h5 #x9b05688c)
        (h6 #x1f83d9ab)
        (h7 #x5be0cd19))
    (while message
      (let* ((block (cl-subseq message 0 64))
             (w (make-vector 64 0))
             (base (nelisp-hash--bytes-to-words-be block))
             (a h0)
             (b h1)
             (c h2)
             (d h3)
             (e h4)
             (f h5)
             (g h6)
             (h h7))
        (dotimes (i 16)
          (aset w i (aref base i)))
        (dotimes (i 48)
          (aset w (+ i 16)
                (nelisp-hash--add32
                 (nelisp-hash--sha256-small-sigma1 (aref w (+ i 14)))
                 (aref w (+ i 9))
                 (nelisp-hash--sha256-small-sigma0 (aref w (+ i 1)))
                 (aref w i))))
        (dotimes (i 64)
          (let* ((t1 (nelisp-hash--add32
                      h
                      (nelisp-hash--sha256-big-sigma1 e)
                      (nelisp-hash--sha256-choice e f g)
                      (aref nelisp-hash--sha256-k i)
                      (aref w i)))
                 (t2 (nelisp-hash--add32
                      (nelisp-hash--sha256-big-sigma0 a)
                      (nelisp-hash--sha256-majority a b c))))
            (setq h g
                  g f
                  f e
                  e (nelisp-hash--add32 d t1)
                  d c
                  c b
                  b a
                  a (nelisp-hash--add32 t1 t2))))
        (setq h0 (nelisp-hash--add32 h0 a)
              h1 (nelisp-hash--add32 h1 b)
              h2 (nelisp-hash--add32 h2 c)
              h3 (nelisp-hash--add32 h3 d)
              h4 (nelisp-hash--add32 h4 e)
              h5 (nelisp-hash--add32 h5 f)
              h6 (nelisp-hash--add32 h6 g)
              h7 (nelisp-hash--add32 h7 h)
              message (nthcdr 64 message))))
    (concat (nelisp-hash--u32-to-hex-be h0)
            (nelisp-hash--u32-to-hex-be h1)
            (nelisp-hash--u32-to-hex-be h2)
            (nelisp-hash--u32-to-hex-be h3)
            (nelisp-hash--u32-to-hex-be h4)
            (nelisp-hash--u32-to-hex-be h5)
            (nelisp-hash--u32-to-hex-be h6)
            (nelisp-hash--u32-to-hex-be h7))))

(defun nelisp-hash-md5 (string)
  "Return RFC 1321 MD5 hex digest of STRING."
  (nelisp-hash--md5-digest (nelisp-hash--string-bytes string)))

(defun nelisp-hash-sha1 (string)
  "Return RFC 3174 SHA-1 hex digest of STRING."
  (nelisp-hash--sha1-digest (nelisp-hash--string-bytes string)))

(defun nelisp-hash-sha256 (string)
  "Return RFC 6234 SHA-256 hex digest of STRING."
  (nelisp-hash--sha256-digest (nelisp-hash--string-bytes string)))

(defun nelisp-hash-secure-hash (algo string)
  "Return hex digest for ALGO over STRING.
Supported ALGO values are `md5', `sha1', and `sha256'.  `sha224',
`sha384', and `sha512' are currently stubbed and signal an error."
  (pcase algo
    ('md5 (nelisp-hash-md5 string))
    ('sha1 (nelisp-hash-sha1 string))
    ('sha256 (nelisp-hash-sha256 string))
    ((or 'sha224 'sha384 'sha512)
     (error "Algorithm not implemented yet: %S" algo))
    (_
     (error "Unsupported secure-hash algorithm: %S" algo))))

(provide 'nelisp-secure-hash)

;;; nelisp-secure-hash.el ends here
