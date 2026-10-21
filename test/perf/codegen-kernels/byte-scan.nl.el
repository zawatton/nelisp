;; byte-scan: find a byte in a unibyte string (memchr-style loop).
;; S is a string Sexp box (tag byte 5, data pointer at +16, byte length
;; at +24, as in lisp/nelisp-sexp-layout.el).  Returns the index of the
;; first byte equal to C, or the length when C is absent.
(defun ck_kernel (s c)
  (let ((p (ptr-read-u64 s 16))
        (n (ptr-read-u64 s 24))
        (i 0))
    (while (and (< i n) (/= (ptr-read-u8 p i) c))
      (setq i (+ i 1)))
    i))

(defun ck_main (n)
  (let ((m 100000)
        (s (ck_mmap 64))
        (data (ck_mmap 100064))
        (i 0))
    (while (< i m)
      (ptr-write-u8 data i 97)
      (setq i (+ i 1)))
    (ptr-write-u8 s 0 5)
    (ptr-write-u64 s 8 (+ m 64))
    (ptr-write-u64 s 16 data)
    (ptr-write-u64 s 24 n)
    (ck_kernel s 122)))
