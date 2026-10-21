;; class-table: run each byte of a unibyte string through a 256-entry
;; table, as the regex fastmap and syntax lookups do; stop at the first
;; byte whose entry is non-zero.  S is a string Sexp box (data pointer at
;; +16, byte length at +24); TABLE is a 256-byte table.
(defun ck_kernel (s table)
  (let ((p (ptr-read-u64 s 16))
        (n (ptr-read-u64 s 24))
        (i 0))
    (while (and (< i n) (= (ptr-read-u8 table (ptr-read-u8 p i)) 0))
      (setq i (+ i 1)))
    i))

(defun ck_main (n)
  (let ((m 100000)
        (s (ck_mmap 64))
        (table (ck_mmap 256))
        (data (ck_mmap 100064))
        (i 0))
    (while (< i m)
      (ptr-write-u8 data i 97)
      (setq i (+ i 1)))
    (ptr-write-u8 table 122 1)
    (ptr-write-u8 s 0 5)
    (ptr-write-u64 s 8 (+ m 64))
    (ptr-write-u64 s 16 data)
    (ptr-write-u64 s 24 n)
    (ck_kernel s table)))
