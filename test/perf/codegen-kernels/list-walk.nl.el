;; list-walk: length of a proper list.
;; W is a value WORD: an immediate (low bit 1; nil is 3) or a pointer to a
;; 32-byte Sexp box whose tag byte 7 marks a cons; the box payload at +8
;; points to the NlConsBox (car WORD at +0, cdr WORD at +8).  The type
;; test is the runtime's `(= (sexp-tag x) 7)' idiom and the step is the
;; body of the runtime's `nl_cons_cdr_word' accessor.
(defun ck_kernel (w)
  (let ((n 0))
    (while (= (sexp-tag w) 7)
      (setq w (ptr-read-u64 (ptr-read-u64 w 8) 8))
      (setq n (+ n 1)))
    n))

(defun ck_main (n)
  (let ((m 100000)
        (boxes (ck_mmap 3200064))
        (cells (ck_mmap 3200064))
        (k 0)
        (next 0))
    (while (< k m)
      (setq next (if (< (+ k 1) m) (+ boxes (* (+ k 1) 32)) 3))
      (ptr-write-u8 boxes (* k 32) 7)
      (ptr-write-u64 boxes (+ (* k 32) 8) (+ cells (* k 32)))
      (ptr-write-u64 cells (* k 32) (+ (* k 4) 1))
      (ptr-write-u64 cells (+ (* k 32) 8) next)
      (setq k (+ k 1)))
    (ck_kernel (if (= n 0) 3 (+ boxes (* (- m n) 32))))))
