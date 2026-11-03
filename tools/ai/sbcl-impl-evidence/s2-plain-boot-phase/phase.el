(let* ((a (nelisp--monotonic-ns)) (b (nelisp--monotonic-ns)) (v (nelisp--boot-phase-ns)))
  (princ (format "mono a=%S b=%S nondecreasing=%S\n" a b (<= a b)))
  (princ (format "boot=%S vectorp=%S len=%S\n" v (vectorp v) (length v)))
  (princ (format "ordered=%S after-reinit=%S\n" (<= (aref v 0) (aref v 1) (aref v 2)) (<= (aref v 2) a))))
