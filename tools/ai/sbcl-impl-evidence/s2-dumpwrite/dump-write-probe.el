;;; dump-write-probe.el --- S2.12 runtime probe  -*- lexical-binding: t; -*-
;; Run on a built image after the dump-write change.
(let ((r (condition-case e
             (cons 'ret (nelisp--arena-dump-image-stream "/nonexistent-dir/x.cold"))
           (error (cons 'sig (car e))))))
  (if (eq (car r) 'sig)
      (princ (format "OK-SIGNALED %S\n" (cdr r)))
    (princ (format "BAD-RETURNED %S\n" (cdr r)))))
(let* ((f (make-temp-file "dump-write-probe" nil ".cold"))
       (n (nelisp--arena-dump-image-stream f))
       (sz (nth 7 (file-attributes f))))
  (princ (if (and (integerp n) (> n 0) (= n sz))
             (format "OK-WRITTEN %d %d\n" n sz)
           (format "BAD-WRITTEN %S %S\n" n sz)))
  (delete-file f))
