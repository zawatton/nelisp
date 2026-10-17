;;; standalone-hash-identity-check.el --- eq/eql hash tables key by identity -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-hash-identity-check.el
;;
;; eq and eql tables hashed cons keys by their contents (to depth 8).  A
;; `setcar' on a stored key then sent `gethash' to another bucket, so a key
;; still `eq' to the stored one was not found (GNU finds it), and each miss
;; on a cons deeper than the hash depth scanned every bucket: filling a
;; table with the conses of one list was quadratic.  Each case below is what
;; GNU Emacs 31.1 answers.  The collections in between check that buckets
;; stay right when compaction moves the keys.  Prints one HASH-IDENTITY line
;; and exits non-zero on failure.

(defvar hash-identity-failures nil)

(defmacro hash-identity-expect (name form expected)
  `(let ((got (condition-case err ,form (error (list 'error err)))))
     (unless (equal got ,expected)
       (push (list ,name got ,expected) hash-identity-failures))))

;; A stored cons key is found after it is mutated.
(let ((k (list 1 2)) (h (make-hash-table :test 'eq)))
  (puthash k 'v h)
  (setcar k 99)
  (hash-identity-expect 'eq-mutated-key (gethash k h) 'v)
  (setcdr k (list 3 4 5))
  (hash-identity-expect 'eq-mutated-cdr (gethash k h) 'v)
  (hash-identity-expect 'eq-count (hash-table-count h) 1))

;; Distinct equal conses are distinct keys under eq and eql, one under equal.
(dolist (test '(eq eql equal))
  (let ((h (make-hash-table :test test)))
    (puthash (list 1 2) 'a h)
    (puthash (list 1 2) 'b h)
    (hash-identity-expect (list test 'equal-conses)
                          (list (hash-table-count h) (gethash (list 1 2) h))
                          (if (eq test 'equal) '(1 b) '(2 nil)))))

;; eql compares numbers by value, eq does not promise to for floats.
(let ((h (make-hash-table :test 'eql)))
  (puthash 1.5 'f h)
  (puthash 7 'i h)
  (hash-identity-expect 'eql-float (gethash (/ 3.0 2) h) 'f)
  (hash-identity-expect 'eql-fixnum (gethash (+ 3 4) h) 'i))

;; Distinct equal strings are distinct eq keys; symbols are found by name.
(let ((h (make-hash-table :test 'eq)) (s (copy-sequence "abc")))
  (puthash s 1 h)
  (puthash 'sym 2 h)
  (hash-identity-expect 'eq-string-same (gethash s h) 1)
  (hash-identity-expect 'eq-string-other (gethash (copy-sequence "abc") h) nil)
  (hash-identity-expect 'eq-symbol (gethash (intern "sym") h) 2))

;; Many cons keys, removal, growth, and collections in between.
(let* ((n 20000)
       (keys (let ((l nil) (i 0)) (while (< i n) (setq l (cons (list i) l) i (1+ i))) l))
       (h (make-hash-table :test 'eq))
       (t0 (float-time)))
  (let ((c keys)) (while c (puthash (car c) (caar c) h) (setq c (cdr c))))
  (let ((fill (- (float-time) t0)))
    (garbage-collect)
    (garbage-collect)
    (let ((bad 0) (c keys))
      (while c (unless (eql (gethash (car c) h) (caar c)) (setq bad (1+ bad))) (setq c (cdr c)))
      (hash-identity-expect 'many-after-gc bad 0))
    (let ((c keys) (i 0))
      (while c (when (= 0 (% i 2)) (remhash (car c) h)) (setq c (cdr c) i (1+ i))))
    (hash-identity-expect 'many-count-after-remhash (hash-table-count h) (/ n 2))
    (let ((seen 0)) (maphash (lambda (_k _v) (setq seen (1+ seen))) h)
      (hash-identity-expect 'many-maphash seen (/ n 2)))
    (let ((h2 (copy-hash-table h)) (c keys) (i 0) (bad 0))
      (garbage-collect)
      (while c
        (unless (eql (gethash (car c) h2) (if (= 0 (% i 2)) nil (caar c))) (setq bad (1+ bad)))
        (setq c (cdr c) i (1+ i)))
      (hash-identity-expect 'copy-after-gc bad 0))
    (clrhash h)
    (hash-identity-expect 'clrhash (list (hash-table-count h) (gethash (car keys) h)) '(0 nil))
    ;; The conses of one list as keys: was 1.76 s for 4000 and quadratic
    ;; (about 45 s for 20000); linear now, bounded by the prelude's
    ;; interpreted `puthash' wrapper (about 150 us per call).
    (let* ((l (make-list 20000 'x)) (h3 (make-hash-table :test 'eq)) (t1 (float-time)))
      (let ((c l)) (while c (puthash c t h3) (setq c (cdr c))))
      (let ((list-fill (- (float-time) t1)))
        (hash-identity-expect 'list-conses-count (hash-table-count h3) 20000)
        (hash-identity-expect 'list-conses-fast (< list-fill 10.0) t)
        (princ (format "  fill %d cons keys %.3f s, %d list conses %.3f s\n"
                       n fill 20000 list-fill))))))

(dolist (f (reverse hash-identity-failures))
  (princ (format "  MISMATCH %S\n" f)))
(princ (format "HASH-IDENTITY failures=%d %s\n" (length hash-identity-failures)
               (if hash-identity-failures "FAIL" "PASS")))
(kill-emacs (if hash-identity-failures 1 0))

;;; standalone-hash-identity-check.el ends here
