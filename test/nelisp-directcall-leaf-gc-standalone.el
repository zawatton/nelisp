;;; nelisp-directcall-leaf-gc-standalone.el --- Leaf calls under forced collection -*- lexical-binding: t; -*-
;; w1leaf, native only.  LEAF_GC_MODE: scanoff (conservative scan disabled,
;; debug switch 28), forced (every VM safepoint collects, switch 70), both.
;; Every leaf id runs in a bytecode loop whose arguments and results are fresh
;; heap objects reachable only from the loop's own roots; the values are
;; checked after the loop.  Exit status 1 on any failure.
(fset 'lg-bc1 #[514 "\1\1!\207" [] 4])
(fset 'lg-bc2 #[771 "\2\2\2\"\207" [] 6])
(fset 'lg-bc3 #[1028 "\3\3\3\3#\207" [] 8])
(defvar lg-checks 0)
(defvar lg-failures nil)
(defmacro lg-check (name form)
  `(progn (setq lg-checks (1+ lg-checks))
          (unless (condition-case e ,form (error (push (list ',name e) lg-failures) nil))
            (push ',name lg-failures))))
(defconst lg-n 300)
(let* ((mode (or (getenv "LEAF_GC_MODE") "both"))
       (scan-off (member mode '("scanoff" "both")))
       (forced (member mode '("forced" "both")))
       (before 0))
  (when scan-off (nelisp--debug-switch 28))
  (when forced (nelisp--debug-switch 70))
  (setq before (nelisp--debug-switch 72))
  (let ((acc nil) (vec (make-vector lg-n nil)) (h (make-hash-table :test 'equal))
        (hs (make-hash-table :test 'eq)) (sum 0) (cars nil))
    (dotimes (i lg-n)
      ;; cons (111): fresh operands, the result only in the accumulator.
      (push (lg-bc2 'cons (list i i) (vector i)) acc)
      ;; car/cdr/identity/1+/1-/+/-/logand on fresh values.
      (push (lg-bc1 'car (list (cons i i) i)) cars)
      (setq sum (+ sum (lg-bc1 'car (list i)) (lg-bc1 '1+ i) (lg-bc1 '1- i)
                   (lg-bc2 '+ i 1) (lg-bc2 '- i 1) (lg-bc2 'logand i 255)
                   (lg-bc1 'identity i)))
      ;; aset (-15) of a fresh list into a vector; nth (10); length (-16).
      (lg-bc3 'aset vec i (list i (vector i)))
      (setq sum (+ sum (lg-bc2 'nth 0 (aref vec i)) (lg-bc1 'length (aref vec i))))
      ;; puthash/gethash/remhash/count with fresh keys and values.
      (lg-bc3 'puthash (list i) (cons i (vector i)) h)
      (lg-bc3 'puthash i (list i) hs))
    (lg-check cons-acc
      (let ((i (1- lg-n)) (ok t))
        (dolist (c acc) (unless (and (equal (car c) (list i i)) (equal (cdr c) (vector i))) (setq ok nil)) (setq i (1- i)))
        ok))
    (lg-check cars (let ((i (1- lg-n)) (ok t)) (dolist (c cars) (unless (equal c (cons i i)) (setq ok nil)) (setq i (1- i))) ok))
    (lg-check sum (= sum (let ((s 0)) (dotimes (i lg-n) (setq s (+ s i (1+ i) (1- i) (+ i 1) (- i 1) (logand i 255) i i 2))) s)))
    (lg-check aset-nth-length
      (let ((ok t)) (dotimes (i lg-n) (unless (equal (aref vec i) (list i (vector i))) (setq ok nil))) ok))
    (lg-check puthash-equal
      (let ((ok t))
        (dotimes (i lg-n)
          (unless (equal (lg-bc2 'gethash (list i) h) (cons i (vector i))) (setq ok nil)))
        ok))
    (lg-check puthash-eq (let ((ok t)) (dotimes (i lg-n) (unless (equal (lg-bc2 'gethash i hs) (list i)) (setq ok nil))) ok))
    (lg-check count (= (lg-bc1 'hash-table-count h) lg-n))
    (lg-check remhash
      (progn (dotimes (i lg-n) (when (= (% i 2) 0) (lg-bc2 'remhash (list i) h)))
             (and (= (hash-table-count h) (/ lg-n 2))
                  (let ((ok t)) (dotimes (i lg-n) (when (and (= (% i 2) 1) (not (equal (gethash (list i) h) (cons i (vector i))))) (setq ok nil))) ok))))
    ;; The walkers' per-element leaf call under collection.
    (lg-check mapcar-car
      (let ((r (mapcar 'car (mapcar (lambda (i) (cons (list i) i)) (number-sequence 1 200)))))
        (equal r (mapcar #'list (number-sequence 1 200)))))
    (lg-check mapcar-cdr
      (equal (mapcar 'cdr (mapcar (lambda (i) (cons i (vector i))) (number-sequence 1 200)))
             (mapcar #'vector (number-sequence 1 200))))
    (lg-check mapc-1+ (let ((n 0)) (mapc '1+ (number-sequence 1 200)) t))
    (lg-check mapcar-identity (equal (mapcar 'identity (mapcar #'list (number-sequence 1 200))) (mapcar #'list (number-sequence 1 200)))))
  (garbage-collect)
  (when forced
    (lg-check forced-collections-ran (> (nelisp--debug-switch 72) before))
    (nelisp--debug-switch 71))
  (when scan-off (nelisp--debug-switch 29))
  (dolist (f (reverse lg-failures)) (princ (format "  MISMATCH %S\n" f)))
  (princ (format "LEAFGC mode=%s checks=%d failures=%d collections=%S %s\n" mode lg-checks (length lg-failures)
                 (- (nelisp--debug-switch 72) before) (if lg-failures "FAIL" "PASS")))
  (kill-emacs (if lg-failures 1 0)))
