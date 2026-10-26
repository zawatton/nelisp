;;; nelisp-corecall-gc-standalone.el --- record aref/aset and delegated leaf ids under forced collection -*- lexical-binding: t; -*-
;; corecall, native only.  CG_MODE: scanoff (conservative scan disabled, debug switch 28),
;; forced (every VM safepoint collects, switch 70), both (default).  Fresh heap objects
;; are stored into records through Baset and funcall'd aset/puthash/cons and read back
;; through Baref; they are reachable only from the records and the loop's own frame.
;; Exit status 1 on any failure.
(fset 'cg-aref #[514 "H\207" [] 4 "

(fn R I)"])
(fset 'cg-aset #[771 "I\207" [] 6 "

(fn R I V)"])
(fset 'cg-call2 #[771 "\"\207" [] 6 "

(fn F A B)"])
(fset 'cg-call3 #[1028 "#\207" [] 8 "

(fn F A B C)"])
(defvar cg-checks 0)
(defvar cg-failures nil)
(defmacro cg-check (name form)
  `(progn (setq cg-checks (1+ cg-checks))
          (unless (condition-case e ,form (error (push (list ',name e) cg-failures) nil))
            (push ',name cg-failures))))
(defconst cg-n 400)
(let* ((mode (or (getenv "CG_MODE") "both"))
       (scan-off (member mode '("scanoff" "both")))
       (forced (member mode '("forced" "both")))
       (before 0))
  (when scan-off (nelisp--debug-switch 28))
  (when forced (nelisp--debug-switch 70))
  (setq before (nelisp--debug-switch 72))
  (let ((recs (make-vector cg-n nil)) (h (make-hash-table :test 'eq)) (acc nil))
    (dotimes (i cg-n)
      (let ((r (record 'cg-t nil nil nil)))
        ;; fresh values stored by the byte-code aset opcode and by funcall of aset
        (cg-aset r 1 (list i (vector i i)))
        (cg-call3 'aset r 2 (cons i (make-string 3 ?x)))
        (cg-aset r 3 (number-to-string (* i 1000003)))
        (aset recs i r)
        (cg-call3 'puthash i (list i) h)
        (push (cg-call2 'cons i (vector i)) acc)))
    (garbage-collect)
    (cg-check aref-records
      (let ((ok t))
        (dotimes (i cg-n)
          (let ((r (aref recs i)))
            (unless (and (eq (cg-aref r 0) 'cg-t)
                         (equal (cg-aref r 1) (list i (vector i i)))
                         (equal (cg-call2 'aref r 2) (cons i "xxx"))
                         (equal (cg-aref r 3) (number-to-string (* i 1000003))))
              (setq ok nil))))
        ok))
    (cg-check puthash-cons
      (let ((ok t) (j (1- cg-n)))
        (dotimes (i cg-n)
          (unless (equal (gethash i h) (list i)) (setq ok nil)))
        (dolist (c acc) (unless (equal c (cons j (vector j))) (setq ok nil)) (setq j (1- j)))
        ok))
    (cg-check type-slot-roundtrip
      (let ((r (record 'cg-a 1 2)))
        (and (eq (cg-aref r 0) 'cg-a) (= (cg-aref r 2) 2)
             (condition-case nil (progn (cg-aref r 3) nil) (args-out-of-range t)))))
    (cg-check forced-collections-ran (or (not forced) (> (nelisp--debug-switch 72) before)))
    (nelisp--debug-switch 71))
  (princ (format "CG-RESULT mode=%s checks=%d failures=%S\n" mode cg-checks cg-failures))
  (kill-emacs (if cg-failures 1 0)))
