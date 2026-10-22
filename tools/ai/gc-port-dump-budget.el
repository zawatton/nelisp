;;; gc-port-dump-budget.el --- the image dump's address-space budget  -*- lexical-binding: t; -*-

;; Builds a heap with a large pointer-free part (GCDB_GEN=1) or adds to a restored one (2),
;; tells the harness through GCDB_MARK that it is ready, and dumps it to GCDB_IMAGE.  The harness samples the process's VmSize around the dump:
;; the relocation table must be sized by the pointer fields the walk reaches (8 bytes each), not
;; by the heap -- `ulimit -v' counts the demand-paged table.  Prints "GCDB dump N" (the image
;; size, -1 on failure) and "GCDB PASS|FAIL".
(defvar gcdb-big nil)
(defvar gcdb-small nil)
(defvar gcdb-extra nil)
(defvar gcdb-gen (or (getenv "GCDB_GEN") "1"))
(if (equal gcdb-gen "1")
    ;; first generation: a 300 MB pointer-free string and 20,000 small lists
    (progn
      (setq gcdb-big (make-string 300000000 ?a))
      (setq gcdb-small (let ((l nil) (i 0)) (while (< i 20000) (setq l (cons (list i (number-to-string i)) l)) (setq i (1+ i))) l)))
  ;; second generation (run under --cold-load-from the first image): the restored
  ;; image's objects stay, a little new data joins them
  (setq gcdb-extra (let ((l nil) (i 0)) (while (< i 5000) (setq l (cons (list i (make-string 20 ?x)) l)) (setq i (1+ i))) l))
  (unless (and (stringp gcdb-big) (= (length gcdb-big) 300000000) (= (length gcdb-small) 20000))
    (princ "GCDB restored heap is wrong\n")))
(garbage-collect)
(write-region "ready" nil (getenv "GCDB_MARK"))
(let ((n (nelisp--arena-dump-image-stream (getenv "GCDB_IMAGE"))))
  (princ (format "GCDB dump %S\n" n))
  (princ (format "GCDB %s\n" (if (and (integerp n) (> n 0)) "PASS" "FAIL"))))
nil
