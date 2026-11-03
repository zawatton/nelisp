;;; ht-probe.el --- Doc 243 S2.1: hash-table behaviour after cold-load  -*- lexical-binding: t -*-
;; Prints counts and lookup results only (no addresses, no iteration order of
;; address-keyed tables), so the output is comparable between builds.
(defvar nl243--keys nil)
(defun nl243--mk-keys ()
  (let ((l nil) (i 0))
    (while (< i 40)
      (setq l (cons (list i (intern (format "nl243-sym-%d" i)) (* i 1000003)
                          (cons i (format "str-%d" i)) (format "key-%d" i)
                          (+ 0.5 i))
                    l))
      (setq i (1+ i)))
    (nreverse l)))
(defun nl243--exercise (test)
  (let* ((h (make-hash-table :test test))
         (keys (nl243--mk-keys))
         (found 0) (missing 0) (cnt nil))
    ;; keys: one fresh object per kind, kept alive in KEYS
    (dolist (k keys)
      (dolist (kk (cdr k)) (puthash kk (car k) h)))
    (setq cnt (hash-table-count h))
    (dolist (k keys)
      (dolist (kk (cdr k))
        (if (eq (gethash kk h 'none) (car k)) (setq found (1+ found)) (setq missing (1+ missing)))))
    ;; fresh but equal objects: only eql/equal may find fixnums/floats, only equal may find conses/strings
    (let ((fresh-cons 0) (fresh-str 0) (fresh-fix 0) (fresh-flt 0))
      (dolist (k keys)
        (when (eq (gethash (cons (car k) (format "str-%d" (car k))) h 'none) (car k)) (setq fresh-cons (1+ fresh-cons)))
        (when (eq (gethash (format "key-%d" (car k)) h 'none) (car k)) (setq fresh-str (1+ fresh-str)))
        (when (eq (gethash (* (car k) 1000003) h 'none) (car k)) (setq fresh-fix (1+ fresh-fix)))
        (when (eq (gethash (+ 0.5 (car k)) h 'none) (car k)) (setq fresh-flt (1+ fresh-flt))))
      ;; remove half and re-check
      (dolist (k keys) (when (cl-evenp (car k)) (remhash (nth 1 k) h)))
      (list test cnt found missing fresh-cons fresh-str fresh-fix fresh-flt (hash-table-count h)
            (gethash (nth 1 (nth 2 keys)) h 'gone) (gethash (nth 1 (nth 3 keys)) h 'gone)))))
(unless (fboundp 'cl-evenp) (defun cl-evenp (n) (= 0 (% n 2))))
(dolist (tst '(eq eql equal))
  (prin1 (nl243--exercise tst)) (terpri))
;; symbol-keyed lookup of a prelude symbol through a fresh table
(let ((h (make-hash-table :test 'eq)))
  (dolist (s '(car cdr cons mapcar nl243-sym-1 t nil)) (puthash s (symbol-name s) h))
  (prin1 (list (hash-table-count h) (gethash 'car h) (gethash 'mapcar h) (gethash 'nl243-sym-1 h) (gethash 'absent-sym h 'none)))
  (terpri))
;; Hash tables created by the prelude / image build and bound in the image:
;; every key is looked up again through gethash (for eq tables this exercises
;; the address-keyed index after relocation).  Unbound names are skipped.
(let ((tot 0) (hit 0) (miss 0) (ntab 0))
  (dolist (name '(nelisp--tp-string-properties nelisp--unicode-case-memo
                  nlre--case-owner-cache nlre--compiled-cache nlre--nat-progs-fold
                  nelisp-buffer--text-cache nelisp--tf-zone-cache nelisp--file-locks))
    (when (and (boundp name) (hash-table-p (symbol-value name)))
      (let* ((h (symbol-value name)) (n (hash-table-count h)) (th 0) (tm 0))
        (maphash (lambda (k _v)
                   (if (eq (gethash k h 'nl243-none) 'nl243-none) (setq tm (1+ tm)) (setq th (1+ th))))
                 h)
        (setq ntab (1+ ntab) tot (+ tot n) hit (+ hit th) miss (+ miss tm))
        (prin1 (list 'prelude-table name (hash-table-test h) n th tm)) (terpri))))
  (prin1 (list 'prelude-sweep ntab tot hit miss)) (terpri))
(kill-emacs 0)
