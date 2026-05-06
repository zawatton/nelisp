;;; nelisp-stdlib-list.el --- Sweep 9 G1 list operations  -*- lexical-binding: t; -*-

(defun nthcdr (n list)
  (if (= n 0) list
    (if (null list) nil
      (nthcdr (1- n) (cdr list)))))

(defun nth (n list)
  (car (nthcdr n list)))

(defun reverse (list)
  (let ((acc nil))
    (while list
      (setq acc (cons (car list) acc))
      (setq list (cdr list)))
    acc))

(defun nreverse (list)
  (reverse list))

;; Rust-min batch 6o (2026-05-06): `append' migrated from Rust to
;; elisp.  The previous `bi_append' (~61 LOC) implemented the
;; multi-arg sequence concatenation contract:
;;
;;   * 0 args                → nil
;;   * 1 arg                 → return the arg unchanged (no copy)
;;   * N args (N >= 2)       → fresh proper-list spine made of every
;;                             element from non-final args
;;                             (left-to-right, listwise) ending in
;;                             the FINAL arg (used as the tail
;;                             unchanged — can be any value, even
;;                             a non-list improper tail)
;;
;; Non-final args may be: nil (skipped), cons (walked spine), vector
;; (iter), or string (iter as int-codepoints).  An improper-list
;; non-final arg (= dotted tail) signals `wrong-type-argument' once
;; the cons chain exits onto a non-cons non-nil cell.  Non-sequence
;; non-final arg (e.g., an integer) signals immediately.
;;
;; All ingredients (`consp' / `vectorp' / `stringp' / `aref' /
;; `length' / `cons' / `signal') are primitives, so the elisp
;; version is straight transcription of the Rust loop.

(defun nelisp--append-collect (acc seq)
  "Walk SEQ and `cons' each element onto ACC (= reverse-order
accumulator).  SEQ may be nil / cons / vector / string.  Returns
the new ACC.  Signals `wrong-type-argument' for improper-list cons
or non-sequence atom."
  (cond
   ((null seq) acc)
   ((consp seq)
    (let ((cur seq))
      (while (consp cur)
        (setq acc (cons (car cur) acc))
        (setq cur (cdr cur)))
      (when cur
        (signal 'wrong-type-argument (list 'listp seq)))
      acc))
   ((vectorp seq)
    (let ((i 0)
          (n (length seq)))
      (while (< i n)
        (setq acc (cons (aref seq i) acc))
        (setq i (1+ i)))
      acc))
   ((stringp seq)
    (let ((i 0)
          (n (length seq)))
      (while (< i n)
        (setq acc (cons (aref seq i) acc))
        (setq i (1+ i)))
      acc))
   (t (signal 'wrong-type-argument (list 'sequencep seq)))))

(defun append (&rest args)
  "Concatenate sequences ARGS into a fresh proper-list spine.
Non-final args may be list / vector / string / nil.  The FINAL arg
is used as the tail (= unchanged, can be any value).  Single-arg
call returns the arg unchanged (= no copy)."
  (cond
   ((null args) nil)
   ((null (cdr args)) (car args))
   (t
    (let ((cur args)
          (acc nil)
          (tail nil))
      (while (cdr cur)
        (setq acc (nelisp--append-collect acc (car cur)))
        (setq cur (cdr cur)))
      (setq tail (car cur))
      (let ((result tail))
        (while acc
          (setq result (cons (car acc) result))
          (setq acc (cdr acc)))
        result)))))

;; nelisp-stdlib-list.el ends here
