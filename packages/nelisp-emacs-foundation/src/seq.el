;;; seq.el --- lightweight standard seq facade for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Vendor Emacs Lisp frequently requires `seq'.  The full vendor seq.el
;; depends on cl-generic and pcase integration that is still heavier than
;; the current standalone path needs, while `emacs-stub-bulk' only leaves
;; placeholder functions.  This file provides the common sequence API over
;; lists, strings, and vectors.

;;; Code:

(require 'cl-lib)

(defun seqp (object)
  "Return non-nil when OBJECT is a list, string, or vector."
  (or (listp object) (stringp object) (vectorp object)))

(defun seq-length (sequence)
  "Return SEQUENCE length."
  (length sequence))

(defun seq-elt (sequence n)
  "Return the Nth element of SEQUENCE."
  (elt sequence n))

(defun seq-first (sequence)
  "Return the first element of SEQUENCE."
  (seq-elt sequence 0))

(defun seq-rest (sequence)
  "Return SEQUENCE without its first element."
  (seq-drop sequence 1))

(defun seq-copy (sequence)
  "Return a shallow copy of SEQUENCE."
  (copy-sequence sequence))

(defun seq--list (sequence)
  "Return SEQUENCE as a list."
  (cond
   ((listp sequence) sequence)
   ((vectorp sequence) (append sequence nil))
   ((stringp sequence)
    (let ((i 0)
          (n (length sequence))
          out)
      (while (< i n)
        (push (aref sequence i) out)
        (setq i (1+ i)))
      (nreverse out)))
   (t (signal 'wrong-type-argument (list 'sequencep sequence)))))

(defun seq--same-type (list prototype)
  "Return LIST converted to PROTOTYPE's sequence type."
  (cond
   ((listp prototype) list)
   ((vectorp prototype) (apply #'vector list))
   ((stringp prototype) (apply #'string list))
   (t list)))

(defun seq-into (sequence type)
  "Convert SEQUENCE into TYPE.
TYPE can be `list', `vector', `string', or `sequence'."
  (cond
   ((or (eq type 'sequence) (eq type nil)) sequence)
   ((eq type 'list) (seq--list sequence))
   ((eq type 'vector) (apply #'vector (seq--list sequence)))
   ((eq type 'string) (apply #'string (seq--list sequence)))
   (t (signal 'wrong-type-argument (list 'type-specifier-p type)))))

(defalias 'seq-into-sequence #'seq-into)

(defun seq-do (function sequence)
  "Call FUNCTION for every element of SEQUENCE and return SEQUENCE."
  (mapc function sequence)
  sequence)

(defalias 'seq-each #'seq-do)

(defmacro seq-doseq (spec &rest body)
  "Loop over SPEC's sequence, evaluating BODY for each element."
  (declare (indent 1))
  `(seq-do (lambda (,(car spec)) ,@body) ,(cadr spec)))

(defun seq-do-indexed (function sequence)
  "Call FUNCTION for every element of SEQUENCE with element and index."
  (let ((i 0))
    (seq-do (lambda (elt)
              (funcall function elt i)
              (setq i (1+ i)))
            sequence))
  nil)

(defun seq-map (function sequence)
  "Return a list of FUNCTION applied to each element of SEQUENCE."
  (mapcar function sequence))

(defun seq-map-indexed (function sequence)
  "Return a list of FUNCTION applied to each element and index."
  (let ((i 0))
    (seq-map (lambda (elt)
               (prog1 (funcall function elt i)
                 (setq i (1+ i))))
             sequence)))

(defun seq-mapn (function sequence &rest sequences)
  "Map FUNCTION over SEQUENCE and SEQUENCES until the shortest ends."
  (let ((lists (mapcar #'seq--list (cons sequence sequences)))
        out)
    (while (not (memq nil lists))
      (push (apply function (mapcar #'car lists)) out)
      (setq lists (mapcar #'cdr lists)))
    (nreverse out)))

(defun seq-subseq (sequence start &optional end)
  "Return the subsequence of SEQUENCE from START to END."
  (cond
   ((or (stringp sequence) (vectorp sequence))
    (substring sequence start end))
   ((listp sequence)
    (let* ((len (length sequence))
           (s (if (< start 0) (+ len start) start))
           (e (if end (if (< end 0) (+ len end) end) len)))
      (when (or (< s 0) (> s len))
        (error "Start index out of bounds: %s" start))
      (when (or (< e s) (> e len))
        (error "End index out of bounds: %s" end))
      (let ((rest (nthcdr s sequence))
            (n (- e s))
            out)
        (while (> n 0)
          (push (car rest) out)
          (setq rest (cdr rest)
                n (1- n)))
        (nreverse out))))
   (t (signal 'wrong-type-argument (list 'sequencep sequence)))))

(defun seq-take (sequence n)
  "Return the first N elements of SEQUENCE."
  (seq-subseq sequence 0 (min (max n 0) (seq-length sequence))))

(defun seq-drop (sequence n)
  "Return SEQUENCE without its first N elements."
  (if (<= n 0)
      sequence
    (seq-subseq sequence (min n (seq-length sequence)))))

(defun seq-take-while (predicate sequence)
  "Return leading elements of SEQUENCE while PREDICATE is non-nil."
  (let (out
        done)
    (seq-do (lambda (elt)
              (unless done
                (if (funcall predicate elt)
                    (push elt out)
                  (setq done t))))
            sequence)
    (seq--same-type (nreverse out) sequence)))

(defun seq-drop-while (predicate sequence)
  "Drop leading elements of SEQUENCE while PREDICATE is non-nil."
  (let ((list (seq--list sequence)))
    (while (and list (funcall predicate (car list)))
      (setq list (cdr list)))
    (seq--same-type list sequence)))

(defun seq-filter (predicate sequence)
  "Return elements of SEQUENCE for which PREDICATE returns non-nil."
  (let (out)
    (seq-do (lambda (elt)
              (when (funcall predicate elt)
                (push elt out)))
            sequence)
    (nreverse out)))

(defun seq-remove (predicate sequence)
  "Return elements of SEQUENCE for which PREDICATE returns nil."
  (seq-filter (lambda (elt) (not (funcall predicate elt))) sequence))

(defun seq-find (predicate sequence &optional default)
  "Return the first element in SEQUENCE satisfying PREDICATE, or DEFAULT."
  (catch 'found
    (seq-do (lambda (elt)
              (when (funcall predicate elt)
                (throw 'found elt)))
            sequence)
    default))

(defun seq-some (predicate sequence)
  "Return first non-nil value of PREDICATE over SEQUENCE."
  (catch 'found
    (seq-do (lambda (elt)
              (let ((value (funcall predicate elt)))
                (when value
                  (throw 'found value))))
            sequence)
    nil))

(defun seq-every-p (predicate sequence)
  "Return non-nil when PREDICATE is non-nil for every element."
  (not (seq-some (lambda (elt) (not (funcall predicate elt))) sequence)))

(defun seq-empty-p (sequence)
  "Return non-nil when SEQUENCE has no elements."
  (= (seq-length sequence) 0))

(defun seq-contains-p (sequence elt &optional testfn)
  "Return non-nil when SEQUENCE contains ELT."
  (let ((test (or testfn #'equal)))
    (seq-some (lambda (candidate) (funcall test candidate elt)) sequence)))

(defun seq-position (sequence elt &optional testfn)
  "Return index of ELT in SEQUENCE, or nil."
  (let ((test (or testfn #'equal))
        (i 0)
        found)
    (catch 'done
      (seq-do (lambda (candidate)
                (when (funcall test candidate elt)
                  (setq found i)
                  (throw 'done found))
                (setq i (1+ i)))
              sequence))
    found))

(defun seq-reduce (function sequence initial-value)
  "Reduce SEQUENCE by calling FUNCTION with accumulator and element."
  (let ((acc initial-value))
    (seq-do (lambda (elt)
              (setq acc (funcall function acc elt)))
            sequence)
    acc))

(defun seq-uniq (sequence &optional testfn)
  "Return a list of SEQUENCE elements with duplicates removed."
  (let ((test (or testfn #'equal))
        seen
        out)
    (seq-do (lambda (elt)
              (unless (seq-some (lambda (existing)
                                  (funcall test existing elt))
                                seen)
                (push elt seen)
                (push elt out)))
            sequence)
    (nreverse out)))

(defun seq-concatenate (type &rest sequences)
  "Concatenate SEQUENCES and convert the result to TYPE."
  (seq-into (apply #'append (mapcar #'seq--list sequences)) type))

(defun seq-sort (predicate sequence)
  "Return a sorted copy of SEQUENCE as a list."
  (sort (seq--list (seq-copy sequence)) predicate))

(defun seq-sort-by (function predicate sequence)
  "Sort SEQUENCE by values returned from FUNCTION using PREDICATE."
  (seq-sort (lambda (a b)
              (funcall predicate (funcall function a) (funcall function b)))
            sequence))

(defun seq-max (sequence)
  "Return the numerically largest element of SEQUENCE."
  (let ((list (seq--list sequence)))
    (unless list (error "empty sequence"))
    (seq-reduce #'max (cdr list) (car list))))

(defun seq-min (sequence)
  "Return the numerically smallest element of SEQUENCE."
  (let ((list (seq--list sequence)))
    (unless list (error "empty sequence"))
    (seq-reduce #'min (cdr list) (car list))))

(defun seq-random-elt (sequence)
  "Return a random element from SEQUENCE."
  (let ((len (seq-length sequence)))
    (when (= len 0)
      (error "empty sequence"))
    (seq-elt sequence (random len))))

(defun seq-group-by (function sequence)
  "Group SEQUENCE elements by FUNCTION, returning an alist."
  (let (groups)
    (seq-do (lambda (elt)
              (let* ((key (funcall function elt))
                     (cell (assoc key groups)))
                (if cell
                    (setcdr cell (cons elt (cdr cell)))
                  (push (list key elt) groups))))
            sequence)
    (mapcar (lambda (cell)
              (cons (car cell) (nreverse (cdr cell))))
            (nreverse groups))))

;;; Doc 16 breadth round 2 — set / partition / mapcat / keep / reverse.
;; These complete the common seq API the facade was missing; vendor
;; packages (project / eshell / transient / magit-section) rely on them.

;; `seq-reverse' is preloaded on host Emacs as a `cl-defgeneric'; gate so
;; the facade only defines it on the standalone runtime (where it is void)
;; and does not narrow the host generic's arglist at byte-compile time.
(unless (fboundp 'seq-reverse)
  (defun seq-reverse (sequence)
    "Return a sequence with the elements of SEQUENCE in reverse order."
    (reverse sequence)))

(defun seq-partition (sequence n)
  "Return a list of the elements of SEQUENCE grouped into sublists of length N."
  (setq n (max n 1))
  (let ((seq (append sequence nil))
        (result nil))
    (while seq
      (push (take n seq) result)
      (setq seq (nthcdr n seq)))
    (nreverse result)))

(defun seq-mapcat (function sequence &optional type)
  "Concatenate the results of applying FUNCTION to each element of SEQUENCE.
The result is a sequence of TYPE, or a list when TYPE is nil."
  (apply #'seq-concatenate (or type 'list)
         (seq-map function sequence)))

(defun seq-keep (function sequence)
  "Apply FUNCTION to each element of SEQUENCE, returning the non-nil results."
  (delq nil (seq-map function sequence)))

(defun seq-difference (sequence1 sequence2 &optional testfn)
  "Return a list of the elements of SEQUENCE1 that are not in SEQUENCE2.
Equality is tested with TESTFN (default `equal')."
  (seq-remove (lambda (elt) (seq-contains-p sequence2 elt testfn))
              sequence1))

(defun seq-intersection (sequence1 sequence2 &optional testfn)
  "Return a list of the elements that appear in both SEQUENCE1 and SEQUENCE2.
Equality is tested with TESTFN (default `equal')."
  (seq-filter (lambda (elt) (seq-contains-p sequence2 elt testfn))
              sequence1))

(defun seq-union (sequence1 sequence2 &optional testfn)
  "Return a list of the elements that appear in either SEQUENCE1 or SEQUENCE2.
Equality is tested with TESTFN (default `equal')."
  (append (append sequence1 nil)
          (seq-difference sequence2 sequence1 testfn)))

(provide 'seq)

;;; seq.el ends here
