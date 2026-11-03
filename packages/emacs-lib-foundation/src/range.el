;;; range.el --- lightweight numeric range helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Pure Elisp implementation of the small `range' API used by several
;; vendor modules.  A range is either an integer, a cons span (START . END),
;; or a list mixing both forms.

;;; Code:

(defun range-normalize (range)
  "Normalize RANGE.
If RANGE is a single range element, return a one-element list."
  (if (listp (cdr-safe range))
      range
    (list range)))

(defun range-denormalize (range)
  "Return a single span when RANGE contains exactly one cons span."
  (if (and (consp (car range))
           (null (cdr range)))
      (car range)
    range))

(defun range--span-start (span)
  "Return first number in SPAN."
  (if (consp span) (car span) span))

(defun range--span-end (span)
  "Return last number in SPAN."
  (if (consp span) (cdr span) span))

(defun range--insert-sorted-unique (number numbers)
  "Insert NUMBER into sorted NUMBERS unless already present."
  (cond
   ((null numbers) (list number))
   ((= number (car numbers)) numbers)
   ((< number (car numbers)) (cons number numbers))
   (t (cons (car numbers)
            (range--insert-sorted-unique number (cdr numbers))))))

(defun range--numbers (range)
  "Expand RANGE into a sorted list of unique numbers."
  (let (numbers)
    (dolist (span (range-normalize range))
      (cond
       ((numberp span)
        (setq numbers (range--insert-sorted-unique span numbers)))
       ((consp span)
        (let ((n (car span))
              (end (cdr span)))
          (while (<= n end)
            (setq numbers (range--insert-sorted-unique n numbers))
            (setq n (1+ n)))))))
    numbers))

(defun range--number-member-p (number numbers)
  "Return non-nil when NUMBER appears in sorted NUMBERS."
  (catch 'done
    (while numbers
      (cond
       ((= number (car numbers))
        (throw 'done t))
       ((< number (car numbers))
        (throw 'done nil)))
      (setq numbers (cdr numbers)))
    nil))

(defun range-compress-list (numbers)
  "Convert a sorted list of NUMBERS to a range list."
  (let ((numbers (copy-sequence numbers))
        result first last)
    (while numbers
      (let ((n (car numbers)))
        (cond
         ((null first)
          (setq first n
                last n))
         ((= n last)
          nil)
         ((= n (1+ last))
          (setq last n))
         (t
          (push (if (= first last) first (cons first last)) result)
          (setq first n
                last n))))
      (setq numbers (cdr numbers)))
    (when first
      (push (if (= first last) first (cons first last)) result))
    (nreverse result)))

(defun range-uncompress (ranges)
  "Expand RANGES into a list of numbers."
  (range--numbers ranges))

(defun range-concat (range1 range2)
  "Return the union of RANGE1 and RANGE2."
  (range-compress-list
   (let ((numbers (range--numbers range1)))
     (dolist (number (range--numbers range2) numbers)
       (setq numbers (range--insert-sorted-unique number numbers))))))

(defun range-add-list (ranges list)
  "Return RANGES with sorted number LIST added."
  (range-concat ranges (range-compress-list list)))

(defun range-difference (range1 range2)
  "Return the elements in RANGE1 that do not appear in RANGE2."
  (let ((remove (range--numbers range2))
        result)
    (dolist (number (range--numbers range1))
      (unless (range--number-member-p number remove)
        (push number result)))
    (range-compress-list (nreverse result))))

(defun range-remove (range1 range2)
  "Return RANGE1 with RANGE2 removed."
  (range-difference range1 range2))

(defun range-intersection (range1 range2)
  "Return the intersection of RANGE1 and RANGE2."
  (let ((right (range--numbers range2))
        result)
    (dolist (number (range--numbers range1))
      (when (range--number-member-p number right)
        (push number result)))
    (range-denormalize (range-compress-list (nreverse result)))))

(defun range-member-p (number ranges)
  "Return non-nil when NUMBER is in RANGES."
  (catch 'done
    (dolist (span (range-normalize ranges))
      (let ((start (range--span-start span))
            (end (range--span-end span)))
        (cond
         ((and (<= start number) (<= number end))
          (throw 'done t))
         ((> start number)
          (throw 'done nil)))))
    nil))

(defun range-list-intersection (list ranges)
  "Return numbers from sorted LIST that are members of RANGES."
  (let (result)
    (dolist (number list)
      (when (range-member-p number ranges)
        (push number result)))
    (nreverse result)))

(defun range-list-difference (list ranges)
  "Return numbers from sorted LIST that are not members of RANGES."
  (let (result)
    (dolist (number list)
      (unless (range-member-p number ranges)
        (push number result)))
    (nreverse result)))

(defun range-length (range)
  "Return the length RANGE would have if uncompressed."
  (length (range--numbers range)))

(defun range-map (func range)
  "Call FUNC once for each number represented by RANGE."
  (dolist (number (range--numbers range))
    (funcall func number)))

(provide 'range)

;;; range.el ends here
