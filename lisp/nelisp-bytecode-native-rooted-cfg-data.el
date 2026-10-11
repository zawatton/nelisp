;;; nelisp-bytecode-native-rooted-cfg-data.el --- Shared compiler data -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Compile-local hash consing.  Intern only completed, acyclic data; consumers
;; still see ordinary Lisp forms and serialization retains the tree grammar.
;; Integer identity buckets avoid the standalone table's mutable-key scans.

;;; Code:
(require 'cl-lib)

;; Snapshot contract data without introducing optimizing compiler dependencies.
(defun nelisp-bytecode-native-rooted-cfg-data-snapshot (value _depth)
  "Copy mutable VALUE iteratively, preserving sharing and opaque owners.
Visit and copy each container in one DFS, rejecting an active ancestor."
  (let ((copies (make-vector 4096 nil)) pending)
    ;; Fixed identity buckets avoid general hash-table work for every node.
    ;; Collisions still resolve through the original object's eq identity.
    (cl-labels
        ((identity-get (item)
           (cdr (assq item (aref copies (logand (sxhash-eq item) 4095)))))
         (identity-put (item value)
           (let* ((key (logand (sxhash-eq item) 4095)) (bucket (aref copies key)))
             (aset copies key (cons (cons item value) bucket)))
           value)
         (allocate (item)
           (cond
            ;; Most recipe and plan nodes are scalar metadata. Classify them
            ;; before inspecting callable/opaque containers.
            ((or (null item) (symbolp item) (numberp item)) item)
            ((or (hash-table-p item) (byte-code-function-p item) (functionp item)) item)
            ((stringp item)
             (or (identity-get item)
                 (let ((copy (substring item 0))) (identity-put item copy) copy)))
            ((or (consp item) (vectorp item))
             (let ((record (identity-get item)))
               (if record
                   (if (eq (car record) 'active) (error "Cyclic contract snapshot") (cdr record))
                 (let* ((copy (if (consp item) (cons nil nil) (make-vector (length item) nil)))
                        (record (cons 'active copy)))
                   (identity-put item record)
                   (push (vector item copy 0 record) pending)
                   copy))))
            (t (error "Unsupported contract snapshot value")))))
      (let ((root (allocate value)))
        (while pending
          (let* ((frame (car pending)) (old (aref frame 0)) (new (aref frame 1))
                 (index (aref frame 2)) (count (if (consp old) 2 (length old))))
            (if (= index count)
                (progn (setcar (aref frame 3) 'done) (pop pending))
              (aset frame 2 (1+ index))
              ;; ALLOCATE pushes a child frame, so it finishes before the
              ;; next sibling. An active record is therefore an ancestor.
              (let ((child (allocate (if (consp old) (if (= index 0) (car old) (cdr old)) (aref old index)))))
                (if (consp old)
                    (if (= index 0) (setcar new child) (setcdr new child))
                  (aset new index child))))))
        root))))

(defun nelisp-bytecode-native-rooted-cfg-data-context ()
  "Return a fresh compilation-owned interning context."
  (vector (make-vector 16384 nil) (make-vector 16384 nil)))

(defun nelisp-bytecode-native-rooted-cfg-data-share (value &optional context)
  "Copy acyclic VALUE, interning equal containers in compile-local CONTEXT.
Use only immutable compiler forms, not live constants or mutable runtime values.
No caller-owned object is modified.  Repeated identities are visited once.
Atoms retain their identities; cons and vector signatures compare canonical
children by identity, so hashing never walks a growing subtree."
  (let* ((context (or context (nelisp-bytecode-native-rooted-cfg-data-context)))
         (copies (aref context 0)) (interns (aref context 1)) pending)
    (cl-labels
        ((record (node)
           (cdr (assq node (aref copies (logand (sxhash-eq node) 16383)))))
         (remember (node state)
           (let ((key (logand (sxhash-eq node) 16383)))
             (aset copies key (cons (cons node state) (aref copies key))))
           state)
         (intern-node (node)
           (let* ((key (logand
                        (cond ((consp node)
                               (logxor (sxhash-eq (car node))
                                       (* 33 (sxhash-eq (cdr node)))))
                              ((stringp node) (sxhash-equal node))
                              (t (let ((hash (length node)))
                                   (dotimes (i (length node))
                                     (setq hash (logand (logxor (* hash 33)
                                                               (sxhash-eq (aref node i)))
                                                        #x1fffffff)))
                                   hash)))
                        16383))
                  (bucket (aref interns key)) found)
             (while (and bucket (not found))
               (let ((other (car bucket)))
                 (when (cond ((consp node)
                              (and (consp other) (eq (car node) (car other))
                                   (eq (cdr node) (cdr other))))
                             ((stringp node)
                              (and (stringp other) (equal node other)
                                   (or (not (fboundp 'equal-including-properties))
                                       (equal-including-properties node other))))
                             (t (and (vectorp other) (= (length node) (length other))
                                     (let ((i 0) (same t))
                                       (while (and same (< i (length node)))
                                         (setq same (eq (aref node i) (aref other i)) i (1+ i)))
                                       same))))
                   (setq found other)))
               (setq bucket (cdr bucket)))
             (or found (progn (aset interns key (cons node (aref interns key))) node))))
         (start (node)
           (cond ((stringp node)
                  (or (record node)
                      (remember node (cons 'done (intern-node (substring node 0))))))
                 ((or (consp node) (vectorp node))
                  (let ((known (record node)))
                    (if known
                        (if (eq (car known) 'active)
                            (error "Cyclic shared compiler data") known)
                      (let ((state (remember node (cons 'active nil))))
                        ;; Retain child records directly.  They finish before
                        ;; this frame, avoiding a second identity lookup.
                        (push (vector node 0 state
                                      (and (vectorp node) (make-vector (length node) nil))
                                      nil)
                              pending)
                        state))))
                 (t node))))
      (let ((root (start value)))
        (while pending
          (let* ((frame (car pending)) (node (aref frame 0)) (i (aref frame 1))
                 (count (if (consp node) 2 (length node))))
            (if (= i count)
                (let ((copy (if (consp node)
                                (cons (if (consp (aref frame 3)) (cdr (aref frame 3)) (aref frame 3))
                                      (if (consp (aref frame 4)) (cdr (aref frame 4)) (aref frame 4)))
                              (let ((copy (make-vector count nil)))
                                (dotimes (j count)
                                  (let ((child (aref (aref frame 3) j)))
                                    (aset copy j (if (consp child) (cdr child) child))))
                                copy))))
                  (setcdr (aref frame 2) (intern-node copy))
                  (setcar (aref frame 2) 'done) (pop pending))
              (aset frame 1 (1+ i))
              (let ((child (start (if (consp node) (if (= i 0) (car node) (cdr node)) (aref node i)))))
                (if (consp node) (aset frame (+ i 3) child)
                  (aset (aref frame 3) i child))))))
        (if (consp root) (cdr root) root)))))

(defvar nelisp--prn-call-cache nil)
(defvar print-circle nil)
(declare-function nelisp--prn-to-string "nelisp-stdlib-prn" (obj escape &optional depth memo))

(defun nelisp-bytecode-native-rooted-cfg-data-print (value)
  "Serialize VALUE completely, memoizing printed identities within this call.
The host printer retains its own grammar.  The standalone's ordinary printer
uses the same grammar and consumes the call-owned cache when unbounded.
No memo survives a mutation or a subsequent serialization.  Sharing never
introduces reader references or truncation into the authenticated tree."
  (let ((print-circle nil) (print-length nil) (print-level nil))
    (if (and (fboundp 'nelisp--prn-to-string)
             (not (subrp (symbol-function 'prin1-to-string))))
        (nelisp--prn-to-string value t nil (make-vector 4096 nil))
      (prin1-to-string value))))

(defun nelisp-bytecode-native-rooted-cfg-data-equal (left right)
  "Compare acyclic data LEFT and RIGHT, checking each identity pair once.
Memoization lasts only for this comparison.  It conveys no authority across
calls or mutations, and never skips a distinct right-hand occurrence."
  (let ((seen (make-vector 16384 nil)) (pending (list (cons left right))) (same t))
    (while (and same pending)
      (let* ((pair (pop pending)) (a (car pair)) (b (cdr pair)))
        (unless (eq a b)
          (cond
           ((or (and (consp a) (consp b))
                (and (vectorp a) (vectorp b) (= (length a) (length b))))
            (let* ((key (logand (logxor (sxhash-eq a) (* 33 (sxhash-eq b))) 16383))
                   (bucket (aref seen key)) found)
              (while (and bucket (not found))
                (when (and (eq a (caar bucket)) (eq b (cdar bucket))) (setq found t))
                (setq bucket (cdr bucket)))
              (unless found
                (aset seen key (cons pair (aref seen key)))
                (if (consp a)
                    (progn (push (cons (cdr a) (cdr b)) pending)
                           (push (cons (car a) (car b)) pending))
                  (dotimes (i (length a)) (push (cons (aref a i) (aref b i)) pending))))))
           (t (setq same (equal a b)))))))
    same))

(provide 'nelisp-bytecode-native-rooted-cfg-data)
;;; nelisp-bytecode-native-rooted-cfg-data.el ends here
