;;; nl-clj-vector.el --- Persistent bit-partitioned vector for nl-clj -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 §4.1: a persistent vector, the standard Bagwell/Clojure
;; array-mapped trie -- 5 bits per level, 32-wide branch nodes, a TAIL
;; array holding the last <=32 elements uncommitted to the trie so
;; `nl-clj-conj'/`nl-clj-peek' are O(1) amortized rather than always
;; O(log32 N).  Built entirely from plain `vector'/`aref'/`aset'/
;; `make-vector' -- Doc 195 §2.1 measured this the one data type in
;; this codebase's own type-reporting story that is fully trustworthy
;; (`type-of' answers `vector', not `cons', unlike `record'/
;; `cl-defstruct').
;;
;; Representation (Doc 195 §3.2, `nl-clj-core.el'):
;;
;;   [nl-clj--pvec COUNT SHIFT ROOT TAIL]
;;
;; ROOT is either a 32-element (always exactly 32, never sparse --
;; unlike the HAMT in nl-clj-hash.el, a vector's branch factor is
;; dense/position-indexed, not bitmap-packed) plain vector of children,
;; or, at the bottom, a leaf array holding actual elements.  TAIL is a
;; plain vector, length 0..32, holding the last committed-to-vector-
;; but-not-yet-committed-to-trie elements.  SHIFT is the bit shift
;; needed to index at ROOT's own level (a multiple of 5); an empty
;; vector's ROOT is a fresh all-nil 32-vector at SHIFT=5, matching real
;; Clojure's own EMPTY_NODE convention (never Elisp `nil' itself, so
;; the trie-walk code never needs a nil-root special case).
;;
;; `assoc' (index update) and `get'/`nth' walk SHIFT bits at a time; an
;; update copies exactly SHIFT/5 + 1 nodes on the path from root to the
;; changed leaf -- every other subtree is shared by reference,
;; unchanged, with the pre-update vector (real structural sharing, not
;; a copy-on-write illusion; verified in
;; `packages/nl-clj/test/nl-clj-vector-test.el' by `eq'-checking the
;; untouched subtree, not merely checking the new vector's values).
;;
;; Deliberate divergence from real Clojure, named per Doc 195 §4.1:
;; `nl-clj-subvec' here is an eager O(k) rebuild (walks and re-conjes
;; the requested range into a fresh vector), not Clojure's O(1)
;; offset/end-aware view-node type -- Doc 195 §4.1 explicitly defers
;; that optimization as "real, separable new work," and this package's
;; task brief asks for `subvec' in the public API, so it ships
;; correctness-first rather than not at all.

;;; Code:

(require 'nl-clj-core)

(defconst nl-clj-vector--bits 5)
(defconst nl-clj-vector--width 32)
(defconst nl-clj-vector--mask 31)

;;;; Construction --------------------------------------------------------

(defun nl-clj-vector--empty ()
  "Return a fresh empty persistent vector."
  (vector nl-clj--pvec-tag 0 nl-clj-vector--bits (make-vector 32 nil) (vector)))

(defun nl-clj-vector-p (object)
  "Return non-nil when OBJECT is an nl-clj persistent vector."
  (nl-clj--tagged-p object nl-clj--pvec-tag))

(defun nl-clj-vector (&rest items)
  "Return a new persistent vector containing ITEMS, in order."
  (let ((v (nl-clj-vector--empty)))
    (dolist (it items) (setq v (nl-clj-vector--conj v it)))
    v))

;;;; Internal trie mechanics ----------------------------------------------

(defun nl-clj-vector--tailoff (count)
  "Logical index of the first element held in the TAIL, given COUNT."
  (if (< count 32)
      0
    (ash (ash (1- count) -5) 5)))

(defun nl-clj-vector--array-for (v i)
  "Return the leaf array (plain vector) holding logical index I of V."
  (let ((count (aref v 1)) (shift (aref v 2)) (root (aref v 3)) (tail (aref v 4)))
    (if (>= i (nl-clj-vector--tailoff count))
        tail
      (let ((node root) (level shift))
        (while (> level 0)
          (setq node (aref node (logand (ash i (- level)) nl-clj-vector--mask)))
          (setq level (- level nl-clj-vector--bits)))
        node))))

(defun nl-clj-vector--nth (v i)
  "Return the element of V at logical index I.  I must already be in range."
  (aref (nl-clj-vector--array-for v i) (logand i nl-clj-vector--mask)))

(defun nl-clj-vector--new-path (level node)
  "Wrap NODE in LEVEL/5 nil-else-NODE branch levels above it."
  (if (= level 0)
      node
    (let ((ret (make-vector 32 nil)))
      (aset ret 0 (nl-clj-vector--new-path (- level nl-clj-vector--bits) node))
      ret)))

(defun nl-clj-vector--push-tail (count level parent tail-array)
  "Path-copy PARENT, inserting TAIL-ARRAY at the slot COUNT (pre-conj,
matching real Clojure's own `cnt' field read before the append)
addresses at LEVEL."
  (let* ((subidx (logand (ash (1- count) (- level)) nl-clj-vector--mask))
         (ret (copy-sequence parent)))
    (if (= level nl-clj-vector--bits)
        (aset ret subidx tail-array)
      (let ((child (aref parent subidx)))
        (aset ret subidx
              (if child
                  (nl-clj-vector--push-tail count (- level nl-clj-vector--bits) child tail-array)
                (nl-clj-vector--new-path (- level nl-clj-vector--bits) tail-array)))))
    ret))

(defun nl-clj-vector--conj (v val)
  "Return a new persistent vector: V with VAL appended."
  (let* ((count (aref v 1)) (shift (aref v 2)) (root (aref v 3)) (tail (aref v 4)))
    (if (< (- count (nl-clj-vector--tailoff count)) 32)
        ;; Room in the tail: no trie walk at all.
        (vector nl-clj--pvec-tag (1+ count) shift root (vconcat tail (vector val)))
      ;; Tail is full (32 elements): push it into the trie as one leaf.
      (let (new-root new-shift)
        (if (> (ash count -5) (ash 1 shift))
            ;; Root is full at its own height: grow the tree by one level.
            (let ((nr (make-vector 32 nil)))
              (aset nr 0 root)
              (aset nr 1 (nl-clj-vector--new-path shift tail))
              (setq new-root nr new-shift (+ shift nl-clj-vector--bits)))
          (setq new-root (nl-clj-vector--push-tail count shift root tail)
                new-shift shift))
        (vector nl-clj--pvec-tag (1+ count) new-shift new-root (vector val))))))

(defun nl-clj-vector--do-assoc (level node i val)
  "Path-copy NODE, replacing the element at logical index I with VAL."
  (let ((ret (copy-sequence node)))
    (if (= level 0)
        (aset ret (logand i nl-clj-vector--mask) val)
      (let ((subidx (logand (ash i (- level)) nl-clj-vector--mask)))
        (aset ret subidx
              (nl-clj-vector--do-assoc (- level nl-clj-vector--bits) (aref node subidx) i val))))
    ret))

(defun nl-clj-vector--assoc-n (v i val)
  "Return a new persistent vector: V with index I set to VAL.
I == (nl-clj-count v) is legal and extends the vector by one element
(matching Clojure's own `assoc' contract); any other out-of-range I
signals `nl-clj-index-error'."
  (let ((count (aref v 1)) (shift (aref v 2)) (root (aref v 3)) (tail (aref v 4)))
    (cond
     ((and (>= i 0) (< i count))
      (if (>= i (nl-clj-vector--tailoff count))
          (let ((new-tail (copy-sequence tail)))
            (aset new-tail (logand i nl-clj-vector--mask) val)
            (vector nl-clj--pvec-tag count shift root new-tail))
        (vector nl-clj--pvec-tag count shift
                (nl-clj-vector--do-assoc shift root i val) tail)))
     ((= i count) (nl-clj-vector--conj v val))
     (t (signal 'nl-clj-index-error (list 'nl-clj-assoc i count))))))

(defun nl-clj-vector--pop-tail (count level node)
  "Path-copy NODE, dropping its last leaf (COUNT is V's count before pop).
Returns nil when NODE becomes entirely empty at this level."
  (let ((subidx (logand (ash (- count 2) (- level)) nl-clj-vector--mask)))
    (cond
     ((> level nl-clj-vector--bits)
      (let ((new-child (nl-clj-vector--pop-tail count (- level nl-clj-vector--bits)
                                                 (aref node subidx))))
        (if (and (null new-child) (= subidx 0))
            nil
          (let ((ret (copy-sequence node)))
            (aset ret subidx new-child)
            ret))))
     ((= subidx 0) nil)
     (t (let ((ret (copy-sequence node)))
          (aset ret subidx nil)
          ret)))))

(defun nl-clj-vector--pop (v)
  "Return a new persistent vector: V with its last element removed.
Signals `nl-clj-index-error' on an empty vector, matching Clojure's
own `pop' contract (an error, not a silent no-op)."
  (let ((count (aref v 1)) (shift (aref v 2)) (root (aref v 3)) (tail (aref v 4)))
    (cond
     ((= count 0) (signal 'nl-clj-index-error (list 'nl-clj-pop "empty vector")))
     ((= count 1) (nl-clj-vector--empty))
     ((> (- count (nl-clj-vector--tailoff count)) 1)
      (vector nl-clj--pvec-tag (1- count) shift root
              (substring tail 0 (1- (length tail)))))
     (t
      (let* ((new-tail (nl-clj-vector--array-for v (- count 2)))
             (new-root (nl-clj-vector--pop-tail count shift root))
             (new-shift shift))
        (unless new-root (setq new-root (make-vector 32 nil)))
        (when (and (> shift nl-clj-vector--bits) (null (aref new-root 1)))
          (setq new-root (aref new-root 0) new-shift (- shift nl-clj-vector--bits)))
        (vector nl-clj--pvec-tag (1- count) new-shift new-root new-tail))))))

(defun nl-clj-vector--to-list (v)
  "Materialize V's elements, in order, as a plain Elisp list."
  (let ((count (aref v 1)) (i 0) acc)
    (while (< i count)
      (push (nl-clj-vector--nth v i) acc)
      (setq i (1+ i)))
    (nreverse acc)))

;;;; Internal accessors used by tests (structural-sharing proof) ---------

(defun nl-clj-vector--root (v)
  "Return V's root trie node -- exposed so callers can `eq'-check
structural sharing between two vectors after an operation (Doc 195
§4.1's own against-the-bug shape)."
  (aref v 3))

(defun nl-clj-vector--tail (v)
  "Return V's tail array."
  (aref v 4))

(provide 'nl-clj-vector)

;;; nl-clj-vector.el ends here
