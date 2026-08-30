;;; nl-clj-hash.el --- Persistent HAMT map/set for nl-clj -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 §4.2: a persistent hash-array-mapped trie (HAMT) map, and a
;; set built on the identical engine (a map whose values are ignored).
;; Leaf key hashing uses `sxhash-equal' -- Doc 195 §2.3 measured it
;; `fboundp' and stable across two structurally-equal-but-`eq'-distinct
;; compound values on `target/nelisp', exactly the property a HAMT
;; bucket function needs (`secure-hash' is absent there, but that is
;; irrelevant: a HAMT needs a fast well-distributed hash, not a
;; cryptographic digest -- the same choice Clojure itself makes).
;;
;; This file deliberately does NOT use the standard-library `logcount'
;; function for the bitmap popcount-prefix trick Doc 195 §4.2 itself
;; describes -- a repo-wide grep (`grep -rl logcount --include=*.el .')
;; found zero hits anywhere in this tree, meaning nothing has ever
;; exercised it on `target/nelisp', and this package's own DoD-shaped
;; risk (per its brief) is specifically proving it survives the
;; standalone.  `nl-clj-hash--popcount' below is a manual bit loop
;; using only `logand'/`ash', both directly confirmed present in
;; `scripts/nelisp-standalone-build.el''s dispatch table -- "measured,
;; not assumed" applied to a primitive choice, not just a fact claim.
;;
;; Representation (Doc 195 §3.2, §4.2, refined per `nl-clj-core.el's
;; Commentary for the map/set tag split):
;;
;;   map: [nl-clj--pmap COUNT ROOT]   set: [nl-clj--pset COUNT ROOT]
;;
;; ROOT is nil for an empty map/set, or a *branch node* -- a cons
;; (BITMAP . PACKED-ARRAY): BITMAP is a 32-bit-ish integer, bit N set
;; iff hash-slice N is populated at this level; PACKED-ARRAY holds
;; only the populated slots, indexed by the standard popcount-prefix
;; trick (Doc 195 §4.2: `(logcount (logand bitmap (1- (ash 1 idx))))',
;; here `nl-clj-hash--popcount' in place of `logcount').  Memory cost
;; stays proportional to actual size, unlike a dense 32-wide array.
;;
;; A PACKED-ARRAY slot holds one of three untagged-vs-tagged shapes:
;;   - a leaf:      [nl-clj--leaf KEY VAL]           (a lone key/value)
;;   - a collision: [nl-clj--collision HASH ALIST]   (two+ keys whose
;;                  hash agrees in every 5-bit slice all the way to
;;                  `nl-clj-hash--max-shift' -- Doc 195 §4.2's named
;;                  "collision handling" requirement)
;;   - a branch node (BITMAP . PACKED-ARRAY), recursing one level down
;; A branch node's own cons cell is never confused with a leaf/
;; collision vector -- `consp' vs `vectorp' dispatches unambiguously.
;;
;; Fidelity (Doc 195 §4.2): real structural sharing on assoc/dissoc,
;; matching Clojure's HAMT complexity shape.  Divergence, named: no
;; small-map array-map special case (Clojure's own PersistentHashMap
;; flattens <=8-entry maps for cache-friendliness before promoting to
;; a true HAMT) -- always-HAMT here, correct but not tuned for small
;; maps; Doc 195 §8 leaves this an open question pending real usage
;; data, not designed further here.

;;; Code:

(require 'nl-clj-core)

(defconst nl-clj-hash--bits 5)
(defconst nl-clj-hash--width 32)
(defconst nl-clj-hash--mask 31)
(defconst nl-clj-hash--max-shift 60
  "Shift at which further descent stops adding information and any
further colliding keys fall back to one flat, `equal'-scanned bucket.
`sxhash-equal' values are fixnums, and `most-positive-fixnum' on this
tree's own standalone/host builds is 2^61-1 (61 significant bits,
confirmed at `scripts/nelisp-stdlib-prelude.el:9044'); 60 (12 levels
of 5 bits) covers all but the top bit.  Not a correctness boundary --
a bucket beyond this depth still resolves keys by `equal', never by
address -- only a (never-observed-in-practice) performance one.")

;;;; Leaf / collision / branch node helpers -------------------------------

(defvar nl-clj-hash--hash-fn #'sxhash-equal
  "The function `nl-clj-hash--hash-key' calls.  A single indirection
point so `packages/nl-clj/test/nl-clj-hash-test.el' can `let'-bind it
to a function that forces two specific keys to collide, driving the
collision-bucket path deterministically end to end (both the initial
insert AND every later re-derivation of an already-stored key's hash
during a merge/split) -- Doc 195 §4.2's own recommended technique
(\"buildable deterministically by hash-value construction, not left
to chance\"), since real `sxhash-equal' offers no practical way to
manufacture two colliding fixnums by hand.  Production code never
rebinds this; it is always plain `sxhash-equal'.")

(defun nl-clj-hash--hash-key (key)
  "Return the leaf hash of KEY.  Deliberately plain `sxhash-equal' (via
`nl-clj-hash--hash-fn'), not `nl-clj-hash' (nl-clj-seq.el) -- Doc 195
§3.2's named trap: a HAMT's own *leaf* keys/values are ordinary Elisp
values, where structural `sxhash-equal' already is the right notion;
it must never be used to hash two nl-clj *collections* against each
other."
  (funcall nl-clj-hash--hash-fn key))

(defun nl-clj-hash--mk-leaf (k v) (vector 'nl-clj--leaf k v))
(defun nl-clj-hash--leaf-p (x) (nl-clj--tagged-p x 'nl-clj--leaf))
(defun nl-clj-hash--leaf-key (x) (aref x 1))
(defun nl-clj-hash--leaf-val (x) (aref x 2))

(defun nl-clj-hash--mk-collision (hash alist) (vector 'nl-clj--collision hash alist))
(defun nl-clj-hash--collision-p (x) (nl-clj--tagged-p x 'nl-clj--collision))
(defun nl-clj-hash--collision-hash (x) (aref x 1))
(defun nl-clj-hash--collision-alist (x) (aref x 2))

(defun nl-clj-hash--mk-branch (bitmap array) (cons bitmap array))
(defun nl-clj-hash--branch-bitmap (x) (car x))
(defun nl-clj-hash--branch-array (x) (cdr x))

(defun nl-clj-hash--popcount (n)
  "Population count of non-negative integer N.  See this file's
Commentary for why this does not call the standard-library `logcount'."
  (let ((c 0))
    (while (> n 0)
      (setq c (+ c (logand n 1)))
      (setq n (ash n -1)))
    c))

(defun nl-clj-hash--bit-pos (hash shift)
  "5-bit slice of HASH at SHIFT, in [0, 31]."
  (logand (ash hash (- shift)) nl-clj-hash--mask))

(defun nl-clj-hash--popcount-below (bitmap bit-idx)
  "Number of populated slots below BIT-IDX in BITMAP -- PACKED-ARRAY offset."
  (nl-clj-hash--popcount (logand bitmap (1- (ash 1 bit-idx)))))

(defun nl-clj-hash--array-insert (array idx item)
  "Return a new array: ARRAY with ITEM inserted before position IDX."
  (vconcat (substring array 0 idx) (vector item) (substring array idx)))

(defun nl-clj-hash--array-remove (array idx)
  "Return a new array: ARRAY with the element at IDX removed."
  (vconcat (substring array 0 idx) (substring array (1+ idx))))

(defun nl-clj-hash--alist-remove (alist key)
  "Return ALIST with the pair whose car is `equal' to KEY removed."
  (let (acc)
    (dolist (pair alist) (unless (equal (car pair) key) (push pair acc)))
    (nreverse acc)))

;;;; merge-two: combine two single-key leaves into a fresh branch chain --

(defun nl-clj-hash--merge-two (shift k1 h1 leaf1 k2 h2 leaf2)
  "Merge two distinct single-key nodes LEAF1 (key K1, hash H1) and
LEAF2 (key K2, hash H2) into a branch node (or, if their hash slices
keep agreeing, a deeper chain of one-child branches, or finally a
collision bucket at `nl-clj-hash--max-shift') rooted at SHIFT."
  (if (>= shift nl-clj-hash--max-shift)
      (nl-clj-hash--mk-collision h1 (list (cons k2 (nl-clj-hash--leaf-val leaf2))
                                           (cons k1 (nl-clj-hash--leaf-val leaf1))))
    (let ((b1 (nl-clj-hash--bit-pos h1 shift))
          (b2 (nl-clj-hash--bit-pos h2 shift)))
      (if (= b1 b2)
          (nl-clj-hash--mk-branch
           (ash 1 b1)
           (vector (nl-clj-hash--merge-two (+ shift nl-clj-hash--bits) k1 h1 leaf1 k2 h2 leaf2)))
        (let ((arr (make-vector 2 nil)))
          (if (< b1 b2)
              (progn (aset arr 0 leaf1) (aset arr 1 leaf2))
            (progn (aset arr 0 leaf2) (aset arr 1 leaf1)))
          (nl-clj-hash--mk-branch (logior (ash 1 b1) (ash 1 b2)) arr))))))

;;;; Node-level assoc / get / dissoc ---------------------------------------
;;
;; These take an explicit HASH parameter rather than recomputing it
;; from KEY, exactly so tests can drive the collision path
;; deterministically (Doc 195 §4.2's own against-the-bug shape:
;; "construct two keys whose hash values agree... buildable
;; deterministically by hash-value construction, not left to chance").

(defun nl-clj-hash--node-assoc (node shift hash key val)
  "Return (NEW-NODE . ADDED), ADDED non-nil iff KEY was not already present."
  (cond
   ((null node) (cons (nl-clj-hash--mk-leaf key val) t))
   ((nl-clj-hash--leaf-p node)
    (let ((lk (nl-clj-hash--leaf-key node)) (lv (nl-clj-hash--leaf-val node)))
      (cond
       ((equal lk key) (cons (nl-clj-hash--mk-leaf key val) nil))
       ((>= shift nl-clj-hash--max-shift)
        (cons (nl-clj-hash--mk-collision hash (list (cons key val) (cons lk lv))) t))
       (t (cons (nl-clj-hash--merge-two shift
                                         lk (nl-clj-hash--hash-key lk) (nl-clj-hash--mk-leaf lk lv)
                                         key hash (nl-clj-hash--mk-leaf key val))
                t)))))
   ((nl-clj-hash--collision-p node)
    (let* ((alist (nl-clj-hash--collision-alist node))
           (existing (assoc key alist)))
      (if existing
          (cons (nl-clj-hash--mk-collision
                 (nl-clj-hash--collision-hash node)
                 (cons (cons key val) (nl-clj-hash--alist-remove alist key)))
                nil)
        (cons (nl-clj-hash--mk-collision (nl-clj-hash--collision-hash node)
                                          (cons (cons key val) alist))
              t))))
   (t ;; branch node
    (let* ((bitmap (nl-clj-hash--branch-bitmap node))
           (array (nl-clj-hash--branch-array node))
           (bit (nl-clj-hash--bit-pos hash shift))
           (bitval (ash 1 bit))
           (idx (nl-clj-hash--popcount-below bitmap bit)))
      (if (zerop (logand bitmap bitval))
          (cons (nl-clj-hash--mk-branch (logior bitmap bitval)
                                         (nl-clj-hash--array-insert array idx (nl-clj-hash--mk-leaf key val)))
                t)
        (let* ((child (aref array idx))
               (result (nl-clj-hash--node-assoc child (+ shift nl-clj-hash--bits) hash key val))
               (new-array (copy-sequence array)))
          (aset new-array idx (car result))
          (cons (nl-clj-hash--mk-branch bitmap new-array) (cdr result))))))))

(defun nl-clj-hash--node-get (node shift hash key not-found)
  "Return the value stored for KEY under NODE, or NOT-FOUND."
  (cond
   ((null node) not-found)
   ((nl-clj-hash--leaf-p node)
    (if (equal (nl-clj-hash--leaf-key node) key) (nl-clj-hash--leaf-val node) not-found))
   ((nl-clj-hash--collision-p node)
    (let ((entry (assoc key (nl-clj-hash--collision-alist node))))
      (if entry (cdr entry) not-found)))
   (t (let* ((bitmap (nl-clj-hash--branch-bitmap node))
              (bit (nl-clj-hash--bit-pos hash shift))
              (bitval (ash 1 bit)))
        (if (zerop (logand bitmap bitval))
            not-found
          (nl-clj-hash--node-get (aref (nl-clj-hash--branch-array node)
                                        (nl-clj-hash--popcount-below bitmap bit))
                                  (+ shift nl-clj-hash--bits) hash key not-found))))))

(defun nl-clj-hash--node-dissoc (node shift hash key)
  "Return (NEW-NODE . REMOVED).  NEW-NODE is nil when NODE becomes empty.
When KEY is absent, returns (NODE . nil) -- NODE itself, `eq'-identical,
unchanged (Doc 195 §4.2's named `dissoc'-of-absent-key no-op guarantee)."
  (cond
   ((null node) (cons nil nil))
   ((nl-clj-hash--leaf-p node)
    (if (equal (nl-clj-hash--leaf-key node) key) (cons nil t) (cons node nil)))
   ((nl-clj-hash--collision-p node)
    (let* ((alist (nl-clj-hash--collision-alist node)))
      (if (not (assoc key alist))
          (cons node nil)
        (let ((new-alist (nl-clj-hash--alist-remove alist key)))
          (cond
           ((null new-alist) (cons nil t))
           ((null (cdr new-alist))
            (cons (nl-clj-hash--mk-leaf (caar new-alist) (cdar new-alist)) t))
           (t (cons (nl-clj-hash--mk-collision (nl-clj-hash--collision-hash node) new-alist) t)))))))
   (t
    (let* ((bitmap (nl-clj-hash--branch-bitmap node))
           (array (nl-clj-hash--branch-array node))
           (bit (nl-clj-hash--bit-pos hash shift))
           (bitval (ash 1 bit)))
      (if (zerop (logand bitmap bitval))
          (cons node nil)
        (let* ((idx (nl-clj-hash--popcount-below bitmap bit))
               (child (aref array idx))
               (result (nl-clj-hash--node-dissoc child (+ shift nl-clj-hash--bits) hash key))
               (new-child (car result)))
          (if (not (cdr result))
              (cons node nil)
            (if (null new-child)
                (if (= (length array) 1)
                    (cons nil t)
                  (cons (nl-clj-hash--mk-branch (logand bitmap (lognot bitval))
                                                 (nl-clj-hash--array-remove array idx))
                        t))
              (let ((new-array (copy-sequence array)))
                (aset new-array idx new-child)
                (cons (nl-clj-hash--mk-branch bitmap new-array) t))))))))))

(defun nl-clj-hash--node-entries (node)
  "Return every (KEY . VAL) pair under NODE, as a plain Elisp list.
Eager for this phase (Doc 195 §4.7 defers a lazy walk)."
  (cond
   ((null node) nil)
   ((nl-clj-hash--leaf-p node)
    (list (cons (nl-clj-hash--leaf-key node) (nl-clj-hash--leaf-val node))))
   ((nl-clj-hash--collision-p node) (copy-sequence (nl-clj-hash--collision-alist node)))
   (t (let ((acc nil) (arr (nl-clj-hash--branch-array node)) (i 0))
        (while (< i (length arr))
          (setq acc (nconc acc (nl-clj-hash--node-entries (aref arr i))))
          (setq i (1+ i)))
        acc))))

;;;; Map public surface -----------------------------------------------------

(defun nl-clj-map-p (object)
  "Return non-nil when OBJECT is an nl-clj persistent map."
  (nl-clj--tagged-p object nl-clj--pmap-tag))

(defun nl-clj-hash--map-assoc (m key val)
  (let* ((hash (nl-clj-hash--hash-key key))
         (result (nl-clj-hash--node-assoc (aref m 2) 0 hash key val)))
    (vector nl-clj--pmap-tag (+ (aref m 1) (if (cdr result) 1 0)) (car result))))

(defun nl-clj-hash--map-get (m key not-found)
  (nl-clj-hash--node-get (aref m 2) 0 (nl-clj-hash--hash-key key) key not-found))

(defun nl-clj-hash--map-dissoc (m key)
  (let* ((hash (nl-clj-hash--hash-key key))
         (result (nl-clj-hash--node-dissoc (aref m 2) 0 hash key)))
    (if (not (cdr result))
        m ;; no-op: return the SAME object, `eq'-identical (Doc 195 §4.2)
      (vector nl-clj--pmap-tag (1- (aref m 1)) (car result)))))

(defun nl-clj-hash--map-entries (m)
  "Return M's entries as a plain Elisp list of (KEY . VAL) conses."
  (nl-clj-hash--node-entries (aref m 2)))

(defun nl-clj-hash-map (&rest kvs)
  "Return a new persistent map from KVS, alternating KEY VAL KEY VAL ..."
  (when (= 1 (mod (length kvs) 2))
    (signal 'nl-clj-error (list 'nl-clj-hash-map "odd number of arguments" kvs)))
  (let ((m (vector nl-clj--pmap-tag 0 nil)))
    (while kvs
      (setq m (nl-clj-hash--map-assoc m (car kvs) (cadr kvs)))
      (setq kvs (cddr kvs)))
    m))

(defun nl-clj-keys (m)
  "Return M's keys as a plain Elisp list."
  (unless (nl-clj-map-p m) (signal 'nl-clj-type-error (list 'nl-clj-keys m)))
  (mapcar #'car (nl-clj-hash--map-entries m)))

(defun nl-clj-vals (m)
  "Return M's values as a plain Elisp list."
  (unless (nl-clj-map-p m) (signal 'nl-clj-type-error (list 'nl-clj-vals m)))
  (mapcar #'cdr (nl-clj-hash--map-entries m)))

;;;; Set public surface (Doc 195 §4.2: a map whose values are ignored) ----

(defun nl-clj-set-p (object)
  "Return non-nil when OBJECT is an nl-clj persistent set."
  (nl-clj--tagged-p object nl-clj--pset-tag))

(defun nl-clj-hash--set-conj (s item)
  (let* ((hash (nl-clj-hash--hash-key item))
         (result (nl-clj-hash--node-assoc (aref s 2) 0 hash item nl-clj--set-member)))
    (vector nl-clj--pset-tag (+ (aref s 1) (if (cdr result) 1 0)) (car result))))

(defun nl-clj-hash--set-disj (s item)
  (let* ((hash (nl-clj-hash--hash-key item))
         (result (nl-clj-hash--node-dissoc (aref s 2) 0 hash item)))
    (if (not (cdr result))
        s ;; no-op: `eq'-identical, same guarantee as map dissoc
      (vector nl-clj--pset-tag (1- (aref s 1)) (car result)))))

(defun nl-clj-hash--set-elements (s)
  "Return S's elements as a plain Elisp list."
  (mapcar #'car (nl-clj-hash--node-entries (aref s 2))))

(defun nl-clj-hash-set (&rest items)
  "Return a new persistent set containing ITEMS (duplicates collapse)."
  (let ((s (vector nl-clj--pset-tag 0 nil)))
    (dolist (it items) (setq s (nl-clj-hash--set-conj s it)))
    s))

(provide 'nl-clj-hash)

;;; nl-clj-hash.el ends here
