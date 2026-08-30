;;; nl-clj-hash-test.el --- Tests for nl-clj-hash.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 §4.2's own DoD-shaped verification: no host-Emacs control
;; (plain Emacs has no HAMT), so every assertion below is checked
;; against Clojure's own documented `assoc'/`dissoc'/`get' contract by
;; hand.  Three against-the-bug shapes Doc 195 §4.2 names specifically:
;; (1) the collision-handling path -- two keys forced, deterministically
;; (not left to chance), to agree in every 5-bit hash slice down to
;; `nl-clj-hash--max-shift', each independently retrievable and
;; dissoc-able without disturbing the other; (2) structural sharing,
;; same shape as the vector's; (3) `dissoc' of a nonexistent key is a
;; no-op returning an `eq'-identical map (a real Clojure guarantee, and
;; a performance assertion in disguise: a correct HAMT has nothing to
;; copy on that path).

;;; Code:

(require 'ert)
(require 'nl-clj-hash)

;;;; Map: construction, get, count -------------------------------------

(ert-deftest nl-clj-hash-test-map-empty ()
  (let ((m (vector 'nl-clj--pmap 0 nil)))
    (should (nl-clj-map-p m))
    (should (= (aref m 1) 0))))

(ert-deftest nl-clj-hash-test-map-predicate-rejects-non-maps ()
  (should-not (nl-clj-map-p [1 2 3]))
  (should-not (nl-clj-map-p nil))
  (should-not (nl-clj-map-p (vector 'nl-clj--pset 0 nil))))

(ert-deftest nl-clj-hash-test-hash-map-constructor ()
  (let ((m (nl-clj-hash-map :a 1 :b 2)))
    (should (= (aref m 1) 2))
    (should (= (nl-clj-hash--map-get m :a :nf) 1))
    (should (= (nl-clj-hash--map-get m :b :nf) 2))))

(ert-deftest nl-clj-hash-test-hash-map-odd-args-signals ()
  (should-error (nl-clj-hash-map :a 1 :b) :type 'nl-clj-error))

(ert-deftest nl-clj-hash-test-map-800-entry-build-and-get ()
  (let ((m (vector 'nl-clj--pmap 0 nil)))
    (dotimes (i 800) (setq m (nl-clj-hash--map-assoc m i (* i 10))))
    (should (= (aref m 1) 800))
    (dotimes (i 800)
      (should (= (nl-clj-hash--map-get m i :nf) (* i 10))))
    (should (eq (nl-clj-hash--map-get m 999999 :nf) :nf))))

;;;; Map: immutability / structural sharing ---------------------------

(ert-deftest nl-clj-hash-test-map-assoc-does-not-mutate-original ()
  (let* ((m (vector 'nl-clj--pmap 0 nil)))
    (dotimes (i 800) (setq m (nl-clj-hash--map-assoc m i (* i 10))))
    (let ((_m2 (nl-clj-hash--map-assoc m 500 :changed)))
      (should (= (nl-clj-hash--map-get m 500 :nf) 5000)))))

(ert-deftest nl-clj-hash-test-map-assoc-existing-key-updates-value-not-count ()
  (let ((m (nl-clj-hash-map :a 1)))
    (let ((m2 (nl-clj-hash--map-assoc m :a 2)))
      (should (= (aref m2 1) 1))
      (should (= (nl-clj-hash--map-get m2 :a :nf) 2)))))

;;;; Map: dissoc -----------------------------------------------------------

(ert-deftest nl-clj-hash-test-map-dissoc-absent-key-is-eq-identical ()
  (let* ((m (vector 'nl-clj--pmap 0 nil)))
    (dotimes (i 800) (setq m (nl-clj-hash--map-assoc m i (* i 10))))
    (should (eq (nl-clj-hash--map-dissoc m 999999) m))))

(ert-deftest nl-clj-hash-test-map-dissoc-present-key ()
  (let* ((m (vector 'nl-clj--pmap 0 nil)))
    (dotimes (i 800) (setq m (nl-clj-hash--map-assoc m i (* i 10))))
    (let ((m2 (nl-clj-hash--map-dissoc m 500)))
      (should (= (aref m2 1) 799))
      (should (eq (nl-clj-hash--map-get m2 500 :nf) :nf))
      ;; unrelated key undisturbed
      (should (= (nl-clj-hash--map-get m2 501 :nf) 5010))
      ;; original untouched
      (should (= (nl-clj-hash--map-get m 500 :nf) 5000)))))

;;;; Map: keys / vals -------------------------------------------------------

(ert-deftest nl-clj-hash-test-keys-vals ()
  (let ((m (nl-clj-hash-map :a 1 :b 2 :c 3)))
    (should (= (length (nl-clj-keys m)) 3))
    (should (= (length (nl-clj-vals m)) 3))
    (should (equal (sort (mapcar #'symbol-name (nl-clj-keys m)) #'string<)
                   '(":a" ":b" ":c")))
    (should (equal (sort (nl-clj-vals m) #'<) '(1 2 3)))))

(ert-deftest nl-clj-hash-test-keys-signals-on-non-map ()
  (should-error (nl-clj-keys [1 2]) :type 'nl-clj-type-error))

;;;; Forced collision path (Doc 195 §4.2's own recommended technique) -----

(ert-deftest nl-clj-hash-test-forced-collision-two-keys ()
  (let* ((nl-clj-hash--hash-fn
          (lambda (k) (if (member k '("keyA" "keyB")) 424242 (sxhash-equal k))))
         (m (nl-clj-hash-map "keyA" 1 "keyB" 2)))
    (should (= (nl-clj-hash--map-get m "keyA" :nf) 1))
    (should (= (nl-clj-hash--map-get m "keyB" :nf) 2))
    (should (= (aref m 1) 2))))

(ert-deftest nl-clj-hash-test-forced-collision-independent-dissoc ()
  (let* ((nl-clj-hash--hash-fn
          (lambda (k) (if (member k '("keyA" "keyB")) 424242 (sxhash-equal k))))
         (m (nl-clj-hash-map "keyA" 1 "keyB" 2))
         (m2 (nl-clj-hash--map-dissoc m "keyA")))
    (should (eq (nl-clj-hash--map-get m2 "keyA" :nf) :nf))
    (should (= (nl-clj-hash--map-get m2 "keyB" :nf) 2))
    (should (= (aref m2 1) 1))
    ;; original untouched
    (should (= (nl-clj-hash--map-get m "keyA" :nf) 1))))

(ert-deftest nl-clj-hash-test-forced-collision-node-exists-in-tree ()
  (let* ((nl-clj-hash--hash-fn
          (lambda (k) (if (member k '("keyA" "keyB")) 424242 (sxhash-equal k))))
         (m (nl-clj-hash-map "keyA" 1 "keyB" 2)))
    (should (nl-clj-hash-test--find-collision (aref m 2)))))

(defun nl-clj-hash-test--find-collision (node)
  "Walk NODE and return the first collision bucket found, or nil."
  (cond
   ((null node) nil)
   ((nl-clj-hash--collision-p node) node)
   ((nl-clj-hash--leaf-p node) nil)
   (t (let ((arr (nl-clj-hash--branch-array node)) (i 0) found)
        (while (and (< i (length arr)) (not found))
          (setq found (nl-clj-hash-test--find-collision (aref arr i)))
          (setq i (1+ i)))
        found))))

(ert-deftest nl-clj-hash-test-forced-collision-three-keys ()
  (let* ((nl-clj-hash--hash-fn
          (lambda (k) (if (member k '("keyA" "keyB" "keyC")) 424242 (sxhash-equal k))))
         (m (nl-clj-hash-map "keyA" 1 "keyB" 2 "keyC" 3)))
    (should (= (nl-clj-hash--map-get m "keyA" :nf) 1))
    (should (= (nl-clj-hash--map-get m "keyB" :nf) 2))
    (should (= (nl-clj-hash--map-get m "keyC" :nf) 3))
    ;; dissoc one of three, the other two survive
    (let ((m2 (nl-clj-hash--map-dissoc m "keyB")))
      (should (eq (nl-clj-hash--map-get m2 "keyB" :nf) :nf))
      (should (= (nl-clj-hash--map-get m2 "keyA" :nf) 1))
      (should (= (nl-clj-hash--map-get m2 "keyC" :nf) 3))
      (should (= (aref m2 1) 2)))))

;;;; Set --------------------------------------------------------------------

(ert-deftest nl-clj-hash-test-set-predicate ()
  (should (nl-clj-set-p (nl-clj-hash-set 1 2 3)))
  (should-not (nl-clj-set-p (nl-clj-hash-map :a 1))))

(ert-deftest nl-clj-hash-test-set-construct-and-count ()
  (let ((s (nl-clj-hash-set 1 2 3 2 1)))
    ;; duplicates collapse
    (should (= (aref s 1) 3))))

(ert-deftest nl-clj-hash-test-set-200-elt-conj-disj ()
  (let ((s (vector 'nl-clj--pset 0 nil)))
    (dotimes (i 200) (setq s (nl-clj-hash--set-conj s i)))
    (should (= (aref s 1) 200))
    ;; duplicate conj is a no-op on count
    (should (= (aref (nl-clj-hash--set-conj s 100) 1) 200))
    (let ((s2 (nl-clj-hash--set-disj s 100)))
      (should (= (aref s2 1) 199))
      ;; original untouched
      (should (= (aref s 1) 200)))))

(ert-deftest nl-clj-hash-test-set-disj-absent-is-eq-identical ()
  (let ((s (nl-clj-hash-set 1 2 3)))
    (should (eq (nl-clj-hash--set-disj s 999) s))))

(ert-deftest nl-clj-hash-test-set-elements ()
  (let ((s (nl-clj-hash-set 1 2 3)))
    (should (equal (sort (nl-clj-hash--set-elements s) #'<) '(1 2 3)))))

;;; nl-clj-hash-test.el ends here
