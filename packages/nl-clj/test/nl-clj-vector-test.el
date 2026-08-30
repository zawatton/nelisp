;;; nl-clj-vector-test.el --- Tests for nl-clj-vector.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 §4.1's own DoD-shaped verification: no host-Emacs control
;; exists (plain Emacs has no persistent vector at all), so every
;; assertion below is checked against Clojure's own documented
;; complexity/sharing contract by hand, not by differential testing.
;; Three against-the-bug shapes this file specifically covers, named
;; in Doc 195 §4.1: (1) structural sharing -- an `eq' check on the
;; UNTOUCHED subtree, plus reading the ORIGINAL vector back after the
;; fact (not just checking the new vector's values -- a mutation bug
;; here is exactly "silent wrong result"); (2) the 32/33 and
;; height-growth tail-to-trie promotion boundaries; (3) `pop'/`peek'
;; as an exact round-trip inverse of `conj' over an operation sequence.

;;; Code:

(require 'ert)
(require 'nl-clj-vector)

;;;; Construction / predicate -----------------------------------------------

(ert-deftest nl-clj-vector-test-empty ()
  (let ((v (nl-clj-vector)))
    (should (nl-clj-vector-p v))
    (should (= (aref v 1) 0))))

(ert-deftest nl-clj-vector-test-predicate-rejects-non-vectors ()
  (should-not (nl-clj-vector-p [1 2 3]))
  (should-not (nl-clj-vector-p nil))
  (should-not (nl-clj-vector-p '(1 2 3))))

(ert-deftest nl-clj-vector-test-construct-from-items ()
  (let ((v (nl-clj-vector 1 2 3)))
    (should (= (aref v 1) 3))
    (should (= (nl-clj-vector--nth v 0) 1))
    (should (= (nl-clj-vector--nth v 1) 2))
    (should (= (nl-clj-vector--nth v 2) 3))))

;;;; conj / nth over a range of sizes -----------------------------------

(ert-deftest nl-clj-vector-test-conj-nth-1000-elements ()
  (let ((v (nl-clj-vector)))
    (dotimes (i 1000) (setq v (nl-clj-vector--conj v i)))
    (should (= (aref v 1) 1000))
    (dotimes (i 1000)
      (should (= (nl-clj-vector--nth v i) i)))))

;; The classic off-by-one boundaries for this data structure: where the
;; TAIL commits into the trie (32/33) and where the ROOT itself must
;; grow one level taller.
(ert-deftest nl-clj-vector-test-boundary-32-33 ()
  (let ((v (nl-clj-vector)))
    (dotimes (i 32) (setq v (nl-clj-vector--conj v i)))
    (should (= (length (nl-clj-vector--tail v)) 32))
    (setq v (nl-clj-vector--conj v 32))
    ;; the tail just committed 32 elements to the trie and now holds 1
    (should (= (length (nl-clj-vector--tail v)) 1))
    (dotimes (i 33) (should (= (nl-clj-vector--nth v i) i)))))

(ert-deftest nl-clj-vector-test-boundary-root-height-growth ()
  (let ((v (nl-clj-vector)) (grow-at nil))
    (dotimes (i 2000)
      (let ((before (aref v 2)))
        (setq v (nl-clj-vector--conj v i))
        (when (and (not grow-at) (> (aref v 2) before))
          (setq grow-at (1+ i)))))
    ;; height grows exactly once between shift=5 and shift=10 capacity
    ;; (32 root slots * 32-elt leaves = 1024, plus a full 32-elt tail)
    (should (= grow-at 1057))
    (dotimes (i 2000) (should (= (nl-clj-vector--nth v i) i)))))

(ert-deftest nl-clj-vector-test-large-multi-level-vector ()
  (let ((v (nl-clj-vector)))
    (dotimes (i 3000) (setq v (nl-clj-vector--conj v i)))
    (should (= (aref v 1) 3000))
    (dotimes (i 3000) (should (= (nl-clj-vector--nth v i) i)))))

;;;; assoc: correctness, extend-by-one, out of range -----------------------

(ert-deftest nl-clj-vector-test-assoc-updates-in-place-value ()
  (let* ((v (nl-clj-vector 1 2 3))
         (v2 (nl-clj-vector--assoc-n v 1 :changed)))
    (should (eq (nl-clj-vector--nth v2 1) :changed))))

(ert-deftest nl-clj-vector-test-assoc-does-not-mutate-original ()
  ;; Against-the-bug: read the ORIGINAL back after building v2, not
  ;; merely check v2's own values.
  (let* ((v (nl-clj-vector 1 2 3))
         (_v2 (nl-clj-vector--assoc-n v 1 :changed)))
    (should (= (nl-clj-vector--nth v 0) 1))
    (should (= (nl-clj-vector--nth v 1) 2))
    (should (= (nl-clj-vector--nth v 2) 3))))

(ert-deftest nl-clj-vector-test-assoc-at-count-extends-by-one ()
  (let* ((v (nl-clj-vector 1 2 3))
         (v2 (nl-clj-vector--assoc-n v 3 4)))
    (should (= (aref v2 1) 4))
    (should (= (nl-clj-vector--nth v2 3) 4))))

(ert-deftest nl-clj-vector-test-assoc-out-of-range-signals ()
  (let ((v (nl-clj-vector 1 2 3)))
    (should-error (nl-clj-vector--assoc-n v 5 :x) :type 'nl-clj-index-error)))

(ert-deftest nl-clj-vector-test-assoc-1000-elements-full-correctness ()
  (let ((v (nl-clj-vector)))
    (dotimes (i 1000) (setq v (nl-clj-vector--conj v i)))
    (let ((v2 (nl-clj-vector--assoc-n v 0 :changed)))
      (should (eq (nl-clj-vector--nth v2 0) :changed))
      (dotimes (i 999)
        (should (= (nl-clj-vector--nth v2 (1+ i)) (1+ i))))
      ;; original fully unchanged
      (should (= (nl-clj-vector--nth v 0) 0)))))

;;;; Structural sharing (against-the-bug, Doc 195 §4.1 property 1) --------

(ert-deftest nl-clj-vector-test-conj-shares-root-when-tail-has-room ()
  (let* ((v (nl-clj-vector 1 2 3))
         (v2 (nl-clj-vector--conj v 4)))
    (should (eq (nl-clj-vector--root v) (nl-clj-vector--root v2)))))

(ert-deftest nl-clj-vector-test-assoc-shares-untouched-branch ()
  (let ((v (nl-clj-vector)))
    (dotimes (i 5000) (setq v (nl-clj-vector--conj v i)))
    (let* ((idx-touch 4500)
           (idx-untouched 5)
           (top-touch (logand (ash idx-touch (- (aref v 2))) 31))
           (top-untouched (logand (ash idx-untouched (- (aref v 2))) 31))
           (v2 (nl-clj-vector--assoc-n v idx-touch :deep)))
      (should (/= top-touch top-untouched)) ;; test setup sanity
      (should (eq (nl-clj-vector--nth v2 idx-touch) :deep))
      (should (= (nl-clj-vector--nth v idx-touch) idx-touch))
      ;; the untouched top-level branch must be the SAME object
      (should (eq (aref (nl-clj-vector--root v) top-untouched)
                  (aref (nl-clj-vector--root v2) top-untouched)))
      ;; the touched branch must be a FRESH copy (not eq)
      (should-not (eq (aref (nl-clj-vector--root v) top-touch)
                       (aref (nl-clj-vector--root v2) top-touch))))))

;;;; peek / pop: inverse-of-conj round trip ---------------------------------

(ert-deftest nl-clj-vector-test-pop-on-empty-signals ()
  (should-error (nl-clj-vector--pop (nl-clj-vector)) :type 'nl-clj-index-error))

(ert-deftest nl-clj-vector-test-pop-to-single-element-then-empty ()
  (let* ((v (nl-clj-vector 1))
         (v2 (nl-clj-vector--pop v)))
    (should (= (aref v2 1) 0))))

(ert-deftest nl-clj-vector-test-pop-peek-round-trip-over-500-ops ()
  (let ((v (nl-clj-vector)) (stack nil))
    (dotimes (i 500)
      (cond
       ((or (null stack) (= 0 (mod i 3)))
        (setq v (nl-clj-vector--conj v i))
        (push i stack))
       (t
        (let ((top (nl-clj-vector--nth v (1- (aref v 1)))))
          (should (= top (car stack)))
          (setq v (nl-clj-vector--pop v))
          (pop stack)))))
    (should (= (aref v 1) (length stack)))
    (let ((i 0))
      (dolist (x (reverse stack))
        (should (= (nl-clj-vector--nth v i) x))
        (setq i (1+ i))))))

(ert-deftest nl-clj-vector-test-pop-3000-elements-back-to-empty ()
  (let ((v (nl-clj-vector)))
    (dotimes (i 3000) (setq v (nl-clj-vector--conj v i)))
    (dotimes (_ 3000) (setq v (nl-clj-vector--pop v)))
    (should (= (aref v 1) 0))))

(ert-deftest nl-clj-vector-test-pop-does-not-mutate-original ()
  (let* ((v (nl-clj-vector 1 2 3))
         (_v2 (nl-clj-vector--pop v)))
    (should (= (aref v 1) 3))
    (should (= (nl-clj-vector--nth v 2) 3))))

;;; nl-clj-vector-test.el ends here
