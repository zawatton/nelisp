;;; nelisp-stdlib-test.el --- ERT tests for Phase 1 standard library  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Round-out of the Phase 1 builtin set from docs/03-architecture.org
;; §7.1.  Covers the ~30 host-delegated primitives added in the stdlib
;; pass plus the NeLisp-aware higher-order wrappers (mapcar / mapc /
;; mapconcat / boundp / fboundp / symbol-value).

;;; Code:

(require 'ert)
(require 'nelisp-eval)

;;; List primitives ---------------------------------------------------

(ert-deftest nelisp-stdlib-list-shape-predicates ()
  (should (eq (nelisp-eval '(listp nil)) t))
  (should (eq (nelisp-eval '(listp (list 1 2))) t))
  (should (eq (nelisp-eval '(listp 42)) nil)))

(ert-deftest nelisp-stdlib-length ()
  (should (= (nelisp-eval '(length (list 1 2 3 4))) 4))
  (should (= (nelisp-eval '(length nil)) 0))
  (should (= (nelisp-eval '(length "hello")) 5)))

(ert-deftest nelisp-stdlib-nth-nthcdr ()
  (should (= (nelisp-eval '(nth 2 (list 10 20 30 40))) 30))
  (should (equal (nelisp-eval '(nthcdr 2 (list 10 20 30 40)))
                 '(30 40))))

(ert-deftest nelisp-stdlib-last ()
  (should (equal (nelisp-eval '(last (list 1 2 3))) '(3))))

(ert-deftest nelisp-stdlib-reverse ()
  (should (equal (nelisp-eval '(reverse (list 1 2 3))) '(3 2 1))))

(ert-deftest nelisp-stdlib-append ()
  (should (equal (nelisp-eval '(append (list 1 2) (list 3 4)))
                 '(1 2 3 4)))
  (should (equal (nelisp-eval '(append nil (list 1))) '(1))))

(ert-deftest nelisp-stdlib-member-memq ()
  (should (equal (nelisp-eval '(memq 2 (list 1 2 3))) '(2 3)))
  (should (eq    (nelisp-eval '(memq 99 (list 1 2 3))) nil))
  (should (equal (nelisp-eval '(member "b" (list "a" "b" "c")))
                 '("b" "c"))))

(ert-deftest nelisp-stdlib-assq-assoc ()
  (should (equal (nelisp-eval
                  '(assq (quote b)
                         (list (cons (quote a) 1)
                               (cons (quote b) 2))))
                 '(b . 2)))
  (should (equal (nelisp-eval
                  '(assoc "k"
                          (list (cons "j" 1) (cons "k" 2))))
                 '("k" . 2))))

;;; Arithmetic --------------------------------------------------------

(ert-deftest nelisp-stdlib-arith-extras ()
  (should (= (nelisp-eval '(1+ 5)) 6))
  (should (= (nelisp-eval '(1- 5)) 4))
  (should (= (nelisp-eval '(mod 10 3)) 1))
  (should (eq (nelisp-eval '(/= 1 2)) t))
  (should (eq (nelisp-eval '(/= 1 1)) nil))
  (should (= (nelisp-eval '(abs -7)) 7))
  (should (= (nelisp-eval '(max 3 1 4 1 5)) 5))
  (should (= (nelisp-eval '(min 3 1 4 1 5)) 1))
  (should (eq (nelisp-eval '(zerop 0)) t))
  (should (eq (nelisp-eval '(zerop 1)) nil)))

(ert-deftest nelisp-stdlib-numeric-predicates ()
  (should (eq (nelisp-eval '(numberp 42)) t))
  (should (eq (nelisp-eval '(numberp "42")) nil))
  (should (eq (nelisp-eval '(integerp 42)) t))
  (should (eq (nelisp-eval '(integerp "42")) nil)))

;;; Equality ----------------------------------------------------------

(ert-deftest nelisp-stdlib-eql-on-integers ()
  "In Phase 1 we only have integers, so eql ~ eq on atoms."
  (should (eq (nelisp-eval '(eql 42 42)) t))
  (should (eq (nelisp-eval '(eql 42 43)) nil)))

;;; String primitives -------------------------------------------------

(ert-deftest nelisp-stdlib-stringp ()
  (should (eq (nelisp-eval '(stringp "x")) t))
  (should (eq (nelisp-eval '(stringp 42)) nil)))

(ert-deftest nelisp-stdlib-concat ()
  (should (equal (nelisp-eval '(concat "foo" "bar" "baz"))
                 "foobarbaz"))
  (should (equal (nelisp-eval '(concat "" "x")) "x")))

(ert-deftest nelisp-stdlib-substring ()
  (should (equal (nelisp-eval '(substring "hello" 1 4)) "ell"))
  (should (equal (nelisp-eval '(substring "hello" 2)) "llo")))

(ert-deftest nelisp-stdlib-string-compare ()
  (should (eq (nelisp-eval '(string= "abc" "abc")) t))
  (should (eq (nelisp-eval '(string= "abc" "abd")) nil)))

(ert-deftest nelisp-stdlib-string-number-conversions ()
  (should (= (nelisp-eval '(string-to-number "42")) 42))
  (should (equal (nelisp-eval '(number-to-string 42)) "42")))

(ert-deftest nelisp-stdlib-case-conversions ()
  (should (equal (nelisp-eval '(upcase "hello")) "HELLO"))
  (should (equal (nelisp-eval '(downcase "HELLO")) "hello")))

(ert-deftest nelisp-stdlib-format ()
  (should (equal (nelisp-eval '(format "%d" 42)) "42"))
  (should (equal (nelisp-eval '(format "%s=%s" "k" "v")) "k=v"))
  (should (equal (nelisp-eval '(format "(%d,%d)" 1 2)) "(1,2)")))

;;; Symbol primitives -------------------------------------------------

(ert-deftest nelisp-stdlib-intern-and-name ()
  (should (eq (nelisp-eval '(intern "a-fresh-sym")) 'a-fresh-sym))
  (should (equal (nelisp-eval '(symbol-name (quote hello))) "hello")))

(ert-deftest nelisp-stdlib-boundp ()
  "NeLisp `boundp' consults the global table, not the lexical env."
  (nelisp--reset)
  (should (eq (nelisp-eval '(boundp (quote no-such-var))) nil))
  (nelisp-eval '(defvar *bp* 1))
  (should (eq (nelisp-eval '(boundp (quote *bp*))) t))
  ;; Lexical let does not make a var globally boundp.
  (should (eq (nelisp-eval '(let ((lexical 1))
                              (boundp (quote lexical))))
              nil)))

(ert-deftest nelisp-stdlib-fboundp ()
  (nelisp--reset)
  (should (eq (nelisp-eval '(fboundp (quote no-such-fn))) nil))
  (should (eq (nelisp-eval '(fboundp (quote +))) t))
  (nelisp-eval '(defun my-fn () 1))
  (should (eq (nelisp-eval '(fboundp (quote my-fn))) t)))

(ert-deftest nelisp-stdlib-symbol-value ()
  (nelisp--reset)
  (nelisp-eval '(defvar *sv* 123))
  (should (= (nelisp-eval '(symbol-value (quote *sv*))) 123)))

;;; Error plumbing ----------------------------------------------------

(ert-deftest nelisp-stdlib-error-raises ()
  (should (equal
           (nelisp-eval '(condition-case e
                             (error "boom %d" 42)
                           (error (cadr e))))
           "boom 42")))

(ert-deftest nelisp-stdlib-user-error-matches-error ()
  "`user-error' inherits from `error' so a generic handler catches it."
  (should (eq (nelisp-eval '(condition-case e
                                (user-error "nope")
                              (error :got-it)))
              :got-it)))

(ert-deftest nelisp-stdlib-signal-custom ()
  (should (eq (nelisp-eval '(condition-case e
                                (signal (quote arith-error) (list "bad"))
                              (arith-error :caught)))
              :caught)))

;;; Higher-order primitives (NeLisp-aware) ----------------------------

(ert-deftest nelisp-stdlib-mapcar-with-lambda ()
  (should (equal (nelisp-eval
                  '(mapcar (lambda (x) (* x x)) (list 1 2 3)))
                 '(1 4 9))))

(ert-deftest nelisp-stdlib-mapcar-with-host-symbol ()
  (should (equal (nelisp-eval '(mapcar (quote 1+) (list 1 2 3)))
                 '(2 3 4))))

(ert-deftest nelisp-stdlib-mapcar-with-nelisp-defun ()
  "mapcar dispatched to a NeLisp-only defun must resolve via the
NeLisp function table, not host `symbol-function'."
  (nelisp--reset)
  (nelisp-eval '(defun my-square (x) (* x x)))
  (should (equal (nelisp-eval '(mapcar (quote my-square) (list 1 2 3)))
                 '(1 4 9))))

(ert-deftest nelisp-stdlib-mapc-returns-seq ()
  (nelisp--reset)
  (nelisp-eval '(defvar *acc* 0))
  (should (equal (nelisp-eval
                  '(mapc (lambda (x) (setq *acc* (+ *acc* x)))
                         (list 1 2 3 4)))
                 '(1 2 3 4)))
  (should (= (nelisp-eval '*acc*) 10)))

(ert-deftest nelisp-stdlib-mapconcat-joins ()
  (should (equal (nelisp-eval
                  '(mapconcat (quote upcase) (list "ab" "cd") "-"))
                 "AB-CD"))
  (should (equal (nelisp-eval
                  '(mapconcat (lambda (n) (number-to-string n))
                              (list 1 2 3)
                              ","))
                 "1,2,3")))

(ert-deftest nelisp-stdlib-maphash-with-nelisp-closure ()
  "maphash must route FN through `nelisp--apply' so NeLisp closures
and NeLisp-only defuns see (KEY VALUE) — not the raw host callee."
  (nelisp--reset)
  (nelisp-eval '(defvar *seen* nil))
  (nelisp-eval
   '(let ((h (make-hash-table :test (quote equal))))
      (puthash "a" 1 h)
      (puthash "b" 2 h)
      (maphash (lambda (k v) (setq *seen* (cons (cons k v) *seen*))) h)))
  (let ((seen (nelisp-eval '*seen*)))
    (should (equal 2 (length seen)))
    (should (equal 3 (+ (cdr (assoc "a" seen))
                        (cdr (assoc "b" seen)))))))

(ert-deftest nelisp-stdlib-maphash-with-nelisp-defun ()
  "maphash via a quoted symbol must resolve through
`nelisp--functions', reaching a NeLisp-only defun."
  (nelisp--reset)
  (nelisp-eval '(defvar *tot* 0))
  (nelisp-eval '(defun my-sum-kv (_k v) (setq *tot* (+ *tot* v))))
  (nelisp-eval
   '(let ((h (make-hash-table)))
      (puthash (quote x) 10 h)
      (puthash (quote y) 20 h)
      (maphash (quote my-sum-kv) h)))
  (should (= 30 (nelisp-eval '*tot*))))

;;; New primitives added in Phase 5-A.0 --------------------------------

(ert-deftest nelisp-stdlib-car-safe ()
  (should (eq (nelisp-eval '(car-safe (list))) nil))
  (should (eq (nelisp-eval '(car-safe 7)) nil))
  (should (= 1 (nelisp-eval '(car-safe (list 1 2))))))

(ert-deftest nelisp-stdlib-caddr-cdddr ()
  (should (= 3 (nelisp-eval '(caddr (list 1 2 3 4)))))
  (should (equal '(4 5) (nelisp-eval '(cdddr (list 1 2 3 4 5))))))

(ert-deftest nelisp-stdlib-plist-get-put ()
  (should (= 2 (nelisp-eval '(plist-get (list :a 1 :b 2) :b))))
  (should (equal '(:a 9)
                 (nelisp-eval '(plist-put (list :a 1) :a 9)))))

(ert-deftest nelisp-stdlib-vector-ops ()
  (should (equal [1 2 3] (nelisp-eval '(vector 1 2 3))))
  (should (equal [0 0 0] (nelisp-eval '(make-vector 3 0)))))

(ert-deftest nelisp-stdlib-bit-arithmetic ()
  (should (= 4 (nelisp-eval '(ash 1 2))))
  (should (= 1 (nelisp-eval '(logand 3 5))))
  (should (= 7 (nelisp-eval '(logior 3 5)))))

(ert-deftest nelisp-stdlib-float-primitive ()
  (should (equal 3.0 (nelisp-eval '(float 3)))))

(ert-deftest nelisp-stdlib-message-returns-string ()
  "`message' is delegated to host and must return the formatted string."
  (should (equal "hi 1" (nelisp-eval '(message "hi %d" 1)))))

;;; funcall / apply interaction with new primitives -------------------

(ert-deftest nelisp-stdlib-funcall-on-symbol ()
  (nelisp--reset)
  (should (= (nelisp-eval '(funcall (quote +) 1 2 3)) 6)))

(ert-deftest nelisp-stdlib-apply-on-symbol ()
  (nelisp--reset)
  (should (= (nelisp-eval '(apply (quote +) 1 2 (list 3 4))) 10)))

;;; Phase 5-B.0 — sequence / string / terminal / timer primitives -----

(ert-deftest nelisp-stdlib-copy-sequence-string ()
  (let ((orig "abc"))
    (should (equal (nelisp-eval (list 'copy-sequence orig)) "abc"))
    (should-not (eq (nelisp-eval (list 'copy-sequence orig)) orig))))

(ert-deftest nelisp-stdlib-copy-sequence-list ()
  (let ((orig '(1 2 3)))
    (should (equal (nelisp-eval (list 'copy-sequence (list 'quote orig)))
                   '(1 2 3)))
    (should-not (eq (nelisp-eval (list 'copy-sequence (list 'quote orig)))
                    orig))))

(ert-deftest nelisp-stdlib-elt-string-and-list ()
  (should (= ?b (nelisp-eval '(elt "abc" 1))))
  (should (= 20 (nelisp-eval '(elt (list 10 20 30) 1)))))

(ert-deftest nelisp-stdlib-nconc-merges-lists ()
  (should (equal '(1 2 3 4)
                 (nelisp-eval '(nconc (list 1 2) (list 3 4))))))

(ert-deftest nelisp-stdlib-delq-removes-eq ()
  (should (equal '(1 3)
                 (nelisp-eval '(delq 2 (list 1 2 3))))))

(ert-deftest nelisp-stdlib-string-search-matches ()
  (should (= 3 (nelisp-eval '(string-search "lo" "hello world"))))
  (should (null (nelisp-eval '(string-search "xyz" "hello")))))

(ert-deftest nelisp-stdlib-split-string-default ()
  (should (equal '("foo" "bar" "baz")
                 (nelisp-eval '(split-string "foo bar baz")))))

(ert-deftest nelisp-stdlib-split-string-with-separator ()
  (should (equal '("a" "b" "c")
                 (nelisp-eval '(split-string "a,b,c" ",")))))

(ert-deftest nelisp-stdlib-frame-width-height-return-positive ()
  "Host `frame-width' / `frame-height' delegated unchanged."
  (let ((w (nelisp-eval '(frame-width)))
        (h (nelisp-eval '(frame-height))))
    (should (integerp w))
    (should (integerp h))
    (should (>= w 1))
    (should (>= h 1))))

(ert-deftest nelisp-stdlib-send-string-to-terminal-returns-nil ()
  "`send-string-to-terminal' is registered — we do not assert output
here (batch tty differs by platform); only that the primitive is
resolvable and completes without error returning nil."
  (should (null (nelisp-eval '(send-string-to-terminal "")))))

(ert-deftest nelisp-stdlib-run-at-time-registered ()
  "`run-at-time' is in the primitive table.  We schedule a trivial
timer and immediately cancel it via host `cancel-timer' (called
from ERT, not NeLisp) to avoid leaking a real timer into the test
suite."
  (let ((timer (nelisp-eval
                '(run-at-time 3600 nil (lambda () (ignore))))))
    (should (timerp timer))
    (cancel-timer timer)))

(provide 'nelisp-stdlib-test)

;;; nelisp-stdlib-test.el ends here
