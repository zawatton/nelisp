;;; nelisp-artifact-native-exec-test.el --- Doc 142 general native exec tests  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side proof tests for Doc 142's builtin-calling native-exec path.

;;; Code:

(require 'ert)
(require 'nelisp-artifact)

(defun nelisp-artifact-native-exec-test--linux-x86_64-p ()
  "Return non-nil when the host matches the current native proof lane."
  (and (eq system-type 'gnu/linux)
       (string-match-p "x86_64" (or system-configuration ""))))

(ert-deftest nelisp-artifact/native-exec-general-builtin-call1 ()
  "Builtin-calling `.neln' defuns execute through the host proof harness.
The current proof lane covers the hidden boundary-slot setup plus the
`nl_alloc_symbol' / `nelisp_aot_builtin_call1' runtime shims on
x86_64 Linux."
  (skip-unless (and (nelisp-artifact-native-exec-test--linux-x86_64-p)
                    (executable-find "cc")
                    (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-native-exec-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source
          "(defun nat-ng-let-if (x)
  (let ((y (1+ x)))
    (if (> x 0) (1+ y) (1- y))))
(defun nat-ng-compare (x y)
  (if (> x y) (1+ x) (1- y)))
(defun nat-ng-multi (a b c)
  (if (> a b) (1+ c) (1- c)))
(provide 'nat-ng)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (load source-path nil t)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-let-if" '(5))
                     (nat-ng-let-if 5)))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-let-if" '(-2))
                     (nat-ng-let-if -2)))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-compare" '(9 4))
                     (nat-ng-compare 9 4)))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-compare" '(1 4))
                     (nat-ng-compare 1 4)))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-multi" '(9 4 7))
                     (nat-ng-multi 9 4 7)))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-multi" '(1 4 7))
                     (nat-ng-multi 1 4 7))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/native-exec-general-builtin-calln-eq ()
  "A vararg builtin calln defun executes through the host proof harness."
  (skip-unless (and (nelisp-artifact-native-exec-test--linux-x86_64-p)
                    (executable-find "cc")
                    (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-native-exec-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source
          "(defun nat-ng-eq-flag (x y)
  (eq x y))
(provide 'nat-ng-eq)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (load source-path nil t)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-eq-flag" '(5 5))
                     1))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-eq-flag" '(-2 7))
                     0)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/native-exec-general-string-args ()
  "Host native exec can pass string slots to string-reading native defuns."
  (skip-unless (and (nelisp-artifact-native-exec-test--linux-x86_64-p)
                    (executable-find "cc")
                    (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-native-string-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source
          "(defun nat-ng-string-len (s)
  (str-len s))
(defun nat-ng-string-first-byte (s)
  (str-byte-at s 0))
(provide 'nat-ng-string)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (load source-path nil t)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-string-len" '("abc"))
                     3))
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-string-first-byte" '("abc"))
                     ?a)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/native-exec-general-string-result ()
  "Host native exec can decode string results from native defuns."
  (skip-unless (and (nelisp-artifact-native-exec-test--linux-x86_64-p)
                    (executable-find "cc")
                    (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-native-string-result-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source
          "(defun nat-ng-string-echo (s)
  s)
(defun nat-ng-string-select (s flag)
  (if (> flag 0) s \"no\"))
(provide 'nat-ng-string-result)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (load source-path nil t)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (should (equal (nelisp-artifact-native-exec-general
                          artifact-path "nat-ng-string-echo" '("abc"))
                         "abc"))
          (should (equal (nelisp-artifact-native-exec-general
                          artifact-path "nat-ng-string-select" '("abc" 1))
                         "abc"))
          (should (equal (nelisp-artifact-native-exec-general
                          artifact-path "nat-ng-string-select" '("abc" 0))
                         "no")))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/native-exec-general-mut-str-builder ()
  "Host native exec can run the mutable-string builder extern lane."
  (skip-unless (and (nelisp-artifact-native-exec-test--linux-x86_64-p)
                    (executable-find "cc")
                    (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-native-mut-str-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source
          "(defun nat-ng-mut-str-build (slot)
  (and (mut-str-make-empty slot 4)
       (mut-str-push-byte slot 111)
       (mut-str-push-byte slot 107)
       (mut-str-push-byte slot 10)
       (mut-str-finalize slot slot)))
(provide 'nat-ng-mut-str)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (load source-path nil t)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (should (equal (nelisp-artifact-native-exec-general
                          artifact-path "nat-ng-mut-str-build" '(""))
                         "ok\n")))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/native-exec-general-deep-tail-recursion-smoke ()
  "Doc 171 G3: USER artifact code reaches native TCO and survives deep recursion."
  (skip-unless (and (nelisp-artifact-native-exec-test--linux-x86_64-p)
                    (executable-find "cc")
                    (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-native-tco-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (depth 200000)
         (expected (/ (* depth (1+ depth)) 2))
         (process-environment (cons "NELISP_TCO=1" process-environment))
         (source
          "(defun nat-ng-tail-sum (n acc)
  (if (= n 0) acc
    (nat-ng-tail-sum (- n 1) (+ acc n))))
(provide 'nat-ng-tail)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (should (= (nelisp-artifact-native-exec-general
                      artifact-path "nat-ng-tail-sum" (list depth 0))
                     expected)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/native-exec-general-value-word-boundary ()
  "A raw machine word never crosses a defun boundary in this lane.

Runtime-entered defuns take their parameters as Sexp pointers and read
them back with the immediate-aware unwrap, so an argument or a return
that is a raw word gets dereferenced by the other side.  Every shape
below did that: `(g 0)' loaded `[0+8]', `(g (str-len s))' loaded past a
length, and `merge-bit' returning `(+ a b)' handed its caller an integer
to follow as an address.

Each case is checked against the same source interpreted by Emacs rather
than a written-down constant, because the failure was as often a wrong
answer as a crash -- the mask cases returned 0 for 10 while exiting 0,
which an exit-status assertion calls a pass."
  (skip-unless (and (nelisp-artifact-native-exec-test--linux-x86_64-p)
                    (executable-find "cc")
                    (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-native-vw-" t))
         (source-path (expand-file-name "vw.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source
          "(defun nat-vw-ge (a b) (if (>= a b) 111 222))
(defun nat-vw-lit (x) (nat-vw-ge 0 x))
(defun nat-vw-arith (x) (nat-vw-ge (+ x 1) x))
(defun nat-vw-branch-arg (x) (nat-vw-ge (if (= x 0) 7 3) x))
(defun nat-vw-add (mask bit)
  (if (= bit 0) mask (if (= (logand mask bit) 0) (+ mask bit) mask)))
(defun nat-vw-merge (mask bits)
  (nat-vw-add (nat-vw-add mask (if (= (logand bits 1) 0) 0 1))
              (if (= (logand bits 2) 0) 0 2)))
(provide 'nat-vw)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (load source-path nil t)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (dolist (probe '(("nat-vw-lit" (5))
                           ("nat-vw-lit" (-1))
                           ("nat-vw-arith" (5))
                           ("nat-vw-branch-arg" (0))
                           ("nat-vw-branch-arg" (4))
                           ("nat-vw-add" (0 2))
                           ("nat-vw-add" (2 2))
                           ("nat-vw-merge" (0 3))
                           ("nat-vw-merge" (4 1))))
            (let ((name (car probe))
                  (args (cadr probe)))
              (should (equal (nelisp-artifact-native-exec-general
                              artifact-path name args)
                             (apply (intern name) args))))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/native-exec-general-connective-boundary ()
  "`and'/`or' answer in the raw domain and are boxed at the boundary.

`--emit-logic' short-circuits on a zero test of the machine word, and the
arm that stops it is also the form's value -- one register serving as
both.  That is why an arm has to stay raw (a Sexp pointer is never zero,
so a boxed false would read true and the short-circuit would never fire)
and why the conversion has to go on the whole form instead.

Every shape below was wrong before: `(g (and 1 3) 2)' answered 222 for
111, because 3 handed over raw read back as a tagged immediate is
`3 >> 2' = 0, and `(g (and 1 (+ x 1)) 4)' took SIGSEGV.  The string case
is the other side of the same rule: `mut-str-finalize' answers with a
slot, so its `and' must NOT be boxed, and a helper whose whole body is a
raw `mut-str-push-byte' must be -- getting that pair wrong turned the
report into `0' while still exiting 0."
  (skip-unless (and (nelisp-artifact-native-exec-test--linux-x86_64-p)
                    (executable-find "cc")
                    (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-native-ao-" t))
         (source-path (expand-file-name "ao.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source
          "(defun nat-ao-cmp (a b) (if (>= a b) 111 222))
(defun nat-ao-lit (x) (nat-ao-cmp (and 1 3) x))
(defun nat-ao-arith (x) (nat-ao-cmp (and 1 (+ x 1)) 4))
(defun nat-ao-last (x) (nat-ao-cmp (and 1 x) 4))
(defun nat-ao-or (x) (nat-ao-cmp (or x 9) 4))
(defun nat-ao-ret (x) (and 1 (+ x 1)))
(defun nat-ao-use (x) (nat-ao-cmp (nat-ao-ret x) 4))
(provide 'nat-ao)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (load source-path nil t)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (dolist (probe '(("nat-ao-lit" (5)) ("nat-ao-lit" (2))
                           ("nat-ao-arith" (5)) ("nat-ao-last" (5))
                           ("nat-ao-or" (9)) ("nat-ao-ret" (5))
                           ("nat-ao-use" (5))))
            (let ((name (car probe))
                  (args (cadr probe)))
              (should (equal (nelisp-artifact-native-exec-general
                              artifact-path name args)
                             (apply (intern name) args))))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/native-exec-general-mut-str-and-chain ()
  "A helper returning a raw sentinel is boxed; one returning a slot is not.

The two halves have to agree.  `mut-str-push-byte' answers `rax = 1
sentinel' and `mut-str-finalize' answers with the caller-owned slot, so
one is boxed at a boundary and the other must be left alone.  Boxing the
second wraps a string in an integer; not boxing the first has the caller
read 1 as a tagged immediate and get 0, which turns the enclosing `and'
false and the whole report into `0' -- while still exiting 0."
  (skip-unless (and (nelisp-artifact-native-exec-test--linux-x86_64-p)
                    (executable-find "cc")
                    (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-native-ms-" t))
         (source-path (expand-file-name "ms.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source
          "(declare-function mut-str-make-empty \"ext:nelisp-native\" (out capacity))
(declare-function mut-str-push-byte \"ext:nelisp-native\" (out byte))
(declare-function mut-str-finalize \"ext:nelisp-native\" (out dest))
(defun nat-ms-bare (out) (mut-str-push-byte out 68))
(defun nat-ms-one (out) (and (mut-str-push-byte out 65)))
(defun nat-ms-two (out) (and (mut-str-push-byte out 66) (mut-str-push-byte out 67)))
(defun nat-ms-r-bare (text out)
  (and (mut-str-make-empty out 32) (nat-ms-bare out) (mut-str-finalize out out)))
(defun nat-ms-r-one (text out)
  (and (mut-str-make-empty out 32) (nat-ms-one out) (mut-str-finalize out out)))
(defun nat-ms-r-two (text out)
  (and (mut-str-make-empty out 32) (nat-ms-two out) (mut-str-finalize out out)))
(provide 'nat-ms)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (dolist (probe '(("nat-ms-r-bare" "D")
                           ("nat-ms-r-one" "A")
                           ("nat-ms-r-two" "BC")))
            (should (equal (nelisp-artifact-native-exec-general
                            artifact-path (car probe) (list "x" ""))
                           (cadr probe)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(provide 'nelisp-artifact-native-exec-test)

;;; nelisp-artifact-native-exec-test.el ends here
