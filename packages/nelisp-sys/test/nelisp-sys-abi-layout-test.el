;;; nelisp-sys-abi-layout-test.el --- ERT tests for nelisp-sys layout -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Stage 132.2/130.2 gate: sizeof/alignof/offsetof + :repr c struct layout
;; for linux/x86_64, plus a cross-target invariant snapshot for all five
;; (currently all 64-bit, so layouts coincide).

;;; Code:

(require 'ert)
(require 'nelisp-sys-abi-layout)

(defconst nelisp-sys-abi-layout-test--lx "x86_64-unknown-linux-gnu")

(defun nelisp-sys-abi-layout-test--env ()
  (let ((env (nelisp-sys-types-env-make)))
    (nelisp-sys-types-env-add env 'point 'c '((x i32) (y i32)))
    ;; mixed: forces padding (i8, i32, i8) -> size 12, align 4
    (nelisp-sys-types-env-add env 'mixed 'c '((a i8) (b i32) (c i8)))
    ;; outer: nested struct + trailing scalar
    (nelisp-sys-types-env-add env 'outer 'c '((p (struct point)) (tag i8)))
    ;; withptr: pointer field forces 8-byte alignment
    (nelisp-sys-types-env-add env 'withptr 'c '((flag i8) (p (ptr i32))))
    env))

(ert-deftest nelisp-sys-layout-scalar-sizes ()
  (let ((tg nelisp-sys-abi-layout-test--lx))
    (should (= 1 (nelisp-sys-layout-sizeof 'i8 tg)))
    (should (= 1 (nelisp-sys-layout-sizeof 'bool tg)))
    (should (= 2 (nelisp-sys-layout-sizeof 'u16 tg)))
    (should (= 4 (nelisp-sys-layout-sizeof 'i32 tg)))
    (should (= 8 (nelisp-sys-layout-sizeof 'u64 tg)))
    (should (= 8 (nelisp-sys-layout-sizeof 'usize tg)))
    (should (= 0 (nelisp-sys-layout-sizeof 'void tg)))
    (should (= 8 (nelisp-sys-layout-sizeof '(ptr i32) tg)))
    (should (= 8 (nelisp-sys-layout-alignof '(ptr i32) tg)))
    (should (= 1 (nelisp-sys-layout-alignof 'void tg)))))

(ert-deftest nelisp-sys-layout-array-and-slice ()
  (let ((tg nelisp-sys-abi-layout-test--lx))
    (should (= 16 (nelisp-sys-layout-sizeof '(array i32 4) tg)))
    (should (= 4 (nelisp-sys-layout-alignof '(array i32 4) tg)))
    ;; slice = ptr + usize len
    (should (= 16 (nelisp-sys-layout-sizeof '(slice u8) tg)))
    (should (= 8 (nelisp-sys-layout-alignof '(slice-mut u8) tg)))))

(ert-deftest nelisp-sys-layout-point-struct ()
  (let* ((tg nelisp-sys-abi-layout-test--lx)
         (env (nelisp-sys-abi-layout-test--env))
         (l (nelisp-sys-layout-struct 'point tg env)))
    (should (= 8 (plist-get l :size)))
    (should (= 4 (plist-get l :align)))
    (should (= 0 (nelisp-sys-layout-offsetof 'point 'x tg env)))
    (should (= 4 (nelisp-sys-layout-offsetof 'point 'y tg env)))))

(ert-deftest nelisp-sys-layout-padding ()
  (let* ((tg nelisp-sys-abi-layout-test--lx)
         (env (nelisp-sys-abi-layout-test--env))
         (l (nelisp-sys-layout-struct 'mixed tg env)))
    (should (= 12 (plist-get l :size)))
    (should (= 4 (plist-get l :align)))
    (should (= 0 (nelisp-sys-layout-offsetof 'mixed 'a tg env)))
    (should (= 4 (nelisp-sys-layout-offsetof 'mixed 'b tg env)))
    (should (= 8 (nelisp-sys-layout-offsetof 'mixed 'c tg env)))))

(ert-deftest nelisp-sys-layout-nested-struct ()
  (let* ((tg nelisp-sys-abi-layout-test--lx)
         (env (nelisp-sys-abi-layout-test--env))
         (l (nelisp-sys-layout-struct 'outer tg env)))
    ;; point(8,a4) @0, tag i8 @8, struct align 4 -> size round-up(9,4)=12
    (should (= 12 (plist-get l :size)))
    (should (= 4 (plist-get l :align)))
    (should (= 0 (nelisp-sys-layout-offsetof 'outer 'p tg env)))
    (should (= 8 (nelisp-sys-layout-offsetof 'outer 'tag tg env)))))

(ert-deftest nelisp-sys-layout-pointer-field-alignment ()
  (let* ((tg nelisp-sys-abi-layout-test--lx)
         (env (nelisp-sys-abi-layout-test--env))
         (l (nelisp-sys-layout-struct 'withptr tg env)))
    ;; flag i8 @0, ptr @8 (aligned), size 16, align 8
    (should (= 16 (plist-get l :size)))
    (should (= 8 (plist-get l :align)))
    (should (= 8 (nelisp-sys-layout-offsetof 'withptr 'p tg env)))))

(ert-deftest nelisp-sys-layout-recursive-struct-rejected ()
  (let ((env (nelisp-sys-types-env-make)))
    (nelisp-sys-types-env-add env 'node 'c '((next (struct node))))
    (should-error (nelisp-sys-layout-struct 'node nelisp-sys-abi-layout-test--lx env)
                  :type 'nelisp-sys-layout-error)))

(ert-deftest nelisp-sys-layout-cross-target-invariant ()
  "All five currently-supported targets are 64-bit: withptr is 16/8 on each."
  (let ((env (nelisp-sys-abi-layout-test--env)))
    (dolist (tr (nelisp-sys-target-list))
      (let ((l (nelisp-sys-layout-struct 'withptr tr env)))
        (should (= 16 (plist-get l :size)))
        (should (= 8 (plist-get l :align)))))))

;;; nelisp-sys-abi-layout-test.el ends here
