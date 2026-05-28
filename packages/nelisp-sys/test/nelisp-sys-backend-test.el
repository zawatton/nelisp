;;; nelisp-sys-backend-test.el --- ERT tests for the nelisp-sys codegen backend -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Stage 130.3/132.3 gate.  Pure lowering tests (no toolchain), plus an
;; end-to-end object + C-harness test that runs only when the NeLisp
;; toolchain and a C compiler are available.

;;; Code:

(require 'ert)
(require 'nelisp-sys-frontend)
(require 'nelisp-sys-driver)
(require 'nelisp-sys-backend)

(defconst nelisp-sys-backend-test--lx "x86_64-unknown-linux-gnu")

(defun nelisp-sys-backend-test--lower (forms)
  (nelisp-sys-backend-lower-module
   (nelisp-sys-frontend-parse-module forms) nelisp-sys-backend-test--lx))

;;; Lowering (no toolchain needed).

(ert-deftest nelisp-sys-backend-lower-add ()
  (should (equal '(defun nl_add (a b) (+ a b))
                 (nelisp-sys-backend-test--lower
                  '((sys:defun add ((a i32) (b i32)) i32
                      (:abi c :export "nl_add") (+ a b)))))))

(ert-deftest nelisp-sys-backend-lower-control-flow ()
  (should (equal '(defun mx (a b) (if (< a b) b a))
                 (nelisp-sys-backend-test--lower
                  '((sys:defun mx ((a i32) (b i32)) i32 () (if (< a b) b a)))))))

(ert-deftest nelisp-sys-backend-lower-ne-and-cast ()
  (should (equal '(defun ne (a b) (if (not (= a b)) 1 0))
                 (nelisp-sys-backend-test--lower
                  '((sys:defun ne ((a i32) (b i32)) i32 ()
                      (if (/= a b) 1 0))))))
  ;; integer width cast is identity at the 64-bit register level
  (should (equal '(defun w (a) (+ a 1))
                 (nelisp-sys-backend-test--lower
                  '((sys:defun w ((a i32)) i64 () (+ (sys:cast i64 a) 1)))))))

(ert-deftest nelisp-sys-backend-lower-let-multibody ()
  (should (equal '(defun f (n) (let ((s 0) (i 0))
                                 (seq (setq s (+ s i)) s)))
                 (nelisp-sys-backend-test--lower
                  '((sys:defun f ((n i32)) i32 ()
                      (let ((s i32 0) (i i32 0))
                        (set! s (+ s i))
                        s)))))))

(ert-deftest nelisp-sys-backend-folds-sizeof-offsetof ()
  (should (equal '(defun sz () 4)
                 (nelisp-sys-backend-test--lower
                  '((sys:defun sz () usize () (sys:sizeof i32))))))
  (should (equal '(defun oy () 4)
                 (nelisp-sys-backend-test--lower
                  '((sys:defstruct point (:repr c) (x i32) (y i32))
                    (sys:defun oy () usize () (sys:offsetof point y)))))))

(ert-deftest nelisp-sys-backend-lowers-struct-field ()
  "A struct field load through a (ptr S) place lowers to a ptr-read of the
field width at its offset (Stage 130.4)."
  (should (equal '(defun nl_distance2 (p)
                    (let ((x (ptr-read-u32 p 0)) (y (ptr-read-u32 p 4)))
                      (+ (* x x) (* y y))))
                 (nelisp-sys-backend-test--lower
                  '((sys:defstruct point (:repr c) (x i32) (y i32))
                    (sys:defun distance2 ((p (ptr point))) i64
                      (:abi c :export "nl_distance2")
                      (let ((x i32 (sys:load-field p x))
                            (y i32 (sys:load-field p y)))
                        (+ (* (sys:cast i64 x) (sys:cast i64 x))
                           (* (sys:cast i64 y) (sys:cast i64 y))))))))))

(ert-deftest nelisp-sys-backend-lowers-raw-pointer ()
  "Raw pointer load/store lower to ptr-read/-write of the pointee width."
  (should (equal '(defun rd (p) (ptr-read-u32 p 0))
                 (nelisp-sys-backend-test--lower
                  '((sys:defun rd ((p (ptr i32))) i32 () (sys:load p))))))
  (should (equal '(defun wr (p v) (ptr-write-u32 p 0 v))
                 (nelisp-sys-backend-test--lower
                  '((sys:defun wr ((p (ptr i32)) (v i32)) void ()
                      (sys:store! p v)))))))

(ert-deftest nelisp-sys-backend-lower-slice-len ()
  "(sys:slice-len S) reads the len word at offset = pointer size (8 on lp64)."
  (should (equal '(defun f (s) (ptr-read-u64 s 8))
                 (nelisp-sys-backend-test--lower
                  '((sys:defun f ((s (slice u8))) usize () (sys:slice-len s)))))))

(ert-deftest nelisp-sys-backend-lower-slice-ref ()
  "(sys:slice-ref S I) loads the data ptr (offset 0) then reads elem at I*size."
  (should (equal '(defun g (s i)
                    (ptr-read-u8 (+ (ptr-read-u64 s 0) (* i 1)) 0))
                 (nelisp-sys-backend-test--lower
                  '((sys:defun g ((s (slice u8)) (i usize)) u8 ()
                      (sys:slice-ref s i))))))
  ;; element stride follows sizeof(T): u32 -> *4 with a u32-wide read.
  (should (equal '(defun g2 (s i)
                    (ptr-read-u32 (+ (ptr-read-u64 s 0) (* i 4)) 0))
                 (nelisp-sys-backend-test--lower
                  '((sys:defun g2 ((s (slice u32)) (i usize)) u32 ()
                      (sys:slice-ref s i)))))))

(ert-deftest nelisp-sys-backend-lower-slice-set ()
  "(sys:slice-set! S I V) writes V into element I of a mutable slice."
  (should (equal '(defun h (s i v)
                    (ptr-write-u32 (+ (ptr-read-u64 s 0) (* i 4)) 0 v))
                 (nelisp-sys-backend-test--lower
                  '((sys:defun h ((s (slice-mut u32)) (i usize) (v u32)) u32 ()
                      (sys:slice-set! s i v)))))))

(ert-deftest nelisp-sys-backend-rejects-slice-through-non-var ()
  "A slice op through a non-var place is a clear error, not a miscompile."
  (should-error
   (nelisp-sys-backend-test--lower
    '((sys:defun f ((p (ptr u8))) usize ()
        (sys:slice-len (sys:load p)))))
   :type 'nelisp-sys-backend-error))

(ert-deftest nelisp-sys-backend-rejects-too-many-params ()
  (should-error
   (nelisp-sys-backend-test--lower
    '((sys:defun f ((a i32) (b i32) (c i32) (d i32) (e i32) (g i32) (h i32))
        i32 () a)))
   :type 'nelisp-sys-backend-error))

;;; End-to-end: object emission + C harness (needs toolchain + cc).

(ert-deftest nelisp-sys-backend-emit-object-and-call-from-c ()
  (skip-unless (and (nelisp-sys-adapter-available-p)
                    (executable-find "cc")))
  (let* ((tmp (make-temp-file "nelisp-sys-e2e" t))
         (obj (expand-file-name "nl_add.o" tmp))
         (cfile (expand-file-name "harness.c" tmp))
         (exe (expand-file-name "harness" tmp))
         (forms '((sys:defun add ((a i32) (b i32)) i32
                    (:abi c :export "nl_add" :alloc none) (+ a b)))))
    (unwind-protect
        (progn
          ;; analyze + compile to object
          (nelisp-sys-compile-object forms obj)
          (should (file-exists-p obj))
          ;; ABI sidecar emitted
          (should (file-exists-p (concat obj ".abi")))
          ;; C harness: long nl_add(long,long); main returns nl_add(40,2)
          (with-temp-file cfile
            (insert "long nl_add(long,long);\n"
                    "int main(void){return (int)nl_add(40,2);}\n"))
          (should (= 0 (call-process "cc" nil nil nil cfile obj "-o" exe)))
          (should (= 42 (call-process exe nil nil nil))))
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest nelisp-sys-backend-struct-pointer-from-c ()
  "Stage 130.4 e2e: a struct-pointer reader compiles and returns the right
value when called from C with a real struct."
  (skip-unless (and (nelisp-sys-adapter-available-p)
                    (executable-find "cc")))
  (let* ((tmp (make-temp-file "nelisp-sys-d2" t))
         (obj (expand-file-name "d2.o" tmp))
         (cfile (expand-file-name "harness.c" tmp))
         (exe (expand-file-name "harness" tmp))
         (forms '((sys:defstruct point (:repr c) (x i32) (y i32))
                  (sys:defun distance2 ((p (ptr point))) i64
                    (:abi c :export "nl_distance2" :alloc none)
                    (let ((x i32 (sys:load-field p x))
                          (y i32 (sys:load-field p y)))
                      (+ (* (sys:cast i64 x) (sys:cast i64 x))
                         (* (sys:cast i64 y) (sys:cast i64 y))))))))
    (unwind-protect
        (progn
          (nelisp-sys-compile-object forms obj)
          (should (file-exists-p obj))
          (with-temp-file cfile
            (insert "struct point{int x,y;};
long nl_distance2(struct point*);
int main(void){struct point p={3,4};return (int)nl_distance2(&p);}
"))
          (should (= 0 (call-process "cc" nil nil nil cfile obj "-o" exe)))
          (should (= 25 (call-process exe nil nil nil)))) ; 3*3 + 4*4
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest nelisp-sys-backend-slice-from-c ()
  "Slice ABI e2e: a slice is a pointer to a {data,len} header.  A getter,
length, and setter compile and behave correctly when called from C with a
real header over a u32 array."
  (skip-unless (and (nelisp-sys-adapter-available-p)
                    (executable-find "cc")))
  (let* ((tmp (make-temp-file "nelisp-sys-slice" t))
         (obj (expand-file-name "slice.o" tmp))
         (cfile (expand-file-name "harness.c" tmp))
         (exe (expand-file-name "harness" tmp))
         (forms '((sys:defun slice_get ((s (slice u32)) (i usize)) u32
                    (:abi c :export "nl_get" :alloc none)
                    (sys:slice-ref s i))
                  (sys:defun slice_len ((s (slice u32))) usize
                    (:abi c :export "nl_len" :alloc none)
                    (sys:slice-len s))
                  (sys:defun slice_set ((s (slice-mut u32)) (i usize) (v u32)) u32
                    (:abi c :export "nl_set" :alloc none)
                    (seq (sys:slice-set! s i v) v)))))
    (unwind-protect
        (progn
          (nelisp-sys-compile-object forms obj)
          (should (file-exists-p obj))
          (with-temp-file cfile
            (insert "#include <stddef.h>
struct sl { unsigned int *p; size_t n; };
unsigned long nl_get(struct sl*, size_t);
unsigned long nl_len(struct sl*);
unsigned long nl_set(struct sl*, size_t, unsigned int);
int main(void){
  unsigned int a[4]={10u,20u,30u,40u};
  struct sl s={a,4};
  if (nl_len(&s)!=4) return 1;
  if (nl_get(&s,2)!=30) return 2;
  nl_set(&s,1,99u);
  if (a[1]!=99u) return 3;
  if (nl_get(&s,1)!=99) return 4;
  return 42;
}
"))
          (should (= 0 (call-process "cc" nil nil nil cfile obj "-o" exe)))
          (should (= 42 (call-process exe nil nil nil))))
      (ignore-errors (delete-directory tmp t)))))

;;; nelisp-sys-backend-test.el ends here
