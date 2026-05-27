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

(ert-deftest nelisp-sys-backend-rejects-memory-ops ()
  (should-error
   (nelisp-sys-backend-test--lower
    '((sys:defstruct point (:repr c) (x i32))
      (sys:defun f ((p (ptr point))) i32 () (sys:load-field p x))))
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

;;; nelisp-sys-backend-test.el ends here
