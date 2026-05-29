;;; nelisp-sys-eval-kernel-test.el --- Doc 133 eval-kernel buildout -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 133 next phase (standalone-interpreter buildout): the first piece
;; of the eval kernel expressed in nelisp-sys and verified by running a
;; native binary — the eval DISPATCH on a Sexp tag.  An Int is
;; self-evaluating: `eval' reads the tag at offset 0 of a Sexp slot and,
;; for SEXP_TAG_INT, returns the payload at offset 8.  This is the
;; control-flow core of the interpreter (branch on the variant tag),
;; built on the Phase 0/1/2 primitives (struct offsets, mmap, peek) and
;; native-verified end-to-end with no Rust runtime.

;;; Code:

(require 'ert)
(require 'nelisp-sys-driver)
(require 'nelisp-sys-backend)

(defconst nelisp-sys-eval-kernel-test--sexp
  '((sys:defstruct sexp (:repr c)
      (tag u8) (payload u64) (pad (array u8 16)))))

(ert-deftest nelisp-sys-eval-kernel-lower-dispatch ()
  "eval_int lowers to a tag-dispatch over the Sexp slot."
  (should (equal '(defun eval_int (slot)
                    (if (= (ptr-read-u64 (+ slot 0) 0) 2)
                        (ptr-read-u64 (+ slot 8) 0)
                      0))
                 (nelisp-sys-backend-lower-module
                  (nelisp-sys-frontend-parse-module
                   (append nelisp-sys-eval-kernel-test--sexp
                           '((sys:defun eval_int ((slot usize)) i64 (:alloc none)
                               (if (= (sys:peek-u64
                                       (+ slot (sys:offsetof sexp tag))) 2)
                                   (sys:peek-u64
                                    (+ slot (sys:offsetof sexp payload)))
                                 0)))))
                  "x86_64-unknown-linux-gnu"))))

(ert-deftest nelisp-sys-eval-kernel-int-dispatch-runs ()
  "Doc 133 eval-kernel e2e: construct a Sexp Int (tag=INT, payload=42)
in an mmap'd slot, run the nelisp-sys eval dispatch (tag==INT -> payload),
exit 42.  First native-verified piece of the eval kernel — the variant
dispatch — with no Rust runtime."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           (append
            nelisp-sys-eval-kernel-test--sexp
            '(;; SEXP_TAG_INT = 2 (see nelisp-sexp--tag-int).
              (sys:defun eval_int ((slot usize)) i64 (:alloc none)
                (if (= (sys:peek-u64 (+ slot (sys:offsetof sexp tag))) 2)
                    (sys:peek-u64 (+ slot (sys:offsetof sexp payload)))
                  0))
              (sys:defun main () i64 (:syscall may :alloc none)
                (let ((slot usize (sys:syscall 9 0 4096 3 34 -1 0)))
                  (sys:poke-u64 (+ slot (sys:offsetof sexp tag)) 2)
                  (sys:poke-u64 (+ slot (sys:offsetof sexp payload)) 42)
                  (eval_int slot)))
              (sys:defun _start () void
                (:abi nelisp-internal :syscall may :alloc none)
                (sys:exit (main)))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

;;; nelisp-sys-eval-kernel-test.el ends here
