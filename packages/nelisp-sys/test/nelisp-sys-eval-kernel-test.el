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

(ert-deftest nelisp-sys-eval-kernel-recursive-eval-runs ()
  "Doc 133 eval-kernel e2e: a recursive tree-walking evaluator.
Node layout: tag@0, field1@8, field2@16.  INT node = (tag 2, value);
ADD node = (tag 100, left-ptr, right-ptr).  nl_eval dispatches on tag:
INT -> self (payload), ADD -> eval(left) + eval(right) (recursion).
Builds `(+ 40 2)' as an ADD of two INT nodes, evals it -> exit 42.
This is the essence of the eval loop (recursion over a Sexp tree with
variant dispatch), native-verified with no Rust runtime."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-rec")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '((sys:defun nl_eval ((node usize)) i64 (:alloc none)
               (let ((tag i64 (sys:peek-u64 node)))
                 (cond
                  ((= tag 2) (sys:peek-u64 (+ node 8)))
                  ((= tag 100)
                   (+ (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                      (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))))
                  (else -1))))
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 (sys:poke-u64 (+ r 0) 2)   (sys:poke-u64 (+ r 8) 40)  ; INT 40 @0
                 (sys:poke-u64 (+ r 24) 2)  (sys:poke-u64 (+ r 32) 2)  ; INT 2  @24
                 (sys:poke-u64 (+ r 48) 100)                           ; ADD    @48
                 (sys:poke-u64 (+ r 56) (+ r 0))                       ;  left = INT 40
                 (sys:poke-u64 (+ r 64) (+ r 24))                      ;  right = INT 2
                 (nl_eval (+ r 48))))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-sys-eval-kernel-arith-if-runs ()
  "Doc 133 eval-kernel e2e: a small expression evaluator with arithmetic
and conditionals.  32-byte nodes: tag@0, a@8, b@16, c@24.  Tags: INT=2,
ADD=100, SUB=101, MUL=102, IF=103 (a=cond, b=then, c=else).  Builds
`(if (- 2 1) (* 6 7) 0)' = (cond 1 -> then 42) and evals it -> exit 42.
Native-verified: nested recursion + SUB/MUL + conditional branching, all
as nelisp-sys-emitted native code, no Rust runtime."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-if")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '((sys:defun nl_eval ((node usize)) i64 (:alloc none)
               (let ((tag i64 (sys:peek-u64 node)))
                 (cond
                  ((= tag 2) (sys:peek-u64 (+ node 8)))
                  ((= tag 100)
                   (+ (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                      (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))))
                  ((= tag 101)
                   (- (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                      (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))))
                  ((= tag 102)
                   (* (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                      (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))))
                  ((= tag 103)
                   (if (/= (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8)))) 0)
                       (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))
                     (nl_eval (sys:cast usize (sys:peek-u64 (+ node 24))))))
                  (else -1))))
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 (sys:poke-u64 (+ r 0) 2)    (sys:poke-u64 (+ r 8) 2)    ; INT 2 @0
                 (sys:poke-u64 (+ r 32) 2)   (sys:poke-u64 (+ r 40) 1)   ; INT 1 @32
                 (sys:poke-u64 (+ r 64) 101)                             ; SUB  @64
                 (sys:poke-u64 (+ r 72) (+ r 0)) (sys:poke-u64 (+ r 80) (+ r 32))
                 (sys:poke-u64 (+ r 96) 2)   (sys:poke-u64 (+ r 104) 6)  ; INT 6 @96
                 (sys:poke-u64 (+ r 128) 2)  (sys:poke-u64 (+ r 136) 7)  ; INT 7 @128
                 (sys:poke-u64 (+ r 160) 102)                            ; MUL  @160
                 (sys:poke-u64 (+ r 168) (+ r 96)) (sys:poke-u64 (+ r 176) (+ r 128))
                 (sys:poke-u64 (+ r 192) 2)  (sys:poke-u64 (+ r 200) 0)  ; INT 0 @192
                 (sys:poke-u64 (+ r 224) 103)                            ; IF   @224
                 (sys:poke-u64 (+ r 232) (+ r 64))                       ;  cond = SUB
                 (sys:poke-u64 (+ r 240) (+ r 160))                      ;  then = MUL
                 (sys:poke-u64 (+ r 248) (+ r 192))                      ;  else = INT 0
                 (nl_eval (+ r 224))))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

;;; nelisp-sys-eval-kernel-test.el ends here
