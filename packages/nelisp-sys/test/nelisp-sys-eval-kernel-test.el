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

(ert-deftest nelisp-sys-eval-kernel-lower-lookup ()
  "nl_lookup lowers to a while-loop scan over an env of (sym,val) pairs.
Env layout: count@0, then 16-byte (sym,val) pairs from offset 8.  This is
the variable-environment core of the interpreter (Doc 133 env)."
  (should (equal '(defun nl_lookup (p n sym)
                    (if (= n 0)
                        -1
                      (if (= (ptr-read-u64 p 0) sym)
                          (ptr-read-u64 (+ p 8) 0)
                        (nl_lookup (+ p 16) (- n 1) sym))))
                 (nelisp-sys-backend-lower-module
                  (nelisp-sys-frontend-parse-module
                   '((sys:defun nl_lookup ((p usize) (n i64) (sym i64)) i64 (:alloc none)
                       (if (= n 0)
                           (sys:cast i64 -1)
                         (if (= (sys:peek-u64 p) sym)
                             (sys:peek-u64 (+ p 8))
                           (nl_lookup (+ p 16) (- n 1) sym))))))
                  "x86_64-unknown-linux-gnu"))))

(ert-deftest nelisp-sys-eval-kernel-env-runs ()
  "Doc 133 eval-kernel e2e: a variable environment + VAR lookup.
Env at the mmap base: count@0, then (sym,val) pairs (16B each) from
offset 8.  Nodes (32B): INT=2 (payload@8), ADD=100 (left@8,right@16),
VAR=104 (sym-id@8).  nl_eval threads `env' through the recursion; a VAR
node resolves via nl_lookup's while-loop scan.  Builds `(+ x y)' under
env {x->40, y->2} and evals it -> exit 42.  Native-verified: the eval
loop now carries a lexical environment, no Rust runtime."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-env")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '(;; nl_lookup: recursive scan of the (sym,val) env; -1 if absent.
             ;; p = current pair ptr, n = remaining count, sym = key.  Every
             ;; `if' sits in tail position (Phase 47 if/cond are tail-only:
             ;; they cannot nest as a setq value or call argument).
             (sys:defun nl_lookup ((p usize) (n i64) (sym i64)) i64 (:alloc none)
               (if (= n 0)
                   (sys:cast i64 -1)   ; literal in tail-then defaults to i32; pin i64
                 (if (= (sys:peek-u64 p) sym)
                     (sys:peek-u64 (+ p 8))
                   (nl_lookup (+ p 16) (- n 1) sym))))
             ;; nl_eval: tree-walk, threading the lexical env through recursion.
             (sys:defun nl_eval ((node usize) (env usize)) i64 (:alloc none)
               (let ((tag i64 (sys:peek-u64 node)))
                 (cond
                  ((= tag 2) (sys:peek-u64 (+ node 8)))
                  ((= tag 104)
                   (nl_lookup (+ env 8) (sys:peek-u64 env)
                              (sys:peek-u64 (+ node 8))))
                  ((= tag 100)
                   (+ (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))) env)
                      (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))) env)))
                  (else -1))))
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 ;; env: count=2, (sym 1 -> 40), (sym 2 -> 2)
                 (sys:poke-u64 (+ r 0) 2)
                 (sys:poke-u64 (+ r 8) 1)   (sys:poke-u64 (+ r 16) 40)
                 (sys:poke-u64 (+ r 24) 2)  (sys:poke-u64 (+ r 32) 2)
                 ;; nodes: VAR x @64, VAR y @96, ADD @128
                 (sys:poke-u64 (+ r 64) 104) (sys:poke-u64 (+ r 72) 1)
                 (sys:poke-u64 (+ r 96) 104) (sys:poke-u64 (+ r 104) 2)
                 (sys:poke-u64 (+ r 128) 100)
                 (sys:poke-u64 (+ r 136) (+ r 64))
                 (sys:poke-u64 (+ r 144) (+ r 96))
                 (nl_eval (+ r 128) r)))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-sys-eval-kernel-lower-apply ()
  "A CALL node applies a stored function pointer to an evaluated arg.
Lowers (sys:call-ptr FN ARG) to the Phase 47 (call-ptr FN ARG) — the
Doc 133 Phase 0 indirect-call capability, now driving the eval loop's
function-application path."
  (should (equal '(defun nl_apply (node)
                    (call-ptr (ptr-read-u64 (+ node 8) 0)
                              (ptr-read-u64 (+ node 16) 0)))
                 (nelisp-sys-backend-lower-module
                  (nelisp-sys-frontend-parse-module
                   '((sys:defun nl_apply ((node usize)) i64 (:alloc none)
                       (sys:call-ptr (sys:peek-u64 (+ node 8))
                                     (sys:peek-u64 (+ node 16))))))
                  "x86_64-unknown-linux-gnu"))))

(ert-deftest nelisp-sys-eval-kernel-apply-runs ()
  "Doc 133 eval-kernel e2e: function application through a code pointer.
A CALL node (tag 105) stores a raw function pointer at offset 8 and an
argument sub-node at offset 16.  nl_eval evaluates the argument tree,
then applies the stored pointer via (sys:call-ptr ...) — the Phase 0
fn-ptr capability inside the interpreter.  Builds CALL{fn=&nl_double,
arg=INT 21}, evals it (nl_double(21) = 21*2) -> exit 42.  Native-verified
apply, no Rust runtime."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-apply")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '(;; a builtin the interpreter can apply via its address.
             (sys:defun nl_double ((x i64)) i64 (:alloc none)
               (* x 2))
             ;; nl_eval: INT self-evals; CALL evaluates its arg sub-tree and
             ;; applies the stored fn-ptr (Doc 133 apply via Phase 0 call-ptr).
             (sys:defun nl_eval ((node usize)) i64 (:alloc none)
               (let ((tag i64 (sys:peek-u64 node)))
                 (cond
                  ((= tag 2) (sys:peek-u64 (+ node 8)))
                  ((= tag 105)
                   (sys:call-ptr
                    (sys:peek-u64 (+ node 8))
                    (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))))
                  (else -1))))
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 (sys:poke-u64 (+ r 0) 2)   (sys:poke-u64 (+ r 8) 21)   ; INT 21 @0
                 (sys:poke-u64 (+ r 32) 105)                            ; CALL  @32
                 (sys:poke-u64 (+ r 40) (sys:addr-of nl_double))        ;  fn-ptr
                 (sys:poke-u64 (+ r 48) (+ r 0))                        ;  arg = INT 21
                 (nl_eval (+ r 32))))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-sys-eval-kernel-lower-cmp ()
  "A comparison node lowers `(if (< a b) 1 0)' — the comparison yields an
i64 1/0 so it composes with the rest of the eval dispatch (Doc 133 cmp
nodes).  Drafted by codex (gpt-5.3-codex-spark) from the surface
cheat-sheet; lowering verified by the integrator."
  (should (equal '(defun cmp_lt (a b) (if (< a b) 1 0))
                 (nelisp-sys-backend-lower-module
                  (nelisp-sys-frontend-parse-module
                   '((sys:defun cmp_lt ((a i64) (b i64)) i64 (:alloc none)
                       (if (< a b) 1 0))))
                  "x86_64-unknown-linux-gnu"))))

(ert-deftest nelisp-sys-eval-kernel-cmp-runs ()
  "Doc 133 eval-kernel e2e: comparison nodes LT=106 and EQ=107.
Both are binary (left@8, right@16) and yield i64 1/0.  Evaluates
`(if (< (- 5 2) 4) (if (= 7 7) 42 99) 99)': LT(SUB(5,2)=3, 4)=true ->
inner IF; EQ(7,7)=true -> 42.  Exercises LT and EQ at runtime (plus
SUB/IF/INT) -> exit 42 on a standalone binary, no Rust runtime."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-cmp")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '((sys:defun nl_eval ((node usize)) i64 (:alloc none)
               (let ((tag i64 (sys:peek-u64 node)))
                 (cond
                  ((= tag 2) (sys:peek-u64 (+ node 8)))
                  ((= tag 101)
                   (- (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                      (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))))
                  ((= tag 106)
                   (if (< (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                          (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16)))))
                       1 0))
                  ((= tag 107)
                   (if (= (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                          (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16)))))
                       1 0))
                  ((= tag 103)
                   (if (/= (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8)))) 0)
                       (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))
                     (nl_eval (sys:cast usize (sys:peek-u64 (+ node 24))))))
                  (else -1))))
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 (sys:poke-u64 (+ r 0) 103)            ; IF   @0
                 (sys:poke-u64 (+ r 8) (+ r 32))       ;  cond = LT  @32
                 (sys:poke-u64 (+ r 16) (+ r 64))      ;  then = IF2 @64
                 (sys:poke-u64 (+ r 24) (+ r 96))      ;  else = INT 99 @96
                 (sys:poke-u64 (+ r 32) 106)           ; LT   @32
                 (sys:poke-u64 (+ r 40) (+ r 128))     ;  left  = SUB @128
                 (sys:poke-u64 (+ r 48) (+ r 160))     ;  right = INT 4 @160
                 (sys:poke-u64 (+ r 64) 103)           ; IF2  @64
                 (sys:poke-u64 (+ r 72) (+ r 192))     ;  cond = EQ  @192
                 (sys:poke-u64 (+ r 80) (+ r 224))     ;  then = INT 42 @224
                 (sys:poke-u64 (+ r 88) (+ r 96))      ;  else = INT 99 @96
                 (sys:poke-u64 (+ r 96) 2)   (sys:poke-u64 (+ r 104) 99)  ; INT 99
                 (sys:poke-u64 (+ r 128) 101)          ; SUB  @128
                 (sys:poke-u64 (+ r 136) (+ r 256))    ;  left  = INT 5 @256
                 (sys:poke-u64 (+ r 144) (+ r 288))    ;  right = INT 2 @288
                 (sys:poke-u64 (+ r 160) 2)  (sys:poke-u64 (+ r 168) 4)   ; INT 4
                 (sys:poke-u64 (+ r 192) 107)          ; EQ   @192
                 (sys:poke-u64 (+ r 200) (+ r 320))    ;  left  = INT 7 @320
                 (sys:poke-u64 (+ r 208) (+ r 352))    ;  right = INT 7 @352
                 (sys:poke-u64 (+ r 224) 2)  (sys:poke-u64 (+ r 232) 42)  ; INT 42
                 (sys:poke-u64 (+ r 256) 2)  (sys:poke-u64 (+ r 264) 5)   ; INT 5
                 (sys:poke-u64 (+ r 288) 2)  (sys:poke-u64 (+ r 296) 2)   ; INT 2
                 (sys:poke-u64 (+ r 320) 2)  (sys:poke-u64 (+ r 328) 7)   ; INT 7
                 (sys:poke-u64 (+ r 352) 2)  (sys:poke-u64 (+ r 360) 7)   ; INT 7
                 (nl_eval (+ r 0))))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-sys-eval-kernel-lower-reader ()
  "The reader's char read double-derefs the cursor cell: nl_peek reads
*(*cur).  (sys:cast usize ...) is erased in lowering, so it collapses to
a nested ptr-read-u64 (Doc 133 reader)."
  (should (equal '(defun nl_peek (cur)
                    (ptr-read-u64 (ptr-read-u64 cur 0) 0))
                 (nelisp-sys-backend-lower-module
                  (nelisp-sys-frontend-parse-module
                   '((sys:defun nl_peek ((cur usize)) i64 (:alloc none)
                       (sys:peek-u64 (sys:cast usize (sys:peek-u64 cur))))))
                  "x86_64-unknown-linux-gnu"))))

(ert-deftest nelisp-sys-eval-kernel-reader-runs ()
  "Doc 133 eval-kernel e2e: a minimal reader (text -> Sexp) feeding eval.
The input text is a char-per-word array (one char code per 8-byte slot;
0 terminates).  A recursive-descent parser (nl_parse <-> nl_plist, with
forward-referenced mutual recursion) skips whitespace, reads `(' OP A B
`)' lists and multi-digit integer literals, bump-allocates 32-byte Sexp
nodes (INT=2, ADD=100, SUB=101, MUL=102), and returns a node tree.
Parses `(+ (* 2 19) 4)' -> (* 2 19)=38, (+ 38 4)=42 -> native eval ->
exit 42.  text -> Sexp -> native evaluation, no Rust runtime."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-reader")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '(;; --- reader primitives (cursor/arena are u64 cells in memory) ---
             ;; read current char: *(*cur)
             (sys:defun nl_peek ((cur usize)) i64 (:alloc none)
               (sys:peek-u64 (sys:cast usize (sys:peek-u64 cur))))
             ;; advance the cursor by one char slot (8 bytes)
             (sys:defun nl_adv ((cur usize)) void (:alloc none)
               (sys:poke-u64 cur (+ (sys:peek-u64 cur) 8)))
             ;; skip ASCII spaces
             (sys:defun nl_skipws ((cur usize)) void (:alloc none)
               (while (= (nl_peek cur) 32) (nl_adv cur)))
             ;; digit predicate -> i32 flag
             (sys:defun nl_isdig ((c i64)) i32 (:alloc none)
               (if (>= c 48) (if (<= c 57) 1 0) 0))
             ;; operator char -> Sexp tag (i32): + -> ADD, - -> SUB, * -> MUL
             (sys:defun nl_optag ((op i64)) i32 (:alloc none)
               (if (= op 43) 100 (if (= op 45) 101 (if (= op 42) 102 -1))))
             ;; bump-allocate a 32-byte node from the arena cell, return its addr
             (sys:defun nl_alloc ((arena usize)) usize (:alloc none)
               (let ((node usize (sys:cast usize (sys:peek-u64 arena))))
                 (sys:poke-u64 arena (+ node 32))
                 node))
             ;; build an INT node holding VAL
             (sys:defun nl_make_int ((arena usize) (val i64)) usize (:alloc none)
               (let ((node usize (nl_alloc arena)))
                 (sys:poke-u64 node 2)
                 (sys:poke-u64 (+ node 8) val)
                 node))
             ;; parse a (multi-digit) integer literal -> INT node.  Written as
             ;; tail recursion over an accumulator rather than while+set!: the
             ;; accumulator is a PARAM (always a frame slot), sidestepping the
             ;; Phase 47 trap where a mutable local with a foldable init (0)
             ;; that is only set inside a loop is constant-folded, not slotted.
             (sys:defun nl_pint ((cur usize) (arena usize) (acc i64)) usize
               (:alloc none)
               (if (/= (nl_isdig (nl_peek cur)) 0)
                   (let ((d i64 (- (nl_peek cur) 48)))
                     (nl_adv cur)
                     (nl_pint cur arena (+ (* acc 10) d)))
                 (nl_make_int arena acc)))
             ;; parse a `(' OP A B `)' list -> binary op node.  Nested
             ;; single-assignment lets (each bound once from a call = a
             ;; non-foldable init) keep every local in a frame slot without
             ;; any set!, avoiding the mutable-local folding trap entirely.
             (sys:defun nl_plist ((cur usize) (arena usize)) usize (:alloc none)
               (nl_adv cur)
               (nl_skipws cur)
               (let ((op i64 (nl_peek cur)))
                 (nl_adv cur)
                 (let ((l usize (nl_parse cur arena)))
                   (let ((rr usize (nl_parse cur arena)))
                     (nl_skipws cur)
                     (nl_adv cur)
                     (let ((node usize (nl_alloc arena)))
                       (sys:poke-u64 node (nl_optag op))
                       (sys:poke-u64 (+ node 8) l)
                       (sys:poke-u64 (+ node 16) rr)
                       node)))))
             ;; parse any expression (mutually recursive with nl_plist)
             (sys:defun nl_parse ((cur usize) (arena usize)) usize (:alloc none)
               (nl_skipws cur)
               (if (= (nl_peek cur) 40)
                   (nl_plist cur arena)
                 (nl_pint cur arena 0)))
             ;; the tree-walking evaluator (INT/ADD/SUB/MUL)
             (sys:defun nl_eval ((node usize)) i64 (:alloc none)
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
                  (else -1))))
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 ;; text "(+ (* 2 19) 4)" as a char-per-word array, 0-terminated
                 (sys:poke-u64 (+ r 0) 40)   (sys:poke-u64 (+ r 8) 43)    ; ( +
                 (sys:poke-u64 (+ r 16) 32)  (sys:poke-u64 (+ r 24) 40)   ; _ (
                 (sys:poke-u64 (+ r 32) 42)  (sys:poke-u64 (+ r 40) 32)   ; * _
                 (sys:poke-u64 (+ r 48) 50)  (sys:poke-u64 (+ r 56) 32)   ; 2 _
                 (sys:poke-u64 (+ r 64) 49)  (sys:poke-u64 (+ r 72) 57)   ; 1 9
                 (sys:poke-u64 (+ r 80) 41)  (sys:poke-u64 (+ r 88) 32)   ; ) _
                 (sys:poke-u64 (+ r 96) 52)  (sys:poke-u64 (+ r 104) 41)  ; 4 )
                 (sys:poke-u64 (+ r 112) 0)                               ; NUL
                 ;; cursor cell @512 -> text start; arena cell @520 -> node region
                 (sys:poke-u64 (+ r 512) (+ r 0))
                 (sys:poke-u64 (+ r 520) (+ r 1024))
                 (nl_eval (nl_parse (+ r 512) (+ r 520)))))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

;; --- codex spark parallel batch (Doc 133): DIV / GT,GE / MOD nodes ---
;; Drafted concurrently by three gpt-5.3-codex-spark agents from the
;; surface cheat-sheet; lowering tests used as drafted (correct), e2e
;; fixtures paren-fixed and verified natively by the integrator (all
;; three raw outputs had the same clause/if mis-close as the pilot).

(ert-deftest nelisp-sys-eval-kernel-lower-div ()
  "DIV node lowers (/ a b)."
  (should (equal '(defun div2 (a b) (/ a b))
                 (nelisp-sys-backend-lower-module
                  (nelisp-sys-frontend-parse-module
                   '((sys:defun div2 ((a i64) (b i64)) i64 (:alloc none)
                       (/ a b))))
                  "x86_64-unknown-linux-gnu"))))

(ert-deftest nelisp-sys-eval-kernel-div-runs ()
  "Doc 133 eval-kernel e2e: integer division node DIV=110.
Builds `(/ 84 2)' (84/2=42) -> exit 42 on a standalone binary.  Enabled
once the Phase 47 value emitter learned to emit `/' (idiv quotient)."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-div")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '((sys:defun nl_eval ((node usize)) i64 (:alloc none)
               (let ((tag i64 (sys:peek-u64 node)))
                 (cond
                  ((= tag 2) (sys:peek-u64 (+ node 8)))
                  ((= tag 110)
                   (/ (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                      (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))))
                  (else -1))))
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 (sys:poke-u64 (+ r 0) 110)
                 (sys:poke-u64 (+ r 8) (+ r 32))
                 (sys:poke-u64 (+ r 16) (+ r 64))
                 (sys:poke-u64 (+ r 32) 2)  (sys:poke-u64 (+ r 40) 84)
                 (sys:poke-u64 (+ r 64) 2)  (sys:poke-u64 (+ r 72) 2)
                 (nl_eval (+ r 0))))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-sys-eval-kernel-lower-gt ()
  "GT comparison node lowers (if (> a b) 1 0)."
  (should (equal '(defun gtq (a b) (if (> a b) 1 0))
                 (nelisp-sys-backend-lower-module
                  (nelisp-sys-frontend-parse-module
                   '((sys:defun gtq ((a i64) (b i64)) i32 (:alloc none)
                       (if (> a b) 1 0))))
                  "x86_64-unknown-linux-gnu"))))

(ert-deftest nelisp-sys-eval-kernel-gt-runs ()
  "Doc 133 eval-kernel e2e: comparison nodes GT=108 and GE=109.
Builds `(if (> 9 4) (if (>= 5 5) 42 0) 0)': GT(9,4)=true -> inner IF,
GE(5,5)=true -> 42.  Exercises GT and GE at runtime -> exit 42."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-gt")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '((sys:defun nl_eval ((node usize)) i64 (:alloc none)
               (let ((tag i64 (sys:peek-u64 node)))
                 (cond
                  ((= tag 2) (sys:peek-u64 (+ node 8)))
                  ((= tag 108)
                   (if (> (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                          (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16)))))
                       1 0))
                  ((= tag 109)
                   (if (>= (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                           (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16)))))
                       1 0))
                  ((= tag 103)
                   (if (/= (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8)))) 0)
                       (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))
                     (nl_eval (sys:cast usize (sys:peek-u64 (+ node 24))))))
                  (else -1))))
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 (sys:poke-u64 (+ r 0) 103)
                 (sys:poke-u64 (+ r 8) (+ r 32))     ; cond = GT  @32
                 (sys:poke-u64 (+ r 16) (+ r 64))    ; then = IF2 @64
                 (sys:poke-u64 (+ r 24) (+ r 96))    ; else = INT 0 @96
                 (sys:poke-u64 (+ r 32) 108)         ; GT  @32
                 (sys:poke-u64 (+ r 40) (+ r 128))   ;  left  = INT 9
                 (sys:poke-u64 (+ r 48) (+ r 160))   ;  right = INT 4
                 (sys:poke-u64 (+ r 64) 103)         ; IF2 @64
                 (sys:poke-u64 (+ r 72) (+ r 192))   ;  cond = GE @192
                 (sys:poke-u64 (+ r 80) (+ r 224))   ;  then = INT 42
                 (sys:poke-u64 (+ r 88) (+ r 96))    ;  else = INT 0
                 (sys:poke-u64 (+ r 96) 2)   (sys:poke-u64 (+ r 104) 0)   ; INT 0
                 (sys:poke-u64 (+ r 128) 2)  (sys:poke-u64 (+ r 136) 9)   ; INT 9
                 (sys:poke-u64 (+ r 160) 2)  (sys:poke-u64 (+ r 168) 4)   ; INT 4
                 (sys:poke-u64 (+ r 192) 109)        ; GE  @192
                 (sys:poke-u64 (+ r 200) (+ r 256))  ;  left  = INT 5
                 (sys:poke-u64 (+ r 208) (+ r 288))  ;  right = INT 5
                 (sys:poke-u64 (+ r 224) 2)  (sys:poke-u64 (+ r 232) 42)  ; INT 42
                 (sys:poke-u64 (+ r 256) 2)  (sys:poke-u64 (+ r 264) 5)   ; INT 5
                 (sys:poke-u64 (+ r 288) 2)  (sys:poke-u64 (+ r 296) 5)   ; INT 5
                 (nl_eval (+ r 0))))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-sys-eval-kernel-lower-mod ()
  "MOD node lowers (mod a b)."
  (should (equal '(defun modq (a b) (mod a b))
                 (nelisp-sys-backend-lower-module
                  (nelisp-sys-frontend-parse-module
                   '((sys:defun modq ((a i64) (b i64)) i64 (:alloc none)
                       (mod a b))))
                  "x86_64-unknown-linux-gnu"))))

(ert-deftest nelisp-sys-eval-kernel-mod-runs ()
  "Doc 133 eval-kernel e2e: modulo node MOD=111.
Builds `(mod 142 100)' (=42) -> exit 42 on a standalone binary."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-mod")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '((sys:defun nl_eval ((node usize)) i64 (:alloc none)
               (let ((tag i64 (sys:peek-u64 node)))
                 (cond
                  ((= tag 2) (sys:peek-u64 (+ node 8)))
                  ((= tag 111)
                   (mod (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                        (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))))
                  (else -1))))
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 (sys:poke-u64 (+ r 0) 111)
                 (sys:poke-u64 (+ r 8) (+ r 32))
                 (sys:poke-u64 (+ r 16) (+ r 64))
                 (sys:poke-u64 (+ r 32) 2)  (sys:poke-u64 (+ r 40) 142)
                 (sys:poke-u64 (+ r 64) 2)  (sys:poke-u64 (+ r 72) 100)
                 (nl_eval (+ r 0))))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

;; --- codex spark parallel batch (Doc 133): LE/NE, NOT/NEG, MIN/MAX nodes ---
;; Drafted concurrently by three gpt-5.3-codex-spark agents; lowering tests
;; used as drafted (correct), e2e fixtures paren-fixed by the integrator:
;;   evk-le-ne:   tag-103 cond clause missing closing ), main missing sys:exit
;;   evk-not-neg: let-bindings for tag-114/115 missing closing ), main missing
;;                sys:exit, path arg missing in compile-executable call
;;   evk-min-max: if-comparison missing closing ) for inner (</>) call in
;;                tag-116/117 clauses, main missing sys:exit, path arg missing

(ert-deftest nelisp-sys-eval-kernel-lower-le ()
  "LE comparison node lowers (if (<= a b) 1 0)."
  (should (equal '(defun leq (a b) (if (<= a b) 1 0))
                 (nelisp-sys-backend-lower-module
                  (nelisp-sys-frontend-parse-module
                   '((sys:defun leq ((a i64) (b i64)) i32 (:alloc none)
                       (if (<= a b) 1 0))))
                  "x86_64-unknown-linux-gnu"))))

(ert-deftest nelisp-sys-eval-kernel-le-ne-runs ()
  "Doc 133 eval-kernel e2e: comparison nodes LE=112 and NE=113.
Builds `(if (<= 4 4) (if (/= 3 8) 42 0)':
LE(4,4)=true -> inner IF; NE(3,8)=true -> 42."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-le-ne")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '((sys:defun nl_eval ((node usize)) i64 (:alloc none)
               (let ((tag i64 (sys:peek-u64 node)))
                 (cond
                  ((= tag 2) (sys:peek-u64 (+ node 8)))
                  ((= tag 103)
                   (if (/= (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8)))) 0)
                       (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))
                     (nl_eval (sys:cast usize (sys:peek-u64 (+ node 24))))))
                  ((= tag 112)
                   (if (<= (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                           (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16)))))
                       1 0))
                  ((= tag 113)
                   (if (/= (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                           (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16)))))
                       1 0))
                  (else -1))))
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 (sys:poke-u64 (+ r 0) 103)
                 (sys:poke-u64 (+ r 8) (+ r 32))    ; root.cond = LE
                 (sys:poke-u64 (+ r 16) (+ r 64))   ; root.then = IF
                 (sys:poke-u64 (+ r 24) (+ r 96))   ; root.else = INT 0
                 (sys:poke-u64 (+ r 32) 112)        ; LE @32
                 (sys:poke-u64 (+ r 40) (+ r 128))  ;  left  = INT 4
                 (sys:poke-u64 (+ r 48) (+ r 160))  ;  right = INT 4
                 (sys:poke-u64 (+ r 64) 103)        ; IF @64
                 (sys:poke-u64 (+ r 72) (+ r 192))  ;  cond = NE
                 (sys:poke-u64 (+ r 80) (+ r 224))  ;  then = INT 42
                 (sys:poke-u64 (+ r 88) (+ r 256))  ;  else = INT 0
                 (sys:poke-u64 (+ r 96) 2)  (sys:poke-u64 (+ r 104) 0)    ; INT 0 @96
                 (sys:poke-u64 (+ r 128) 2) (sys:poke-u64 (+ r 136) 4)    ; INT 4 @128
                 (sys:poke-u64 (+ r 160) 2) (sys:poke-u64 (+ r 168) 4)    ; INT 4 @160
                 (sys:poke-u64 (+ r 192) 113)        ; NE @192
                 (sys:poke-u64 (+ r 200) (+ r 288))  ;  left  = INT 3
                 (sys:poke-u64 (+ r 208) (+ r 320))  ;  right = INT 8
                 (sys:poke-u64 (+ r 224) 2) (sys:poke-u64 (+ r 232) 42)   ; INT 42 @224
                 (sys:poke-u64 (+ r 256) 2) (sys:poke-u64 (+ r 264) 0)    ; INT 0 @256
                 (sys:poke-u64 (+ r 288) 2) (sys:poke-u64 (+ r 296) 3)    ; INT 3 @288
                 (sys:poke-u64 (+ r 320) 2) (sys:poke-u64 (+ r 328) 8)    ; INT 8 @320
                 (nl_eval (+ r 0))))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-sys-eval-kernel-lower-neg ()
  "NEG node lowers (- 0 a)."
  (should (equal '(defun neg1 (a) (- 0 a))
                 (nelisp-sys-backend-lower-module
                  (nelisp-sys-frontend-parse-module
                   '((sys:defun neg1 ((a i64)) i64 (:alloc none)
                       (- 0 a))))
                  "x86_64-unknown-linux-gnu"))))

(ert-deftest nelisp-sys-eval-kernel-not-neg-runs ()
  "Doc 133 eval-kernel e2e: unary nodes NOT=114 and NEG=115.
Builds IF(NOT(INT 0), NEG(INT -42), INT 0):
NOT(0)=1 -> true -> NEG(-42)=42 -> exit 42."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-not-neg")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '((sys:defun nl_eval ((node usize)) i64 (:alloc none)
               (let ((tag i64 (sys:peek-u64 node)))
                 (cond
                  ((= tag 2) (sys:peek-u64 (+ node 8)))
                  ((= tag 114)
                   (let ((C i64 (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))))
                     (if (= C 0) 1 0)))
                  ((= tag 115)
                   (let ((C i64 (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))))
                     (- 0 C)))
                  ((= tag 103)
                   (if (/= (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8)))) 0)
                       (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))
                     (nl_eval (sys:cast usize (sys:peek-u64 (+ node 24))))))
                  (else -1))))
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 (sys:poke-u64 (+ r 0) 103)
                 (sys:poke-u64 (+ r 8) (+ r 32))
                 (sys:poke-u64 (+ r 16) (+ r 64))
                 (sys:poke-u64 (+ r 24) (+ r 96))
                 (sys:poke-u64 (+ r 32) 114)
                 (sys:poke-u64 (+ r 40) (+ r 128))
                 (sys:poke-u64 (+ r 64) 115)
                 (sys:poke-u64 (+ r 72) (+ r 160))
                 (sys:poke-u64 (+ r 96) 2)
                 (sys:poke-u64 (+ r 104) 0)
                 (sys:poke-u64 (+ r 128) 2)
                 (sys:poke-u64 (+ r 136) 0)
                 (sys:poke-u64 (+ r 160) 2)
                 (sys:poke-u64 (+ r 168) -42)
                 (nl_eval (+ r 0))))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-sys-eval-kernel-lower-max ()
  "MAX node lowers (if (> a b) a b)."
  (should (equal '(defun mx (a b) (if (> a b) a b))
                 (nelisp-sys-backend-lower-module
                  (nelisp-sys-frontend-parse-module
                   '((sys:defun mx ((a i64) (b i64)) i64 (:alloc none)
                       (if (> a b) a b))))
                  "x86_64-unknown-linux-gnu"))))

(ert-deftest nelisp-sys-eval-kernel-min-max-runs ()
  "Doc 133 eval-kernel e2e: binary nodes MIN=116 and MAX=117.
Builds MAX(MIN(42,50), 40): MIN(42,50)=42, MAX(42,40)=42 -> exit 42."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-min-max")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '((sys:defun nl_eval ((node usize)) i64 (:alloc none)
               (let ((tag i64 (sys:peek-u64 node)))
                 (cond
                  ((= tag 2) (sys:peek-u64 (+ node 8)))
                  ((= tag 116)
                   (if (< (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                          (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16)))))
                       (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                       (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))))
                  ((= tag 117)
                   (if (> (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                          (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16)))))
                       (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                       (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))))
                  (else -1))))
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 (sys:poke-u64 (+ r 0) 117)
                 (sys:poke-u64 (+ r 8) (+ r 32))
                 (sys:poke-u64 (+ r 16) (+ r 104))
                 (sys:poke-u64 (+ r 32) 116)
                 (sys:poke-u64 (+ r 40) (+ r 64))
                 (sys:poke-u64 (+ r 48) (+ r 84))
                 (sys:poke-u64 (+ r 64) 2)
                 (sys:poke-u64 (+ r 72) 42)
                 (sys:poke-u64 (+ r 84) 2)
                 (sys:poke-u64 (+ r 92) 50)
                 (sys:poke-u64 (+ r 104) 2)
                 (sys:poke-u64 (+ r 112) 40)
                 (nl_eval (+ r 0))))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-sys-eval-kernel-reader-ops-runs ()
  "Doc 133 eval-kernel e2e: extended reader with / < > = operators and
negative integer literals.  nl_optag maps +/-/*//</>=/= to tags 100-110.
nl_digits is a pure tail-recursive accumulator (no arena); nl_pint_neg
consumes a leading '-' and negates; nl_pint dispatches on peek==45.
nl_eval handles INT=2, ADD=100, SUB=101, MUL=102, DIV=110, LT=106,
GT=108, EQ=107.  Parses '(/ (+ -42 126) 2)':
+(-42, 126)=84; /(84, 2)=42 -> exit 42.  text -> Sexp -> native eval."
  (unless (and (eq system-type 'gnu/linux)
               (string-prefix-p "x86_64" system-configuration))
    (ert-skip "requires x86_64 Linux"))
  (require 'nelisp-sys-adapter-nelisp)
  (unless (nelisp-sys-adapter-available-p)
    (ert-skip "NeLisp toolchain not available"))
  (let ((path (make-temp-file "nelisp-sys-eval-reader-ops")))
    (unwind-protect
        (progn
          (delete-file path)
          (nelisp-sys-compile-executable
           '(;; --- reader primitives ---
             ;; read current char: *(*cur)
             (sys:defun nl_peek ((cur usize)) i64 (:alloc none)
               (sys:peek-u64 (sys:cast usize (sys:peek-u64 cur))))
             ;; advance the cursor by one char slot (8 bytes)
             (sys:defun nl_adv ((cur usize)) void (:alloc none)
               (sys:poke-u64 cur (+ (sys:peek-u64 cur) 8)))
             ;; skip ASCII spaces
             (sys:defun nl_skipws ((cur usize)) void (:alloc none)
               (while (= (nl_peek cur) 32) (nl_adv cur)))
             ;; digit predicate -> i32 flag
             (sys:defun nl_isdig ((c i64)) i32 (:alloc none)
               (if (>= c 48) (if (<= c 57) 1 0) 0))
             ;; extended operator char -> Sexp tag (i32)
             ;; + -> 100 ADD, - -> 101 SUB, * -> 102 MUL,
             ;; / -> 110 DIV, < -> 106 LT, = -> 107 EQ, > -> 108 GT
             (sys:defun nl_optag ((op i64)) i32 (:alloc none)
               (if (= op 43) 100
                 (if (= op 45) 101
                   (if (= op 42) 102
                     (if (= op 47) 110
                       (if (= op 60) 106
                         (if (= op 62) 108
                           (if (= op 61) 107
                             -1))))))))
             ;; bump-allocate a 32-byte node from the arena cell, return its addr
             (sys:defun nl_alloc ((arena usize)) usize (:alloc none)
               (let ((node usize (sys:cast usize (sys:peek-u64 arena))))
                 (sys:poke-u64 arena (+ node 32))
                 node))
             ;; build an INT node holding VAL
             (sys:defun nl_make_int ((arena usize) (val i64)) usize (:alloc none)
               (let ((node usize (nl_alloc arena)))
                 (sys:poke-u64 node 2)
                 (sys:poke-u64 (+ node 8) val)
                 node))
             ;; accumulate decimal digits, tail-recursive, no arena.
             ;; Stops when peek is not a digit, returns accumulated i64 value.
             (sys:defun nl_digits ((cur usize) (acc i64)) i64 (:alloc none)
               (if (/= (nl_isdig (nl_peek cur)) 0)
                   (let ((d i64 (- (nl_peek cur) 48)))
                     (nl_adv cur)
                     (nl_digits cur (+ (* acc 10) d)))
                 acc))
             ;; parse a negative integer literal (leading '-' already peeked)
             (sys:defun nl_pint_neg ((cur usize) (arena usize)) usize (:alloc none)
               (nl_adv cur)
               (nl_make_int arena (- 0 (nl_digits cur 0))))
             ;; parse an integer literal (positive or negative) -> INT node
             (sys:defun nl_pint ((cur usize) (arena usize)) usize (:alloc none)
               (if (= (nl_peek cur) 45)
                   (nl_pint_neg cur arena)
                 (nl_make_int arena (nl_digits cur 0))))
             ;; parse a `(' OP A B `)' list -> binary op node
             (sys:defun nl_plist ((cur usize) (arena usize)) usize (:alloc none)
               (nl_adv cur)
               (nl_skipws cur)
               (let ((op i64 (nl_peek cur)))
                 (nl_adv cur)
                 (let ((l usize (nl_parse cur arena)))
                   (let ((rr usize (nl_parse cur arena)))
                     (nl_skipws cur)
                     (nl_adv cur)
                     (let ((node usize (nl_alloc arena)))
                       (sys:poke-u64 node (nl_optag op))
                       (sys:poke-u64 (+ node 8) l)
                       (sys:poke-u64 (+ node 16) rr)
                       node)))))
             ;; parse any expression (mutually recursive with nl_plist)
             (sys:defun nl_parse ((cur usize) (arena usize)) usize (:alloc none)
               (nl_skipws cur)
               (if (= (nl_peek cur) 40)
                   (nl_plist cur arena)
                 (nl_pint cur arena)))
             ;; tree-walking evaluator: INT/ADD/SUB/MUL/DIV/LT/EQ/GT
             (sys:defun nl_eval ((node usize)) i64 (:alloc none)
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
                  ((= tag 110)
                   (/ (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                      (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16))))))
                  ((= tag 106)
                   (if (< (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                          (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16)))))
                       1 0))
                  ((= tag 108)
                   (if (> (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                          (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16)))))
                       1 0))
                  ((= tag 107)
                   (if (= (nl_eval (sys:cast usize (sys:peek-u64 (+ node 8))))
                          (nl_eval (sys:cast usize (sys:peek-u64 (+ node 16)))))
                       1 0))
                  (else -1))))
             ;; main: build "(/ (+ -42 126) 2)" as char-per-word array, parse, eval
             ;; Char sequence: ( / sp ( + sp - 4 2 sp 1 2 6 ) sp 2 ) NUL
             ;; Codes:        40 47 32 40 43 32 45 52 50 32 49 50 54 41 32 50 41  0
             ;; Offsets:       0  8 16 24 32 40 48 56 64 72 80 88 96 104 112 120 128 136
             ;; Evaluation:  +(- 42,126)=84; /(84,2)=42
             (sys:defun main () i64 (:syscall may :alloc none)
               (let ((r usize (sys:syscall 9 0 4096 3 34 -1 0)))
                 (sys:poke-u64 (+ r 0)   40)   ; (
                 (sys:poke-u64 (+ r 8)   47)   ; /
                 (sys:poke-u64 (+ r 16)  32)   ; space
                 (sys:poke-u64 (+ r 24)  40)   ; (
                 (sys:poke-u64 (+ r 32)  43)   ; +
                 (sys:poke-u64 (+ r 40)  32)   ; space
                 (sys:poke-u64 (+ r 48)  45)   ; -
                 (sys:poke-u64 (+ r 56)  52)   ; 4
                 (sys:poke-u64 (+ r 64)  50)   ; 2
                 (sys:poke-u64 (+ r 72)  32)   ; space
                 (sys:poke-u64 (+ r 80)  49)   ; 1
                 (sys:poke-u64 (+ r 88)  50)   ; 2
                 (sys:poke-u64 (+ r 96)  54)   ; 6
                 (sys:poke-u64 (+ r 104) 41)   ; )
                 (sys:poke-u64 (+ r 112) 32)   ; space
                 (sys:poke-u64 (+ r 120) 50)   ; 2
                 (sys:poke-u64 (+ r 128) 41)   ; )
                 (sys:poke-u64 (+ r 136) 0)    ; NUL
                 ;; cursor cell @512 -> text start; arena cell @520 -> node region
                 (sys:poke-u64 (+ r 512) (+ r 0))
                 (sys:poke-u64 (+ r 520) (+ r 1024))
                 (nl_eval (nl_parse (+ r 512) (+ r 520)))))
             (sys:defun _start () void
               (:abi nelisp-internal :syscall may :alloc none)
               (sys:exit (main))))
           path)
          (should (file-executable-p path))
          (should (= 42 (call-process path nil nil nil))))
      (ignore-errors (delete-file path)))))

;;; nelisp-sys-eval-kernel-test.el ends here
