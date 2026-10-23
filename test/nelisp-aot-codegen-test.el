;;; nelisp-aot-codegen-test.el --- Doc 224 code-shape mechanisms, end to end  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Each test compiles one freestanding program with
;; `nelisp-aot-compile-sexp', runs it and checks the exit status against
;; a value computed independently (the comment beside each expectation
;; gives the computation).  The programs exercise the Doc 224 mechanisms:
;; CG1 branch-context lowering and rotated loops, CG2 direct operands and
;; in-place updates, CG3 register promotion of call-free loops (including
;; more live variables than registers and values used after the loop),
;; CG4 folded addresses and memory compares, and CG5 fused tag tests.

;;; Code:

(require 'ert)
(require 'nelisp-aot-compiler)

(defun nelisp-aot-codegen-test--run (program)
  "Compile PROGRAM to a temporary ELF, run it and return its exit status."
  (let ((path (make-temp-file "nelisp-aot-codegen-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp program path)
          (call-process path nil nil nil))
      (delete-file path))))

(defmacro nelisp-aot-codegen-test--deftest (name doc program expected)
  "Define test NAME running PROGRAM and expecting exit status EXPECTED."
  (declare (indent 1) (doc-string 2))
  `(ert-deftest ,name ()
     ,doc
     (skip-unless (and (eq system-type 'gnu/linux)
                       (string-match-p "x86_64" system-configuration)))
     (should (= (nelisp-aot-codegen-test--run ',program) ,expected))))

(ert-deftest nelisp-aot-codegen/affine-widths-and-wrapping ()
  "All read/write widths preserve full bits under affine address rewrites."
  (let ((definitions nil) (checks nil) (folds 0)
        (emit (symbol-function 'nelisp-aot-compiler--emit-affine-address-x86))
        (shapes '(((+ p (* (- i 1) 8)) 0)
                  ((+ p 8) (shl (- i 2) 3))
                  (p (* (+ i -1) 8))
                  ((+ (* i 8) p) -8)
                  ((+ p (* i 4)) (* (- i 2) 4))
                  ((+ p (* i 8)) 18446744073709551608))))
    (dolist (width '(8 16 32 64))
      (cl-loop for (ptr offset) in shapes for k from 0 do
               (let ((name (intern (format "affine_w%d_s%d" width k)))
                     (read-op (intern (format "ptr-read-u%d" width)))
                     (write-op (intern (format "ptr-write-u%d" width))))
                 (push `(defun ,name (p i value expected)
                          (seq (,write-op ,ptr ,offset value)
                               (if (= (,read-op ,ptr ,offset) expected) 0 1))) definitions)
                 (dolist (i '(-2 0 1 5))
                   (dolist (value '(-1 0 1311768467139281697))
                     (let ((expected (logand value (1- (ash 1 width)))))
                       (when (and (= width 64) (>= expected (ash 1 63)))
                         (setq expected (- expected (ash 1 64))))
                       (push `(,name (+ mem 256) ,i ,value ,expected) checks)))))))
    ;; 288 comparisons: OR the independent boolean failures, avoiding an
    ;; exit-status sum that could wrap to zero after 256 failed checks.
    (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-affine-address-x86)
               (lambda (plan buf) (setq folds (1+ folds)) (funcall emit plan buf))))
      (should (= 0 (nelisp-aot-codegen-test--run
                    `(seq (defun mmap-page () (syscall-direct 9 0 4096 3 34 -1 0))
                          ,@(nreverse definitions)
                          (defun run ()
                            (let ((mem (mmap-page)) (failure 0))
                              ,@(mapcar (lambda (check) `(setq failure (logior failure ,check))) checks)
                              failure))
                          (exit (run)))))))
    (should (> folds 0))))

(ert-deftest nelisp-aot-codegen/affine-effects-and-memory-rhs ()
  "Pointer/offset/value mutations retain order and aliased RHS reads retain bits."
  (should
   (= 0 (nelisp-aot-codegen-test--run
         '(seq
           (defun run (p)
             (let ((i 1))
               ;; Address captures i=2 before OFFSET advances i to 3;
               ;; value then advances i to 4.  The store belongs at p+16.
               (ptr-write-u64
                (seq (setq i (+ i 1)) (+ p (* (- i 1) 8)))
                (seq (setq i (+ i 1)) 8)
                (seq (setq i (+ i 1)) i))
               (if (= (ptr-read-u64 p 16) 4)
                   (seq
                    (setq i 2)
                    (ptr-write-u64 (+ p (* (- i 1) 8)) 0
                                   (seq (setq i 7) 99))
                    (if (= (ptr-read-u64 p 8) 99)
                        (seq
                         (ptr-write-u64 (+ p (* (- i 1) 8)) 0
                                        (+ (ptr-read-u64 (+ p (* (- i 6) 8)) 0)
                                           (ptr-read-u64 (+ p (* (- i 5) 8)) 0)))
                         (if (= (ptr-read-u64 p 48) 103) 0 3))
                      2))
                 1)))
           (defun mmap-page () (syscall-direct 9 0 4096 3 34 -1 0))
           (exit (run (mmap-page))))))))

(ert-deftest nelisp-aot-codegen/affine-large-displacement-fallback ()
  "Unencodable displacements fall back, while cancelling constants normalize."
  (should
   (= 0 (nelisp-aot-codegen-test--run
         '(seq
           (defun read-large (p) (ptr-read-u64 p 1099511627776))
           (defun write-large (p v) (ptr-write-u64 p -1099511627776 v))
           (defun cancelled (p i)
             (ptr-read-u64 (- p 2147483648) (+ 2147483648 (* (- i 1) 8))))
           (defun odd-scale (p i) (ptr-read-u64 (+ p (* i 3)) 0))
           (defun coefficient-wrap (p i)
             (ptr-read-u64 (+ p (* i 18446744073709551624)) -8))
           (defun run (p)
             (seq (write-large (+ p 1099511627776) 123456789)
                  (if (= (read-large (- p 1099511627776)) 123456789)
                      (if (= (cancelled p 1) 123456789)
                          (if (= (odd-scale (- p 3) 1) 123456789)
                              (if (= (coefficient-wrap p 1) 123456789) 0 4) 3) 2) 1)))
           (defun mmap-page () (syscall-direct 9 0 4096 3 34 -1 0))
           (exit (run (mmap-page))))))))

(ert-deftest nelisp-aot-codegen/affine-checked-arithmetic ()
  "Checked arithmetic retains its guards and original evaluation path."
  (let ((nelisp-aot-compiler--checked-arith t) (folds 0)
        (emit (symbol-function 'nelisp-aot-compiler--emit-affine-address-x86)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-affine-address-x86)
               (lambda (plan buf) (setq folds (1+ folds)) (funcall emit plan buf))))
      (should (= 11 (nelisp-aot-codegen-test--run
                     '(seq
                       (defun run (p i)
                         (seq (ptr-write-u64 (+ p (* (- i 1) 8)) 0 11)
                              (ptr-read-u64 (+ p (* (- i 1) 8)) 0)))
                       (defun mmap-page () (syscall-direct 9 0 4096 3 34 -1 0))
                       (exit (run (mmap-page) 2))))))
      (should (= 97 (nelisp-aot-codegen-test--run
                     '(seq (defun run (p i)
                             (ptr-read-u64 (+ p (* (+ i 1) 8)) 0))
                           (exit (run 0 2305843009213693951)))))))
    (should (zerop folds))))

(ert-deftest nelisp-aot-codegen/affine-preflight-and-registers ()
  "Rejected displacements emit nothing; live result registers remain excluded."
  (let* ((nelisp-aot-compiler--arch 'x86_64)
         (nelisp-aot-compiler--checked-arith nil)
         (nelisp-aot-compiler--promoted-slots nil)
         (p (nelisp-aot-compiler--make-ir 'ref :slot 0 :class 'gp))
         (i (nelisp-aot-compiler--make-ir 'ref :slot 1 :class 'gp))
         (zero (nelisp-aot-compiler--make-ir 'imm :value 0))
         (eight (nelisp-aot-compiler--make-ir 'imm :value 8))
         (huge (nelisp-aot-compiler--make-ir 'imm :value 1099511627776))
         (bad (nelisp-aot-compiler--make-ir 'arith :op '+ :a p :b huge))
         (good (nelisp-aot-compiler--make-ir
                'arith :op '+ :a p
                :b (nelisp-aot-compiler--make-ir 'arith :op '* :a i :b eight)))
         (buf (nelisp-asm-x86_64-make-buffer)))
    (should-not (nelisp-aot-compiler--x86-affine-address-plan bad zero))
    (should-not (nelisp-aot-compiler--x86-address bad zero buf t))
    (should (zerop (nelisp-asm-x86_64-buffer-pos buf)))
    (should (nelisp-aot-compiler--x86-affine-address-plan good zero))
    (dolist (reg '(rax r10 r11 rsp))
      (let ((nelisp-aot-compiler--promoted-slots (list (cons 0 reg))))
        (should-not (nelisp-aot-compiler--x86-affine-address-plan good zero))))
    (dolist (reg '(rdi rsi rdx rcx))
      (let ((nelisp-aot-compiler--promoted-slots (list (cons 0 reg))))
        (should (nelisp-aot-compiler--x86-affine-address-plan good zero))))
    (let ((nelisp-aot-compiler--promoted-slots '((0 . r8) (1 . r9))))
      (should (nelisp-aot-compiler--x86-affine-address-plan good zero)))
    (let ((nelisp-aot-compiler--runtime-entry-params '(out)))
      (should-not (nelisp-aot-compiler--x86-affine-address-plan good zero)))))

(ert-deftest nelisp-aot-codegen/self-tco-incoming-cycle-and-full-word ()
  "Incoming R8/R9 cycles and zero/deep loops preserve all 64 result bits."
  (let ((nelisp-aot-compiler-tco-enabled t) (forwards 0) (cycles 0)
        (emit (symbol-function 'nelisp-aot-compiler--emit-tco-parallel-moves-x86)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-tco-parallel-moves-x86)
               (lambda (moves buf)
                 (setq forwards (1+ forwards))
                 (when (and (member '(r8 . r9) moves) (member '(r9 . r8) moves))
                   (setq cycles (1+ cycles)))
                 (funcall emit moves buf))))
      (dolist (n '(-1 0 1 2 17 10000))
        (dolist (pair '((7 11) (-9223372036854775808 9223372036854775807)))
          (let ((x (car pair)) (y (cadr pair)) (j n))
            (while (> j 0) (setq x (* x 2) y (* y 3) j (1- j)))
            (let ((expected (logand (+ 3 4 5 (* x 5) (* y 7)) (1- (ash 1 64)))))
              (when (>= expected (ash 1 63)) (setq expected (- expected (ash 1 64))))
              (should
               (= 0 (nelisp-aot-codegen-test--run
                     `(seq
                       (defun hop (n a b c x y)
                         (if (and (> n 0) (> (+ n n) 0))
                             (hop (- n 1) a b c (+ x x) (+ y (+ y y)))
                           (+ a (+ b (+ c (+ (* x 5) (* y 7)))))))
                       (exit (if (= (hop ,n 3 4 5 ,(car pair) ,(cadr pair)) ,expected) 0 1))))))))))
    (should (> forwards 0))
    (should (> cycles 0)))))

(ert-deftest nelisp-aot-codegen/self-tco-mutation-and-call-exit ()
  "Mutation order and unchanged loop inputs survive the flushed call exit."
  (let ((nelisp-aot-compiler-tco-enabled t) (i 37) (x 11) (y 19) (call-exits 0)
        (plan (symbol-function 'nelisp-aot-compiler--x86-self-tco-entry-plan))
        (call (symbol-function 'nelisp-aot-compiler--emit-call)))
    (while (> i 0)
      (let* ((next-i (1- i)) (next-x (+ x next-i)) (next-y (+ y x)))
        (setq i next-i x next-x y next-y)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--x86-self-tco-entry-plan)
               (lambda (ir)
                 (let ((result (funcall plan ir)))
                   (when (and result (not (plist-get result :inline-exit)))
                     (setq call-exits (1+ call-exits))) result)))
              ((symbol-function 'nelisp-aot-compiler--emit-call)
               (lambda (node buf)
                 (should-not nelisp-aot-compiler--promoted-slots)
                 (funcall call node buf))))
      (should
       (= 0 (nelisp-aot-codegen-test--run
             `(seq
               (defun finish (h k n) (+ h (+ (* k 11) (* n 13))))
               (defun hop (n h k)
                 (if (> n 0) (hop (- n 1) (+ h k) k) (finish h k n)))
               (defun mutate (n x y)
                 (if (> n 0) (mutate (setq n (- n 1)) (+ x n) (+ y x))
                   (+ x (* y 7))))
               (exit (if (= (hop 37 11 17) ,(+ 11 (* 37 17) (* 17 11)))
                         (if (= (mutate 37 11 19) ,(+ x (* y 7))) 0 2) 1)))))))
    (should (> call-exits 0))))

(ert-deftest nelisp-aot-codegen/self-tco-provenance-and-fallback ()
  "Plain source while, allocating/calling loops and protected frames retain lowering."
  (let* ((nelisp-aot-compiler-tco-enabled t)
         (nelisp-aot-compiler--arch 'x86_64) (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--checked-arith nil)
         (nelisp-aot-compiler--runtime-entry-params nil)
         (nelisp-aot-compiler--promoted-slots nil)
         (nelisp-aot-compiler--x86-loop-regs '(rbx r8 r9 r12 r13 r14 r15))
         (plain (nelisp-aot-compiler--parse
                 '(defun plain (n h) (let ((n n) (h h))
                                      (seq (while (> n 0) (setq h (+ h n)) (setq n (- n 1))) h)))))
         (generated (nelisp-aot-compiler--parse
                     '(defun looped (n h) (if (> n 0) (looped (- n 1) (+ h n)) h)))))
    (should-not (nelisp-aot-compiler--ir-get plain :self-tco-raw))
    (should-not (nelisp-aot-compiler--x86-self-tco-entry-plan plain))
    (should (nelisp-aot-compiler--ir-get generated :self-tco-raw))
    (should (nelisp-aot-compiler--x86-self-tco-entry-plan generated))
    (dolist (field '((:variadic . t) (:gc-root-slots . (0))
                    (:internal-entry . t) (:param-class . f64)
                    (:param-regs . (a b c d e f g))))
      (should-not (nelisp-aot-compiler--x86-self-tco-entry-plan
                   (nelisp-aot-compiler--ir-with generated (car field) (cdr field)))))
    (let ((nelisp-aot-compiler--checked-arith t))
      (should-not (nelisp-aot-compiler--x86-self-tco-entry-plan generated)))
    (let ((nelisp-aot-compiler--runtime-entry-params '(out)))
      (should-not (nelisp-aot-compiler--x86-self-tco-entry-plan generated)))
    (let ((plans 0) (plan (symbol-function 'nelisp-aot-compiler--x86-self-tco-entry-plan)))
      (cl-letf (((symbol-function 'nelisp-aot-compiler--x86-self-tco-entry-plan)
                 (lambda (ir) (let ((result (funcall plan ir)))
                                (when result (setq plans (1+ plans))) result))))
        (should (= 122 (nelisp-aot-codegen-test--run
                        '(seq (defun helper (x) (+ x 3))
                              (defun hop (n h) (if (> n 0) (hop (- n 1) (helper h)) h))
                              (exit (hop 37 11)))))))
      (should (zerop plans)))))

(ert-deftest nelisp-aot-codegen/self-tco-native-callee-saves ()
  "Entry forwarding and inline exit restore RBX and R12-R15 before returning."
  (skip-unless (and (executable-find "gcc") (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (let ((nelisp-aot-compiler-tco-enabled t)
        (object (make-temp-file "nelisp-tco-entry-" nil ".o"))
        (source (make-temp-file "nelisp-tco-entry-" nil ".c"))
        (binary (make-temp-file "nelisp-tco-entry-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun hop (n a b c d e)
              (if (> n 0) (hop (- n 1) (+ a b) (+ b c) (+ c d) (+ d e) (+ e a))
                (+ a (+ (* b 3) (+ (* c 5) (+ (* d 7) (* e 11))))))) object)
          (with-temp-file source
            (insert "extern long hop(long,long,long,long,long,long);\n"
                    "int main(void) {\n"
                    " register long b __asm__(\"rbx\")=101, c __asm__(\"r12\")=102,\n"
                    " d __asm__(\"r13\")=103, e __asm__(\"r14\")=104, f __asm__(\"r15\")=105;\n"
                    " __asm__ volatile(\"\" : \"+r\"(b),\"+r\"(c),\"+r\"(d),\"+r\"(e),\"+r\"(f));\n"
                    " long value=hop(2,1,2,3,4,5);\n"
                    " __asm__ volatile(\"\" : \"+r\"(b),\"+r\"(c),\"+r\"(d),\"+r\"(e),\"+r\"(f));\n"
                    " return value!=328 || b!=101 || c!=102 || d!=103 || e!=104 || f!=105; }\n"))
          (should (= 0 (call-process "gcc" nil nil nil "-O2" source object "-o" binary)))
          (should (= 0 (call-process binary nil nil nil))))
      (dolist (path (list object source binary)) (delete-file path)))))

(ert-deftest nelisp-aot-codegen/leaf-entry-cycle-and-full-word ()
  "Acyclic parameter references use CG3 registers, including R8/R9 cycles."
  (let ((plans 0) (cycles 0)
        (emit (symbol-function 'nelisp-aot-compiler--emit-tco-parallel-moves-x86)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-tco-parallel-moves-x86)
               (lambda (moves buf)
                 (when nelisp-aot-compiler--x86-leaf-entry-state
                   (setq plans (1+ plans))
                   (when (and (member '(r8 . r9) moves) (member '(r9 . r8) moves))
                     (setq cycles (1+ cycles))))
                 (funcall emit moves buf))))
      (dolist (triple '((7 11 13) (0 0 0) (-1 1 -1)
                        (-9223372036854775808 9223372036854775807 -9223372036854775808)
                        (9223372036854775807 -9223372036854775808 9223372036854775807)))
        (let* ((a (nth 0 triple)) (e (nth 1 triple)) (f (nth 2 triple))
               (expected (logand (+ (* a 5) (* f 4) (* e 3) 2 3 5) (1- (ash 1 64)))))
          (when (>= expected (ash 1 63)) (setq expected (- expected (ash 1 64))))
          (should
           (= 0 (nelisp-aot-codegen-test--run
                 `(seq
                   (defun leaf (a b c d e f)
                     (+ a (+ a (+ a (+ a (+ a (+ f (+ f (+ f (+ f
                        (+ e (+ e (+ e (+ b (+ c d)))))))))))))))
                   (exit (if (= (leaf ,a 2 3 5 ,e ,f) ,expected) 0 1)))))))))
    (should (> plans 0))
    (should (> cycles 0))))

(ert-deftest nelisp-aot-codegen/leaf-entry-mutation-alias-and-memory ()
  "Actual slots preserve branches, lexical shadows, aliases and memory effects."
  (dolist (p '(0 17 -9223372036854775808 9223372036854775807))
    (dolist (c '(-1 1))
      (let* ((q 7) (old p) (v (+ p q)) (next-p p))
        (if (< c 0) (setq next-p (+ p 7) v (- v next-p)) (setq next-p (- p 9)))
        (setq v (+ v (* (+ q 3) 2)))
        (let ((expected (logand (+ old next-p v q q) (1- (ash 1 64)))))
          (when (>= expected (ash 1 63)) (setq expected (- expected (ash 1 64))))
          (should
           (= 0 (nelisp-aot-codegen-test--run
                 `(seq
                   (defun leaf (p q c)
                     (let* ((old p) (v (+ p q)))
                       (seq (if (< c 0) (seq (setq p (+ p 7)) (setq v (- v p)))
                              (setq p (- p 9)))
                            (let ((p (+ q 3))) (setq v (+ v (+ p p))))
                            (+ old (+ p (+ v (+ q q)))))))
                   (exit (if (= (leaf ,p ,q ,c) ,expected) 0 1))))))))))
  (should
   (= 0 (nelisp-aot-codegen-test--run
         '(seq
           (defun leaf (p q n)
             (let ((alias p))
               (seq (ptr-write-u8 p 0 19)
                    (ptr-write-u16 q 2 4660)
                    (setq n (+ n 1))
                    (if (> n 0) (ptr-write-u32 alias 4 n) (ptr-write-u32 q 4 0))
                    (ptr-write-u64 p 8 (+ n (ptr-read-u32 q 4)))
                    (+ (ptr-read-u8 alias 0)
                       (+ (ptr-read-u16 p 2) (+ (ptr-read-u32 q 4) (ptr-read-u64 alias 8)))))))
           (defun mmap-page () (syscall-direct 9 0 4096 3 34 -1 0))
           (defun main () (let ((p (mmap-page))) (if (= (leaf p p 5) 4697) 0 1)))
           (exit (main)))))))

(ert-deftest nelisp-aot-codegen/leaf-entry-protected-and-loop-fallback ()
  "Calls, loops, implicit clobbers and protected frames retain ordinary lowering."
  (let* ((nelisp-aot-compiler--arch 'x86_64) (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--checked-arith nil)
         (nelisp-aot-compiler--runtime-entry-params nil)
         (nelisp-aot-compiler--promoted-slots nil)
         (nelisp-aot-compiler--x86-loop-regs '(rbx r8 r9 r12 r13 r14 r15))
         (plain (nelisp-aot-compiler--parse '(defun leaf (x) (+ x (+ x (+ x x)))))))
    (should (nelisp-aot-compiler--x86-leaf-entry-plan plain))
    (should-not
     (nelisp-aot-compiler--x86-leaf-entry-plan
      (nelisp-aot-compiler--ir-with
       plain :body (nelisp-aot-compiler--make-ir
                    'value-seq :forms (list (nelisp-aot-compiler--ir-get plain :body)
                                            (nelisp-aot-compiler--make-ir 'value-seq :forms nil))))))
    (dolist (field '((:variadic . t) (:optional-count . 1) (:rest-p . t) (:ret-class . f64)
                    (:gc-root-slots . (0)) (:internal-entry . t)
                    (:param-class . f64) (:param-regs . (a b c d e f g))))
      (should-not (nelisp-aot-compiler--x86-leaf-entry-plan
                   (nelisp-aot-compiler--ir-with plain (car field) (cdr field)))))
    (let ((nelisp-aot-compiler--checked-arith t))
      (should-not (nelisp-aot-compiler--x86-leaf-entry-plan plain)))
    (let ((nelisp-aot-compiler--runtime-entry-params '(out)))
      (should-not (nelisp-aot-compiler--x86-leaf-entry-plan plain)))
    (dolist (body '((seq (while (> x 0) (setq x (- x 1))) (+ x (+ x x)))
                    (+ x (+ x (+ x (extern-call helper x))))
                    (+ x (+ x (+ x (alloc-bytes 16 8))))
                    (+ x (+ x (+ x (addr-of x))))))
      (should-not (nelisp-aot-compiler--x86-leaf-entry-plan
                   (nelisp-aot-compiler--parse `(defun leaf (x) ,body))))))
  (let ((selected 0) (calls 0)
        (plan (symbol-function 'nelisp-aot-compiler--x86-leaf-entry-plan))
        (call (symbol-function 'nelisp-aot-compiler--emit-call)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--x86-leaf-entry-plan)
               (lambda (ir)
                 (let ((result (funcall plan ir)))
                   (when (and result (eq (nelisp-aot-compiler--ir-get ir :name) 'leaf))
                     (setq selected (1+ selected))) result)))
              ((symbol-function 'nelisp-aot-compiler--emit-call)
               (lambda (ir buf)
                 (setq calls (1+ calls))
                 (should-not nelisp-aot-compiler--promoted-slots)
                 (funcall call ir buf))))
      (should (= 99 (nelisp-aot-codegen-test--run
                     '(seq (defun helper (x) (+ x 3))
                           (defun leaf (x) (+ x (+ x (+ x (helper x)))))
                           (exit (leaf 24)))))))
    (should (> calls 0))
    (should (= selected 0))))

(ert-deftest nelisp-aot-codegen/leaf-entry-readonly-copy ()
  "Late copy sharing uses the source's actual slot inside the alias scope."
  (let ((copies 0) (copy (symbol-function 'nelisp-aot-compiler--x86-copy-let-source-register)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--x86-copy-let-source-register)
               (lambda (ir)
                 (let ((reg (funcall copy ir)))
                   (when (and reg nelisp-aot-compiler--x86-leaf-entry-state)
                     (setq copies (1+ copies))) reg))))
      (should (= 92 (nelisp-aot-codegen-test--run
                     '(seq (defun leaf (p)
                             (let ((alias p)) (+ alias (+ alias (+ alias p)))))
                           (exit (leaf 23)))))))
    (should (> copies 0))))

(ert-deftest nelisp-aot-codegen/leaf-entry-native-callee-saves ()
  "Whole-body placement restores RBX and every CG3 callee-saved register."
  (skip-unless (and (executable-find "gcc") (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (let* ((object (make-temp-file "nelisp-leaf-entry-" nil ".o"))
         (source (make-temp-file "nelisp-leaf-entry-" nil ".c"))
         (binary (make-temp-file "nelisp-leaf-entry-"))
         (terms (append (make-list 7 'v) (make-list 6 'a) (make-list 5 'b)
                        (make-list 4 'c) (make-list 4 'd) (make-list 4 'e) (make-list 4 'f)))
         (body (cl-reduce (lambda (a b) (list '+ a b)) terms :from-end t)))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object `(defun leaf (a b c d e f) (let ((v (+ a b))) ,body)) object)
          (with-temp-file source
            (insert "extern long leaf(long,long,long,long,long,long);\n"
                    "int main(void) {\n"
                    " register long b __asm__(\"rbx\")=101, c __asm__(\"r12\")=102,\n"
                    " d __asm__(\"r13\")=103, e __asm__(\"r14\")=104, f __asm__(\"r15\")=105;\n"
                    " __asm__ volatile(\"\" : \"+r\"(b),\"+r\"(c),\"+r\"(d),\"+r\"(e),\"+r\"(f));\n"
                    " long value=leaf(1,2,3,4,5,6);\n"
                    " __asm__ volatile(\"\" : \"+r\"(b),\"+r\"(c),\"+r\"(d),\"+r\"(e),\"+r\"(f));\n"
                    " return value!=109 || b!=101 || c!=102 || d!=103 || e!=104 || f!=105; }\n"))
          (should (= 0 (call-process "gcc" nil nil nil "-O2" source object "-o" binary)))
          (should (= 0 (call-process binary nil nil nil))))
      (dolist (path (list object source binary)) (delete-file path)))))

(ert-deftest nelisp-aot-codegen/frameless-leaf-cycles-and-no-frame-access ()
  "A complete existing plan omits frame accesses and preserves incoming cycles."
  (let* ((terms (append (make-list 9 'a) (make-list 8 'f) (make-list 7 'e)
                        (make-list 6 'b) (make-list 5 'c) (make-list 4 'd)))
         (body (cl-reduce (lambda (a b) (list '+ a b)) terms :from-end t))
         (selected 0) (cycles 0)
         (leaf (symbol-function 'nelisp-aot-compiler--emit-leaf-body-x86))
         (move (symbol-function 'nelisp-aot-compiler--emit-tco-parallel-moves-x86))
         (op (symbol-function 'nelisp-aot-compiler--x86-op))
         (load (symbol-function 'nelisp-asm-x86_64-mov-reg-mem-disp8))
         (store (symbol-function 'nelisp-asm-x86_64-mov-mem-reg-disp8)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-leaf-body-x86)
               (lambda (node buf)
                 (when (plist-get nelisp-aot-compiler--x86-leaf-entry-state :frameless)
                   (setq selected (1+ selected)))
                 (funcall leaf node buf)))
              ((symbol-function 'nelisp-aot-compiler--emit-tco-parallel-moves-x86)
               (lambda (moves buf)
                 (when (and (plist-get nelisp-aot-compiler--x86-leaf-entry-state :frameless)
                            (member '(r8 . r9) moves) (member '(r9 . r8) moves))
                   (setq cycles (1+ cycles)))
                 (funcall move moves buf)))
              ((symbol-function 'nelisp-aot-compiler--x86-op)
               (lambda (buf w opcode reg rm &rest rest)
                 (when (and (plist-get nelisp-aot-compiler--x86-leaf-entry-state :frameless)
                            (listp rm))
                   (should-not (memq 'rbp rm)))
                 (apply op buf w opcode reg rm rest)))
              ((symbol-function 'nelisp-asm-x86_64-mov-reg-mem-disp8)
               (lambda (buf reg base disp)
                 (when (plist-get nelisp-aot-compiler--x86-leaf-entry-state :frameless)
                   (should-not (eq base 'rbp)))
                 (funcall load buf reg base disp)))
              ((symbol-function 'nelisp-asm-x86_64-mov-mem-reg-disp8)
               (lambda (buf base disp reg)
                 (when (plist-get nelisp-aot-compiler--x86-leaf-entry-state :frameless)
                   (should-not (eq base 'rbp)))
                 (funcall store buf base disp reg))))
      (dolist (pair '((1 6) (-9223372036854775808 9223372036854775807)
                      (9223372036854775807 -9223372036854775808)))
        (let ((expected (logand (+ (* (car pair) 9) (* (cadr pair) 8)
                                  (* 5 7) (* 2 6) (* 3 5) (* 4 4)) (1- (ash 1 64)))))
          (when (>= expected (ash 1 63)) (setq expected (- expected (ash 1 64))))
          (should (= 0 (nelisp-aot-codegen-test--run
                        `(seq (defun leaf (a b c d e f) ,body)
                              (exit (if (= (leaf ,(car pair) 2 3 4 5 ,(cadr pair)) ,expected)
                                        0 1)))))))))
    (should (> selected 0))
    (should (> cycles 0))))

(ert-deftest nelisp-aot-codegen/frameless-leaf-coverage-fallback ()
  "Any access omitted by the unchanged plan retains the ordinary frame."
  (let* ((nelisp-aot-compiler--arch 'x86_64) (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--checked-arith nil)
         (nelisp-aot-compiler--runtime-entry-params nil)
         (nelisp-aot-compiler--promoted-slots nil)
         (nelisp-aot-compiler--x86-loop-regs '(rbx r8 r9 r12 r13 r14 r15))
         (simple (nelisp-aot-compiler--parse '(defun leaf (x) (+ x (+ x (+ x x))))))
         (full (nelisp-aot-compiler--x86-leaf-entry-plan simple))
         (terms (append (make-list 9 'a) (make-list 8 'f) (make-list 7 'e) '(b c d)))
         (body (cl-reduce (lambda (a b) (list '+ a b)) terms :from-end t))
         (partial (nelisp-aot-compiler--x86-leaf-entry-plan
                   (nelisp-aot-compiler--parse `(defun leaf (a b c d e f) ,body)))))
    (should (plist-get full :frameless))
    (should-not (nelisp-aot-compiler--x86-leaf-frame-free-p
                 (nelisp-aot-compiler--ir-get simple :body) nil))
    (should partial)
    (should-not (plist-get partial :frameless))
    (should (= 101 (nelisp-aot-codegen-test--run
                    `(seq (defun leaf (a b c d e f) ,body)
                          (exit (leaf 1 2 3 4 5 6))))))))

(ert-deftest nelisp-aot-codegen/frameless-leaf-native-callee-saves ()
  "Frameless branches preserve RBP, RBX and R12-R15 for a native C caller."
  (skip-unless (and (executable-find "gcc") (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (let* ((object (make-temp-file "nelisp-frameless-" nil ".o"))
         (source (make-temp-file "nelisp-frameless-" nil ".c"))
         (binary (make-temp-file "nelisp-frameless-"))
         (terms (append (make-list 7 'v) (make-list 6 'a) (make-list 5 'b)
                        (make-list 4 'c) (make-list 4 'd) (make-list 4 'e) (make-list 4 'f)))
         (body (cl-reduce (lambda (a b) (list '+ a b)) terms :from-end t))
         (selected 0) (leaf (symbol-function 'nelisp-aot-compiler--emit-leaf-body-x86)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-leaf-body-x86)
                     (lambda (node buf)
                       (when (plist-get nelisp-aot-compiler--x86-leaf-entry-state :frameless)
                         (setq selected (1+ selected)))
                       (funcall leaf node buf))))
            (nelisp-aot-compile-to-object
             `(defun leaf (a b c d e f) (let ((v (+ a b))) (if (> a 0) ,body (- 0 ,body)))) object))
          (should (> selected 0))
          (with-temp-file source
            (insert "extern long leaf(long,long,long,long,long,long);\n"
                    "int main(void) {\n"
                    " register long b __asm__(\"rbx\")=101, c __asm__(\"r12\")=102,\n"
                    " d __asm__(\"r13\")=103, e __asm__(\"r14\")=104, f __asm__(\"r15\")=105,\n"
                    " g __asm__(\"rbp\")=106;\n"
                    " __asm__ volatile(\"\" : \"+r\"(b),\"+r\"(c),\"+r\"(d),\"+r\"(e),\"+r\"(f),\"+r\"(g));\n"
                    " long value=leaf(1,2,3,4,5,6), other=leaf(-1,2,3,4,5,6);\n"
                    " __asm__ volatile(\"\" : \"+r\"(b),\"+r\"(c),\"+r\"(d),\"+r\"(e),\"+r\"(f),\"+r\"(g));\n"
                    " return value!=109 || other!=-83 || b!=101 || c!=102 || d!=103 || e!=104 || f!=105 || g!=106; }\n"))
          (should (= 0 (call-process "gcc" nil nil nil "-O2" "-fomit-frame-pointer" source object "-o" binary)))
          (should (= 0 (call-process binary nil nil nil))))
      (dolist (path (list object source binary)) (delete-file path)))))

(defun nelisp-aot-codegen-test--terminal-string-form ()
  '(defun late_equal (left right mode)
     (let ((alias left))
       (if (and (= (sexp-tag left) 5) (= (sexp-tag right) 5)
                (= alias left) (= left alias) (= alias left) (= left alias))
           (if (> mode 0) (str-eq alias right) mode)
         mode))))

(ert-deftest nelisp-aot-codegen/terminal-barrier-loops-mutation-full-word ()
  "Pure loop branches and terminal argument mutations preserve all result bits."
  (let ((barriers 0) (calls 0)
        (emit (symbol-function 'nelisp-aot-compiler--emit-terminal-barrier-x86))
        (call (symbol-function 'nelisp-aot-compiler--emit-call)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-terminal-barrier-x86)
               (lambda (ir buf) (setq barriers (1+ barriers)) (funcall emit ir buf)))
              ((symbol-function 'nelisp-aot-compiler--emit-call)
               (lambda (ir buf) (setq calls (1+ calls))
                 (should-not nelisp-aot-compiler--promoted-slots) (funcall call ir buf))))
      (dolist (n '(-1 0 1 7))
        (dolist (pair '((11 17) (-9223372036854775808 9223372036854775807)))
          (let* ((x (car pair)) (y (+ (cadr pair) 7)) (old x) (acc (+ x (cadr pair)))
                 (expected (if (> n 0) (+ acc (* n old) y old)
                             (setq x (+ x y)) (+ x (* (+ old x) 3) (* (+ acc y) 5)))))
            (setq expected (logand expected (1- (ash 1 64))))
            (when (>= expected (ash 1 63)) (setq expected (- expected (ash 1 64))))
            (should (= 0 (nelisp-aot-codegen-test--run
                          `(seq
                            (defun finish (a b c) (+ a (+ (* b 3) (* c 5))))
                            (defun choose (n x y)
                              (let* ((old x) (acc (+ x y)))
                                (seq (setq y (+ y 7))
                                     (if (> n 0)
                                         (seq (while (> n 0) (setq acc (+ acc old)) (setq n (- n 1)))
                                              (+ acc (+ y old)))
                                       (finish (setq x (+ x y)) (+ old x) (+ acc y))))))
                            (exit (if (= (choose ,n ,(car pair) ,(cadr pair)) ,expected) 0 1))))))))))
    (should (> barriers 0)) (should (> calls 0))))

(ert-deftest nelisp-aot-codegen/terminal-barrier-dynamic-alias-flush ()
  "Flush each actual slot once, using its innermost alias register."
  (let ((aliases 0) (flush (symbol-function 'nelisp-aot-compiler--flush-promoted-slots-x86))
        (op (symbol-function 'nelisp-aot-compiler--x86-op)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--flush-promoted-slots-x86)
               (lambda (buf)
                 (let* ((map nelisp-aot-compiler--promoted-slots)
                        (slots (delete-dups (mapcar #'car map))) (stores nil))
                   (when (< (length slots) (length map)) (setq aliases (1+ aliases)))
                   (cl-letf (((symbol-function 'nelisp-aot-compiler--x86-op)
                              (lambda (b w opcode reg rm &rest rest)
                                (when (equal opcode (unibyte-string #x89))
                                  (let ((slot (cl-find-if
                                               (lambda (s) (equal rm (nelisp-aot-compiler--x86-slot-mem s)))
                                               slots)))
                                    (should (integerp slot))
                                    (should (eq reg (cdr (assq slot map))))
                                    (push slot stores)))
                                (apply op b w opcode reg rm rest))))
                     (funcall flush buf))
                   (should (= (length stores) (length slots)))
                   (should (= (length stores) (length (delete-dups stores))))))))
      (let* ((terms (append (make-list 9 'a) (make-list 8 'alias) (make-list 7 'b)))
             (sum (cl-reduce (lambda (a b) (list '+ a b)) terms :from-end t)))
        (should (= 183 (nelisp-aot-codegen-test--run
                       `(seq (defun finish (a b c) (+ a (+ (* b 3) (* c 5))))
                             (defun choose (a b)
                               (let ((alias a))
                                 (if (> ,sum 0) (finish alias a b) 0)))
                             (exit (choose 17 23))))))))
    (should (> aliases 0))))

(ert-deftest nelisp-aot-codegen/terminal-barrier-late-string-word-guards ()
  "String fallback stays behind WORD tag guards and ends the register map."
  (let ((strings 0) (emit (symbol-function 'nelisp-aot-compiler--emit-str-eq)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-str-eq)
               (lambda (ir buf) (setq strings (1+ strings))
                 (should-not nelisp-aot-compiler--promoted-slots) (funcall emit ir buf))))
      (should
       (= 0 (nelisp-aot-codegen-test--run
             `(seq ,(nelisp-aot-codegen-test--terminal-string-form)
               (defun mmap-page () (syscall-direct 9 0 4096 3 34 -1 0))
               (defun run (mem)
                 (let ((left (+ mem 32)) (right (+ mem 64)) (a (+ mem 128)) (b (+ mem 160)))
                   (seq
                    (ptr-write-u8 left 0 5) (ptr-write-u64 left 8 3)
                    (ptr-write-u64 left 16 a) (ptr-write-u64 left 24 3)
                    (ptr-write-u8 right 0 5) (ptr-write-u64 right 8 3)
                    (ptr-write-u64 right 16 b) (ptr-write-u64 right 24 3)
                    (ptr-write-u8 a 0 97) (ptr-write-u8 a 1 98) (ptr-write-u8 a 2 99)
                    (ptr-write-u8 b 0 97) (ptr-write-u8 b 1 98) (ptr-write-u8 b 2 99)
                    (ptr-write-u8 mem 0 73) (ptr-write-u8 mem 6 5)
                    (if (= (late_equal left right 1) 1)
                        (seq (ptr-write-u8 b 1 120)
                             (if (= (late_equal left right 1) 0)
                                 (if (= (late_equal 2 right -9223372036854775808) -9223372036854775808)
                                     (if (= (late_equal (+ mem 6) right 9223372036854775807) 9223372036854775807)
                                         (if (= (late_equal left 2 -9223372036854775808) -9223372036854775808) 0 5)
                                       4) 3) 2))
                      1))))
               (exit (run (mmap-page))))))))
    (should (> strings 0))))

(ert-deftest nelisp-aot-codegen/terminal-barrier-position-and-protected-fallback ()
  "Only structural terminal positions qualify; protected/raw-call protocols reject."
  (let* ((nelisp-aot-compiler--arch 'x86_64) (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--checked-arith nil)
         (nelisp-aot-compiler--runtime-entry-params nil)
         (nelisp-aot-compiler--promoted-slots nil)
         (nelisp-aot-compiler--x86-loop-regs '(rbx r8 r9 r12 r13 r14 r15))
         (plain (nelisp-aot-compiler--parse
                 '(defun f (x) (if (> (+ x (+ x (+ x x))) 0) (extern-call sink x) x)))))
    (should (nelisp-aot-compiler--x86-terminal-entry-plan plain))
    (should (nelisp-aot-compiler--x86-terminal-entry-plan
             (nelisp-aot-compiler--parse
              '(defun f (x) (cond ((> (+ x (+ x (+ x x))) 0)
                                   (let ((y (+ x 1))) (extern-call sink y))) (t x))))))
    (dolist (body '((if (extern-call sink x) (+ x x) x)
                    (let ((y (extern-call sink x))) (+ x (+ x (+ x y))))
                    (seq (extern-call sink x) (+ x (+ x x)))
                    (+ x (+ x (+ x (extern-call sink x))))
                    (seq (while (> x 0) (extern-call sink x) (setq x (- x 1))) x)))
      (should-not (nelisp-aot-compiler--x86-terminal-entry-plan
                   (nelisp-aot-compiler--parse `(defun f (x) ,body)))))
    (dolist (field '((:self-tco-raw . t) (:gc-root-slots . (0)) (:internal-entry . t)
                    (:variadic . t) (:optional-count . 1) (:rest-p . t) (:param-class . f64)))
      (should-not (nelisp-aot-compiler--x86-terminal-entry-plan
                   (nelisp-aot-compiler--ir-with plain (car field) (cdr field)))))
    (let ((nelisp-aot-compiler--checked-arith t))
      (should-not (nelisp-aot-compiler--x86-terminal-entry-plan plain)))
    (let ((nelisp-aot-compiler--runtime-entry-params '(out)))
      (should-not (nelisp-aot-compiler--x86-terminal-entry-plan plain)))
    (let* ((call (car (nelisp-aot-compiler--x86-terminal-barriers
                      (nelisp-aot-compiler--ir-get plain :body))))
           (shared (nelisp-aot-compiler--make-ir 'value-seq :forms (list call call))))
      (should-not (nelisp-aot-compiler--x86-terminal-barriers shared))
      (dolist (field '((:internal-boundary-disp . -8) (:fn-value . t)
                      (:ret-class . f64) (:arg-classes . (f64)) (:varargs-p . t)))
        (should-not (nelisp-aot-compiler--x86-terminal-barriers
                     (nelisp-aot-compiler--ir-with call (car field) (cdr field))))))))

(ert-deftest nelisp-aot-codegen/terminal-barrier-native-callee-saves ()
  "An odd save count aligns ordinary calls and both barriers preserve the C ABI."
  (skip-unless (and (executable-find "gcc") (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (let* ((object (make-temp-file "nelisp-terminal-" nil ".o"))
         (source (make-temp-file "nelisp-terminal-" nil ".c"))
         (binary (make-temp-file "nelisp-terminal-"))
         (terms (append (make-list 6 'a) (make-list 5 'b) (make-list 4 'c)
                        (make-list 4 'd) (make-list 4 'e) (make-list 4 'f)))
         (sum (cl-reduce (lambda (a b) (list '+ a b)) terms :from-end t))
         (odd-saves 0) (strings 0)
         (leaf (symbol-function 'nelisp-aot-compiler--emit-leaf-body-x86))
         (ext (symbol-function 'nelisp-aot-compiler--emit-extern-call)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-leaf-body-x86)
                     (lambda (ir buf)
                       (let* ((state nelisp-aot-compiler--x86-leaf-entry-state)
                              (kinds (mapcar #'nelisp-aot-compiler--ir-kind
                                             (plist-get state :terminal-barriers))))
                         (when (memq 'extern-call kinds)
                           (should (= 1 (logand 1 (cl-count-if
                                                  (lambda (p) (memq (cdr p) '(r12 r13 r14 r15)))
                                                  (plist-get state :promotions)))))
                           (setq odd-saves (1+ odd-saves)))
                         (when (memq 'str-eq kinds) (setq strings (1+ strings))))
                       (funcall leaf ir buf)))
                    ((symbol-function 'nelisp-aot-compiler--emit-extern-call)
                     (lambda (ir buf)
                       (should-not nelisp-aot-compiler--promoted-slots) (funcall ext ir buf))))
            (nelisp-aot-compile-to-object
             `(seq (defun route (a b c d e f)
                     (if (> ,sum 0) (extern-call sink (setq a (+ a 1)) (+ b a) c d e f) (- 0 ,sum)))
                   ,(nelisp-aot-codegen-test--terminal-string-form)) object))
          (should (> odd-saves 0)) (should (> strings 0))
          (with-temp-file source
            (insert "#include <stdint.h>\n#include <xmmintrin.h>\n"
                    "extern long route(long,long,long,long,long,long);\nextern long late_equal(void*,void*,long);\n"
                    "__attribute__((noinline)) long sink(long a,long b,long c,long d,long e,long f) { volatile __m128 v=_mm_set1_ps(1); __asm__ volatile(\"\" : \"+m\"(v)); return a+3*b+5*c+7*d+11*e+13*f+(int)v[0]; }\n"
                    "struct S { unsigned char tag,pad[7]; uint64_t cap; const char *ptr; uint64_t len; };\n"
                    "int main(void) { struct S x={5,{0},3,\"abc\",3}, y={5,{0},3,\"abc\",3};\n"
                    " register long b __asm__(\"rbx\")=101,c __asm__(\"r12\")=102,d __asm__(\"r13\")=103,\n"
                    " e __asm__(\"r14\")=104,f __asm__(\"r15\")=105,g __asm__(\"rbp\")=106;\n"
                    " __asm__ volatile(\"\" : \"+r\"(b),\"+r\"(c),\"+r\"(d),\"+r\"(e),\"+r\"(f),\"+r\"(g));\n"
                    " long v=route(1,2,3,4,5,6),w=route(-1,-1,-1,-1,-1,-1),s=late_equal(&x,&y,1);\n"
                    " y.ptr=\"axc\"; long t=late_equal(&x,&y,1),u=late_equal(&x,&y,INT64_MIN);\n"
                    " __asm__ volatile(\"\" : \"+r\"(b),\"+r\"(c),\"+r\"(d),\"+r\"(e),\"+r\"(f),\"+r\"(g));\n"
                    " return v!=191||w!=27||s!=1||t!=0||u!=INT64_MIN||b!=101||c!=102||d!=103||e!=104||f!=105||g!=106; }\n"))
          (should (= 0 (call-process "gcc" nil nil nil "-O2" "-fomit-frame-pointer" source object "-o" binary)))
          (should (= 0 (call-process binary nil nil nil))))
      (dolist (path (list object source binary)) (delete-file path)))))

(nelisp-aot-codegen-test--deftest nelisp-aot-codegen/conditions
  "and/or chains in if, while and cond, rotated loops with zero trips."
  (seq
   (defun f (a b c)
     (let ((r 0))
       (if (and (< a b) (or (= c 0) (> c 5))) (setq r (+ r 1)) 0)
       (if (or (and (> a 10) (< b 0)) (not (= c 3))) (setq r (+ r 2)) 0)
       (while (and (< a b) (/= c 0))
         (setq a (+ a 1))
         (setq c (- c 1))
         (setq r (+ r 4)))
       (while (or (> a 100) (and (= c 77) (< a 0))) (setq a (- a 1)))
       (cond ((and (= a b) (= c 0)) (setq r (+ r 100)))
             ((or (> a b) (= c 9)) (setq r (+ r 50)))
             (t (setq r (+ r 30))))
       r))
   ;; f(1,5,0) = 1+2+0+30 = 33; f(4,2,7) = 0+2+0+50 = 52;
   ;; f(0,3,2) = 0+2+8+30 = 40; f(2,9,7) = 1+2+28+100 = 131
   (exit (logand (+ 7 (+ (f 1 5 0) (+ (f 4 2 7) (+ (f 0 3 2) (f 2 9 7))))) 255)))
  ;; 7 + 33 + 52 + 40 + 131 = 263 -> 7
  7)

(nelisp-aot-codegen-test--deftest nelisp-aot-codegen/in-place-updates
  "setq of leaves and of (OP x LEAF) / (OP x PURE) in effect context."
  (seq
   (defun g (x y)
     (let ((a 0) (b 7) (c 0))
       (setq a x)
       (setq a (+ a 5))
       (setq a (- a y))
       (setq b (logand b 3))
       (setq b (logior b 8))
       (setq b (logxor b 1))
       (setq c (+ c (* x 3)))
       (setq c (- c (+ y 1)))
       (if (> x y) (setq c (+ c 100)) 0)
       (if (< x y) 0 (setq c (+ c 1000)))
       (seq (setq a (+ a 1)) (setq b (+ b 1)))
       (+ a (+ (* b 10) c))))
   ;; g(9,4): a=9+5-4+1=11; b=(7&3)|8=11 ^1=10 +1=11; c=27-5+100+1000=1122
   ;; -> 11 + 110 + 1122 = 1243 -> 1243 & 255 = 219
   (exit (logand (g 9 4) 255)))
  219)

(nelisp-aot-codegen-test--deftest nelisp-aot-codegen/promoted-loop-many-vars
  "A call-free loop with more variables than registers, read after it."
  (seq
   (defun h (n)
     (let ((i 0) (s1 0) (s2 1) (s3 2) (s4 3) (s5 4) (s6 5) (s7 6) (s8 7) (s9 8))
       (while (< i n)
         (setq s1 (+ s1 i))
         (setq s2 (+ s2 s1))
         (setq s3 (logxor s3 s2))
         (setq s4 (+ s4 (logand s3 7)))
         (setq s5 (- s5 1))
         (setq s6 (+ s6 s5))
         (setq s7 (+ s7 (sar s6 1)))
         (setq s8 (+ s8 s7))
         (setq s9 (logand (+ s9 s8) 1023))
         (setq i (+ i 1)))
       (logand (+ s1 (+ s2 (+ s3 (+ s4 (+ s5 (+ s6 (+ s7 (+ s8 s9)))))))) 255)))
   (exit (h 37)))
  ;; A Python model of the same loop (Python ints, >> as `sar') gives 228.
  228)

(nelisp-aot-codegen-test--deftest nelisp-aot-codegen/promoted-nested-loops
  "Nested call-free loops with a runtime let inside the body."
  (seq
   (defun k (n)
     (let ((i 0) (total 0))
       (while (< i n)
         (let ((j 0) (row 0))
           (while (<= j i)
             (setq row (+ row (* j j)))
             (setq j (+ j 1)))
           (setq total (+ total row)))
         (setq i (+ i 1)))
       total))
   ;; sum_{i<12} sum_{j<=i} j^2 = sum_{i<12} i(i+1)(2i+1)/6 = 1716
   ;; 1716 & 255 = 180
   (exit (logand (k 12) 255)))
  180)

(nelisp-aot-codegen-test--deftest nelisp-aot-codegen/addressing
  "Folded addresses: immediate, scaled index, index plus displacement,
absolute base, all widths, stores of immediates and registers, and
memory compares with large narrow constants and negative wide ones."
  (seq
   (defun mm (size) (syscall-direct 9 0 size 3 34 -1 0))
   (defun fixed () (syscall-direct 9 268500992 4096 3 50 -1 0))
   (defun a (n)
     (let ((p (mm 4096)) (i 0) (s 0) (q 0))
       (while (< i n)
         (ptr-write-u64 p (shl i 3) (- 0 i))
         (ptr-write-u32 p (+ (* i 4) 1024) (+ i 4000000000))
         (ptr-write-u16 p (+ (shl i 1) 2048) (+ i 65000))
         (ptr-write-u8 p (+ i 3072) (+ i 200))
         (setq i (+ i 1)))
       (setq i 0)
       (while (< i n)
         (if (< (ptr-read-u64 p (shl i 3)) -5) (setq s (+ s 1)) 0)
         (if (= (ptr-read-u8 p (+ i 3072)) 203) (setq s (+ s 10)) 0)
         (if (/= (ptr-read-u16 p (+ (shl i 1) 2048)) 65002) 0 (setq s (+ s 100)))
         (if (= (ptr-read-u32 p (+ (* i 4) 1024)) 4000000004) (setq s (+ s 1000)) 0)
         (setq q (+ q (ptr-read-u8 p (+ i 3072))))
         (setq i (+ i 1)))
       (setq q (+ q (ptr-read-u32 p 1028)))
       (fixed)
       (ptr-write-u64 268500992 16 77)
       (ptr-write-u8 268500992 24 5)
       (+ s (+ (logand q 255) (+ (ptr-read-u64 268500992 16)
                                 (ptr-read-u8 268500992 24))))))
   ;; n = 10: s = 4 (i = 6..9 have -i < -5) + 10 + 100 + 1000 = 1114;
   ;; q = sum(200..209) + 4000000001 = 4000002046, and q & 255 = 254
   (exit (logand (a 10) 255)))
  ;; 1114 + 254 + 77 + 5 = 1450 -> 1450 & 255 = 170
  170)

(nelisp-aot-codegen-test--deftest nelisp-aot-codegen/tag-tests
  "Fused (= (sexp-tag x) K) against boxes, nil, t, fixnums and an odd
non-fixnum word, in and out of a promoted loop, plus TAGGEDP tests."
  (seq
   (defun mm (size) (syscall-direct 9 0 size 3 34 -1 0))
   (defun tags (w)
     (let ((r 0))
       (if (= (sexp-tag w) 7) (setq r (+ r 1)) 0)
       (if (/= (sexp-tag w) 7) (setq r (+ r 2)) 0)
       (if (= (sexp-tag w) 0) (setq r (+ r 4)) 0)
       (if (= (sexp-tag w) 1) (setq r (+ r 8)) 0)
       (if (= (sexp-tag w) 2) (setq r (+ r 16)) 0)
       (if (= (logand w 3) 1) (setq r (+ r 32)) 0)
       (if (/= (logand w 7) 3) (setq r (+ r 64)) 0)
       r))
   (defun walk (w)
     (let ((n 0))
       (while (= (sexp-tag w) 7)
         (setq w (ptr-read-u64 (ptr-read-u64 w 8) 8))
         (setq n (+ n 1)))
       (+ n (* 10 (if (= (sexp-tag w) 0) 1 0)))))
   (defun main ()
     (let ((b (mm 4096)) (r 0))
       ;; box at b: tag 7 (cons), payload b+64 (cell), cdr at b+72 = box b+128
       (ptr-write-u8 b 0 7)
       (ptr-write-u64 b 8 (+ b 64))
       (ptr-write-u64 b 72 (+ b 128))
       ;; box b+128: tag 7, cell b+192, cdr = nil (3)
       (ptr-write-u8 b 128 7)
       (ptr-write-u64 b 136 (+ b 192))
       (ptr-write-u64 b 200 3)
       ;; box b+256: tag 0 (a boxed nil)
       (ptr-write-u8 b 256 0)
       (setq r (+ (tags b) (* 3 (tags 3))))
       (setq r (+ r (* 5 (tags 7))))
       (setq r (+ r (* 7 (tags 9))))
       (setq r (+ r (* 11 (tags 11))))
       (setq r (+ r (* 13 (tags (+ b 256)))))
       (+ r (walk b))))
   ;; tags(box7)=1+64=65; tags(3)=2+4=6; tags(7)=2+8+64=74;
   ;; tags(9)=2+16+32+64=114; tags(11)=2+8=10; tags(boxnil)=2+4+64=70
   ;; r = 65 + 18 + 370 + 798 + 110 + 910 = 2271; walk(b) = 2 + 10 = 12
   (exit (logand (main) 255)))
  ;; 2283 & 255 = 235
  235)

(nelisp-aot-codegen-test--deftest nelisp-aot-codegen/word-tag-tests
  "Fused tag tests preserve direct-cons and masked vectorlike WORD tags."
  (seq
   (defun tagged (w)
     (let ((r 0))
       (if (= (sexp-tag w) 7) (setq r (+ r 1)) 0)
       (if (/= (sexp-tag w) 7) (setq r (+ r 2)) 0)
       (if (= (sexp-tag w) 4) (setq r (+ r 4)) 0)
       (if (/= (sexp-tag w) 4) (setq r (+ r 8)) 0)
       r))
   (defun main ()
     (let ((b (syscall-direct 9 0 4096 3 34 -1 0)))
       ;; The direct cell's first byte deliberately disagrees with Cons.
       (ptr-write-u8 b 2 4)
       ;; Vectorlike header flag bits must not contribute to its kind.
       (ptr-write-u8 b 64 196)
       (ptr-write-u8 b 70 7)
       (+ (tagged (+ b 2)) (* 10 (tagged (+ b 70))))))
   (exit (main)))
  ;; Direct cons: 1+8=9; vectorlike kind4: 2+4=6.
  69)

;; If-conversion: a conditional register update becomes cmov (both arm
;; orders), and an update that may trap (division) keeps its branch.
(nelisp-aot-codegen-test--deftest nelisp-aot-codegen/if-conversion
  "Conditional updates in a promoted loop, as cmov and as a branch."
  (seq
   (defun cv (n)
     (let ((s 0) (t2 0) (u 0) (i 0))
       (while (< i n)
         (setq s (+ s i))
         (if (>= s 1009) (setq s (- s 1009)) 0)
         (setq t2 (+ t2 (* i 3)))
         (if (< t2 997) 0 (setq t2 (- t2 997)))
         (setq u (+ u i))
         (if (> u 500) (setq u (/ u 2)) 0)
         (setq i (+ i 1)))
       (logand (+ s (+ (* t2 7) (* u 13))) 255)))
   (exit (cv 300)))
  ;; A Python model of the same loop gives 240.
  240)

;; Globals: CG4 addresses `(ptr-read/write (data-addr SYM) OFF)' as
;; [rip + SYM + OFF]; the pc32 addend must also skip any immediate that
;; follows the displacement (stores and compares of constants).
(ert-deftest nelisp-aot-codegen/rip-relative-globals ()
  "Loads, stores and compares of a data blob at non-zero offsets."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
    (skip-unless cc)
    (let* ((dir (make-temp-file "nelisp-aot-codegen-rip" t))
           (obj (expand-file-name "g.o" dir))
           (drv (expand-file-name "drv.c" dir))
           (bin (expand-file-name "prog" dir)))
      (unwind-protect
          (progn
            (nelisp-aot-compile-to-object
             '(seq
               (data-blob gv (1 0 0 0 0 0 0 0  2 0 0 0 0 0 0 0
                              3 0 0 0 0 0 0 0  5 0 0 0 0 0 0 0) data)
               (defun probe ()
                 (let ((r 0))
                   (ptr-write-u64 (data-addr gv) 16 7)
                   (ptr-write-u8 (data-addr gv) 3 200)
                   (if (= (ptr-read-u64 (data-addr gv) 24) 5) (setq r (+ r 1)) 0)
                   (if (= (ptr-read-u8 (data-addr gv) 3) 200) (setq r (+ r 10)) 0)
                   (+ r (+ (* 100 (ptr-read-u64 (data-addr gv) 16))
                           (* 1000 (ptr-read-u64 (data-addr gv) 8)))))))
             obj)
            (with-temp-file drv
              (insert "extern long probe(void);\n"
                      "int main(void){ return probe() == 2711 ? 0 : 1; }\n"))
            (should (zerop (call-process cc nil nil nil drv obj "-o" bin)))
            (should (zerop (call-process bin nil nil nil))))
        (ignore-errors (delete-directory dir t))))))

;; The standalone build links its own units with `nelisp-static-linker',
;; which measures PC32 from the end of the field; the unit builder must
;; therefore bias the assembler's RELA addend by +4 and keep the rest of
;; it (a RIP-relative operand's displacement and trailing immediate).
(ert-deftest nelisp-aot-codegen/build-unit-keeps-rip-addends ()
  "compile-to-unit keeps the displacement part of pc32 addends."
  (let* ((scripts (expand-file-name
                   "../scripts" (file-name-directory
                                 (or load-file-name buffer-file-name
                                     (locate-library "nelisp-aot-codegen-test"))))))
    (skip-unless (file-exists-p (expand-file-name "nelisp-standalone-build.el" scripts)))
    (let ((load-path (cons scripts load-path)))
      (require 'nelisp-standalone-build))
    (skip-unless (eq (nelisp-standalone--target-arch) 'x86_64))
    (let* ((unit (nelisp-standalone--compile-to-unit
                  "codegen-rip-probe.o"
                  '(seq (defun codegen_rip_f () (ptr-read-u64 (data-addr nl_x) 16))
                        (defun codegen_rip_g () (ptr-write-u64 (data-addr nl_x) 8 7)))))
           (addends (mapcar (lambda (r) (plist-get r :addend))
                            (plist-get unit :relocs))))
      ;; load: disp 16, nothing after disp32 -> 16;
      ;; store of imm32 7 at disp 8 -> 8 - 4 (immediate) = 4.
      (should (equal addends '(16 4))))))

(ert-deftest nelisp-aot-codegen/call-free-regions-call-boundaries ()
  "A hot branch keeps locals in registers, then flushes before a call."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (let ((regions 0)
        (original (symbol-function 'nelisp-aot-compiler--emit-promoted-loop-x86)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-promoted-loop-x86)
               (lambda (node buf emit &optional plan private-slots)
                 (when plan (setq regions (1+ regions)))
                 (funcall original node buf emit plan private-slots))))
      (dolist (input '((3 9 0) (3 9 1) (8 4 2) (8 4 19)
                       (-7 11 6) (17 -2 3) (65530 7 12)))
        (pcase-let ((`(,a ,b ,n) input))
          (let ((i 0) (x a) (y b) (z 13))
            (while (< i n)
              (if (= (logand i 1) 0)
                  (progn (setq x (+ x y)) (setq y (logxor y x))
                         (setq z (+ z x)) (setq x (logand x 65535))
                         (setq y (logand y 65535)))
                (setq x (+ x 7)) (setq y (+ y 1)))
              (setq i (+ i 1)))
            (should
             (= (nelisp-aot-codegen-test--run
                 `(seq
                   (defun bump (x) (+ x 7))
                   (defun f (a b n)
                     (let ((i 0) (x a) (y b) (z 13))
                       (while (< i n)
                         (if (= (logand i 1) 0)
                             (seq (setq x (+ x y)) (setq y (logxor y x))
                                  (setq z (+ z x)) (setq x (logand x 65535))
                                  (setq y (logand y 65535)))
                           (seq (setq x (bump x)) (setq y (+ y 1))))
                         (setq i (+ i 1)))
                       (+ x (+ (* y 3) (* z 5)))))
                   (exit (logand (f ,a ,b ,n) 255))))
                (logand (+ x (* y 3) (* z 5)) 255)))))))
    (should (> regions 0))))

(ert-deftest nelisp-aot-codegen/call-free-regions-preserve-result ()
  "Save/restore and slot flushing preserve a branch's returned GP value."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (dolist (x '(-7 0 3 22))
    (let* ((a (+ x 5)) (b (+ a x)) (c (+ b a))
           (expected (if (> x 0) (+ a b c (* a 3) (* b 5))
                       (+ a b c (* a 7) (* c 11)))))
      (should
       (= (nelisp-aot-codegen-test--run
           `(seq
             (defun keep (v) v)
             (defun f (x)
               (keep (let ((a (+ x 5)) (b 0) (c 0))
                 (setq b (+ a x)) (setq c (+ b a))
                 (if (> x 0) (+ a (+ b (+ c (+ (* a 3) (* b 5)))))
                   (+ a (+ b (+ c (+ (* a 7) (* c 11)))))))))
             (exit (logand (f ,x) 255))))
          (logand expected 255))))))

(ert-deftest nelisp-aot-codegen/call-free-regions-safety-gates ()
  "Checked arithmetic, Lisp values, and the opt-out never invoke the plan."
  (let ((nelisp-aot-compiler--arch 'x86_64)
        (node (nelisp-aot-compiler--make-ir 'value-seq :forms nil)))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--x86-call-free-region-plan)
               (lambda (_) (ert-fail "unsafe promotion planner invoked"))))
      (dolist (mode '(checked lisp disabled leaf))
        (let ((nelisp-aot-compiler--checked-arith (eq mode 'checked))
              (nelisp-aot-compiler--x86-call-containing-defun-p (not (eq mode 'leaf)))
              (nelisp-aot-compiler--runtime-entry-params (and (eq mode 'lisp) '(out)))
              (nelisp-aot-compiler--x86-call-free-regions (not (eq mode 'disabled))))
          (should (= 42 (nelisp-aot-compiler--emit-call-free-region
                         node nil (lambda () 42)))))))))

(ert-deftest nelisp-aot-codegen/call-free-regions-checked-arithmetic ()
  "Checked nonoverflow results and the existing overflow exit remain intact."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (let ((nelisp-aot-compiler--checked-arith t))
    (should (= 115 (nelisp-aot-codegen-test--run
                   '(seq (defun f (x)
                           (let ((a (+ x 4)))
                             (setq a (+ a x)) (setq a (* a 3))
                             (+ a (+ x a))))
                         (exit (f 7))))))
    (should (= 97 (nelisp-aot-codegen-test--run
                   '(seq (defun f (x)
                           (let ((a (+ x 1))) (+ a (+ x a))))
                         (exit (f 2305843009213693951))))))))

(ert-deftest nelisp-aot-codegen/call-free-regions-shadow-and-callee-saved ()
  "Nested lexical slots and restored r12-r15 retain their independent values."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)))
  (dolist (n '(-3 0 9))
    (let* ((sum (cl-reduce (lambda (a b) (list '+ a b))
                           '(a a a a b b b b c c c c d d d d
                             e e e e f f f f g g g g h h h h inner)))
           (expected (+ (* 4 (+ (* 8 n) 36)) (* 4 (+ n 12)))))
      (should
       (= (nelisp-aot-codegen-test--run
           `(seq
             (defun keep (v) v)
             (defun q (n)
               (keep (let ((a (+ n 1)) (b (+ n 2)) (c (+ n 3)) (d (+ n 4))
                     (e (+ n 5)) (f (+ n 6)) (g (+ n 7)) (h (+ n 8))
                     (inner 0))
                 (let ((a (+ a 11))) (setq inner (+ a (+ a (+ a a)))))
                 ,sum)))
             (exit (logand (q ,n) 255))))
          (logand expected 255))))))

(ert-deftest nelisp-aot-codegen/signed-power2-division ()
  "Signed quotients match GNU, including boundaries and generic fallbacks."
  (let ((xs (list (- (expt 2 63)) (1+ (- (expt 2 63)))
                  -1025 -1024 -1023 -9 -8 -7 -1 0 1 7 8 9
                  1023 1024 1025 (1- (expt 2 63))))
        (ds (list 2 8 1024 (expt 2 62) 3 -8 1))
        (defs nil) (checks nil))
    (dolist (d ds)
      (let ((name (intern (format "quotient-%s" d))))
        (push `(defun ,name (x) (/ x ,d)) defs)
        (dolist (x xs)
          (push `(seq (ptr-write-u64 p 0 ,x)
                      (if (= (,name (ptr-read-u64 p 0)) ,(/ x d)) 0
                        (setq errors (+ errors 1)))) checks))))
    (should
     (equal
      (nelisp-aot-codegen-test--run
       `(seq ,@(nreverse defs)
             (defun main ()
               (let ((p (syscall-direct 9 0 4096 3 34 -1 0)) (errors 0))
                 ,@(nreverse checks) errors))
             (exit (main))))
      0))))

(ert-deftest nelisp-aot-codegen/wide-u32-masks ()
  "Wide unsigned masks match GNU with full-width inputs and fallback masks."
  (let ((xs (list (- (expt 2 63)) (1+ (- (expt 2 63)))
                  -1025 -1024 -1023 -9 -8 -7 -1 0 1 7 8 9
                  1023 1024 1025 (1- (expt 2 63))))
        (ds (list #x80000000 #xFFFFFFFF #x80000001 #xFEDCBA98 #x1FFFFFFFF -1 255))
        (defs nil) (checks nil))
    (dolist (d ds)
      (let ((name (intern (format "quotient-%s" d))))
        (push `(defun ,name (x) (logand x ,d)) defs)
        (dolist (x xs)
          (push `(seq (ptr-write-u64 p 0 ,x)
                      (if (= (,name (ptr-read-u64 p 0)) ,(logand x d)) 0
                        (setq errors (+ errors 1)))
                      (if (= (logand ,d (ptr-read-u64 p 0)) ,(logand x d)) 0
                        (setq errors (+ errors 1)))
                      (if (= (+ (ptr-read-u64 p 8)
                                (logand (ptr-read-u64 p 0) ,d))
                             ,(let ((v (+ 13 (logand x d)))) (if (>= v (expt 2 63)) (- v (expt 2 64)) v))) 0
                        (setq errors (+ errors 1)))
                      (if (= (if (> (logand (ptr-read-u64 p 0) ,d) 0) 1 2)
                             ,(if (> (logand x d) 0) 1 2)) 0
                        (setq errors (+ errors 1)))) checks))))
    (should
     (equal
      (nelisp-aot-codegen-test--run
       `(seq ,@(nreverse defs)
             (defun main ()
               (let ((p (syscall-direct 9 0 4096 3 34 -1 0)) (errors 0))
                 (ptr-write-u64 p 8 13)
                 ,@(nreverse checks) (if (= errors 0) 0 1)))
             (exit (main))))
      0))))

(ert-deftest nelisp-aot-codegen/wide-mask-evaluates-once ()
  "A constant mask on either side preserves observable operand evaluation."
  (should
   (= 0
      (nelisp-aot-codegen-test--run
       '(seq
         (defun bump (p)
           (ptr-write-u64 p 8 (+ (ptr-read-u64 p 8) 1))
           (ptr-read-u64 p 0))
         (defun main ()
           (let ((p (syscall-direct 9 0 4096 3 34 -1 0)))
             (ptr-write-u64 p 0 -1)
             (ptr-write-u64 p 8 0)
             (let ((n (+ (logand #xFFFFFFFF (bump p))
                         (logand (bump p) #x80000000))))
               (if (and (= n 6442450943) (= (ptr-read-u64 p 8) 2)) 0 1))))
         (exit (main)))))))

(defun nelisp-aot-codegen-test--specialization-pass (source)
  (let ((nelisp-aot-compiler-tco-enabled t))
    (nelisp-aot-compiler--specialize-raw-recursive-source source)))
(defun nelisp-aot-codegen-test--specialization-clones (source original-names)
  (cl-remove-if-not (lambda (f) (and (consp f) (eq (car f) 'defun)
                                    (not (memq (cadr f) original-names)))) (cdr source)))
(defun nelisp-aot-codegen-test--specialization-word (x)
  (let ((u (logand x (1- (ash 1 64))))) (if (>= u (ash 1 63)) (- u (ash 1 64)) u)))
(ert-deftest nelisp-aot-codegen/recursive-context-clone-shape-and-unknown-entry ()
  (let* ((source '(seq
                   (defun tail (n h mask) (if (> n 0) (tail (- n 1) (logand (* h 16777619) mask) mask) h))
                   (defun bulk (n h mask) (if (> n 3) (bulk (- n 4) (+ h 4) mask) (tail n h mask)))
                   (defun unknown (n h mask) (bulk n h mask))
                   (defun known (n h) (bulk n h 4294967295))))
         (out (nelisp-aot-codegen-test--specialization-pass source))
         (clones (nelisp-aot-codegen-test--specialization-clones out '(tail bulk unknown known))))
    (should (= 2 (length clones)))
    (should (equal (nth 1 source) (nth 1 out)))
    (should (equal (nth 2 source) (nth 2 out)))
    (should (equal (nth 3 source) (nth 3 out)))
    (dolist (c clones) (should (= 2 (length (nth 2 c)))))
    (let ((nelisp-aot-compiler-tco-enabled t))
      (should (= 0 (nelisp-aot-codegen-test--run
                    (append source '((exit (if (= (unknown 7 3 4294967295) (known 7 3)) 0 1))))))))))
(ert-deftest nelisp-aot-codegen/recursive-context-full-word-gnu-differential ()
  (let ((nelisp-aot-compiler-tco-enabled t))
    (dolist (mask '(255 4294967295 -9223372036854775808 9223372036854775807))
      (dolist (initial '(-9223372036854775808 9223372036854775807 -1 3))
        (let ((expected initial))
          ;; GNU arbitrary precision arithmetic, normalized at each RAW operation.
          (dotimes (_ 7) (setq expected (nelisp-aot-codegen-test--specialization-word
                                       (logxor (nelisp-aot-codegen-test--specialization-word (* expected 16777619)) mask))))
          (should (= 0 (nelisp-aot-codegen-test--run
                        `(seq
                          (defun mix (n h k) (if (> n 0) (mix (- n 1) (logxor (* h 16777619) k) k) h))
                          (defun entry (n h) (mix n h ,mask))
                          (exit (if (= (entry 7 ,initial) ,expected) 0 1)))))))))))
(ert-deftest nelisp-aot-codegen/recursive-context-effects-permutation-and-deep-loop ()
  (let ((nelisp-aot-compiler-tco-enabled t))
    (should (= 0 (nelisp-aot-codegen-test--run
                  '(seq
                    (defun walk (n a b k)
                      (if (> n 0) (walk (- n 1) (+ a b) (+ b 1) k) (logxor a k)))
                    (defun choose ()
                      (let ((x 0))
                        (walk (seq (setq x 1) 3)
                              (seq (setq x (+ x 10)) x)
                              (seq (setq x (+ x 100)) x) 7)))
                    (exit (if (= (choose) 348) 0 1))))))
    (should (= 0 (nelisp-aot-codegen-test--run
                  '(seq
                    (defun add-memory (n p k)
                      (if (> n 0)
                          (seq (ptr-write-u64 p 0 (+ (ptr-read-u64 p 0) k))
                               (add-memory (- n 1) p k))
                        (ptr-read-u64 p 0)))
                    (defun choose ()
                      (let ((p (syscall-direct 9 0 4096 3 34 -1 0)))
                        (ptr-write-u64 p 0 -9223372036854775808)
                        (add-memory 5 p 3)))
                    (exit (if (= (choose) -9223372036854775793) 0 1))))))
    (dolist (n '(200000 200001))
      (should (= 0 (nelisp-aot-codegen-test--run
                    `(seq
                      (defun swap (n a b k)
                        (if (> n 0) (swap (- n 1) b a k) (+ a (+ (* b 3) k))))
                      (defun choose (n a b) (swap n a b 17))
                      (exit (if (= (choose ,n 5 9) ,(if (= 0 (% n 2)) 49 41)) 0 1)))))))))
(ert-deftest nelisp-aot-codegen/recursive-context-fallback-hygiene-and-protocols ()
  (dolist (source
           '((seq (defun f (n k) (if (> n 0) (f (- n 1) (+ k 1)) k)) (exit (f 5 7)))
             (seq (defun f (n k) (seq (setq k (+ k 1)) (if (> n 0) (f (- n 1) k) k))) (exit (f 5 7)))
             (seq (defun f (n k) (let ((k k)) (if (> n 0) (f (- n 1) k) k))) (exit (f 5 7)))
             (seq (defun f (n k) (if (> n 0) (f (- n 1) k) (current-sp))) (exit (f 5 7)))
             (seq (defun f (n k) (if (> n 0) (f (- n 1) k) (extern-call other k))) (exit (f 5 7)))
             (seq (defun f (n k) (if (> n 0) (f (- n 1) k) (funcall k n))) (exit (f 5 7)))
             (seq (defun f (n (k :type sexp)) (f (- n 1) k)) (exit (f 5 7)))
             (seq (defun f (n &optional k) (f (- n 1) k)) (exit (f 5 7)))
             (seq (defun f (n &rest k) (f (- n 1) k)) (exit (f 5 7)))
             (seq (defun f (n k) (if (> n 0) (f (- n 1) k) (f64-to-int k))) (exit (f 5 7)))
             (seq (defun f (n k) (if (> n 0) (f (- n 1) k) (quote k))) (exit (f 5 7)))
             (seq (defun f (n k) (f n k)) (defun f (n k) k) (exit (f 5 7)))))
    (should (eq source (nelisp-aot-codegen-test--specialization-pass source))))
  (let ((source '(seq (defun f (n k) (if (> n 0) (f (- n 1) k) k)) (exit (f 5 7)))))
    (dolist (mode '(checked lisp win64 arm no-tco))
      (let ((nelisp-aot-compiler--checked-arith (eq mode 'checked))
            (nelisp-aot-compiler--runtime-entry-params (eq mode 'lisp))
            (nelisp-aot-compiler--abi (if (eq mode 'win64) 'win64 'sysv))
            (nelisp-aot-compiler--arch (if (eq mode 'arm) 'aarch64 'x86_64))
            (nelisp-aot-compiler-tco-enabled (not (eq mode 'no-tco))))
        (should (eq source (nelisp-aot-compiler--specialize-raw-recursive-source source)))))))
(ert-deftest nelisp-aot-codegen/recursive-context-fallback-new-overflow ()
  (dolist (body '((if (> n 0) (f (- n 1) k) (if (> (+ k 1) 0) 1 0))
                  (let ((alias k)) (if (> n 0) (f (- n 1) k) (+ alias 1)))))
    (let ((source `(seq (defun f (n k) ,body) (exit (f 5 9223372036854775807)))))
      (should (eq source (nelisp-aot-codegen-test--specialization-pass source)))))
  (let ((nelisp-aot-compiler-tco-enabled t))
    (should (= 0 (nelisp-aot-codegen-test--run
                  '(seq (defun f (n k) (if (> n 0) (f (- n 1) k) (if (> (+ k 1) 0) 1 0)))
                    (exit (f 5 9223372036854775807))))))))
(ert-deftest nelisp-aot-codegen/recursive-context-growth-and-name-collision ()
  (let* ((source '(seq (defun nelisp-context-0 (x) x)
                   (defun f (n k) (if (> n 0) (f (- n 1) k) k))
                   (defun known (n) (+ (f n 1) (+ (f n 2) (+ (f n 3) (+ (f n 4) (f n 5))))))))
         (out (nelisp-aot-codegen-test--specialization-pass source))
         (clones (nelisp-aot-codegen-test--specialization-clones out '(f known nelisp-context-0))))
    (should (= 4 (length clones)))
    (should (= 7 (length (cdr out))))
    (should (= (length (delete-dups (mapcar #'cadr (cdr out)))) (length (cdr out)))))
  (let* ((source `(seq (defun huge (n k)
                        (if (> n 0) (huge (- n 1) k)
                          (seq ,@(make-list 600 1) k))) (exit (huge 3 7)))))
    (should (eq source (nelisp-aot-codegen-test--specialization-pass source)))))

(ert-deftest nelisp-aot-codegen/recursive-context-independent-unit-names ()
  (let* ((one '(seq (defun owner-one (n k) (if (> n 0) (owner-one (- n 1) k) k)) (exit (owner-one 5 7))))
         (two '(seq (defun owner-two (n k) (if (> n 0) (owner-two (- n 1) k) k)) (exit (owner-two 5 7))))
         (a (cadr (car (nelisp-aot-codegen-test--specialization-clones
                       (nelisp-aot-codegen-test--specialization-pass one) '(owner-one)))))
         (b (cadr (car (nelisp-aot-codegen-test--specialization-clones
                       (nelisp-aot-codegen-test--specialization-pass two) '(owner-two))))))
    (should-not (equal (symbol-name a) (symbol-name b)))
    (should (eq a (cadr (car (nelisp-aot-codegen-test--specialization-clones
                             (nelisp-aot-codegen-test--specialization-pass one) '(owner-one))))))))
(ert-deftest nelisp-aot-codegen/recursive-context-malformed-shapes-and-observers ()
  (dolist (source
           '((seq (defun f (n . k) n) (exit 0))
             (seq (defun f (n k) (let . 7)) (exit 0))
             (seq (defun f (n k) (if (> n 0) (f (- n 1) k) k)) (defun))
             (seq (defun f (n k) (if (> n 0) (f (- n 1) k) k)) (cond 7))))
    (should (eq source (nelisp-aot-codegen-test--specialization-pass source)))
    (let ((nelisp-aot-compiler-tco-enabled t)
          (pass (symbol-function 'nelisp-aot-compiler--specialize-raw-recursive-source))
          baseline candidate)
      (cl-letf (((symbol-function 'nelisp-aot-compiler--specialize-raw-recursive-source) #'identity))
        (setq baseline (condition-case e (nelisp-aot-compiler--parse source) (error e))))
      (cl-letf (((symbol-function 'nelisp-aot-compiler--specialize-raw-recursive-source) pass))
        (setq candidate (condition-case e (nelisp-aot-compiler--parse source) (error e))))
      (should (equal baseline candidate))))
  (dolist (source
           '((seq (defun f (n k) (if (> n 0) (f (- n 1) k) (+ k 1 -1))) (exit (f 5 9223372036854775807)))
             (seq (defun f (n k)
                    (let* ((alias k) (unused (setq alias 0)))
                      (if (> n 0) (f (- n 1) k) (+ alias 1)))) (exit (f 5 9223372036854775807)))
             (seq (defun f (n k h) (if (> n 0) (f (- n 1) k h) (logxor h k)))
               (defun observe (n) (f n 7 (aot-current-sp))))
             (seq (defun f (n k h) (if (> n 0) (f (- n 1) k h) (logxor h k)))
               (defun observe (n)
                 (let ((saved (aot-current-sp))) (f n 7 (ptr-read-u64 saved -16)))))
             (seq (defun f (n k h) (if (> n 0) (f (- n 1) k h) (logxor h k)))
               (defun observe (n) (f n 7 (extern-call unknown n))))))
    (should (eq source (nelisp-aot-codegen-test--specialization-pass source)))))
(ert-deftest nelisp-aot-codegen/recursive-context-reserved-context-name ()
  (let* ((source '(seq (defun owner (n k) (if (> n 0) (owner (- n 1) k) k)) (exit (owner 5 7))))
         (first (cadr (car (nelisp-aot-codegen-test--specialization-clones
                           (nelisp-aot-codegen-test--specialization-pass source) '(owner)))))
         (reserved `(seq (defun ,first (x) x) ,@(cdr source)))
         (out (nelisp-aot-codegen-test--specialization-pass reserved))
         (clones (nelisp-aot-codegen-test--specialization-clones out (list 'owner first))))
    (should (= 1 (length clones)))
    (should-not (eq first (cadar clones)))
    (should (equal (cadr reserved) (cadr out)))))

(defun nelisp-aot-codegen-test--sparse-keys (lo)
  (mapcar (lambda (n) (+ lo n)) '(0 1 2 3 4 5 6 7 9)))
(defun nelisp-aot-codegen-test--sparse-result (index)
  (if (= 0 (% index 2)) (+ -9223372036854775808 index) (- 9223372036854775807 index)))
(defun nelisp-aot-codegen-test--sparse-form (keys &optional effect)
  (let ((tail '(seq (ptr-write-u64 p 0 99) (setq k (+ k 1)) alias))
        (index (1- (length keys))))
    (dolist (key (reverse keys))
      (setq tail `(if (= ,(if effect '(seq (ptr-write-u64 p 8 (+ (ptr-read-u64 p 8) 1)) k) 'k) ,key)
                      (seq (ptr-write-u64 p 0 ,(1+ index)) (setq k (+ k 1))
                           ,(nelisp-aot-codegen-test--sparse-result index)) ,tail)
            index (1- index)))
    `(defun sparse-choose (k p)
       (let ((alias k) (result 0))
         (setq result ,tail)
         (if (= alias (- k 1)) result 42)))))
(defun nelisp-aot-codegen-test--sparse-plan (source &optional first-if)
  (let ((nelisp-aot-compiler--label-counter 0) found)
    (nelisp-aot-compiler--walk-ir
     (nelisp-aot-compiler--parse source)
     (lambda (n)
       (when (and (eq (nelisp-aot-compiler--ir-kind n) 'if)
                  (or first-if (nelisp-aot-compiler--x86-dense-if-plan n)))
         (unless found (setq found n))))) found))
(ert-deftest nelisp-aot-codegen/sparse-switch-native-signed-full-word-effects ()
  (dolist (lo '(-2147483648 -6 0 83 2147483638))
    (let* ((keys (nelisp-aot-codegen-test--sparse-keys lo))
           (form (nelisp-aot-codegen-test--sparse-form keys))
           (inputs (append keys (list (1- lo) (+ lo 8) (+ lo 10)
                                      -9223372036854775808 9223372036854775807
                                      (+ (logand (+ lo 3) #xffffffff) (ash 1 32))))))
      (dolist (input inputs)
        (let* ((index (cl-position input keys))
               (expected (if index (nelisp-aot-codegen-test--sparse-result index) input))
               (tag (if index (1+ index) 99)))
          (should (= 0 (nelisp-aot-codegen-test--run
                        `(seq ,form
                          (defun check ()
                            (let ((p (syscall-direct 9 0 4096 3 34 -1 0)))
                              (let ((value (sparse-choose ,input p)))
                                (if (and (= value ,expected) (= (ptr-read-u64 p 0) ,tag)) 0 1))))
                          (exit (check)))))))))))
(ert-deftest nelisp-aot-codegen/sparse-switch-policy-and-stability ()
  (let* ((nelisp-aot-compiler--runtime-entry-params nil)
         (nelisp-aot-compiler--arch 'x86_64)
         (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--promoted-slots nil)
         (source `(seq ,(nelisp-aot-codegen-test--sparse-form
                         (nelisp-aot-codegen-test--sparse-keys 83))))
         (node (nelisp-aot-codegen-test--sparse-plan source)))
    (should node)
    (should-not (nelisp-aot-compiler--x86-sparse-table-profitable-p 8 10))
    (should-not (nelisp-aot-compiler--x86-sparse-table-profitable-p 32 65))
    (should-not (nelisp-aot-compiler--x86-sparse-table-profitable-p 20 50))
    (dolist (bad '((0 1 2 3 5) (0 1 2 3 4 5 6 7 9999)))
      (should-not (nelisp-aot-codegen-test--sparse-plan `(seq ,(nelisp-aot-codegen-test--sparse-form bad)))))
    (let ((duplicate (nelisp-aot-codegen-test--sparse-plan
                      `(seq ,(nelisp-aot-codegen-test--sparse-form '(0 0 1 2 3 4 5 6 7 9))) t)))
      (should duplicate)
      (should-not (nelisp-aot-compiler--x86-dense-if-plan duplicate)))
    (dolist (bad '(other-slot f64 wide-key effect))
      (let* ((test (nelisp-aot-compiler--ir-get node :test))
             (a (pcase bad
                  ('other-slot (nelisp-aot-compiler--make-ir 'ref :slot 1 :class 'gp))
                  ('f64 (nelisp-aot-compiler--make-ir 'ref :slot 0 :class 'f64))
                  ('effect (nelisp-aot-compiler--make-ir 'call :name 'effect :args nil))
                  (_ (nelisp-aot-compiler--ir-get test :a))))
             (b (if (eq bad 'wide-key) (nelisp-aot-compiler--make-ir 'imm :value 4294967295)
                  (nelisp-aot-compiler--ir-get test :b)))
             (copy (nelisp-aot-compiler--make-ir
                    'if :id (nelisp-aot-compiler--ir-get node :id)
                    :test (nelisp-aot-compiler--make-ir 'cmp :op '= :a a :b b)
                    :then (nelisp-aot-compiler--ir-get node :then)
                    :else (nelisp-aot-compiler--ir-get node :else))))
        (should-not (nelisp-aot-compiler--x86-dense-if-plan copy))))
    (let ((nelisp-aot-compiler--abi 'win64))
      (should (nelisp-aot-codegen-test--sparse-plan
               `(seq ,(nelisp-aot-codegen-test--sparse-form '(0 1 2 3))))))
    (should-not (nelisp-aot-codegen-test--sparse-plan
                 `(seq ,(nelisp-aot-codegen-test--sparse-form
                          (nelisp-aot-codegen-test--sparse-keys 83) t))))
    (dolist (mode '(disabled win64 arm lisp rax r10))
      (let ((nelisp-aot-compiler--x86-jump-tables-enabled (not (eq mode 'disabled)))
            (nelisp-aot-compiler--runtime-entry-params (eq mode 'lisp))
            (nelisp-aot-compiler--abi (if (eq mode 'win64) 'win64 'sysv))
            (nelisp-aot-compiler--arch (if (eq mode 'arm) 'aarch64 'x86_64))
            (nelisp-aot-compiler--promoted-slots (and (memq mode '(rax r10)) (list (cons 30 mode)))))
        (should-not (nelisp-aot-compiler--x86-dense-if-plan node))))))
(ert-deftest nelisp-aot-codegen/sparse-switch-inline-data-real-gauge ()
  (let* ((spans nil)
         (nelisp-aot-compiler--x86-inline-data-hook
         (list (lambda (buf offset size)
                 (should (<= 0 offset))
                 (should (<= (+ offset size) (nelisp-asm-x86_64-buffer-pos buf)))
                 (push size spans)))))
    (should (= 0 (nelisp-aot-codegen-test--run
                  `(seq ,(nelisp-aot-codegen-test--sparse-form (nelisp-aot-codegen-test--sparse-keys 83))
                    (defun check ()
                      (let ((p (syscall-direct 9 0 4096 3 34 -1 0)))
                        (if (= (sparse-choose 91 p) 91) 0 1))) (exit (check))))))
    ;; Two ordinary emission passes, each reporting all ten entries including91.
    (should (equal spans '(40 40)))))
(ert-deftest nelisp-aot-codegen/sparse-switch-source-label-collision ()
  (dolist (keys (list (number-sequence 83 91)
                     (nelisp-aot-codegen-test--sparse-keys 83)))
    (let* ((form (nelisp-aot-codegen-test--sparse-form keys))
           (input (if (= 91 (car (last keys))) 92 91))
           (node (nelisp-aot-codegen-test--sparse-plan `(seq ,form)))
           (id (nelisp-aot-compiler--ir-get node :id))
           (table (intern (format "%s-table" id)))
           (fallback (intern (format "%s-default" id)))
           (case-label (intern (format "%s-case-83" id))))
      (should (= 0 (nelisp-aot-codegen-test--run
                    `(seq ,form
                      (defun ,table () 17) (defun ,fallback () 19) (defun ,case-label () 23)
                      (defun check ()
                        (let ((p (syscall-direct 9 0 4096 3 34 -1 0)))
                          (if (and (= (sparse-choose ,input p) ,input) (= (,table) 17)
                                   (= (,fallback) 19) (= (,case-label) 23)) 0 1)))
                      (exit (check)))))))))

(provide 'nelisp-aot-codegen-test)

;;; nelisp-aot-codegen-test.el ends here
