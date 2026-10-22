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
               (lambda (node buf emit &optional plan)
                 (when plan (setq regions (1+ regions)))
                 (funcall original node buf emit plan))))
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

(provide 'nelisp-aot-codegen-test)

;;; nelisp-aot-codegen-test.el ends here
