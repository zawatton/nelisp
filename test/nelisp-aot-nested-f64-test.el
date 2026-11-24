;;; nelisp-aot-nested-f64-test.el --- nested f64 binops in the AOT compiler  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `(f64-add (f64-mul A B) C)' and deeper nestings used to signal
;; `:nested-f64-binop-needs-doc-112'.  The leaf emitter now recurses into
;; the binop emitter and the operand placer spills the left value across a
;; nested right operand.  Each test compiles probes to an object, links it
;; with a C driver printing every probe's raw result bits as an unsigned
;; long, and compares the bits with the same expression evaluated by
;; Emacs' IEEE double arithmetic (bit-exact).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defun nelisp-aot-nested-f64-test--bits (x)
  "Return the IEEE-754 bit pattern of the finite non-zero double X."
  (nelisp-aot-compiler--f64-imm-bits x))

(defun nelisp-aot-nested-f64-test--leaf (x)
  "Return the AOT leaf expression materialising the double X."
  `(bits-to-f64 ,(nelisp-aot-nested-f64-test--bits x)))

(defun nelisp-aot-nested-f64-test--run (defuns)
  "Compile DEFUNS (helpers plus probe0 ... probeN-1), run them and return the printed bit strings."
  (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
    (let* ((dir (make-temp-file "nelisp-nf64" t))
           (obj (expand-file-name "p.o" dir))
           (drv (expand-file-name "d.c" dir))
           (bin (expand-file-name "p" dir))
           (n (cl-count-if (lambda (d) (string-prefix-p "probe" (symbol-name (nth 1 d))))
                           defuns)))
      (unwind-protect
          (progn
            (nelisp-aot-compile-to-object `(seq ,@defuns) obj)
            (with-temp-file drv
              (insert "#include <stdio.h>\n")
              (dotimes (i n) (insert (format "extern unsigned long probe%d(void);\n" i)))
              (insert "int main(void){\n")
              (dotimes (i n) (insert (format "printf(\"%%lu\\n\", probe%d());\n" i)))
              (insert "return 0; }\n"))
            (should (zerop (call-process cc nil nil nil drv obj "-o" bin)))
            (with-temp-buffer
              (call-process bin nil t nil)
              (split-string (string-trim (buffer-string)) "\n")))
        (ignore-errors (delete-directory dir t))))))

(defun nelisp-aot-nested-f64-test--gen (depth rnd)
  "Return a random tree (OP L R) or float leaf of at most DEPTH, using RND."
  (if (or (= depth 0) (< (funcall rnd 10) 2))
      (nth (funcall rnd 8) '(1.5 -2.25 3.0 0.5 7.0 -0.75 10.0 2.5))
    (list (nth (funcall rnd 4) '(f64-add f64-sub f64-mul f64-div))
          (nelisp-aot-nested-f64-test--gen (1- depth) rnd)
          (nelisp-aot-nested-f64-test--gen (1- depth) rnd))))

(defun nelisp-aot-nested-f64-test--eval (tree)
  "Evaluate TREE with Emacs floats."
  (if (numberp tree) tree
    (let ((a (nelisp-aot-nested-f64-test--eval (nth 1 tree)))
          (b (nelisp-aot-nested-f64-test--eval (nth 2 tree))))
      (pcase (car tree)
        ('f64-add (+ a b)) ('f64-sub (- a b))
        ('f64-mul (* a b)) ('f64-div (/ a b))))))

(defun nelisp-aot-nested-f64-test--lower (tree)
  "Lower TREE into AOT source."
  (if (numberp tree) (nelisp-aot-nested-f64-test--leaf tree)
    (list (car tree)
          (nelisp-aot-nested-f64-test--lower (nth 1 tree))
          (nelisp-aot-nested-f64-test--lower (nth 2 tree)))))

(defun nelisp-aot-nested-f64-test--depth (tree)
  (if (numberp tree) 0
    (1+ (max (nelisp-aot-nested-f64-test--depth (nth 1 tree))
             (nelisp-aot-nested-f64-test--depth (nth 2 tree))))))

(defmacro nelisp-aot-nested-f64-test--linux ()
  `(skip-unless (and (eq system-type 'gnu/linux)
                     (string-match-p "x86_64" system-configuration)
                     (or (executable-find "cc") (executable-find "gcc")))))

(ert-deftest nelisp-aot-nested-f64/explicit-shapes ()
  "Left, right and balanced nesting of every op, depth 2 and 3."
  (nelisp-aot-nested-f64-test--linux)
  (let* ((trees '((f64-add (f64-mul 2.0 3.0) 1.0)          ; 7.0
                  (f64-add 1.0 (f64-mul 2.0 3.0))
                  (f64-sub (f64-div 7.0 2.0) 1.5)
                  (f64-sub 1.5 (f64-div 7.0 2.0))          ; non-commutative
                  (f64-div 1.5 (f64-sub 7.0 2.25))
                  (f64-div (f64-sub 7.0 2.25) 1.5)
                  (f64-add (f64-mul (f64-sub (f64-div 7.0 2.0) 1.5) 3.0) 0.5)
                  (f64-sub 0.5 (f64-mul 3.0 (f64-sub 1.5 (f64-div 7.0 2.0))))
                  (f64-mul (f64-add (f64-mul 1.5 2.5) (f64-div 3.0 7.0))
                           (f64-sub (f64-div 10.0 3.0) (f64-mul 0.5 -2.25)))))
         (got (nelisp-aot-nested-f64-test--run
               (cl-loop for tr in trees for i from 0
                        collect `(defun ,(intern (format "probe%d" i)) ()
                                   (f64-bits ,(nelisp-aot-nested-f64-test--lower tr)))))))
    (should (>= (apply #'max (mapcar #'nelisp-aot-nested-f64-test--depth trees)) 3))
    (cl-loop for tr in trees for g in got
             do (should (equal (number-to-string
                                (nelisp-aot-nested-f64-test--bits
                                 (nelisp-aot-nested-f64-test--eval tr)))
                               g)))))

(ert-deftest nelisp-aot-nested-f64/random-trees-bit-exact ()
  "Seeded random trees up to depth 4 give the same bits as Emacs floats."
  (nelisp-aot-nested-f64-test--linux)
  (let* ((state 12345)
         (rnd (lambda (n) (setq state (mod (+ (* state 1103515245) 12345) 2147483648))
                (mod (ash state -8) n)))
         (trees nil) (tries 0))
    (while (and (< (length trees) 40) (< tries 2000))
      (cl-incf tries)
      (let ((tr (nelisp-aot-nested-f64-test--gen 4 rnd)))
        (when (and (consp tr)
                   (>= (nelisp-aot-nested-f64-test--depth tr) 2)
                   (condition-case nil
                       (let ((v (nelisp-aot-nested-f64-test--eval tr)))
                         (and (/= v 0.0) (< 1e-200 (abs v) 1e200)))
                     (error nil)))
          (push tr trees))))
    (setq trees (nreverse trees))
    (should (= 40 (length trees)))
    (should (>= (apply #'max (mapcar #'nelisp-aot-nested-f64-test--depth trees)) 4))
    (let ((got (nelisp-aot-nested-f64-test--run
                (cl-loop for tr in trees for i from 0
                         collect `(defun ,(intern (format "probe%d" i)) ()
                                    (f64-bits ,(nelisp-aot-nested-f64-test--lower tr)))))))
      (cl-loop for tr in trees for g in got
               do (should (equal (number-to-string
                                  (nelisp-aot-nested-f64-test--bits
                                   (nelisp-aot-nested-f64-test--eval tr)))
                                 g))))))

(ert-deftest nelisp-aot-nested-f64/mixed-int-conversions-and-calls ()
  "Nesting mixed with `i64-to-f64', `f64-to-i64-trunc' and calls in leaves."
  (nelisp-aot-nested-f64-test--linux)
  (let* ((x 2.5)
         (defuns
          `((defun clob (v)             ; clobbers xmm0/xmm1 via f64 work
              (f64-bits (f64-mul (f64-add (bits-to-f64 ,(nelisp-aot-nested-f64-test--bits 9.0))
                                          (i64-to-f64 v))
                                 (bits-to-f64 ,(nelisp-aot-nested-f64-test--bits 4.0)))))
            ;; (7 * 2.5) - (-3)  = 20.5
            (defun probe0 ()
              (f64-bits (f64-sub (f64-mul (i64-to-f64 7) ,(nelisp-aot-nested-f64-test--leaf x))
                                 (i64-to-f64 -3))))
            ;; 2.5 + ((clob 1) * (2.5 - 7)): left value must survive the calls
            (defun probe1 ()
              (f64-bits (f64-add ,(nelisp-aot-nested-f64-test--leaf x)
                                 (f64-mul (bits-to-f64 (clob 1))
                                          (f64-sub ,(nelisp-aot-nested-f64-test--leaf x)
                                                   (i64-to-f64 7))))))
            ;; ((clob 2) / 4.0) + ((clob 3) * 0.5)
            (defun probe2 ()
              (f64-bits (f64-add (f64-div (bits-to-f64 (clob 2))
                                          ,(nelisp-aot-nested-f64-test--leaf 4.0))
                                 (f64-mul (bits-to-f64 (clob 3))
                                          ,(nelisp-aot-nested-f64-test--leaf 0.5)))))
            ;; trunc(((7 * 2.5) - 1.0) * 3.0) = trunc(49.5) = 49
            (defun probe3 ()
              (f64-to-i64-trunc
               (f64-mul (f64-sub (f64-mul (i64-to-f64 7) ,(nelisp-aot-nested-f64-test--leaf x))
                                 ,(nelisp-aot-nested-f64-test--leaf 1.0))
                        ,(nelisp-aot-nested-f64-test--leaf 3.0))))))
         (clob (lambda (v) (* (+ 9.0 v) 4.0)))
         (want (list (- (* 7.0 x) -3.0)
                     (+ x (* (funcall clob 1.0) (- x 7.0)))
                     (+ (/ (funcall clob 2.0) 4.0) (* (funcall clob 3.0) 0.5))))
         (got (nelisp-aot-nested-f64-test--run defuns)))
    (should (= 4 (length got)))
    (cl-loop for w in want for g in got
             do (should (equal (number-to-string (nelisp-aot-nested-f64-test--bits w)) g)))
    (should (equal "49" (nth 3 got)))))

(ert-deftest nelisp-aot-nested-f64/aarch64-nested-compiles ()
  "Nested f64 binops also compile for aarch64 (object only; not run here)."
  (let ((path (make-temp-file "nelisp-nf64-a64-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           `(seq (defun probe ()
                   (f64-bits ,(nelisp-aot-nested-f64-test--lower
                               '(f64-sub (f64-div (f64-add 1.5 2.5) (f64-mul 3.0 0.5))
                                         (f64-add (f64-mul 7.0 1.5) 2.5))))))
           path :arch 'aarch64)
          (should (> (file-attribute-size (file-attributes path)) 0)))
      (ignore-errors (delete-file path)))))

(provide 'nelisp-aot-nested-f64-test)
;;; nelisp-aot-nested-f64-test.el ends here
