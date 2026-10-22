;;; nelisp-aot-partial-call-test.el --- Post-staging guarded calls -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defvar nelisp-aot-partial-call-test--enabled t)
(defvar nelisp-aot-partial-call-test--count 0)
(defun nelisp-aot-partial-call-test--record (&rest _)
  (cl-incf nelisp-aot-partial-call-test--count))
(defun nelisp-aot-partial-call-test--object (source file disabled)
  (let ((nelisp-aot-compiler--object-hidden-boundary-symbols nil)
        (nelisp-aot-compiler--x86-partial-call-folding
         (and nelisp-aot-partial-call-test--enabled (not disabled)))
        (nelisp-aot-partial-call-test--count 0))
    (advice-add 'nelisp-aot-compiler--emit-partial-named-call-x86
                :before #'nelisp-aot-partial-call-test--record)
    (unwind-protect
        (progn (nelisp-aot-compile-to-object source file)
               nelisp-aot-partial-call-test--count)
      (advice-remove 'nelisp-aot-compiler--emit-partial-named-call-x86
                     #'nelisp-aot-partial-call-test--record))))
(defun nelisp-aot-partial-call-test--heavy (params)
  `(+ (+ (+ (+ (+ ,(nth 0 params) (* ,(nth 1 params) 2)) (* ,(nth 2 params) 3))
             (* ,(nth 3 params) 4)) (* ,(nth 4 params) 5))
       (+ (* ,(nth 5 params) 6) 21)))

(defun nelisp-aot-partial-call-test--defs (source)
  (let ((nelisp-aot-compiler--object-hidden-boundary-symbols nil))
    (nelisp-aot-compiler--collect-defuns (nelisp-aot-compiler--parse source))))

(ert-deftest nelisp-aot-partial-call/fullword-effects-staging-native-ABI ()
  (skip-unless (and (executable-find "gcc-14") (eq system-type 'gnu/linux)))
  (let* ((dir (make-temp-file "partial-call-" t))
         (c (expand-file-name "probe.c" dir)) (co (expand-file-name "probe.o" dir))
         (o (expand-file-name "target.o" dir)) (exe (expand-file-name "probe" dir))
         (params '(a b c d e f))
         (values '(0 1 -1 2 6 4294967295 -4294967296 1099511627783
                     -1099511627783 9223372036854775807 -9223372036854775808))
         (mask (1- (ash 1 64)))
         (heavy (nelisp-aot-partial-call-test--heavy params))
         (pair-heavy (cl-reduce (lambda (a b) (list '+ a b)) '(a b a b a b a b a b a b)))
         (source nil) (decls nil))
    (unwind-protect
        (progn
          (dotimes (i 6)
            (let ((name (intern (format "guard%d" i)))
                  (wrapper (intern (format "wrapper%d" i))))
              (push `(defun ,name ,params
                       (if (= (logand (- (+ a 7) 3) 1) 1) ,(nth i params) ,heavy)) source)
              (push `(defun ,wrapper ,params (,name ,@params)) source)
              (push (format "extern U wrapper%d(U,U,U,U,U,U);\n" i) decls)))
          (setq source
                (append source
                  `((defun immediate ,params (if (= a 0) 18446744073709551615 ,heavy))
                    (defun immediate_wrapper ,params (immediate ,@params))
                    (defun signed_guard ,params (if (> (+ a 7) 0) d ,heavy))
                    (defun signed_guard_wrapper ,params (signed_guard ,@params))
                    (defun bit_guard ,params (if (= (logxor (logior a 2) 3) 0) b ,heavy))
                    (defun bit_guard_wrapper ,params (bit_guard ,@params))
                    (defun swapped ,params (if (/= a 0) ,heavy e))
                    (defun swapped_wrapper ,params (swapped ,@params))
                    (defun raw_truth ,params (if a f ,heavy))
                    (defun raw_truth_wrapper ,params (raw_truth ,@params))
                    (defun first_cond ,params (cond ((= (logand a 1) 1) c)
                                                   ((= (logand a 2) 2) ,heavy)
                                                   (t (- ,heavy 3))))
                    (defun first_cond_wrapper ,params (first_cond ,@params))
                    (defun pair (a b) (if (= a 0) b ,pair-heavy))
                    (defun effects (p v)
                      (pair (seq (ptr-write-u64 p 0 (+ (ptr-read-u64 p 0) 1)) 0)
                            (seq (ptr-write-u64 p 8 (+ (ptr-read-u64 p 0) 100)) v)))
                    (defun word_guard (w)
                      (cond ((= (logand w 1) 1) w)
                            ((= (logand w 7) 2) 7)
                            ((= (logand w 7) 6) (logand (ptr-read-u8 (- w 6) 0) 63))
                            (t (ptr-read-u8 w 0))))
                    (defun word_wrapper (w) (word_guard w))
                    (defun frame (p)
                      (pair (seq (ptr-write-u64 p 0 (aot-current-sp)) 0)
                            (seq (ptr-write-u64 p 8 (aot-current-sp)) (aot-current-sp)))))))
          (with-temp-file c
            (insert "#include <stdint.h>\ntypedef uint64_t U;\n")
            (insert (apply #'concat decls))
            (insert "extern U immediate_wrapper(U,U,U,U,U,U),swapped_wrapper(U,U,U,U,U,U),raw_truth_wrapper(U,U,U,U,U,U),signed_guard_wrapper(U,U,U,U,U,U),bit_guard_wrapper(U,U,U,U,U,U),first_cond_wrapper(U,U,U,U,U,U),effects(U*,U),frame(U*),word_wrapper(U);\n")
            (insert "__asm__(\".text\\n.globl _start\\n_start:\\ncall run\\nmov %rax,%rdi\\nmov $60,%eax\\nsyscall\\n\");\nU run(void){U fail=0,p[2];\n")
            (dolist (a values)
              (let* ((args (mapcar (lambda (k) (logand (+ a (* k 1099511627779)) mask)) '(0 1 2 3 4 5)))
                     (actual (mapconcat (lambda (n) (format "UINT64_C(%d)" n)) args ","))
                     (slow (logand (+ (cl-loop for v in args for k from 1 sum (* v k)) 21) mask)))
                (dotimes (i 6)
                  (insert (format "fail|=wrapper%d(%s)!=UINT64_C(%d);\n"
                                  i actual (if (= (logand (+ a 4) 1) 1) (nth i args) slow))))
                (insert (format "fail|=immediate_wrapper(%s)!=UINT64_C(%d);\n"
                                actual (if (= a 0) mask slow)))
                (let* ((u (logand (+ a 7) mask)) (signed (if (>= u (ash 1 63)) (- u (ash 1 64)) u)))
                  (insert (format "fail|=signed_guard_wrapper(%s)!=UINT64_C(%d);\n"
                                  actual (if (> signed 0) (nth 3 args) slow))))
                (insert (format "fail|=bit_guard_wrapper(%s)!=UINT64_C(%d);\n"
                                actual (if (= (logxor (logior a 2) 3) 0) (nth 1 args) slow)))
                (insert (format "fail|=swapped_wrapper(%s)!=UINT64_C(%d);\n"
                                actual (if (= a 0) (nth 4 args) slow)))
                (insert (format "fail|=raw_truth_wrapper(%s)!=UINT64_C(%d);\n"
                                actual (if (= a 0) slow (nth 5 args))))
                (insert (format "fail|=first_cond_wrapper(%s)!=UINT64_C(%d);\n"
                                actual (cond ((= (logand a 1) 1) (nth 2 args))
                                             ((= (logand a 2) 2) slow)
                                             (t (logand (- slow 3) mask)))))
                (insert (format "p[0]=1;p[1]=0;fail|=effects(p,UINT64_C(%d))!=UINT64_C(%d)||p[0]!=2||p[1]!=102;\n"
                                (logand a mask) (logand a mask)))))
            (insert "U hdr[2]={UINT64_C(174),0},box[2]={UINT64_C(18),0};fail|=word_wrapper(2)!=7||word_wrapper((U)hdr+6)!=46||word_wrapper((U)box)!=18||word_wrapper(UINT64_MAX)!=UINT64_MAX;\n")
            (insert "p[0]=p[1]=0;U r=frame(p);fail|=r!=p[1]||p[0]-p[1]!=8;return fail;}\n"))
          (should (= 0 (call-process "gcc-14" nil nil nil "-O2" "-fno-stack-protector" "-fno-pie" "-c" c "-o" co)))
          (dolist (disabled '(nil t))
            (let ((count (nelisp-aot-partial-call-test--object `(seq ,@source) o disabled)))
              (should (= 0 (call-process "ld" nil nil nil "-o" exe co o)))
              (should (= 0 (call-process exe nil nil nil)))
              (if disabled (should (= count 0)) (should (>= count 15)))
              (should-not nelisp-aot-compiler--x86-partial-call-facts))))
      (delete-directory dir t))))

(ert-deftest nelisp-aot-partial-call/closed-pure-descriptors ()
  (let* ((params '(a b c d e f))
         (heavy (nelisp-aot-partial-call-test--heavy params))
         (defs (nelisp-aot-partial-call-test--defs
                `(seq (defun allowed ,params (if (= a 0) f ,heavy)))))
         (d (car defs)) (body (nelisp-aot-compiler--ir-get d :body))
         (test (nelisp-aot-compiler--ir-get body :test)))
    (should (gethash 'allowed (nelisp-aot-compiler--x86-partial-call-facts defs)))
    (should-not (gethash 'allowed (nelisp-aot-compiler--x86-partial-call-facts (list d d))))
    (dolist (key '(:internal-hidden :rest-p :variadic :internal-entry :gc-root-slots :self-tco-raw))
      (should-not (gethash 'allowed (nelisp-aot-compiler--x86-partial-call-facts
                                    (list (nelisp-aot-compiler--ir-with d key t))))))
    (dolist (bad '((ptr-read-u64 a 0) (aot-current-sp) (seq (ptr-write-u64 a 0 1) 0)
                  (/ a b) (shl a b) (extern-call x a) (sexp-tag a)))
      (let* ((defs (nelisp-aot-partial-call-test--defs
                    `(seq (defun unsafe ,params (if (= ,bad 0) f ,heavy))))))
        (should-not (gethash 'unsafe (nelisp-aot-compiler--x86-partial-call-facts defs)))))
    (dolist (ref (list (nelisp-aot-compiler--make-ir 'ref :var 'other :slot 0 :class 'gp)
                      (nelisp-aot-compiler--make-ir 'ref :var 'a :slot 1 :class 'gp)
                      (nelisp-aot-compiler--make-ir 'ref :var 'a :slot 0 :class 'gp :root-p t)))
      (should-not (nelisp-aot-compiler--x86-partial-call-value ref params)))
    (let ((simple (nelisp-aot-compiler--ir-with body :else
                   (nelisp-aot-compiler--make-ir 'imm :value 4))))
      (should-not (gethash 'allowed (nelisp-aot-compiler--x86-partial-call-facts
                                    (list (nelisp-aot-compiler--ir-with d :body simple))))))
    ;; A guard may not silently acquire a memory-reading node or an f64 ref.
    (should-not (nelisp-aot-compiler--x86-partial-call-guard-p
                 (nelisp-aot-compiler--ir-with test :a
                  (nelisp-aot-compiler--make-ir 'ptr-read :ptr (nelisp-aot-compiler--ir-get test :a)
                                               :offset 0 :size 1)) params))))

(ert-deftest nelisp-aot-partial-call/callsite-scope-and-fallbacks ()
  (let* ((params '(a b c d e f))
         (heavy (nelisp-aot-partial-call-test--heavy params))
         (defs (nelisp-aot-partial-call-test--defs
                `(seq (defun guarded ,params (if (= a 0) f ,heavy)))))
         (nelisp-aot-compiler--x86-trivial-call-facts nil)
         (nelisp-aot-compiler--x86-partial-call-facts
          (nelisp-aot-compiler--x86-partial-call-facts defs))
         (nelisp-aot-compiler--arch 'x86_64) (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--checked-arith nil)
         (args (mapcar (lambda (_) (nelisp-aot-compiler--make-ir 'imm :value 0)) params))
         (node (nelisp-aot-compiler--make-ir 'call :name 'guarded :args args)))
    (cl-labels ((folded (n)
                 (let ((fold nil))
                   (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-partial-named-call-x86)
                              (lambda (&rest _) (setq fold t)))
                             ((symbol-function 'nelisp-asm-x86_64-call-rel32) #'ignore))
                     (nelisp-aot-compiler--emit-named-gp-call-x86
                      n (nelisp-asm-x86_64-make-buffer 'sysv))) fold)))
      (should (folded node))
      (dolist (change '((:args) (:varargs-p . t) (:fn-value . t)
                        (:internal-boundary-disp . 8) (:ret-class . f64)))
        (should-not (folded (nelisp-aot-compiler--ir-with node (car change) (cdr change)))))
      (should-not (folded (nelisp-aot-compiler--ir-with node :name 'independent)))
      (should-not (folded (nelisp-aot-compiler--ir-with node :args
                           (cons (nelisp-aot-compiler--ir-with (car args) :class 'f64) (cdr args)))))
      (let ((nelisp-aot-compiler--abi 'win64)) (should-not (folded node)))
      (let ((nelisp-aot-compiler--arch 'aarch64)) (should-not (folded node)))
      (let ((nelisp-aot-compiler--checked-arith t)) (should-not (folded node)))
      (cl-letf (((symbol-function 'nelisp-aot-compiler--lisp-values-p) (lambda () t)))
        (should-not (folded node)))
      (let ((nelisp-aot-compiler--native-emit-enabled t))
        (cl-letf (((symbol-function 'nl-jit-call-out-2) #'ignore)) (should-not (folded node)))))))

(provide 'nelisp-aot-partial-call-test)
