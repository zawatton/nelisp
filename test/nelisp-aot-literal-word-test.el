;;; nelisp-aot-literal-word-test.el --- Static WORD discriminators -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defun nelisp-aot-literal-word-test--cmp (word tag &optional op swapped)
  (let ((x (nelisp-aot-compiler--make-ir
            'sexp-tag :ptr (if (integerp word)
                               (nelisp-aot-compiler--make-ir 'imm :value word) word)))
        (k (nelisp-aot-compiler--make-ir 'imm :value tag)))
    (nelisp-aot-compiler--make-ir 'cmp :op (or op '=)
                                :a (if swapped k x) :b (if swapped x k))))

(defun nelisp-aot-literal-word-test--emit (test jump disabled)
  (let ((buf (nelisp-asm-x86_64-make-buffer))
        (target (make-symbol "literal-word-target"))
        (nelisp-aot-compiler--x86-literal-word-tags (not disabled)))
    (let ((result (nelisp-aot-compiler--emit-tag-test-jump-x86 test buf target jump)))
      (nelisp-asm-x86_64-define-label buf target)
      (list result (nelisp-asm-x86_64-resolve-fixups buf)))))

(defvar nelisp-aot-literal-word-test--reads 0)
(defun nelisp-aot-literal-word-test--read (&rest _)
  (cl-incf nelisp-aot-literal-word-test--reads))

(ert-deftest nelisp-aot-literal-word/finite-discriminators-and-old-fallback ()
  (let ((words (list 2 #x8000000000000002 (- 2 #x10000000000000000)
                     (+ 2 #x10000000000000000) #x32000000 #x32000004
                     #x32001006 (+ #x32001006 #x20000000000000000))))
    (advice-add 'nelisp-asm-x86_64-movzx-reg-byte-mem
                :before #'nelisp-aot-literal-word-test--read)
    (unwind-protect
        (dolist (word words)
          (dotimes (tag 256)
            (dolist (op '(= /=))
              (dolist (swap '(nil t))
                (dolist (jump '(nil t))
                  (let* ((nelisp-aot-literal-word-test--reads 0)
                         (cmp (nelisp-aot-literal-word-test--cmp word tag op swap))
                         (result (nelisp-aot-literal-word-test--emit cmp jump nil)))
                    (if (memq tag '(1 2))
                        (should-not (car result))
                      (should (car result))
                      (should (= (if (= (logand word 7) 2) 0 1)
                                 nelisp-aot-literal-word-test--reads)))))))))
      (advice-remove 'nelisp-asm-x86_64-movzx-reg-byte-mem
                     #'nelisp-aot-literal-word-test--read)))
  (let ((ref (nelisp-aot-compiler--make-ir 'ref :slot 0 :var 'x :class 'gp)))
    (dolist (word (list ref 1 3 -1 #x8000000000000001
                       (nelisp-aot-compiler--make-ir 'imm :value #x32000000 :cls 'f64)
                       (nelisp-aot-compiler--make-ir 'imm :value #x32000000 :repr 'word)
                       (nelisp-aot-compiler--make-ir 'imm :value #x32000000 :root-p t)))
      (let ((cmp (nelisp-aot-literal-word-test--cmp word 4)))
        (should (equal (nelisp-aot-literal-word-test--emit cmp nil nil)
                       (nelisp-aot-literal-word-test--emit cmp nil t))))))
  (dolist (mode '(lisp checked typed))
    (let ((nelisp-aot-compiler--runtime-entry-params (eq mode 'lisp))
          (nelisp-aot-compiler--checked-arith (eq mode 'checked))
          (cmp (nelisp-aot-literal-word-test--cmp #x32000000 4)))
      (when (eq mode 'typed) (setq cmp (nelisp-aot-compiler--ir-with cmp :repr 'sexp-ptr)))
      (should (equal (nelisp-aot-literal-word-test--emit cmp t nil)
                     (nelisp-aot-literal-word-test--emit cmp t t))))))

(defconst nelisp-aot-literal-word-test--true #x8123456789abcdef)
(defconst nelisp-aot-literal-word-test--false #x1122334455667788)
(defun nelisp-aot-literal-word-test--source ()
  (let ((words '(2 #x8000000000000002 #x32000000 #x32000004 #x32001006))
        (tags (cons 0 (number-sequence 3 255))) defs)
    (cl-loop for word in words for wi from 0 do
      (dotimes (mode 4)
        (push `(defun ,(intern (format "pw_%d_%d" wi mode)) (tag)
                 (cond
                  ,@(mapcar (lambda (k)
                              (let* ((x `(sexp-tag ,word))
                                     (cmp (list (if (= (% mode 2) 0) '= '/=)
                                                (if (< mode 2) x k) (if (< mode 2) k x))))
                                `((= tag ,k) (if ,cmp ,nelisp-aot-literal-word-test--true
                                               ,nelisp-aot-literal-word-test--false)))) tags)
                  (t 0))) defs)))
    (cl-loop for word in (list (+ 2 #x10000000000000000) (- 2 #x10000000000000000)
                               (- #x8000000000000002 #x10000000000000000)
                               (+ #x32000000 #x10000000000000000)
                               (- #x32000004 #x10000000000000000)
                               (+ #x32001006 #x20000000000000000))
             for i from 0 do
             (push `(defun ,(intern (format "pw_high_%d" i)) ()
                      (if (= (sexp-tag ,word) ,(if (< i 3) 7 4))
                          ,nelisp-aot-literal-word-test--true
                        ,nelisp-aot-literal-word-test--false)) defs))
    (append (list 'seq) (nreverse defs)
            `((defun pw_guard (gate)
                (if (= gate 0) ,nelisp-aot-literal-word-test--true
                  (if (= (sexp-tag #x8000000000000006) 4)
                      ,nelisp-aot-literal-word-test--true ,nelisp-aot-literal-word-test--false)))
              (defun pw_fault ()
                (seq (ptr-write-u64 #x32002000 0 1)
                     (if (= (sexp-tag #x32003006) 4)
                         (seq (ptr-write-u64 #x32002000 8 9) ,nelisp-aot-literal-word-test--true)
                       (seq (ptr-write-u64 #x32002000 8 7) ,nelisp-aot-literal-word-test--false))))))))

(defvar nelisp-aot-literal-word-test--selected 0)
(defun nelisp-aot-literal-word-test--record (fn &rest args)
  (let ((result (apply fn args)))
    (when result (cl-incf nelisp-aot-literal-word-test--selected)) result))

(ert-deftest nelisp-aot-literal-word/native-full-domain-order-fault-and-full64 ()
  (let* ((dir (make-temp-file "nelisp-literal-word-" t))
         (c (expand-file-name "probe.c" dir)) counts)
    (unwind-protect
        (progn
          (with-temp-file c
            (insert "#define _GNU_SOURCE\n#include <stdint.h>\n#include <sys/mman.h>\n#include <signal.h>\n#include <setjmp.h>\n#include <stdlib.h>\n#include <stdio.h>\n")
            (dotimes (wi 5) (dotimes (mode 4)
                              (insert (format "extern uint64_t pw_%d_%d(uint64_t);\n" wi mode))))
            (dotimes (i 6) (insert (format "extern uint64_t pw_high_%d(void);\n" i)))
            (insert "extern uint64_t pw_guard(uint64_t),pw_fault(void);\n"
                    "typedef uint64_t(*fn)(uint64_t);static fn probes[]={")
            (insert (mapconcat (lambda (i) (format "pw_%d_%d" (/ i 4) (% i 4))) (number-sequence 0 19) ","))
            (insert "};static uint64_t(*high[])(void)={pw_high_0,pw_high_1,pw_high_2,pw_high_3,pw_high_4,pw_high_5};\n"
                    "static volatile unsigned faults,bad;static sigjmp_buf landing;\n"
                    "static void handler(int s,siginfo_t*i,void*c){(void)s;(void)c;if(bad){faults++;siglongjmp(landing,1);}uintptr_t a=(uintptr_t)i->si_addr;volatile uint64_t*m=(void*)0x32002000;if(a<0x32003000||a>=0x32004000||m[0]!=1||m[1]!=0)_Exit(90);faults++;if(mprotect((void*)0x32003000,4096,3))_Exit(91);*(volatile unsigned char*)0x32003000=0xc4;}\n"
                    "int main(void){uint8_t*p=(void*)0x32000000;if(mmap(p,16384,3,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED,-1,0)==MAP_FAILED)return 80;struct sigaction sa={0};sa.sa_flags=SA_SIGINFO;sa.sa_sigaction=handler;if(sigaction(SIGSEGV,&sa,0))return 81;const uint64_t yes=UINT64_C(0x8123456789abcdef),no=UINT64_C(0x1122334455667788);unsigned cases=0;for(unsigned h=0;h<64;h++){p[0]=p[4]=p[4096]=(uint8_t)(h|0xc0);for(unsigned w=0;w<5;w++)for(unsigned m=0;m<4;m++)for(unsigned k=0;k<256;k++){if(k==1||k==2)continue;unsigned t=w<2?7:h;int match=t==k;if(m&1)match=!match;if(probes[w*4+m](k)!=(match?yes:no))return 82;cases++;}}p[0]=p[4]=p[4096]=0xc4;for(unsigned i=0;i<6;i++)if(high[i]()!=yes)return 83;if(pw_guard(0)!=yes)return 84;bad=1;if(sigsetjmp(landing,1)==0){pw_guard(1);return 85;}bad=0;if(faults!=1)return 86;faults=0;volatile uint64_t*mark=(void*)0x32002000;mark[0]=mark[1]=0;if(mprotect((void*)0x32003000,4096,PROT_NONE))return 87;if(pw_fault()!=yes||faults!=1||mark[0]!=1||mark[1]!=9)return 88;printf(\"P-NATIVE-PASS cases=%u\\n\",cases+8);return 0;}\n"))
          (dolist (disabled '(nil t))
            (let* ((stem (expand-file-name (if disabled "off" "on") dir))
                   (object (concat stem ".o"))
                   (nelisp-aot-compiler--object-hidden-boundary-symbols nil)
                   (nelisp-aot-compiler--x86-literal-word-tags
                    (not (or disabled (getenv "NELISP_P_FORCE_DISABLED"))))
                   (nelisp-aot-literal-word-test--selected 0))
              (advice-add 'nelisp-aot-compiler--emit-literal-word-tag-test-x86
                          :around #'nelisp-aot-literal-word-test--record)
              (unwind-protect
                  (nelisp-aot-compile-to-object (nelisp-aot-literal-word-test--source) object)
                (advice-remove 'nelisp-aot-compiler--emit-literal-word-tag-test-x86
                               #'nelisp-aot-literal-word-test--record))
              (push nelisp-aot-literal-word-test--selected counts)
              (should (= 0 (call-process "gcc-14" nil nil nil "-O2" "-no-pie" c object "-o" stem)))
              (should (= 0 (call-process stem nil nil nil)))))
          ;; The disabled negative reaches this admission gauge after values.
          (should (= 0 (car counts)))
          (should (> (cadr counts) 0)))
      (delete-directory dir t))))

(provide 'nelisp-aot-literal-word-test)
