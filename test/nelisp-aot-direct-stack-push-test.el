;;; nelisp-aot-direct-stack-push-test.el --- Pure stack leaf PUSH -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-stack-args-test)
(defvar nelisp-aot-direct-stack-push-test--enabled t)
(defvar nelisp-aot-direct-stack-push-test--selected 0)
(defun nelisp-aot-direct-stack-push-test--gauge (result)
  (when result (cl-incf nelisp-aot-direct-stack-push-test--selected)) result)
(defun nelisp-aot-direct-stack-push-test--arg (kind value)
  (nelisp-aot-compiler--make-ir kind (if (eq kind 'imm) :value :slot) value :class 'gp))
(defun nelisp-aot-direct-stack-push-test--as (source)
  (let* ((dir (make-temp-file "stack-push-as-" t)) (s (expand-file-name "p.s" dir))
         (o (expand-file-name "p.o" dir)) (bin (expand-file-name "p.bin" dir)))
    (unwind-protect
        (progn
          (with-temp-file s (insert ".intel_syntax noprefix\n.text\n" source "\n"))
          (with-temp-buffer
            (should (= 0 (call-process "as" nil t nil "--64" s "-o" o)))
            (should (= 0 (call-process "objcopy" nil t nil "-O" "binary" "-j" ".text" o bin))))
          (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally bin) (buffer-string)))
      (delete-directory dir t))))
(ert-deftest nelisp-aot-direct-stack-push/GNUas-signed-limits-alias-register-depth ()
  (let ((cases '((-2147483648 "push -2147483648") (-129 "push -129")
                 (-128 "push -128") (0 "push 0") (127 "push 127")
                 (128 "push 128") (2147483647 "push 2147483647")))
        (all "") (asm ""))
    (dolist (row cases)
      (let ((buf (nelisp-asm-x86_64-make-buffer)) (nelisp-aot-compiler--rsp-temp-depth 3))
        (should (nelisp-aot-compiler--emit-direct-stack-tail-push-x86
                 (nelisp-aot-direct-stack-push-test--arg 'imm (car row)) buf))
        (should (= nelisp-aot-compiler--rsp-temp-depth 4))
        (setq all (concat all (nelisp-asm-x86_64-buffer-bytes buf)) asm (concat asm (cadr row) "\n"))))
    (dolist (row '((0 nil nil "push QWORD PTR [rbp-8]")
                   (20 nil nil "push QWORD PTR [rbp-168]")
                   (99 nil ((99 . 20) (20 . 2)) "push QWORD PTR [rbp-24]")
                   (1 ((1 . rbx)) nil "push rbx") (2 ((2 . r10)) nil "push r10")
                   (3 ((3 . r15)) nil "push r15")))
      (let ((buf (nelisp-asm-x86_64-make-buffer)) (nelisp-aot-compiler--rsp-temp-depth 0)
            (nelisp-aot-compiler--promoted-slots (nth 1 row))
            (nelisp-aot-compiler--x86-frame-read-aliases (nth 2 row)))
        (should (nelisp-aot-compiler--emit-direct-stack-tail-push-x86
                 (nelisp-aot-direct-stack-push-test--arg 'ref (car row)) buf))
        (should (= nelisp-aot-compiler--rsp-temp-depth 1))
        (setq all (concat all (nelisp-asm-x86_64-buffer-bytes buf)) asm (concat asm (nth 3 row) "\n"))))
    (should (equal all (nelisp-aot-direct-stack-push-test--as asm))))
  (dolist (value '(-9223372036854775808 -2147483649 2147483648 4294967295 9223372036854775807))
    (let ((buf (nelisp-asm-x86_64-make-buffer)) (nelisp-aot-compiler--rsp-temp-depth 2))
      (should-not (nelisp-aot-compiler--emit-direct-stack-tail-push-x86
                   (nelisp-aot-direct-stack-push-test--arg 'imm value) buf))
      (should (= 0 (nelisp-asm-x86_64-buffer-pos buf)))
      (should (= nelisp-aot-compiler--rsp-temp-depth 2)))))
(ert-deftest nelisp-aot-direct-stack-push/original-admission-final-RAX-and-six-GP-placements ()
  (funcall (ert-test-body (ert-get-test 'nelisp-aot-stack-args/preflight-borders)))
  (dolist (n '(7 10 13))
    (dolist (depth '(0 1))
      (let* ((node (nelisp-aot-stack-test--node n))
             (args (nelisp-aot-compiler--ir-get node :args)) (seen nil)
             (buf (nelisp-asm-x86_64-make-buffer)) (nelisp-aot-compiler--rsp-temp-depth depth)
             (nelisp-aot-direct-stack-push-test--selected 0)
             (original (symbol-function 'nelisp-aot-compiler--emit-value-into-x86)))
        (should (nelisp-aot-compiler--x86-direct-gp-stack-call-p node))
        (advice-add 'nelisp-aot-compiler--emit-direct-stack-tail-push-x86 :filter-return #'nelisp-aot-direct-stack-push-test--gauge)
        (unwind-protect
            (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-value-into-x86)
                       (lambda (arg buffer dst) (push (cons arg dst) seen) (funcall original arg buffer dst))))
              (nelisp-aot-compiler--emit-direct-gp-stack-call-x86 node buf nil))
          (advice-remove 'nelisp-aot-compiler--emit-direct-stack-tail-push-x86 #'nelisp-aot-direct-stack-push-test--gauge))
        (should (= nelisp-aot-compiler--rsp-temp-depth depth))
        (should (= nelisp-aot-direct-stack-push-test--selected (- n 7)))
        (should (equal (nreverse seen)
                       (cons (cons (nth 6 args) 'rax)
                             (cl-mapcar #'cons (cl-subseq args 0 6) '(rdi rsi rdx rcx r8 r9))))))))
  (dolist (reg '(rax rdi rsi rdx rcx r8 r9 rsp rbp))
    (let ((nelisp-aot-compiler--promoted-slots (list (cons 0 reg)))
          (buf (nelisp-asm-x86_64-make-buffer)) (nelisp-aot-compiler--rsp-temp-depth 0))
      (should-not (nelisp-aot-compiler--emit-direct-stack-tail-push-x86
                   (nelisp-aot-direct-stack-push-test--arg 'ref 0) buf))
      (should (= (nelisp-asm-x86_64-buffer-pos buf) 0))))
  (let ((nelisp-aot-compiler--x86-direct-stack-push nil) (buf (nelisp-asm-x86_64-make-buffer)))
    (should-not (nelisp-aot-compiler--emit-direct-stack-tail-push-x86
                 (nelisp-aot-direct-stack-push-test--arg 'imm 1) buf))
    (should (= (nelisp-asm-x86_64-buffer-pos buf) 0))))
(defconst nelisp-aot-direct-stack-push-test--literals
  '(-2147483648 -129 -128 127 128 2147483647 4294967295 9223372036854775807 -9223372036854775808))
(defun nelisp-aot-direct-stack-push-test--source (prefix)
  (cons 'seq
        (cl-loop for n in '(7 10 13) append
          (cl-loop for mode below 3 append
            (let* ((vars (cl-loop for i below n collect (intern (format "v%d" i))))
                   (name (intern (format "%s%d_m%d" prefix n mode)))
                   (sink (intern (format "%s_sink%d_m%d" prefix n mode)))
                   (args (cl-loop for v in vars for i from 0 collect
                           (if (or (= mode 0) (< i 7)) v
                             (nth (mod (+ (- i 7) (if (= mode 1) 0 4)) 9)
                                  nelisp-aot-direct-stack-push-test--literals)))))
              `((defun ,sink ,vars (extern-call ,(intern (format "cap%d" n)) ,@vars))
                (defun ,name ,vars (,sink ,@args))))))))
(defun nelisp-aot-direct-stack-push-test--snapshot-asm (label)
  (concat
   (mapconcat (lambda (pair) (format "movq %%%s,%s+%d(%%rip)\n" (car pair) label (* 8 (cdr pair))))
              '((rax . 0) (rdi . 1) (rsi . 2) (rdx . 3) (rcx . 4) (r8 . 5) (r9 . 6) (r10 . 7) (r11 . 8)
                (rsp . 10) (rbx . 11) (r12 . 12) (r13 . 13) (r14 . 14) (r15 . 15)) "")
   (format "pushfq\npopq %s+72(%%rip)\n" label)))
(defun nelisp-aot-direct-stack-push-test--asm ()
  (concat ".text\n"
   (mapconcat
    (lambda (n)
      (concat (format ".globl cap%d\ncap%d:\n" n n)
              (nelisp-aot-direct-stack-push-test--snapshot-asm "snap")
              (mapconcat (lambda (i) (format "movq %d(%%rsp),%%rax\nmovq %%rax,snap+%d(%%rip)\n" (+ 8 (* 8 (- i 6))) (* 8 (+ 16 (- i 6))))) (number-sequence 6 (1- n)) "")
              "movabsq $0xabcdeffedcba1234,%rax\nret\n")) '(7 10 13) "")
   (mapconcat
    (lambda (n)
      (mapconcat
       (lambda (mode)
         (mapconcat
          (lambda (prefix)
            (let* ((name (format "%s%d_m%d" prefix n mode)) (tail (- n 6)) (pad (= (mod tail 2) 0)))
              (concat (format ".globl invoke_%s\ninvoke_%s:\npushq %%rbp\nmovq %%rsp,%%rbp\npushq %%rbx\npushq %%r12\npushq %%r13\npushq %%r14\npushq %%r15\nmovq %%rdi,%%rax\n" name name)
                      (if pad "subq $8,%rsp\n" "")
                      (mapconcat (lambda (i) (format "pushq %d(%%rax)\n" (* i 8))) (reverse (number-sequence 6 (1- n))) "")
                      (mapconcat (lambda (pair) (format "movq %d(%%rax),%%%s\n" (* 8 (cdr pair)) (car pair))) '((rdi . 0) (rsi . 1) (rdx . 2) (rcx . 3) (r8 . 4) (r9 . 5)) "")
                      "movabsq $0x1122334455667701,%rbx\nmovabsq $0x1122334455667702,%r12\nmovabsq $0x1122334455667703,%r13\nmovabsq $0x1122334455667704,%r14\nmovabsq $0x1122334455667705,%r15\nmovabsq $0x76543210abcdef02,%r10\nmovabsq $0xfedcba9876543216,%r11\ncmpq %r10,%r11\nmovabsq $0x123456789abcdef0,%rax\n"
                      (format "call %s\n" name)
                      (nelisp-aot-direct-stack-push-test--snapshot-asm "after")
                      (format "addq $%d,%%rsp\n" (* 8 (+ tail (if pad 1 0))))
                      "popq %r15\npopq %r14\npopq %r13\npopq %r12\npopq %rbx\npopq %rbp\nret\n"))) '("on" "off") "")) '(0 1 2) "")) '(7 10 13) "")
   ".section .note.GNU-stack,\"\",@progbits\n"))
(defun nelisp-aot-direct-stack-push-test--fault-code (file disabled last-p)
  (let* ((buf (nelisp-asm-x86_64-make-buffer)) (node (nelisp-aot-stack-test--node 13))
         (nelisp-aot-compiler--rsp-temp-depth 0) (nelisp-aot-compiler--promoted-slots nil)
         (nelisp-aot-compiler--x86-frame-read-aliases (list (cons (if last-p 20 26) 600)))
         (nelisp-aot-compiler--x86-direct-stack-push (not disabled)))
    (nelisp-asm-x86_64-push buf 'rbp)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rbp 'rdi)
    (nelisp-aot-compiler--x86-op buf t (unibyte-string #x89) 'rsp '(:mem nil nil 1 805306368))
    (should (nelisp-aot-compiler--x86-direct-gp-stack-call-p node))
    (nelisp-aot-compiler--emit-direct-gp-stack-call-x86 node buf nil)
    (nelisp-asm-x86_64-pop buf 'rbp) (nelisp-asm-x86_64-ret buf)
    (nelisp-asm-x86_64-define-label buf 'callee) (nelisp-asm-x86_64-ret buf)
    (nelisp-asm-x86_64-resolve-fixups buf)
    (with-temp-file file (set-buffer-multibyte nil) (insert (nelisp-asm-x86_64-buffer-bytes buf)))))
(defun nelisp-aot-direct-stack-push-test--c ()
  (concat
   "#define _GNU_SOURCE\n#include <stdint.h>\n#include <stdio.h>\n#include <string.h>\n#include <stdlib.h>\n#include <signal.h>\n#include <ucontext.h>\n#include <sys/mman.h>\n#include <sys/wait.h>\n#include <unistd.h>\ntypedef uint64_t U;typedef U(*F)(const U*);volatile U snap[32],after[32];\n"
   (mapconcat (lambda(n) (mapconcat (lambda(m) (mapconcat (lambda(p) (format "extern U invoke_%s%d_m%d(const U*);\n" p n m)) '("on" "off") "")) '(0 1 2) "")) '(7 10 13) "")
   "static F calls[3][3][2]={"
   (mapconcat (lambda(n) (concat "{" (mapconcat (lambda(m) (format "{invoke_on%d_m%d,invoke_off%d_m%d}" n m n m)) '(0 1 2) ",") "}")) '(7 10 13) ",") "};\n"
   "__attribute__((noinline)) static int capture(F f,const U*v,U*out){memset((void*)snap,0,sizeof snap);memset((void*)after,0,sizeof after);U r=f(v);if(r!=0xabcdeffedcba1234ULL)return 1;memcpy(out,(const void*)snap,sizeof snap);memcpy(out+32,(const void*)after,sizeof after);return 0;}\n"
   "static volatile U *obs;static U*fake;static unsigned pushed;static void fault(int sig,siginfo_t*si,void*ctx){ucontext_t*u=ctx;U sp=u->uc_mcontext.gregs[REG_RSP];if(sig!=SIGSEGV||si->si_addr!=(void*)(fake-601)||sp!=obs[0]-8-8*pushed)_exit(41);U *s=(U*)(uintptr_t)sp;for(unsigned j=0;j<pushed;j++)if(s[j]!=108+j)_exit(42);_exit(0);}\n"
   "static void*loadcode(const char*path){FILE*f=fopen(path,\"rb\");if(!f)return 0;fseek(f,0,SEEK_END);size_t n=ftell(f);rewind(f);void*p=mmap(0,4096,7,0x22,-1,0);if(p==MAP_FAILED||n>4096||fread(p,1,n,f)!=n)return 0;fclose(f);return p;}\n"
   "int main(int argc,char**argv){U v[13],x[64],y[64];U pool[]={0,1,2,6,~0ULL,0x80000000ULL,0xffffffffULL,0x100000001ULL,0x8000000000000000ULL,0x7fffffffffffffffULL,0xabcdef0123456786ULL,0xffffffff80000000ULL};U literals[]={0xffffffff80000000ULL,(U)-129,(U)-128,127,128,2147483647,4294967295,0x7fffffffffffffffULL,0x8000000000000000ULL};unsigned ns[]={7,10,13},count=0;for(unsigned a=0;a<3;a++)for(unsigned m=0;m<3;m++)for(unsigned k=0;k<12;k++){for(unsigned i=0;i<13;i++)v[i]=pool[(i+k)%12];if(capture(calls[a][m][0],v,x)||capture(calls[a][m][1],v,y))return 1;if(memcmp(x,y,sizeof x))return 2;U expected[13];memcpy(expected,v,sizeof v);for(unsigned i=7;m&&i<ns[a];i++)expected[i]=literals[(i-7+(m==1?0:4))%9];if(x[0]!=expected[6])return 3;for(unsigned i=0;i<6;i++)if(x[i+1]!=expected[i])return 4;for(unsigned i=6;i<ns[a];i++)if(x[16+i-6]!=expected[i])return 5;if((x[10]&15)!=8||x[32]!=0xabcdeffedcba1234ULL)return 6;for(unsigned i=0;i<5;i++)if(x[32+11+i]!=0x1122334455667701ULL+i)return 7;count++;}\n"
   "if(argc!=5)return 8;obs=mmap((void*)0x30000000,4096,3,0x32,-1,0);U*frame=mmap((void*)0x31000000,8192,3,0x32,-1,0);if(obs==(void*)-1||frame==(void*)-1)return 9;fake=frame+1024;for(unsigned i=0;i<13;i++)fake[-(intptr_t)(15+i)]=101+i;fake[-601]=777;struct sigaction sa={0};sa.sa_sigaction=fault;sa.sa_flags=SA_SIGINFO;sigaction(SIGSEGV,&sa,0);if(mprotect(frame,4096,0))return 10;for(unsigned i=0;i<4;i++){void*p=loadcode(argv[i+1]);if(!p)return 11;pushed=i%2?6:0;pid_t child=fork();if(child==0){((U(*)(U*))p)(fake);_exit(43);}int status;waitpid(child,&status,0);if(!WIFEXITED(status)||WEXITSTATUS(status))return 12;}printf(\"NATIVE %u ENTRY EXIT FAULT4\\n\",count);return 0;}\n"))
(ert-deftest nelisp-aot-direct-stack-push/native-full64-entry-exit-order-fault-WORD-SP-callees ()
  (let* ((dir (make-temp-file "direct-stack-native-" t))
         (c (expand-file-name "p.c" dir)) (s (expand-file-name "p.s" dir))
         (exe (expand-file-name "p" dir)) (on (expand-file-name "on.o" dir))
         (off (expand-file-name "off.o" dir)) (counts nil) (faults nil))
    (unwind-protect
        (progn
          (with-temp-file c (insert (nelisp-aot-direct-stack-push-test--c)))
          (with-temp-file s (insert (nelisp-aot-direct-stack-push-test--asm)))
          (advice-add 'nelisp-aot-compiler--emit-direct-stack-tail-push-x86 :filter-return #'nelisp-aot-direct-stack-push-test--gauge)
          (dolist (disabled '(nil t))
            (let ((nelisp-aot-compiler--object-hidden-boundary-symbols nil)
                  (nelisp-aot-compiler--allow-external-user-calls t)
                  (nelisp-aot-compiler--x86-direct-stack-push
                   (and nelisp-aot-direct-stack-push-test--enabled (not disabled)))
                  (nelisp-aot-direct-stack-push-test--selected 0))
              (nelisp-aot-compile-to-object
               (nelisp-aot-direct-stack-push-test--source (if disabled "off" "on")) (if disabled off on))
              (push nelisp-aot-direct-stack-push-test--selected counts)))
          (advice-remove 'nelisp-aot-compiler--emit-direct-stack-tail-push-x86 #'nelisp-aot-direct-stack-push-test--gauge)
          (dolist (disabled '(nil t))
            (dolist (last '(nil t))
              (let ((path (expand-file-name (format "fault-%s-%s.bin" disabled last) dir)))
                (let ((nelisp-aot-compiler--x86-direct-stack-push
                       (and nelisp-aot-direct-stack-push-test--enabled (not disabled))))
                  (if nelisp-aot-direct-stack-push-test--enabled
                      (nelisp-aot-direct-stack-push-test--fault-code path disabled last)
                    (nelisp-aot-direct-stack-push-test--fault-code path t last)))
                (push path faults))))
          (with-temp-buffer
            (should (= 0 (call-process "gcc-14" nil t nil "-O2" "-no-pie" c s on off "-o" exe)))
            (erase-buffer)
            (should (= 0 (apply #'call-process exe nil t nil (nreverse faults))))
            (should (string= (buffer-string) "NATIVE 108 ENTRY EXIT FAULT4\n"))
            (princ (concat "SEMANTICS on/off full64/RAX/FLAGS/allGP/SP/callee/WORD PASS " (buffer-string))))
          ;; Both semantic modes and all four deliberate fault paths precede the gauge.
          (should (= (car counts) 0))
          (should (> (cadr counts) 0)))
      (advice-remove 'nelisp-aot-compiler--emit-direct-stack-tail-push-x86 #'nelisp-aot-direct-stack-push-test--gauge)
      (delete-directory dir t))))
(provide 'nelisp-aot-direct-stack-push-test)
