;;; nelisp-aot-local-prefix-test.el --- write-through local prefix -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(defvar nelisp-prefix-test--at nil)
(defvar nelisp-prefix-test--selected 0)
(defvar nelisp-prefix-test--forced-off (getenv "NELISP_PREFIX_TEST_DISABLE"))
(defun nelisp-prefix-test--binding (original node buf emit)
  (let ((nelisp-prefix-test--at
         (and (or (null nelisp-aot-compiler--promoted-slots)
                  nelisp-aot-compiler--x86-local-prefix-state)
              (nelisp-asm-x86_64-buffer-pos buf))))
    (funcall original node buf emit)))
(defun nelisp-prefix-test--move (buf dst src)
  (when (and nelisp-prefix-test--at
             (= nelisp-prefix-test--at (nelisp-asm-x86_64-buffer-pos buf))
             (memq dst '(r8 r9)) (eq src 'rax))
    (cl-incf nelisp-prefix-test--selected)))
(defconst nelisp-prefix-test--source
  '(seq
    (defun av_forward (x choose)
      (let* ((v (logxor x 18364758544493064720)))
        (if (and (/= v 0) (/= (logand v 255) 255))
            (if (= choose 0)
                (seq (extern-call av_touch v -24 (+ v 7)) (+ v v))
              (+ v v))
          (+ v 3))))
    ;; The operand call clobbers GP registers without mutating a source local.
    ;; av_forward separately tests explicit call-boundary home mutation/reload.
    (defun av_operand (x)
      (let* ((v (+ x 1)))
        (if (and (/= v 0) (/= v -1))
            (let* ((result (+ v (extern-call av_unknown v)))) (+ result v))
          v)))
    (defun av_backedge (n seed)
      (let* ((hold (logxor seed 18364758544493064720)) (sum 0) (i 0))
        (seq (if (and (/= hold 0) (/= hold -1)) 0 0)
             (while (< i n) (seq (setq sum (+ sum hold)) (setq i (+ i 1))))
             (extern-call av_unknown seed) (+ sum hold))))
    (defun av_pressure (x)
      (let* ((a (+ x 1)) (b (+ x 2)) (c (+ x 3)))
        (if (and (/= a b) (/= b c) (/= c a))
            (seq (extern-call av_unknown a) (+ a (+ b c))) 0)))
    (defun av_word (w)
      (let* ((v (+ w 0)))
        (if (= (logand v 7) 2)
            (if (= (logand v 7) 2) 7 99)
          (if (= (logand v 7) 6) (logand (ptr-read-u8 v -6) 63)
            (+ v (extern-call av_unknown v))))))
    (defun av_division (x)
      (let* ((v (logior x 1)))
        (if (and (/= v 0) (/= v 2))
            (+ (/ v 7) (mod v 11)) (extern-call av_unknown v))))
    (defun av_decode (w)
      (let* ((v (+ w 0)))
        (if (= (logand v 1) 1)
            (+ (sexp-int-unwrap v) (sexp-tag v)) (extern-call av_unknown v))))
    (defun av_fault (p)
      (let* ((v (+ p 0)))
        (if (and (= v 0) (= v 0)) (ptr-read-u64 v 0) (extern-call av_unknown v))))))
(defconst nelisp-prefix-test--assembly
".text
.globl av_touch
av_touch:
 mov (%rbp,%rsi),%rax
 cmp %rdi,%rax
 jne .Ltouch_bad
 mov %rdx,(%rbp,%rsi)
 jmp av_unknown
.Ltouch_bad:
 movl $1,av_bad(%rip)
 jmp av_unknown
.globl av_unknown
av_unknown:
 movabs $0xf1e2d3c4b5a69788,%r8
 movabs $0x8877665544332211,%r9
 mov $13,%eax
 ret
.globl av_invoke
av_invoke:
 push %rbp
 mov %rsp,%rbp
 push %rbx
 push %r12
 push %r13
 push %r14
 push %r15
 sub $24,%rsp
 mov %rcx,(%rsp)
 mov %rsp,8(%rsp)
 mov %rdi,%r11
 mov %rsi,%rdi
 mov %rdx,%rsi
 mov $0x10203040,%ebx
 mov $0x20304050,%r12d
 mov $0x30405060,%r13d
 mov $0x40506070,%r14d
 mov $0x50607010,%r15d
 call *%r11
 pushfq
 pop %r10
 mov (%rsp),%r11
 mov %r10,(%r11)
 cmp %rsp,8(%rsp)
 jne .Labi_bad
 cmp $0x10203040,%rbx
 jne .Labi_bad
 cmp $0x20304050,%r12
 jne .Labi_bad
 cmp $0x30405060,%r13
 jne .Labi_bad
 cmp $0x40506070,%r14
 jne .Labi_bad
 cmp $0x50607010,%r15
 je .Labi_done
.Labi_bad:
 movl $1,av_bad(%rip)
.Labi_done:
 add $24,%rsp
 pop %r15
 pop %r14
 pop %r13
 pop %r12
 pop %rbx
 pop %rbp
 ret
.section .note.GNU-stack,\"\",@progbits
")
(defconst nelisp-prefix-test--driver
"#include <stdint.h>
#include <stdio.h>
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>
volatile int av_bad;
extern uint64_t av_forward(uint64_t,uint64_t),av_operand(uint64_t),av_backedge(uint64_t,uint64_t),av_pressure(uint64_t),av_word(uint64_t),av_fault(uint64_t),av_division(uint64_t),av_decode(uint64_t);
extern uint64_t av_invoke(void*,uint64_t,uint64_t,uint64_t*);
static unsigned char header[64] __attribute__((aligned(8)));
static uint64_t digest;
static int check(void *fn,uint64_t a,uint64_t b,uint64_t expected){uint64_t flags=0;uint64_t r=av_invoke(fn,a,b,&flags);if(r!=expected||av_bad){printf(\"PREFIX oracle delta=%lu bad=%d\\n\",r-expected,av_bad);return 0;}digest=(digest<<5)^(digest>>59)^r^(flags&0x8d5);return 1;}
int main(void){const uint64_t C=UINT64_C(0xfedcba9876543210);uint64_t x=0;
for(unsigned i=0;i<320;i++){x=i<6?(uint64_t[]){0,1,UINT64_MAX,UINT64_C(0x8000000000000000),UINT64_C(0x7fffffffffffffff),C}[i]:x*UINT64_C(6364136223846793005)+1;uint64_t v=x^C;for(unsigned choose=0;choose<2;choose++){uint64_t e=(v!=0&&(v&255)!=255)?2*(v+(choose?0:7)):v+3;if(!check(av_forward,x,choose,e))return 11;}v=x+1;if(!check(av_operand,x,0,(v!=0&&v!=UINT64_MAX)?2*v+13:v))return 12;if(!check(av_backedge,i%8,x,(x^C)*(i%8+1)))return 13;if(!check(av_pressure,x,0,3*x+6))return 14;int64_t z=(int64_t)(x|1);if(!check(av_division,x,0,(uint64_t)(z/7+z%11)))return 15;uint64_t w=(x<<2)|1;if(!check(av_decode,w,0,(uint64_t)(((int64_t)w)>>2)+2))return 16;}
header[0]=42;if(!check(av_word,2,0,7)||!check(av_word,(uint64_t)(uintptr_t)header+6,0,42)||!check(av_word,7,0,20))return 15;
pid_t child=fork();if(child<0)return 16;if(child==0){av_fault(0);_exit(99);}int status;if(waitpid(child,&status,0)!=child||!WIFSIGNALED(status)||WTERMSIG(status)!=SIGSEGV)return 17;
printf(\"PREFIX full64/320/forward/cold-home-mutation/operand/loop/pressure/WORD/fault/SP/5callee/FLAGS %016lx\\n\",digest);return av_bad?18:0;}
")
(defun nelisp-prefix-test--compile (source file enabled)
  (let ((nelisp-aot-compiler--runtime-entry-params nil)
        (nelisp-aot-compiler--object-hidden-boundary-symbols nil)
        (nelisp-aot-compiler--arch 'x86_64) (nelisp-aot-compiler--abi 'sysv)
        (nelisp-aot-compiler--os 'linux)
        (nelisp-aot-compiler--x86-local-prefix-caches
         (and enabled (not nelisp-prefix-test--forced-off)))
        (nelisp-prefix-test--selected 0))
    (advice-add 'nelisp-aot-compiler--emit-local-prefix-let-body :around #'nelisp-prefix-test--binding)
    (advice-add 'nelisp-asm-x86_64-mov-reg-reg :before #'nelisp-prefix-test--move)
    (unwind-protect
        (progn (nelisp-aot-compile-to-object source file) nelisp-prefix-test--selected)
      (advice-remove 'nelisp-asm-x86_64-mov-reg-reg #'nelisp-prefix-test--move)
      (advice-remove 'nelisp-aot-compiler--emit-local-prefix-let-body #'nelisp-prefix-test--binding))))
(ert-deftest nelisp-local-prefix/native-forward-fullword-and-boundaries ()
  (skip-unless (and (executable-find "gcc-14") (eq system-type 'gnu/linux)))
  (let* ((retained (getenv "NELISP_PREFIX_TEST_OUT"))
         (dir (or retained (make-temp-file "nelisp-prefix-" t)))
         (c (expand-file-name "driver.c" dir)) (as (expand-file-name "abi.S" dir)) outputs counts)
    (make-directory dir t)
    (unwind-protect
        (progn
          (with-temp-file c (insert nelisp-prefix-test--driver))
          (with-temp-file as (insert nelisp-prefix-test--assembly))
          (dolist (enabled '(nil t))
            (let* ((mode (if enabled "on" "off"))
                   (obj (expand-file-name (concat mode ".o") dir))
                   (exe (expand-file-name mode dir)))
              (push (nelisp-prefix-test--compile nelisp-prefix-test--source obj enabled) counts)
              (with-temp-buffer
                (let ((status (call-process "gcc-14" nil t nil "-O2" "-fno-pie" "-no-pie" c as obj "-o" exe)))
                  (unless (equal status 0) (message "Native link failure: %s" (buffer-string)))
                  (should (= 0 status))))
              (with-temp-buffer
                (let ((status (call-process exe nil t nil)))
                  (unless (equal status 0) (message "Native failure output: %s" (buffer-string)))
                  (should (= 0 status)))
                (should (string-match-p "PREFIX full64/320" (buffer-string)))
                (push (buffer-string) outputs))))
          (should (equal (car outputs) (cadr outputs)))
          (message "PREFIX both native semantic runs PASS before selector gauge")
          (should (= 0 (cadr counts)))
          (should (> (car counts) 0)))
      (unless retained (delete-directory dir t)))))
(ert-deftest nelisp-local-prefix/one-way-dynamic-unwind ()
  (let* ((outer (cons 1 'r8)) (inner (cons 2 'r9))
         (state (vector t (list inner outer)))
         (nelisp-aot-compiler--x86-local-prefix-state state)
         (nelisp-aot-compiler--promoted-slots (list outer)))
    (let ((nelisp-aot-compiler--promoted-slots (list inner outer)))
      (nelisp-aot-compiler--x86-end-local-prefix)
      (should-not nelisp-aot-compiler--promoted-slots))
    (should-not (aref state 0))
    (should-not (assq 1 nelisp-aot-compiler--promoted-slots))))
(provide 'nelisp-aot-local-prefix-test)
