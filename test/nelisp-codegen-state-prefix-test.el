;;; nelisp-codegen-state-prefix-test.el --- Assigned home prefixes -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(defvar nelisp-aot-compiler--x86-assignment-prefix-caches)
(defvar nelisp-standalone--applyfn-bytecode-helpers)
(defvar nelisp-state-prefix-test--selected 0)
(defvar nelisp-state-prefix-test--let-selected 0)
(defconst nelisp-state-prefix-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))
(defvar nelisp-state-prefix-test--force-off (getenv "NELISP_STATE_PREFIX_DISABLE"))
(defun nelisp-state-prefix-test--record (original node buf emit)
  (funcall original node buf
           (lambda (body)
             (when (and (eq (nelisp-aot-compiler--ir-kind node) 'setq-local)
                        (assq (nelisp-aot-compiler--ir-get node :slot)
                              nelisp-aot-compiler--promoted-slots))
               (cl-incf nelisp-state-prefix-test--selected))
             (when (and (eq (nelisp-aot-compiler--ir-kind node) 'let-rt)
                        (assq (nelisp-aot-compiler--ir-get node :slot)
                              nelisp-aot-compiler--promoted-slots))
               (cl-incf nelisp-state-prefix-test--let-selected))
             (funcall emit body))))
(defconst nelisp-state-prefix-test--source
  '(seq
    (defun cs_dispatch (data n seed)
      (let* ((raw 0) (i 0) (sum seed) (go 1))
        (while (> go 0)
          (if (>= i n) (setq go 0)
            (seq (setq raw (ptr-read-u8 data i))
                 (if (< raw 24)
                     (if (< raw 16)
                         (if (< raw 8) (setq sum (+ sum 1)) (setq sum (+ sum 2)))
                       (setq sum (+ sum 3)))
                   (if (= raw 255) (setq sum (+ sum (extern-call cs_unknown raw)))
                     (setq sum (+ sum 4))))
                 (setq i (+ i 1)))))
        sum))
    (defun cs_join (x choose)
      (let* ((v 0))
        (seq (setq v (logxor x 18364758544493064720))
             (if (/= v 0)
                 (if (/= v -1)
                     (if (= choose 0) (extern-call cs_touch v -24 (+ v 7)) (+ v v)) 0) 0)
             (+ v v))))
    (defun cs_reassign (x)
      (let* ((v 0))
        (seq (setq v (+ x 1))
             (if (and (/= v 0) (/= v -1)) (setq v (+ v 7)) 0)
             (extern-call cs_unknown x) (+ v v))))
    (defun cs_word (x)
      (let* ((v 0))
        (seq (setq v (+ x 0))
             (if (and (= (logand v 7) 6) (/= v 0))
                 (ptr-read-u8 v -6) (+ v (extern-call cs_unknown v))))))
    (defun cs_nested (x choose)
      (let* ((a 0) (b 0))
        (seq (setq a (+ x 1))
             (if (/= (+ a a) 0)
                 (seq (setq b (+ a 7))
                      (if (/= (+ b b) 0) (setq a (+ a b)) 0)) 0)
             (if (= choose 0) (setq a (+ a 3)) 0)
             (extern-call cs_unknown x) (+ a b))))
    (defun cs_memory (p x)
      (let* ((v 0))
        (seq (setq v (ptr-read-u64 p 0))
             (if (/= (+ v v) 0) (ptr-write-u64 p 0 x) 0)
             (extern-call cs_unknown x) (+ v (ptr-read-u64 p 0)))))
    (defun cs_fault (p)
      (let* ((v 0))
        (seq (setq v (+ p 0))
             (if (and (= v 0) (= v 0)) (ptr-read-u64 v 0) (extern-call cs_unknown v)))))))
(defun nelisp-state-prefix-test--compile (file enabled)
  (let ((nelisp-aot-compiler--runtime-entry-params nil)
        (nelisp-aot-compiler--object-hidden-boundary-symbols nil)
        (nelisp-aot-compiler--arch 'x86_64) (nelisp-aot-compiler--abi 'sysv)
        (nelisp-aot-compiler--os 'linux)
        (nelisp-aot-compiler--x86-assignment-prefix-caches
         (and enabled (not nelisp-state-prefix-test--force-off)))
        (nelisp-state-prefix-test--selected 0))
    (advice-add 'nelisp-aot-compiler--emit-local-prefix-let-body :around
                #'nelisp-state-prefix-test--record)
    (unwind-protect
        (progn (nelisp-aot-compile-to-object nelisp-state-prefix-test--source file)
               nelisp-state-prefix-test--selected)
      (advice-remove 'nelisp-aot-compiler--emit-local-prefix-let-body
                     #'nelisp-state-prefix-test--record))))
(ert-deftest nelisp-codegen-state-prefix/assignment-selector ()
  (let ((file (make-temp-file "nelisp-state-prefix-" nil ".o")))
    (unwind-protect
        (progn
          (should (= 0 (nelisp-state-prefix-test--compile file nil)))
          (should (> (nelisp-state-prefix-test--compile file t) 0)))
      (delete-file file))))
(ert-deftest nelisp-codegen-state-prefix/flat-sequence-depth-bound ()
  (let* ((file (make-temp-file "nelisp-state-flat-" nil ".o"))
         (forms (cl-loop for i below 96 append
                         `((setq v (ptr-read-u8 data ,i)) (if (/= (+ v v) 0) v 0))))
         (nelisp-state-prefix-test--source
          `(seq (defun cs_flat (data x)
                  (let ((v 0)) ,@forms (extern-call cs_unknown x) v)))))
    (unwind-protect
        (let ((selected (nelisp-state-prefix-test--compile file t)))
          (should (> selected 0)) (should (<= selected 32)))
      (delete-file file))))
(ert-deftest nelisp-codegen-state-prefix/small-scalar-admission ()
  (let ((file (make-temp-file "nelisp-state-scalar-" nil ".o")))
    (unwind-protect
        (dolist (case '(((ptr-read-u8 data 0) . t) ((= x 0) . t) (17 . t)
                        ((ptr-read-u64 data 0)) ((data-addr cs_global))
                        (x) ((+ x 0)) ((shl x 0)) (268435456) (-1)))
          (let* ((rhs (car case))
                 (nelisp-state-prefix-test--source
                  `(seq (defun cs_scalar (data x)
                          (let ((v 0))
                            (setq v ,rhs)
                            (if (/= (+ v v) 0)
                                (extern-call cs_unknown v) (+ v v))))))
                 (selected (nelisp-state-prefix-test--compile file t)))
            (if (cdr case) (should (> selected 0)) (should (= selected 0)))))
      (delete-file file))))
(ert-deftest nelisp-codegen-state-prefix/original-let-policy-unchanged ()
  (let* ((source
          (with-temp-buffer
            (insert-file-contents
             (expand-file-name "test/nelisp-aot-local-prefix-test.el"
                               nelisp-state-prefix-test--root))
            (catch 'found
              (while t
                (let ((form (read (current-buffer))))
                  (when (and (eq (car-safe form) 'defconst)
                             (eq (cadr form) 'nelisp-prefix-test--source))
                    (throw 'found (cadr (nth 2 form)))))))))
         (nelisp-state-prefix-test--source source)
         (file (make-temp-file "nelisp-state-old-let-" nil ".o"))
         (nelisp-state-prefix-test--let-selected 0) off-bytes off-count)
    (unwind-protect
        (progn
          (should (= 0 (nelisp-state-prefix-test--compile file nil)))
          (setq off-count nelisp-state-prefix-test--let-selected
                off-bytes (with-temp-buffer (set-buffer-multibyte nil)
                                           (insert-file-contents-literally file) (buffer-string))
                nelisp-state-prefix-test--let-selected 0)
          (should (> off-count 0))
          (should (= 0 (nelisp-state-prefix-test--compile file t)))
          (should (= off-count nelisp-state-prefix-test--let-selected))
          (should (equal off-bytes
                         (with-temp-buffer (set-buffer-multibyte nil)
                                           (insert-file-contents-literally file) (buffer-string)))))
      (delete-file file))))
(defvar nelisp-state-prefix-test--raw-count 0)
(defun nelisp-state-prefix-test--raw (original node buf emit)
  (funcall original node buf
           (lambda (body)
             (when (and (eq (nelisp-aot-compiler--ir-kind node) 'setq-local)
                        (eq (nelisp-aot-compiler--ir-get node :var) 'raw)
                        (assq (nelisp-aot-compiler--ir-get node :slot)
                              nelisp-aot-compiler--promoted-slots))
               (cl-incf nelisp-state-prefix-test--raw-count))
             (funcall emit body))))
(ert-deftest nelisp-codegen-state-prefix/real-bytecode-raw-selector ()
  (require 'nelisp-standalone-build)
  (let* ((fn (cl-find 'wf_bytecode_w_fast_run nelisp-standalone--applyfn-bytecode-helpers
                      :key #'cadr))
         (retained (getenv "NELISP_STATE_PREFIX_REAL_OUT"))
         (dir (or retained (make-temp-file "nelisp-state-bytecode-" t)))
         ;; Match the standalone producer's existing compiler recursion limit.
         (max-lisp-eval-depth (max max-lisp-eval-depth 40000))
         (nelisp-aot-compiler--allow-external-user-calls t)
         (nelisp-state-prefix-test--source `(seq ,fn))
         (nelisp-state-prefix-test--raw-count 0))
    (should fn) (make-directory dir t)
    (advice-add 'nelisp-aot-compiler--emit-local-prefix-let-body :around
                #'nelisp-state-prefix-test--raw)
    (unwind-protect
        (dolist (enabled '(nil t))
          (setq nelisp-state-prefix-test--raw-count 0)
          (nelisp-state-prefix-test--compile
           (expand-file-name (if enabled "real-fast-on.o" "real-fast-off.o") dir) enabled)
          (if enabled (should (> nelisp-state-prefix-test--raw-count 0))
            (should (= 0 nelisp-state-prefix-test--raw-count))))
      (advice-remove 'nelisp-aot-compiler--emit-local-prefix-let-body
                     #'nelisp-state-prefix-test--raw)
      (unless retained (delete-directory dir t)))))
(defconst nelisp-state-prefix-test--assembly
  ".text
.globl cs_touch
cs_touch:
 cmp %rdi,(%rbp,%rsi)
 jne .Lbad_home
 mov %rdx,(%rbp,%rsi)
 jmp cs_unknown
.Lbad_home:
 movl $1,cs_bad(%rip)
.globl cs_unknown
cs_unknown:
 movabs $0xf1e2d3c4b5a69788,%r8
 movabs $0x8877665544332211,%r9
 mov $13,%eax
 ret
.section .note.GNU-stack,\"\",@progbits
")
(defconst nelisp-state-prefix-test--driver
  "#include <stdint.h>
#include <stdio.h>
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>
volatile int cs_bad;
extern uint64_t cs_dispatch(void*,uint64_t,uint64_t),cs_join(uint64_t,uint64_t),cs_reassign(uint64_t),cs_word(uint64_t),cs_nested(uint64_t,uint64_t),cs_memory(void*,uint64_t),cs_fault(uint64_t);
static unsigned char header[64] __attribute__((aligned(8)));
int main(void){const uint64_t C=UINT64_C(0xfedcba9876543210);uint64_t x=0,digest=0;unsigned char data[257];
for(unsigned i=0;i<257;i++)data[i]=(i*73)%256;
for(unsigned i=0;i<320;i++){x=i<6?(uint64_t[]){0,1,UINT64_MAX,UINT64_C(0x8000000000000000),UINT64_C(0x7fffffffffffffff),C}[i]:x*UINT64_C(6364136223846793005)+1;uint64_t v=x^C;
for(unsigned choose=0;choose<2;choose++){uint64_t e=2*(v+((v&&v!=UINT64_MAX&&!choose)?7:0));if(cs_join(x,choose)!=e||cs_bad)return 11;digest^=e;}
v=x+1;if(v&&v!=UINT64_MAX)v+=7;if(cs_reassign(x)!=2*v)return 12;
for(unsigned choose=0;choose<2;choose++){uint64_t a=x+1,b=0;if(a+a){b=a+7;if(b+b)a+=b;}if(!choose)a+=3;if(cs_nested(x,choose)!=a+b)return 18;}
uint64_t word=x,after=x+x?i:x;if(cs_memory(&word,i)!=x+after||word!=after)return 19;
unsigned n=i%258;uint64_t e=x;for(unsigned j=0;j<n;j++){unsigned b=data[j];e+=b<8?1:b<16?2:b<24?3:b==255?13:4;}if(cs_dispatch(data,n,x)!=e)return 13;digest+=e;
}
header[0]=42;if(cs_word((uint64_t)(uintptr_t)header+6)!=42||cs_word(7)!=20)return 14;
pid_t child=fork();if(child<0)return 15;if(!child){cs_fault(0);_exit(99);}int status;if(waitpid(child,&status,0)!=child||!WIFSIGNALED(status)||WTERMSIG(status)!=SIGSEGV)return 16;
printf(\"STATE-PREFIX 320/full64/dispatch/joins/home-callback/reassign/WORD/fault %016lx\\n\",digest);return cs_bad?17:0;}
")
(ert-deftest nelisp-codegen-state-prefix/native-adversarial ()
  (skip-unless (and (executable-find "gcc-14") (eq system-type 'gnu/linux)))
  (let* ((retained (getenv "NELISP_STATE_PREFIX_OUT"))
         (dir (or retained (make-temp-file "nelisp-state-prefix-" t)))
         (c (expand-file-name "driver.c" dir))
         (as (expand-file-name "callback.S" dir)) outputs)
    (make-directory dir t)
    (unwind-protect
        (progn
          (with-temp-file c (insert nelisp-state-prefix-test--driver))
          (with-temp-file as (insert nelisp-state-prefix-test--assembly))
          (dolist (enabled '(nil t))
            (let* ((mode (if enabled "on" "off"))
                   (obj (expand-file-name (concat mode ".o") dir))
                   (exe (expand-file-name mode dir)))
              (nelisp-state-prefix-test--compile obj enabled)
              (with-temp-buffer
                (should (= 0 (call-process "gcc-14" nil t nil "-O2" "-fno-pie" "-no-pie" c as obj "-o" exe))))
              (with-temp-buffer
                (let ((status (call-process exe nil t nil)))
                  (unless (equal status 0) (message "Native failure: %s %s" status (buffer-string)))
                  (should (equal status 0)))
                (should (string-match-p "STATE-PREFIX 320/full64" (buffer-string)))
                (push (buffer-string) outputs))))
          (should (equal (car outputs) (cadr outputs))))
      (unless retained (delete-directory dir t)))))
(provide 'nelisp-codegen-state-prefix-test)
;;; nelisp-codegen-state-prefix-test.el ends here
