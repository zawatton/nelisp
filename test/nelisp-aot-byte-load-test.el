;;; nelisp-aot-byte-load-test.el --- Adjacent plain byte reads -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(defun nelisp-aot-byte-load-test--ref (slot)
  (nelisp-aot-compiler--make-ir 'ref :slot slot :class 'gp :repr 'raw-i64))
(defun nelisp-aot-byte-load-test--imm (value)
  (nelisp-aot-compiler--make-ir 'imm :value value))
(defun nelisp-aot-byte-load-test--arith (op a b)
  (nelisp-aot-compiler--make-ir 'arith :op op :repr 'raw-i64 :a a :b b))
(defun nelisp-aot-byte-load-test--read (slot offset &optional kind)
  (nelisp-aot-compiler--make-ir (or kind 'ptr-read-u8)
   :ptr (nelisp-aot-byte-load-test--ref slot) :offset offset))
(defun nelisp-aot-byte-load-test--pair (&optional high-slot distance shift low-kind)
  (nelisp-aot-byte-load-test--arith
   '+ (nelisp-aot-byte-load-test--read
       0 (nelisp-aot-byte-load-test--arith '+ (nelisp-aot-byte-load-test--ref 1)
                                          (nelisp-aot-byte-load-test--imm 1)) low-kind)
   (nelisp-aot-compiler--make-ir
    'shift :op 'shl :repr 'raw-i64
    :a (nelisp-aot-byte-load-test--read
        (or high-slot 0) (nelisp-aot-byte-load-test--arith '+ (nelisp-aot-byte-load-test--ref 1)
                                                                    (nelisp-aot-byte-load-test--imm (or distance 2))))
    :b (nelisp-aot-byte-load-test--imm (or shift 8)))))
(defmacro nelisp-aot-byte-load-test--context (&rest body)
  `(let ((nelisp-aot-compiler--x86-trace-loop-exits '((cold . ended)))
         (nelisp-aot-compiler--promoted-slots '((0 . rbx) (1 . r8)))
         (nelisp-aot-compiler--arch 'x86_64) (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--x86-adjacent-byte-loads t)
         (nelisp-aot-compiler--checked-arith nil)) ,@body))
(ert-deftest nelisp-aot-byte-load/preflight-is-pure-and-preserves-original-nodes ()
  (nelisp-aot-byte-load-test--context
   (let* ((node (nelisp-aot-byte-load-test--pair)) (before (prin1-to-string node))
          (load (nelisp-aot-compiler--x86-adjacent-byte-read node)))
     (should (eq (nelisp-aot-compiler--ir-kind load) 'ptr-read-u16))
     (should (eq (nelisp-aot-compiler--ir-get load :ptr)
                 (nelisp-aot-compiler--ir-get (nelisp-aot-compiler--ir-get node :a) :ptr)))
     (should (equal before (prin1-to-string node)))
     (should (equal (nelisp-aot-compiler--x86-memory-tree-info node) '(1 . t)))
     (let ((nelisp-aot-compiler--x86-adjacent-byte-loads nil))
       (should-not (nelisp-aot-compiler--x86-adjacent-byte-read node))
       (should (equal (nelisp-aot-compiler--x86-memory-tree-info node) '(2 . t))))
     (let ((nelisp-aot-compiler--x86-trace-loop-exits nil))
       (should-not (nelisp-aot-compiler--x86-adjacent-byte-read node))))))
(ert-deftest nelisp-aot-byte-load/effects-metadata-classes-and-address-fallback ()
  (nelisp-aot-byte-load-test--context
   (dolist (node (list (nelisp-aot-byte-load-test--arith
                        '+ (nelisp-aot-byte-load-test--read 0 (nelisp-aot-byte-load-test--imm 0))
                        (nelisp-aot-byte-load-test--ref 1))
                       (nelisp-aot-byte-load-test--pair 2)
                       (nelisp-aot-byte-load-test--pair nil 3)
                       (nelisp-aot-byte-load-test--pair nil nil 7)
                       (nelisp-aot-byte-load-test--pair nil nil nil 'ptr-read-u16)))
     (should-not (nelisp-aot-compiler--x86-adjacent-byte-read node)))
   (dolist (key '(:volatile :atomic :may-throw :exception :unknown))
     (let ((node (nelisp-aot-byte-load-test--pair)))
       (should-not (nelisp-aot-compiler--x86-adjacent-byte-read
                    (nelisp-aot-compiler--ir-with node key t)))
       (should-not (nelisp-aot-compiler--x86-adjacent-byte-read
                    (nelisp-aot-compiler--ir-with node :a
                     (nelisp-aot-compiler--ir-with (nelisp-aot-compiler--ir-get node :a) key t))))))
   (dolist (pointer (list (nelisp-aot-byte-load-test--read 0 (nelisp-aot-byte-load-test--imm 0))
                          (nelisp-aot-compiler--make-ir 'setq-local :slot 0
                           :value-ir (nelisp-aot-byte-load-test--ref 0))))
     (let* ((node (nelisp-aot-byte-load-test--pair))
            (low (nelisp-aot-compiler--ir-with (nelisp-aot-compiler--ir-get node :a) :ptr pointer)))
       (should-not (nelisp-aot-compiler--x86-adjacent-byte-read
                    (nelisp-aot-compiler--ir-with node :a low)))))
   (let ((node (nelisp-aot-byte-load-test--pair)))
     (let ((nelisp-aot-compiler--checked-arith t))
       (should-not (nelisp-aot-compiler--x86-adjacent-byte-read node)))
     (let ((nelisp-aot-compiler--abi 'win64))
       (should-not (nelisp-aot-compiler--x86-adjacent-byte-read node))))))
(defun nelisp-aot-byte-load-test--source-pair (ptr offset &optional high-ptr distance shift low-kind)
  `(+ (,(or low-kind 'ptr-read-u8) ,ptr ,offset)
      (shl (ptr-read-u8 ,(or high-ptr ptr) (+ ,offset ,(or distance 1))) ,(or shift 8))))
(defun nelisp-aot-byte-load-test--program ()
  (let* ((pair (nelisp-aot-byte-load-test--source-pair 'p 0))
         (balanced `(logxor (+ ,pair ,(nelisp-aot-byte-load-test--source-pair 'p 2))
                           (+ ,(nelisp-aot-byte-load-test--source-pair 'p 4)
                              ,(nelisp-aot-byte-load-test--source-pair 'p 6))))
         (effect '(seq (ptr-write-u64 q 0 (+ (ptr-read-u64 q 0) 1)) p))
         (arms `((0 (+ ,pair seed)) (1 (+ ,balanced seed))
                 (2 (if (= (logand p 7) 2) 7
                      (if (= (logand p 7) 6)
                          (logand ,(nelisp-aot-byte-load-test--source-pair '(- p 6) 0) 63) -1)))
                 (4 ,(nelisp-aot-byte-load-test--source-pair effect 0))
                 (5 ,(nelisp-aot-byte-load-test--source-pair 'p 0 nil 2))
                 (6 ,(nelisp-aot-byte-load-test--source-pair 'p 0 'q))
                 (7 ,(nelisp-aot-byte-load-test--source-pair 'p 0 nil nil 7))
                 (8 ,(nelisp-aot-byte-load-test--source-pair 'p 0 nil nil nil 'ptr-read-s8))
                 (9 ,(nelisp-aot-byte-load-test--source-pair 'p 0 nil nil '(logand seed 63)))
                 (10 ,(nelisp-aot-byte-load-test--source-pair '(+ p seed) '(- 0 seed)))))
         (tail '(seq (extern-call external_byte_cold p seed) (setq mode 0))))
    (dolist (arm (reverse arms))
      (setq tail `(if (= mode ,(car arm))
                     (seq (setq result ,(cadr arm)) (setq go 0)) ,tail)))
    `(seq (defun byte_probe (p n mode seed q)
            (let* ((go n) (result seed) (iterations 0))
              (while (> go 0) (setq iterations (+ iterations 1)) ,tail)
              result)))))
(defconst nelisp-aot-byte-load-test--c
"#include <stdint.h>
typedef uint64_t U;extern U byte_probe(unsigned char*,U,U,U,unsigned char*);
static U checks,calls;U align_bad;static unsigned char buf[8192] __attribute__((aligned(4096)));
__asm__(\".text\\n.globl _start\\n_start:\\ncall run\\nmov %rax,%rdi\\nmov $60,%eax\\nsyscall\\n.globl external_byte_cold\\nexternal_byte_cold:\\nmov %rsp,%rax\\nand $15,%rax\\ncmp $8,%rax\\nje 1f\\nmovq $1,align_bad(%rip)\\n1:jmp external_byte_cold_impl\\n\");
U external_byte_cold_impl(unsigned char*p,U seed){calls++;p[0]=201;p[1]=153;__asm__ volatile(\"mov $137,%%r8;mov $139,%%r9;mov $149,%%r10;mov $151,%%r11\"::: \"r8\",\"r9\",\"r10\",\"r11\",\"cc\");return seed^calls;}
static U pair(unsigned char*p){return (U)p[0]+((U)p[1]<<8);}
static U verify(unsigned char*p,U n,U mode,U seed,unsigned char*q,U expected){register U b __asm__(\"rbx\")=101;register U a __asm__(\"r12\")=103;register U c __asm__(\"r13\")=107;register U d __asm__(\"r14\")=109;register U e __asm__(\"r15\")=113;__asm__ volatile(\"\":\"+r\"(b),\"+r\"(a),\"+r\"(c),\"+r\"(d),\"+r\"(e));U v=byte_probe(p,n,mode,seed,q);__asm__ volatile(\"\":\"+r\"(b),\"+r\"(a),\"+r\"(c),\"+r\"(d),\"+r\"(e));checks++;return v!=expected||b!=101||a!=103||c!=107||d!=109||e!=113;}
U run(void){U fail=0;U state[2]={0,0};static unsigned char vector[16] __attribute__((aligned(8)))={141,165};U seeds[6]={0,1,UINT64_MAX,UINT64_C(4294967295),INT64_MAX,UINT64_C(9223372036854775808)};
for(U hi=0;hi<256;hi++)for(U lo=0;lo<256;lo++){buf[0]=lo;buf[1]=hi;fail|=verify(buf,1,0,0,buf,pair(buf));}
for(U s=0;s<6;s++)for(U off=0;off<8;off++){unsigned char*p=buf+off;for(U k=0;k<12;k++)p[k]=(k*79+s*17+193)&255;U z=seeds[s];fail|=verify(p,1,0,z,p,pair(p)+z);fail|=verify(p,1,1,z,p,((pair(p)+pair(p+2))^(pair(p+4)+pair(p+6)))+z);fail|=verify(p,1,5,z,p,(U)p[0]+((U)p[2]<<8));fail|=verify(p,1,6,z,p+4,(U)p[0]+((U)p[5]<<8));fail|=verify(p,1,7,z,p,(U)p[0]+((U)p[1]<<7));fail|=verify(p,1,8,z,p,(U)(int64_t)(int8_t)p[0]+((U)p[1]<<8));fail|=verify(p,1,9,z,p,(U)p[0]+((U)p[1]<<(z&63)));fail|=verify(p,1,10,z,p,pair(p));}
for(U mode=0;mode<2;mode++){unsigned char*p=buf+4095;for(U k=0;k<8;k++)p[k]=k*23+11;fail|=verify(p,1,mode,UINT64_MAX,p,mode?((pair(p)+pair(p+2))^(pair(p+4)+pair(p+6)))+UINT64_MAX:pair(p)+UINT64_MAX);}
fail|=verify((unsigned char*)2,1,2,0,buf,7);fail|=verify(vector+6,1,2,0,buf,13);fail|=verify((unsigned char*)2,0,0,UINT64_MAX,buf,UINT64_MAX);
buf[0]=17;buf[1]=231;state[0]=0;fail|=verify(buf,1,4,0,(unsigned char*)state,pair(buf));fail|=state[0]!=2;
buf[0]=1;buf[1]=2;calls=0;fail|=verify(buf,1,3,UINT64_MAX,buf,UINT64_C(39369)+UINT64_MAX);fail|=calls!=1||align_bad;
char out[16];for(U k=0;k<16;k++)out[15-k]=\"0123456789abcdef\"[(checks>>(k*4))&15];long rc;__asm__ volatile(\"syscall\":\"=a\"(rc):\"0\"(1),\"D\"(1),\"S\"(out),\"d\"(16):\"rcx\",\"r11\",\"memory\");return fail;}
")
(ert-deftest nelisp-aot-byte-load/native-all-bytes-fullword-WORD-and-siblings ()
  (skip-unless (and (executable-find "gcc-14") (eq system-type 'gnu/linux)))
  (dotimes (value 65536)
    (should (= value (+ (logand value 255) (ash (ash value -8) 8)))))
  (let* ((dir (make-temp-file "byte-load-" t))
         (c (expand-file-name "probe.c" dir)) (co (expand-file-name "caller.o" dir))
         (obj (expand-file-name "probe.o" dir)) (bin (expand-file-name "probe" dir))
         (nelisp-aot-compiler--object-hidden-boundary-symbols nil)
         (program (nelisp-aot-byte-load-test--program)) (disabled nil) (enabled nil))
    (unwind-protect
        (progn
          (with-temp-file c (insert nelisp-aot-byte-load-test--c))
          (should (= 0 (call-process "gcc-14" nil nil nil "-O2" "-ffreestanding" "-fno-builtin" "-fno-stack-protector" "-c" c "-o" co)))
          ;; Both selectors pass semantics before the instruction gauge.
          (dolist (mode '(nil t))
            (let ((nelisp-aot-compiler--x86-adjacent-byte-loads mode))
              (nelisp-aot-compile-to-object program obj))
            (should (= 0 (call-process "gcc-14" nil nil nil "-nostdlib" "-no-pie" co obj "-o" bin)))
            (with-temp-buffer
              (should (= 0 (call-process bin nil t nil)))
              (should (= (string-to-number (buffer-string) 16) 65927)))
            (let ((bytes (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally obj) (buffer-string))))
              (if mode (setq enabled bytes) (setq disabled bytes))))
          (should-not (equal disabled enabled))
          (message "Byte-load:65927 native cases per selector; exhaustive65536bytepairs,unaligned/pagecross/fullword/siblings/WORD/coldreentry/effectfallback;ABI sentinels"))
      (delete-directory dir t))))
