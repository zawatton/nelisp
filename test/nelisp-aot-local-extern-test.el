;;; nelisp-aot-local-extern-test.el --- Executable local extern calls -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defun nelisp-aot-local-extern-test--old-destination (node buf)
  (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
  (nelisp-asm-x86_64-reloc-plt32-here
   buf (symbol-name (nelisp-aot-compiler--ir-get node :name)) -4 'text))

(defun nelisp-aot-local-extern-test--definition (name arity)
  (nelisp-aot-compiler--make-ir
   'defun :name name :params (cl-loop for i below arity collect (intern (format "p%d" i)))
   :param-regs (cl-subseq '(rdi rsi rdx rcx r8 r9) 0 arity)
   :fixed-param-count arity :param-class (and (> arity 0) 'gp)
   :param-classes (make-list arity 'gp)))

(defun nelisp-aot-local-extern-test--call (name arity)
  (nelisp-aot-compiler--make-ir
   'extern-call :name name :args
   (cl-loop for i below arity collect (nelisp-aot-compiler--make-ir 'imm :value i :cls 'gp))))

(ert-deftest nelisp-aot-local-extern/signature-and-mode-closure ()
  (let* ((d (nelisp-aot-local-extern-test--definition 'local 1))
         (node (nelisp-aot-local-extern-test--call 'local 1))
         (nelisp-aot-compiler--executable-local-externs t)
         (nelisp-aot-compiler--arch 'x86_64)
         (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--runtime-entry-params nil)
         (nelisp-aot-compiler--x86-local-extern-facts
          (nelisp-aot-compiler--x86-local-extern-facts (list d))))
    (should (nelisp-aot-compiler--x86-local-extern-target-p node))
    (dolist (key '(:rest-p :variadic :internal-entry :internal-hidden :gc-root-slots :self-tco-raw))
      (should-not (gethash 'local (nelisp-aot-compiler--x86-local-extern-facts
                                  (list (nelisp-aot-compiler--ir-with d key t))))))
    (dolist (change '((:fixed-param-count . 0) (:param-class . f64)
                      (:param-classes . (f64)) (:param-regs . (rsi)) (:return-repr . f64)))
      (should-not (gethash 'local (nelisp-aot-compiler--x86-local-extern-facts
                                  (list (nelisp-aot-compiler--ir-with d (car change) (cdr change)))))))
    (should-not (gethash 'local (nelisp-aot-compiler--x86-local-extern-facts (list d d d))))
    (should-not
     (gethash 'local
              (nelisp-aot-compiler--x86-local-extern-facts
               (list (nelisp-aot-compiler--ir-with d :params '(a b c d e f g))))))
    (should-not
     (nelisp-aot-compiler--x86-local-extern-target-p
      (nelisp-aot-compiler--ir-with
       node :args (list (nelisp-aot-compiler--make-ir 'imm :value 1.5 :cls 'f64)))))
    (dolist (change '((:name . unknown) (:args) (:fn-value . t)
                      (:varargs-p . t) (:ret-class . f64)))
      (should-not (nelisp-aot-compiler--x86-local-extern-target-p
                   (nelisp-aot-compiler--ir-with node (car change) (cdr change)))))
    (let ((nelisp-aot-compiler--executable-local-externs nil))
      (should-not (nelisp-aot-compiler--x86-local-extern-target-p node)))
    (let ((nelisp-aot-compiler--abi 'win64))
      (should-not (nelisp-aot-compiler--x86-local-extern-target-p node)))
    (let ((nelisp-aot-compiler--arch 'aarch64))
      (should-not (nelisp-aot-compiler--x86-local-extern-target-p node)))
    (let ((nelisp-aot-compiler--runtime-entry-params t))
      (should-not (nelisp-aot-compiler--x86-local-extern-target-p node)))
    (let ((nelisp-aot-compiler--x86-local-extern-facts
           (nelisp-aot-compiler--x86-local-extern-facts
            (list (nelisp-aot-local-extern-test--definition 'zero 0)))))
      (should (nelisp-aot-compiler--x86-local-extern-target-p
               (nelisp-aot-local-extern-test--call 'zero 0))))))

(ert-deftest nelisp-aot-local-extern/object-and-foreign-metadata-exact ()
  (let* ((source '(seq (defun local (x) (ptr-read-u64 x 0))
                      (defun caller (x) (extern-call local x))
                      (defun foreign (x) (extern-call true_foreign x))))
         (nelisp-aot-compiler--object-hidden-boundary-symbols nil)
         (new (nelisp-aot-compile-to-link-unit source))
         (old (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-extern-destination-x86)
                        #'nelisp-aot-local-extern-test--old-destination))
                (nelisp-aot-compile-to-link-unit source))))
    (should (equal new old))
    (should (= 2 (length (plist-get new :relocs))))
    (let* ((nelisp-aot-compiler--executable-local-externs t)
           (nelisp-aot-compiler--x86-local-extern-facts (make-hash-table :test #'eq))
           (buf (nelisp-asm-x86_64-make-buffer 'sysv))
           (reference (nelisp-asm-x86_64-make-buffer 'sysv))
           (node (nelisp-aot-local-extern-test--call 'true_foreign 1)))
      (nelisp-aot-compiler--emit-extern-destination-x86 node buf)
      (nelisp-aot-local-extern-test--old-destination node reference)
      (should (equal buf reference)))))

(defun nelisp-aot-local-extern-test--native-probe (dir)
  "Use the real executable frontend's text inside a genuine C ABI driver."
  (let* ((elf (expand-file-name "source.nl" dir))
         (text (expand-file-name "text.bin" dir))
         (assembly (expand-file-name "functions.S" dir))
         (caller (expand-file-name "caller.c" dir))
         (probe (expand-file-name "probe" dir))
         (pass (symbol-function 'nelisp-aot-compiler--pass))
         (writer (symbol-function 'nelisp-elf-write-binary))
         labels text-bytes)
    (cl-letf (((symbol-function 'nelisp-aot-compiler--pass)
               (lambda (&rest args)
                 (let ((buf (apply pass args)))
                   (setq labels (nelisp-asm-x86_64-buffer-labels buf)) buf)))
              ((symbol-function 'nelisp-elf-write-binary)
               (lambda (path sections)
                 (setq text-bytes (plist-get sections :text))
                 (funcall writer path sections))))
      (nelisp-aot-compile-sexp
       '(seq
         (defun effect0 ()
           (seq (ptr-write-u64 536870912 0 (+ (ptr-read-u64 536870912 0) 1))
                -81985529216486896))
         (defun entry0 () (extern-call effect0))
         (defun mark (digit value)
           (seq (ptr-write-u64 536870912 8 (+ (* (ptr-read-u64 536870912 8) 10) digit)) value))
         (defun effect6 (a b c d e f)
           (seq (ptr-write-u64 536870912 16 a) (ptr-write-u64 536870912 24 b)
                (ptr-write-u64 536870912 32 c) (ptr-write-u64 536870912 40 d)
                (ptr-write-u64 536870912 48 e) (ptr-write-u64 536870912 56 f) f))
         (defun entry6 (a b c d e f)
           (extern-call effect6 (mark 1 a) (mark 2 b) (mark 3 c)
                        (mark 4 d) (mark 5 e) (mark 6 f)))
         (exit 0)) elf))
    (with-temp-buffer
      (set-buffer-multibyte nil) (insert text-bytes)
      (let ((coding-system-for-write 'no-conversion)) (write-region (point-min) (point-max) text nil 'silent)))
    (with-temp-file assembly
      (insert ".text\n.p2align 4\nj_blob:\n.incbin \"" text "\"\n")
      (dolist (name '(entry0 entry6))
        (let ((offset (cdr (assq name labels))))
          (should (integerp offset))
          (insert (format ".globl j_%s\n.set j_%s,j_blob+%d\n" name name offset))))
      (insert ".section .note.GNU-stack,\"\",@progbits\n"))
    (with-temp-file caller
      (insert "#include <stdint.h>\n#include <sys/mman.h>\ntypedef uint64_t U;\nstatic int bad;\n"
              "static U invoke0(U v) {\n"
              "register U b __asm__(\"rbx\")=101, p __asm__(\"r12\")=103,q __asm__(\"r13\")=107,r __asm__(\"r14\")=109,s __asm__(\"r15\")=113;\n"
              "U before,after; __asm__ volatile(\"mov %%rsp,%0\":\"=r\"(before));\n"
              "__asm__ volatile(\"call j_entry0\":\"+a\"(v),\"+b\"(b),\"+r\"(p),\"+r\"(q),\"+r\"(r),\"+r\"(s)::\"rdi\",\"rsi\",\"rdx\",\"rcx\",\"r8\",\"r9\",\"r10\",\"r11\",\"memory\",\"cc\");\n"
              "__asm__ volatile(\"mov %%rsp,%0\":\"=r\"(after)); bad |= before!=after || b!=101 || p!=103 || q!=107 || r!=109 || s!=113; return v; }\n"
              "extern U j_entry6(U,U,U,U,U,U);\n"
              "static U invoke6(U *v) { register U b __asm__(\"rbx\")=101,p __asm__(\"r12\")=103,q __asm__(\"r13\")=107,r __asm__(\"r14\")=109,s __asm__(\"r15\")=113;\n"
              "__asm__ volatile(\"\":\"+r\"(b),\"+r\"(p),\"+r\"(q),\"+r\"(r),\"+r\"(s)); U got=j_entry6(v[0],v[1],v[2],v[3],v[4],v[5]);\n"
              "__asm__ volatile(\"\":\"+r\"(b),\"+r\"(p),\"+r\"(q),\"+r\"(r),\"+r\"(s)); bad |= b!=101 || p!=103 || q!=107 || r!=109 || s!=113; return got; }\n"
              "int main(void) {\n"
              "volatile U *m=mmap((void*)536870912,4096,3,0x32,-1,0); if(m==(void*)-1) return 90;\n"
              "U seeds[]={0,1,536871168,UINT64_MAX};\n"
              "for(int i=0;i<4;i++) if(invoke0(seeds[i])!=UINT64_C(18364758544493064720) || m[0]!=(U)i+1) return 10+i;\n"
              "U vals[]={UINT64_C(0x8000000000000000),UINT64_C(0x7fffffffffffffff),2,6,UINT64_C(0xfedcba9876543210),UINT64_MAX};\n"
              "U before,after; __asm__ volatile(\"mov %%rsp,%0\":\"=r\"(before));\n"
              "U got=invoke6(vals);\n"
              "__asm__ volatile(\"mov %%rsp,%0\":\"=r\"(after)); if(before!=after || got!=UINT64_MAX || m[1]!=123456) return 20;\n"
              "for(int i=0;i<6;i++) if(m[i+2]!=vals[i]) return 30+i; return bad?40:0; }\n"))
    (should (= 0 (call-process (or (executable-find "gcc-14") "gcc") nil nil nil
                               "-O2" "-mno-red-zone" "-fno-omit-frame-pointer" "-no-pie" caller assembly "-o" probe)))
    (call-process probe nil nil nil)))

(ert-deftest nelisp-aot-local-extern/real-effects-fullword-order-sp-callee-saves ()
  (skip-unless (and (executable-find "gcc") (eq system-type 'gnu/linux)))
  (let ((dir (make-temp-file "local-extern-" t)))
    (unwind-protect
        (should (= 0 (nelisp-aot-local-extern-test--native-probe dir)))
      (delete-directory dir t))))

(ert-deftest nelisp-aot-local-extern/genuine-old-emitter-fails-native-effects ()
  (skip-unless (and (executable-find "gcc") (eq system-type 'gnu/linux)))
  (let ((dir (make-temp-file "local-extern-old-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-extern-destination-x86)
                   #'nelisp-aot-local-extern-test--old-destination))
          ;; The first controlled RAX=0 call cannot fake either the full-word
          ;; return or the store.  This checks native behavior, not a counter.
          (should (= 10 (nelisp-aot-local-extern-test--native-probe dir))))
      (delete-directory dir t))))

(provide 'nelisp-aot-local-extern-test)
