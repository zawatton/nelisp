;;; nelisp-aot-private-label-test.el --- Printed export namespace -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defvar nelisp-aot-private-label-test--selected 0)
(defun nelisp-aot-private-label-test--record (&rest _)
  (cl-incf nelisp-aot-private-label-test--selected))

(ert-deftest nelisp-aot-private-label/staged-K-native-export-collisions ()
  (let* ((dir (make-temp-file "nelisp-private-label-" t))
         (o (expand-file-name "probe.o" dir))
         (c (expand-file-name "probe.c" dir))
         (exe (expand-file-name "probe" dir))
         (nelisp-aot-compiler--object-hidden-boundary-symbols nil)
         (nelisp-aot-compiler--x86-guarded-body-folding t)
         (nelisp-aot-private-label-test--selected 0)
         (source '(seq
                   (defun ns_slow (a) (+ a 11))
                   (defun ns_guard (a)
                     (let ((entry (if (= (ptr-read-u64 805306368 0) 0) 0 (ns_slow a))))
                       (if (= entry 0)
                           (seq (ptr-write-u64 805306368 8 a)
                                (+ (ptr-read-u64 805306368 8) 3)) (+ entry 7))))
                   (defun ns_guard2 (a)
                     (let ((entry (if (= (ptr-read-u64 805306368 0) 0) 0 (ns_slow a))))
                       (if (= entry 0)
                           (seq (ptr-write-u64 805306368 16 a)
                                (+ (ptr-read-u64 805306368 16) 4)) (+ entry 9))))
                   (defun ns_wrap (a) (logxor (ns_guard a) 1))
                   (defun ns_wrap2 (a) (logxor (ns_guard2 a) 2))
                   (defun guarded-call-label (a) (+ a 1))
                   (defun shrink-cold (a) (+ a 2))
                   (defun trace-guarded-call-label (a) (+ a 3))
                   (defun trace-guarded-call-label-1 (a) (+ a 4)))))
    (unwind-protect
        (progn
          (advice-add 'nelisp-aot-compiler--emit-guarded-named-call-x86
                      :before #'nelisp-aot-private-label-test--record)
          (nelisp-aot-compile-to-object source o)
          (advice-remove 'nelisp-aot-compiler--emit-guarded-named-call-x86
                         #'nelisp-aot-private-label-test--record)
          (should (>= nelisp-aot-private-label-test--selected 2))
          (with-temp-file c
            (insert "#define _GNU_SOURCE\n#include <stdint.h>\n#include <sys/mman.h>\n"
                    "extern uint64_t ns_wrap(uint64_t),ns_wrap2(uint64_t);\n"
                    "extern uint64_t g(uint64_t) asm(\"\\\"guarded-call-label\\\"\"),c(uint64_t) asm(\"\\\"shrink-cold\\\"\"),t(uint64_t) asm(\"\\\"trace-guarded-call-label\\\"\"),u(uint64_t) asm(\"\\\"trace-guarded-call-label-1\\\"\");\n"
                    "int main(void){uint64_t*p=(void*)0x30000000;if(mmap(p,4096,3,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED,-1,0)==MAP_FAILED)return 80;if(g(40)!=41||c(40)!=42||t(40)!=43||u(40)!=44)return 81;p[0]=0;if(ns_wrap(40)!=42||ns_wrap2(40)!=46||p[1]!=40||p[2]!=40)return 82;p[0]=1;if(ns_wrap(40)!=59||ns_wrap2(40)!=62)return 83;return 0;}\n"))
          (should (= 0 (call-process "gcc-14" nil nil nil "-O2" "-no-pie" c o "-o" exe)))
          (should (= 0 (call-process exe nil nil nil))))
      (advice-remove 'nelisp-aot-compiler--emit-guarded-named-call-x86
                     #'nelisp-aot-private-label-test--record)
      (delete-directory dir t))))

(provide 'nelisp-aot-private-label-test)
