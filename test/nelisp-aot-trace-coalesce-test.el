;;; nelisp-aot-trace-coalesce-test.el --- Ended E slot coalescing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(defvar nelisp-aot-trace-coalesce-test--plans nil)
(defvar nelisp-aot-trace-coalesce-test--writes nil)

(defun nelisp-aot-trace-coalesce-test--tag ()
  '(if (= (logand w 1) 1)
       (if (= (logand w 3) 1) 2 (if (= w 3) 0 1))
     (if (= (logand w 7) 2) 7
       (if (= (logand w 7) 6) (logand (ptr-read-u8 w -6) 63)
         (ptr-read-u8 w 0)))))

(defun nelisp-aot-trace-coalesce-test--definition (name &optional word)
  (let* ((tag (if word (nelisp-aot-trace-coalesce-test--tag) 0))
         (use-p (cl-loop for c in '(c0 c1 c2 c3)
                         collect `(setq ,c (logxor ,c (logxor p (shl p 1))))))
         (use-q (cl-loop for c in '(c0 c1 c2 c3)
                         collect `(setq ,c (logxor ,c (logxor q (shl q 1)))))))
    `(defun ,name (n w)
       (let* ((tick 0) (c0 0) (c1 1) (c2 2) (c3 3) (p 101) (q 202))
         (while (< tick n)
           (setq tick (+ tick 1))
           (setq c0 (+ c0 (+ c0 tick))) (setq c1 (+ c1 (+ c1 tick)))
           (setq c2 (+ c2 (+ c2 tick))) (setq c3 (+ c3 (+ c3 tick)))
           (if (< (logand tick 3) 2)
               (setq c0 (logxor c0 (extern-call L_observe tick w)))
             (if (= (logand tick 1) 1)
                 (seq (setq p (logxor w (logxor tick ,tag)))
                      (setq p (+ p 1)) ,@use-p)
               (seq (setq q w) (setq q (logxor q (logxor ,tag 85))) ,@use-q))))
         (logxor (logxor c0 c1) (logxor c2 c3))))))

(defun nelisp-aot-trace-coalesce-test--ir (definition)
  (let ((nelisp-aot-compiler--object-hidden-boundary-symbols nil)
        (nelisp-aot-compiler--allow-external-user-calls t)
        (nelisp-aot-compiler--runtime-entry-params nil))
    (nelisp-aot-compiler--parse definition nil)))

(defun nelisp-aot-trace-coalesce-test--plan (owner)
  (let ((nelisp-aot-compiler--x86-trace-loop-owner owner)
        (nelisp-aot-compiler--x86-trace-unit-names '("L_test" "L_observe"))
        (nelisp-aot-compiler--arch 'x86_64) (nelisp-aot-compiler--abi 'sysv)
        (nelisp-aot-compiler--checked-arith nil) (nelisp-aot-compiler--runtime-entry-params nil)
        (nelisp-aot-compiler--native-emit-enabled nil) (nelisp-aot-compiler--promoted-slots nil)
        (nelisp-aot-compiler--x86-index-forward-frame-sensitive-p nil)
        (nelisp-aot-compiler--x86-loop-regs '(rbx r8 r9 r12 r13 r14 r15)) loop)
    (nelisp-aot-compiler--walk-ir owner
      (lambda (n) (when (eq (nelisp-aot-compiler--ir-kind n) 'while) (setq loop n))))
    (and loop (nelisp-aot-compiler--x86-trace-loop-plan loop))))

(ert-deftest nelisp-aot-trace-coalesce/original-rank-and-complete-scope ()
  (let* ((owner (nelisp-aot-trace-coalesce-test--ir
                 (nelisp-aot-trace-coalesce-test--definition 'L_test)))
         (trace (nelisp-aot-trace-coalesce-test--plan owner))
         (new (car (plist-get trace :plan)))
         (old (let ((nelisp-aot-compiler--x86-trace-coalescing nil))
                (car (plist-get (nelisp-aot-trace-coalesce-test--plan owner) :plan)))))
    (should (= (length new) 8))
    (should (= (length (delete-dups (mapcar #'cdr new))) 7))
    (should (equal (mapcar #'car old) (mapcar #'car (butlast new))))
    (should (equal (sort (copy-sequence (plist-get trace :write-through)) #'<) '(7 8)))))

(ert-deftest nelisp-aot-trace-coalesce/overlap-observer-and-control-fallback ()
  (let ((trace (nelisp-aot-trace-coalesce-test--plan
                (nelisp-aot-trace-coalesce-test--ir
                 '(defun small (n)
                    (let* ((i 0) (s 0))
                      (while (< i n)
                        (setq i (+ i 1))
                        (if (= i 1) (extern-call L_observe i n)
                          (if (= i 2) (setq s (+ s 3)) (setq s (+ s 7))))) s))))))
    (should trace)
    (should-not (plist-get trace :write-through)))
  (let ((base (nelisp-aot-trace-coalesce-test--definition 'L_test)))
    (dolist (edit '(overlap header exit short-circuit opaque control))
      (let* ((source (copy-tree base)) (body (nth 3 source))
             (loop (nth 2 body)))
        (pcase edit
          ('header (setcar (cdr loop) '(and (< tick n) (> p -1))))
          ('exit (setcar (last body) '(logxor p (logxor c0 c1))))
          ('short-circuit (setcdr (cdr body) (cons '(and 0 (setq p 42)) (cddr body))))
          ('opaque (setcdr (cdr body) (cons '(future-opaque-effect) (cddr body))))
          ('control (setcdr (cdr body) (cons '(cond ((= n 0) 1) (t 2)) (cddr body))))
          ('overlap
           (cl-labels ((rewrite (x)
                         (if (equal x '(setq p (+ p 1)))
                             '(seq (setq p (+ p 1)) (setq q w) (setq c0 (+ p q)))
                           (if (consp x) (mapcar #'rewrite x) x))))
             (setq source (rewrite source)))))
        (if (eq edit 'opaque)
            ;; Unknown IR is rejected by the new complete walk before output.
            (let* ((owner (nelisp-aot-trace-coalesce-test--ir base))
                   (outer (nelisp-aot-compiler--ir-get owner :body)))
              (setq owner (nelisp-aot-compiler--ir-with
                           owner :body (nelisp-aot-compiler--make-ir
                                        'value-seq :forms
                                        (list (nelisp-aot-compiler--make-ir 'future-opaque-effect) outer))))
              (should-not (plist-get (nelisp-aot-trace-coalesce-test--plan owner) :write-through)))
          (should-not (plist-get (nelisp-aot-trace-coalesce-test--plan
                                 (nelisp-aot-trace-coalesce-test--ir source)) :write-through)))))
    (dolist (mode '(root hidden variadic sp shared unknown-metadata))
      (let ((owner (nelisp-aot-trace-coalesce-test--ir base)))
        (pcase mode
          ('root (setq owner (nelisp-aot-compiler--ir-with owner :gc-root-slots '(7))))
          ('hidden (setq owner (nelisp-aot-compiler--ir-with owner :internal-hidden '(0))))
          ('variadic (setq owner (nelisp-aot-compiler--ir-with owner :variadic t)))
          ('unknown-metadata (setq owner (nelisp-aot-compiler--ir-with owner :future-frame-effect t)))
          ('sp (setq owner (nelisp-aot-compiler--ir-with
                           owner :body (nelisp-aot-compiler--make-ir
                                        'value-seq :forms
                                        (list (nelisp-aot-compiler--make-ir 'aot-current-sp)
                                              (nelisp-aot-compiler--ir-get owner :body))))))
          ('shared (let ((body (nelisp-aot-compiler--ir-get owner :body)))
                     (setq owner (nelisp-aot-compiler--ir-with
                                  owner :body (nelisp-aot-compiler--make-ir
                                               'value-seq :forms (list body body)))))))
        (should-not (plist-get (nelisp-aot-trace-coalesce-test--plan owner) :write-through))))))

(defun nelisp-aot-trace-coalesce-test--oracle (n w tag)
  "GNU full-word results and all observed source frame words."
  (let ((mask #xffffffffffffffff) (cs (vector 0 1 2 3)) (p 101) (q 202) observations)
    (dotimes (k n)
      (let ((tick (1+ k)))
        (dotimes (i 4) (aset cs i (logand mask (+ (* 2 (aref cs i)) tick))))
        (if (< (logand tick 3) 2)
            (progn
              (push (list p q) observations)
              (setq p (logxor #x1111111111111111 tick)
                    q (logxor #x2222222222222222 tick))
              (aset cs 0 (logxor (aref cs 0) (logand mask (logxor #xfedcba9876543210 w tick)))))
          (let ((v (if (= (logand tick 1) 1)
                       (setq p (logand mask (1+ (logxor w tick tag))))
                     (setq q (logand mask (logxor w tag 85))))))
            (dotimes (i 4) (aset cs i (logand mask (logxor (aref cs i) v (ash v 1)))))))))
    (list (logand mask (logxor (aref cs 0) (aref cs 1) (aref cs 2) (aref cs 3)))
          (nreverse observations))))

(defun nelisp-aot-trace-coalesce-test--record (node _buf trace)
  (push (list (nelisp-aot-compiler--ir-get nelisp-aot-compiler--x86-trace-loop-owner :name)
              (plist-get trace :write-through) (car (plist-get trace :plan)))
        nelisp-aot-trace-coalesce-test--plans))
(defun nelisp-aot-trace-coalesce-test--write (_buf slot)
  (when (memq slot nelisp-aot-compiler--x86-trace-write-through)
    (push slot nelisp-aot-trace-coalesce-test--writes)))
(defun nelisp-aot-trace-coalesce-test--cold (node _buf)
  (when (eq (nelisp-aot-compiler--ir-get node :name) 'L_observe)
    (should-not nelisp-aot-compiler--promoted-slots)
    (should-not nelisp-aot-compiler--x86-trace-loop-exits)
    (should-not nelisp-aot-compiler--x86-trace-write-through)
    (should (= nelisp-aot-compiler--rsp-temp-depth 0))))

(defun nelisp-aot-trace-coalesce-test--object (dir disabled)
  (let* ((suffix (if disabled "off" "on"))
         (nelisp-aot-compiler--x86-trace-coalescing
          (and (not disabled) (not (equal (getenv "NELISP_TRACE_COALESCE_TEST_DISABLE") "1"))))
         (nelisp-aot-compiler--object-hidden-boundary-symbols nil)
         (nelisp-aot-compiler--runtime-entry-params nil))
    (nelisp-aot-compile-to-object
     `(seq ,(nelisp-aot-trace-coalesce-test--definition (intern (concat "L_raw_" suffix)))
           ,(nelisp-aot-trace-coalesce-test--definition (intern (concat "L_word_" suffix)) t))
     (expand-file-name (concat suffix ".o") dir))))

(ert-deftest nelisp-aot-trace-coalesce/gnu-frame-observer-reentry-native-ABI ()
  (skip-unless (and (eq system-type 'gnu/linux) (executable-find "gcc-14")))
  (let* ((dir (make-temp-file "trace-coalesce-" t))
         (c (expand-file-name "caller.c" dir)) (bin (expand-file-name "probe" dir))
         (nelisp-aot-trace-coalesce-test--plans nil)
         (nelisp-aot-trace-coalesce-test--writes nil) cases)
    (dolist (w '(0 1 -1 -9223372036854775808 9223372036854775807 2 6 -81985529216486896))
      (dolist (n '(0 1 2 4 5 9)) (push (list 0 n w 0) cases)))
    (dolist (wt '((2 7) (1 2) (3 0) (536870918 63) (536871168 4)))
      (dolist (n '(0 3 5 9)) (push (list 1 n (car wt) (cadr wt)) cases)))
    (push '(1 0 0 0) cases) (push '(1 1 0 0) cases)
    (push '(1 -1 0 0) cases) (push '(1 -9223372036854775808 0 0) cases)
    (setq cases (nreverse cases))
    (advice-add 'nelisp-aot-compiler--emit-trace-loop-x86 :before #'nelisp-aot-trace-coalesce-test--record)
    (advice-add 'nelisp-aot-compiler--emit-trace-write-through-x86 :before #'nelisp-aot-trace-coalesce-test--write)
    (advice-add 'nelisp-aot-compiler--emit-extern-call :before #'nelisp-aot-trace-coalesce-test--cold)
    (unwind-protect
        (progn
          (nelisp-aot-trace-coalesce-test--object dir nil)
          (nelisp-aot-trace-coalesce-test--object dir t)
          (with-temp-file c
            (insert "#include <stdint.h>\n#include <sys/mman.h>\ntypedef uint64_t U;\n"
                    "extern U L_raw_on(U,U),L_raw_off(U,U),L_word_on(U,U),L_word_off(U,U);\n"
                    "struct C{U word,n,w,answer,count,obs[8][2];};\nstatic struct C cases[]={\n")
            (dolist (case cases)
              (pcase-let* ((`(,word ,n ,w ,tag) case)
                           (`(,answer ,obs) (nelisp-aot-trace-coalesce-test--oracle n w tag)))
                (insert (format "{%d,%d,UINT64_C(%d),UINT64_C(%d),%d,{" word n (logand w #xffffffffffffffff) answer (length obs)))
                (dolist (pair obs) (insert (format "{UINT64_C(%d),UINT64_C(%d)}," (car pair) (cadr pair))))
                (insert "}},\n")))
            (insert "};\nstatic struct C *cur;static U seen;static int bad,variant;static uintptr_t deltas[2];\n"
                    "__attribute__((noinline)) U L_observe(U tick,U w){volatile U *caller=__builtin_frame_address(1);uintptr_t me=(uintptr_t)__builtin_frame_address(0);\n"
                    "if((me&15)||seen>=cur->count)bad=1;else if(caller[-8]!=cur->obs[seen][0]||caller[-9]!=cur->obs[seen][1])bad=2;seen++;\n"
                    "uintptr_t d=(uintptr_t)caller-me;if(deltas[variant]&&deltas[variant]!=d)bad=3;deltas[variant]=d;\n"
                    "caller[-8]=UINT64_C(0x1111111111111111)^tick;caller[-9]=UINT64_C(0x2222222222222222)^tick;\n"
                    "U result=UINT64_C(0xfedcba9876543210)^tick^w;\n"
                    "__asm__ volatile(\"mov $41,%%rdi;mov $43,%%rsi;mov $47,%%rdx;mov $53,%%rcx;mov $59,%%r8;mov $61,%%r9;mov $67,%%r10;mov $71,%%r11\"::: \"rdi\",\"rsi\",\"rdx\",\"rcx\",\"r8\",\"r9\",\"r10\",\"r11\",\"memory\",\"cc\");return result;}\n"
                    "static int check(U(*f)(U,U),struct C*c){register U b __asm__(\"rbx\")=101,p __asm__(\"r12\")=103,q __asm__(\"r13\")=107,r __asm__(\"r14\")=109,s __asm__(\"r15\")=113;uintptr_t before,after;\n"
                    "__asm__ volatile(\"\":\"+r\"(b),\"+r\"(p),\"+r\"(q),\"+r\"(r),\"+r\"(s));__asm__ volatile(\"mov %%rsp,%0\":\"=r\"(before));U got=f(c->n,c->w);\n"
                    "__asm__ volatile(\"mov %%rsp,%0\":\"=r\"(after));__asm__ volatile(\"\":\"+r\"(b),\"+r\"(p),\"+r\"(q),\"+r\"(r),\"+r\"(s));return got!=c->answer||seen!=c->count||bad||before!=after||b!=101||p!=103||q!=107||r!=109||s!=113;}\n"
                    "int main(void){unsigned char*m=mmap((void*)536870912,4096,3,0x32,-1,0);if(m==(void*)-1)return 90;m[0]=63;m[256]=4;\n"
                    "for(unsigned i=0;i<sizeof(cases)/sizeof(cases[0]);i++){cur=&cases[i];deltas[0]=deltas[1]=0;for(variant=0;variant<2;variant++){seen=0;bad=0;U(*f)(U,U)=cur->word?(variant?L_word_off:L_word_on):(variant?L_raw_off:L_raw_on);if(check(f,cur))return 10+bad;}if(deltas[0]!=deltas[1])return 20;}return 0;}\n"))
          (should (= 0 (call-process "gcc-14" nil nil nil "-O2" "-mno-red-zone" "-fno-omit-frame-pointer" "-fno-stack-protector"
                                     "-no-pie" c (expand-file-name "on.o" dir) (expand-file-name "off.o" dir) "-o" bin)))
          (should (= 0 (call-process bin nil nil nil)))
          (message "L native GNU values, original frame observer/mutation/reentry and ABI PASS before admission gauge")
          ;; A genuine disabled run passes all semantics, then fails here.
          (should (= 2 (length (cl-remove-if-not #'cadr nelisp-aot-trace-coalesce-test--plans))))
          (should (= 8 (length nelisp-aot-trace-coalesce-test--writes)))
          (dolist (p (cl-remove-if-not #'cadr nelisp-aot-trace-coalesce-test--plans))
            (should (= 8 (length (nth 2 p))))
            (should (= 7 (length (delete-dups (mapcar #'cdr (nth 2 p))))))))
      (advice-remove 'nelisp-aot-compiler--emit-trace-loop-x86 #'nelisp-aot-trace-coalesce-test--record)
      (advice-remove 'nelisp-aot-compiler--emit-trace-write-through-x86 #'nelisp-aot-trace-coalesce-test--write)
      (advice-remove 'nelisp-aot-compiler--emit-extern-call #'nelisp-aot-trace-coalesce-test--cold)
      (delete-directory dir t))))

(provide 'nelisp-aot-trace-coalesce-test)
