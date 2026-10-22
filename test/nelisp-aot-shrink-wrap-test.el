;;; nelisp-aot-shrink-wrap-test.el --- Cold frame entry -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defun nelisp-aot-shrink-wrap-test--source ()
  '(seq
    (defun m_entry (a)
      (let* ((head (ptr-read-u64 805306368 0))
             (tail (ptr-read-u64 805310464 0)))
        (if (and (> head 0) (>= a head) (< a (+ head 16))) (+ a head)
          (let* ((chunk (ptr-read-u64 805306368 16)) (count 0))
            (seq (while (> chunk 0)
                   (seq (setq count (+ count chunk)) (setq chunk (- chunk 1))))
                 (+ (+ (+ a head) tail) count))))))
    (defun m_word (w)
      (let* ((head (ptr-read-u64 805306368 0))
             (tail (ptr-read-u64 805310464 0)))
        (if (> head 0) head
          (let ((i 0))
            (seq (while (< i 3) (setq i (+ i 1)))
                 (+ tail (if (= (logand w 7) 2) 7
                           (if (= (logand w 7) 6)
                               (logand (ptr-read-u64 (- w 6) 0) 63) 0))))))))))

(defun nelisp-aot-shrink-wrap-test--defs (source)
  (let ((nelisp-aot-compiler--object-hidden-boundary-symbols nil))
    (nelisp-aot-compiler--collect-defuns (nelisp-aot-compiler--parse source))))

(defun nelisp-aot-shrink-wrap-test--plans (defs)
  (let ((nelisp-aot-compiler--x86-frame-alias-definitions
         (nelisp-aot-compiler--x86-frame-alias-definitions defs))
        (nelisp-aot-compiler--x86-frame-alias-proof-cache (make-hash-table :test #'eq)))
    (mapcar (lambda (d) (cons (nelisp-aot-compiler--ir-get d :name)
                             (nelisp-aot-compiler--x86-shrink-wrap-plan d))) defs)))

(ert-deftest nelisp-aot-shrink-wrap/closed-prefix-and-cold-cut ()
  (let* ((defs (nelisp-aot-shrink-wrap-test--defs (nelisp-aot-shrink-wrap-test--source)))
         (plans (nelisp-aot-shrink-wrap-test--plans defs)))
    (dolist (name '(m_entry m_word))
      (let ((p (cdr (assq name plans))))
        (should p)
        (should (= 2 (length (plist-get p :snapshots))))
        (should (= 2 (length (plist-get p :prefix))))
        (should (assq :fallback (plist-get p :code)))
        (should-not (assq :store (plist-get p :code)))))))

(defun nelisp-aot-shrink-wrap-test--property (node key value)
  (apply #'nelisp-aot-compiler--make-ir (aref node 0)
         (plist-put (cdr (append node nil)) key value)))

(ert-deftest nelisp-aot-shrink-wrap/closed-owner-metadata-pressure-negatives ()
  (let* ((source (nelisp-aot-shrink-wrap-test--source))
         (d (car (nelisp-aot-shrink-wrap-test--defs source))))
    (dolist (property '((:unknown . t) (:internal-hidden . (0)) (:internal-entry . secret)
                        (:gc-root-slots . (1)) (:variadic . t) (:rest-p . t)
                        (:self-tco-raw . t) (:param-class . f64) (:param-repr . word)
                        (:return-repr . f64) (:fixed-param-count . 2) (:rt-slot-count . 100)))
      (should-not (cdar (nelisp-aot-shrink-wrap-test--plans
                        (list (nelisp-aot-shrink-wrap-test--property
                               d (car property) (cdr property)))))))
    (dolist (expr '((aot-current-sp) (addr-of a) (opaque a) (syscall-direct 9 0 0 0 0 0 0)
                    (atomic-fetch-add 805306368 1) (sexp-tag a)))
      (let ((s `(seq (defun bad (a)
                      (let* ((h ,expr) (t0 2))
                        (if (> h 0) h (let ((i 0))
                                        (seq (while (< i 2) (setq i (+ i 1))) (+ a t0)))))))))
        (condition-case nil
            (should-not (cdar (nelisp-aot-shrink-wrap-test--plans
                              (nelisp-aot-shrink-wrap-test--defs s))))
          (nelisp-aot-compiler-error nil))))
    ;; Six incoming registers leave no admitted spare for either initializer.
    (let ((s '(seq (defun pressure (a b c d e f)
                    (let* ((h (+ a b)) (t0 (+ c d)))
                      (if (> h 0) (+ e f)
                        (let ((i 0)) (seq (while (< i 2) (setq i (+ i 1))) t0))))))))
      (should-not (cdar (nelisp-aot-shrink-wrap-test--plans
                        (nelisp-aot-shrink-wrap-test--defs s)))))))

(defvar nelisp-aot-shrink-wrap-test--admissions 0)
(defvar nelisp-aot-shrink-wrap-test--layouts nil)
(defvar nelisp-aot-shrink-wrap-test--plans nil)
(defun nelisp-aot-shrink-wrap-test--record (&rest _)
  (cl-incf nelisp-aot-shrink-wrap-test--admissions))
(defun nelisp-aot-shrink-wrap-test--layout (fn d buf)
  (setf (alist-get (nelisp-aot-compiler--ir-get d :name)
                  nelisp-aot-shrink-wrap-test--layouts)
        (secure-hash 'sha256 (prin1-to-string d)))
  (funcall fn d buf))
(defun nelisp-aot-shrink-wrap-test--plan (fn node &rest args)
  (let ((result (apply fn node args)))
    (when result
      (push (secure-hash 'sha256 (prin1-to-string (list node result)))
            nelisp-aot-shrink-wrap-test--plans))
    result))

(defun nelisp-aot-shrink-wrap-test--object (file disabled)
  (let ((nelisp-aot-compiler--object-hidden-boundary-symbols nil)
        (nelisp-aot-compiler--x86-shrink-wrap
         (not (or disabled (getenv "NELISP_M_FORCE_DISABLED"))))
        (nelisp-aot-shrink-wrap-test--admissions 0)
        (nelisp-aot-shrink-wrap-test--layouts nil)
        (nelisp-aot-shrink-wrap-test--plans nil)
        (planners '(nelisp-aot-compiler--x86-promotion-plan
                    nelisp-aot-compiler--x86-call-free-region-plan
                    nelisp-aot-compiler--x86-leaf-entry-plan
                    nelisp-aot-compiler--x86-terminal-entry-plan
                    nelisp-aot-compiler--x86-trace-loop-plan
                    nelisp-aot-compiler--x86-self-tco-entry-plan)))
    (advice-add 'nelisp-aot-compiler--emit-shrink-wrap-prefix-x86
                :before #'nelisp-aot-shrink-wrap-test--record)
    (advice-add 'nelisp-aot-compiler--emit-defun
                :around #'nelisp-aot-shrink-wrap-test--layout)
    (dolist (fn planners) (advice-add fn :around #'nelisp-aot-shrink-wrap-test--plan))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object (nelisp-aot-shrink-wrap-test--source) file)
          (list nelisp-aot-shrink-wrap-test--admissions
                nelisp-aot-shrink-wrap-test--layouts nelisp-aot-shrink-wrap-test--plans))
      (advice-remove 'nelisp-aot-compiler--emit-shrink-wrap-prefix-x86
                     #'nelisp-aot-shrink-wrap-test--record)
      (advice-remove 'nelisp-aot-compiler--emit-defun #'nelisp-aot-shrink-wrap-test--layout)
      (dolist (fn planners) (advice-remove fn #'nelisp-aot-shrink-wrap-test--plan)))))

(ert-deftest nelisp-aot-shrink-wrap/native-fast-cold-read-once-fullword-ABI ()
  (let* ((dir (make-temp-file "nelisp-shrink-wrap-" t))
         (c (expand-file-name "probe.c" dir))
         (counts nil) (frames nil))
    (unwind-protect
        (progn
          (with-temp-file c
            (insert "#define _GNU_SOURCE\n#include <stdint.h>\n#include <sys/mman.h>\n#include <signal.h>\n#include <stdlib.h>\n#include <stdio.h>\n#include <ucontext.h>\n"
                    "extern uint64_t m_entry(uint64_t),m_word(uint64_t);\n"
                    "static volatile uint64_t *h=(void*)0x30000000,*t=(void*)0x30001000;static volatile int faults,frames;static volatile uintptr_t before;\n"
                    "static void handler(int s,siginfo_t*i,void*c){(void)s;if((uintptr_t)i->si_addr<0x30001000||(uintptr_t)i->si_addr>=0x30002000)_Exit(81);faults++;if((uintptr_t)((ucontext_t*)c)->uc_mcontext.gregs[REG_RSP]==before-8)frames++;if(mprotect((void*)t,4096,PROT_READ|PROT_WRITE))_Exit(82);h[0]=101;}\n"
                    "static void prepare(uint64_t x){h[0]=x;h[2]=3;*t=11;faults=0;if(mprotect((void*)t,4096,PROT_NONE))abort();}\n"
                    "__attribute__((noinline))static uint64_t invoke(uint64_t(*f)(uint64_t),uint64_t x){register uint64_t a asm(\"rbx\")=0x11223344,b asm(\"r12\")=0x22334455,c asm(\"r13\")=0x33445566,d asm(\"r14\")=0x44556677,e asm(\"r15\")=0x55667788;asm volatile(\"\":\"+r\"(a),\"+r\"(b),\"+r\"(c),\"+r\"(d),\"+r\"(e));asm volatile(\"mov %%rsp,%0\":\"=m\"(before)::\"memory\");uint64_t r=f(x);asm volatile(\"\":\"+r\"(a),\"+r\"(b),\"+r\"(c),\"+r\"(d),\"+r\"(e));if(a!=0x11223344||b!=0x22334455||c!=0x33445566||d!=0x44556677||e!=0x55667788)abort();return r;}\n"
                    "int main(void){if(mmap((void*)h,8192,3,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED,-1,0)==MAP_FAILED)return 80;struct sigaction sa={0};sa.sa_sigaction=handler;sa.sa_flags=SA_SIGINFO;if(sigaction(SIGSEGV,&sa,0))return 83;uint64_t xs[]={0,1,UINT64_MAX,5,6,20,21,INT64_MAX,UINT64_C(1)<<63};for(unsigned i=0;i<sizeof(xs)/sizeof(*xs);i++){prepare(5);uint64_t x=xs[i],want=((int64_t)x>=5&&(int64_t)x<21)?x+5:x+22;if(invoke(m_entry,x)!=want||faults!=1)return 84;}uint64_t box[2] __attribute__((aligned(8)))={39,0};uint64_t ws[]={2,(uintptr_t)box+6,5};uint64_t want[]={18,50,11};for(unsigned i=0;i<3;i++){prepare(0);if(invoke(m_word,ws[i])!=want[i]||faults!=1)return 85;}prepare(5);if(invoke(m_word,2)!=5||faults!=1)return 86;printf(\"M-NATIVE-13-PASS frames=%d\\n\",frames);return 0;}\n"))
          (dolist (disabled '(nil t))
            (let* ((stem (expand-file-name (if disabled "off" "on") dir))
                   (object (concat stem ".o")))
              (push (nelisp-aot-shrink-wrap-test--object object disabled) counts)
              (should (= 0 (call-process "gcc-14" nil nil nil "-O2" "-no-pie" c object "-o" stem)))
              (with-temp-buffer
                (should (= 0 (call-process stem nil t nil)))
                (goto-char (point-min))
                (should (re-search-forward "M-NATIVE-13-PASS frames=\\([0-9]+\\)" nil t))
                (push (string-to-number (match-string 1)) frames))))
          ;; The forced-off negative reaches this gauge after both value runs.
          (should (= 0 (caar counts)))
          (should (equal (nth 1 (car counts)) (nth 1 (cadr counts))))
          (should (equal (nth 2 (car counts)) (nth 2 (cadr counts))))
          (should (= 0 (car frames)))
          (should (= 13 (cadr frames)))
          (should (> (car (cadr counts)) 0)))
      (delete-directory dir t))))

(ert-deftest nelisp-aot-shrink-wrap/native-private-label-export-collisions ()
  (let* ((dir (make-temp-file "nelisp-shrink-collision-" t))
         (o (expand-file-name "probe.o" dir))
         (c (expand-file-name "probe.c" dir))
         (exe (expand-file-name "probe" dir))
         (nelisp-aot-compiler--object-hidden-boundary-symbols nil)
         (nelisp-aot-compiler--x86-shrink-wrap t)
         (nelisp-aot-shrink-wrap-test--admissions 0)
         (source (append (nelisp-aot-shrink-wrap-test--source)
                         '((defun guarded-call-label (a) (+ a 1))
                           (defun shrink-cold (a) (+ a 2))
                           (defun trace-guarded-call-label (a) (+ a 3))))))
    (unwind-protect
        (progn
          (advice-add 'nelisp-aot-compiler--emit-shrink-wrap-prefix-x86
                      :before #'nelisp-aot-shrink-wrap-test--record)
          (nelisp-aot-compile-to-object source o)
          (advice-remove 'nelisp-aot-compiler--emit-shrink-wrap-prefix-x86
                         #'nelisp-aot-shrink-wrap-test--record)
          (should (= 2 nelisp-aot-shrink-wrap-test--admissions))
          (with-temp-file c
            (insert "#define _GNU_SOURCE\n#include <stdint.h>\n#include <sys/mman.h>\n"
                    "extern uint64_t m_entry(uint64_t),m_word(uint64_t);\n"
                    "extern uint64_t g(uint64_t) asm(\"\\\"guarded-call-label\\\"\"),c(uint64_t) asm(\"\\\"shrink-cold\\\"\"),t(uint64_t) asm(\"\\\"trace-guarded-call-label\\\"\");\n"
                    "int main(void){uint64_t*h=(void*)0x30000000,*tail=(void*)0x30001000;if(mmap(h,8192,3,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED,-1,0)==MAP_FAILED)return 80;h[0]=5;h[2]=3;*tail=11;if(g(40)!=41||c(40)!=42||t(40)!=43)return 81;if(m_entry(6)!=11||m_entry(100)!=122||m_word(2)!=5)return 82;h[0]=0;if(m_word(2)!=18)return 83;return 0;}\n"))
          (should (= 0 (call-process "gcc-14" nil nil nil "-O2" "-no-pie" c o "-o" exe)))
          (should (= 0 (call-process exe nil nil nil))))
      (advice-remove 'nelisp-aot-compiler--emit-shrink-wrap-prefix-x86
                     #'nelisp-aot-shrink-wrap-test--record)
      (delete-directory dir t))))

(provide 'nelisp-aot-shrink-wrap-test)
