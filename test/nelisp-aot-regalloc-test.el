;;; nelisp-aot-regalloc-test.el --- Path-weighted region registers, scaled indexes, PIC tables -*- lexical-binding: t; -*-
;; regalloc: `nelisp-aot-compiler-regalloc' (1) counts a call-free region's frame accesses along
;; its most used path and splits regions at exclusive branches, (2) folds E*SCALE+DISP offsets
;; into [base+index*scale+disp], (3) emits jump tables whose entries are offsets from the table
;; start.  The native test runs a small byte-code VM (threaded dispatch, vm-register state) on
;; fixed and random programs under both settings and checks each result against a C reference.
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defvar nelisp-aot-compiler-regalloc)
(defvar nelisp-aot-compiler-threaded-dispatch-functions)
(defvar nelisp-aot-compiler-vm-register-functions)

(defconst regalloc-test--vm
  '(seq
    (defun ra_run (code mem n)
      (let* ((pc 0) (sp 0) (go n) (raw 0) (acc 0) (k 0))
        (seq
         (while (< pc go)
           (setq raw (ptr-read-u8 code pc))
           (if (< raw 4)
               (if (< raw 2)
                   (if (= raw 0)
                       (seq (ptr-write-u64 mem (* sp 8) (+ (shl (ptr-read-u8 code (+ pc 1)) 2) 1))
                            (setq sp (+ sp 1)) (setq pc (+ pc 2)))
                     (if (< sp 2) (setq go 0)
                       (let* ((pa (ptr-read-u64 mem (* (- sp 2) 8)))
                              (pb (ptr-read-u64 mem (* (- sp 1) 8)))
                              (pn (+ (sar pa 2) (sar pb 2))))
                         (if (if (= (logand pa 3) 1) (if (= (logand pb 3) 1) (if (> pn 100000) 0 1) 0) 0)
                             (seq (setq sp (- sp 1))
                                  (ptr-write-u64 mem (* (- sp 1) 8) (+ (shl pn 2) 1))
                                  (setq pc (+ pc 1)))
                           (seq (setq acc (+ acc (ra_cold pn))) (setq go 0))))))
                 (if (= raw 2)
                     (if (< sp 1) (setq go 0)
                       (seq (ptr-write-u64 mem (* sp 8) (ptr-read-u64 mem (* (- sp 1) 8)))
                            (setq sp (+ sp 1)) (setq pc (+ pc 1))))
                   (seq (setq k (ptr-read-u8 code (+ pc 1)))
                        (if (>= k sp) (setq go 0)
                          (seq (ptr-write-u64 mem (* sp 8) (ptr-read-u64 mem (* (- sp k 1) 8)))
                               (setq sp (+ sp 1)) (setq pc (+ pc 2)))))))
             (if (< raw 7)
                 (if (= raw 4)
                     (if (< sp 2) (setq go 0)
                       (let* ((la (ptr-read-u64 mem (* (- sp 2) 8)))
                              (lb (ptr-read-u64 mem (* (- sp 1) 8))))
                         (seq (setq sp (- sp 2))
                              (if (< la lb) (setq pc (ptr-read-u8 code (+ pc 1))) (setq pc (+ pc 2))))))
                   (if (= raw 5)
                       (if (< sp 1) (setq go 0)
                         (seq (setq k (ptr-read-u8 code (+ pc 1)))
                              (if (>= k sp) (setq go 0)
                                (seq (ptr-write-u64 mem (* (- sp k 1) 8) (ptr-read-u64 mem (* (- sp 1) 8)))
                                     (setq sp (- sp 1)) (setq pc (+ pc 2))))))
                     (seq (setq acc (+ acc (ra_cold sp))) (setq pc (+ pc 1)))))
               (setq go 0))))
         (+ (* sp 1000000) (* acc 1000)
            (if (> sp 0) (logand (sar (ptr-read-u64 mem (* (- sp 1) 8)) 2) 1023) 0)))))
    ;; Equality and interval chains: the non-threaded dense and ordered jump tables.
    (defun ra_sw (x)
      (if (= x 1) 11 (if (= x 2) 22 (if (= x 3) 33 (if (= x 4) 44 (if (= x 5) 55 (if (= x 6) 66
        (if (= x 7) 77 (if (= x 8) 88 (if (= x 9) 99 (if (= x 10) 110 (if (= x 12) 120 -1))))))))))))
    (defun ra_ord (x)
      (if (< x 10) 1 (if (< x 20) 2 (if (< x 30) 3 (if (< x 40) 4 (if (< x 50) 5 (if (< x 60) 6
        (if (< x 70) 7 (if (< x 80) 8 (if (< x 90) 9 (if (< x 100) 10 11)))))))))))))

(defconst regalloc-test--driver "
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
extern int64_t ra_run(uint8_t *code, int64_t *mem, int64_t n);
extern int64_t ra_sw(int64_t x), ra_ord(int64_t x);
static int64_t colds;
int64_t ra_cold(int64_t x) { colds++; return (x & 7) + 1; }
static int64_t ref(uint8_t *code, int64_t *mem, int64_t n) {
  int64_t pc = 0, sp = 0, go = n, acc = 0, k;
  while (pc < go) {
    int raw = code[pc];
    if (raw == 0) { mem[sp++] = ((int64_t)code[pc + 1] << 2) + 1; pc += 2; }
    else if (raw == 1) { if (sp < 2) { go = 0; continue; }
      int64_t pa = mem[sp - 2], pb = mem[sp - 1], pn = (pa >> 2) + (pb >> 2);
      if ((pa & 3) == 1 && (pb & 3) == 1 && pn <= 100000) { sp--; mem[sp - 1] = (pn << 2) + 1; pc++; }
      else { acc += (pn & 7) + 1; go = 0; } }
    else if (raw == 2) { if (sp < 1) { go = 0; continue; } mem[sp] = mem[sp - 1]; sp++; pc++; }
    else if (raw == 3) { k = code[pc + 1]; if (k >= sp) { go = 0; continue; } mem[sp] = mem[sp - k - 1]; sp++; pc += 2; }
    else if (raw == 4) { if (sp < 2) { go = 0; continue; } int64_t la = mem[sp - 2], lb = mem[sp - 1]; sp -= 2;
      pc = la < lb ? code[pc + 1] : pc + 2; }
    else if (raw == 5) { if (sp < 1) { go = 0; continue; } k = code[pc + 1]; if (k >= sp) { go = 0; continue; }
      mem[sp - k - 1] = mem[sp - 1]; sp--; pc += 2; }
    else if (raw == 6) { acc += (sp & 7) + 1; pc++; }
    else go = 0;
  }
  return sp * 1000000 + acc * 1000 + (sp > 0 ? ((mem[sp - 1] >> 2) & 1023) : 0);
}
int main(void) {
  /* sum loop: s=0 i=0; L: s+=3; i+=1; if i<200 goto L */
  uint8_t sum[] = {0,0, 0,0, 3,1, 0,3, 1, 5,2, 3,0, 0,1, 1, 5,1, 3,0, 0,200, 4,4, 6, 255};
  int64_t m1[4096], m2[4096]; int fails = 0;
  int64_t a = ra_run(sum, m1, sizeof sum), b = ref(sum, m2, sizeof sum);
  if (a != b) { printf(\"sum got %lld want %lld\\n\", (long long)a, (long long)b); fails++; }
  srand(12345);
  for (int t = 0; t < 4000; t++) {
    uint8_t code[66] = {0}; int n = 4 + rand() % 60;
    for (int i = 0; i < n; i++) code[i] = (i % 2 == 0) ? rand() % 8 : rand() % 6;
    /* forward jumps only, so every random program ends */
    for (int i = 0; i + 1 < n; i++) { if (code[i] == 4) { code[i + 1] = i + 2 + code[i + 1] % (n - i - 1); i++; } }
    for (int i = 0; i < 4096; i++) m1[i] = m2[i] = 0;
    int64_t x = ra_run(code, m1, n);
    int64_t y = ref(code, m2, n);
    if (x != y) { if (fails++ < 5) printf(\"prog %d got %lld want %lld\\n\", t, (long long)x, (long long)y); }
  }
  for (int64_t x = -5; x <= 300; x++) {
    int64_t w = (x >= 1 && x <= 10) ? 11 * x : (x == 12 ? 120 : -1);
    int64_t o = x < 100 ? (x < 10 ? 1 : x / 10 + 1) : 11;
    if (ra_sw(x) != w || ra_ord(x) != o) { if (fails++ < 5) printf(\"table x=%lld sw %lld ord %lld\\n\", (long long)x, (long long)ra_sw(x), (long long)ra_ord(x)); }
  }
  printf(\"regalloc-vm fails=%d\\n\", fails);
  return fails != 0;
}
")

(defun regalloc-test--build (regalloc dir)
  "Compile the VM with REGALLOC into DIR, link the C driver; return (EXE . RA_RUN-INSN-COUNT)."
  (let ((obj (expand-file-name (format "vm-%s.o" regalloc) dir))
        (c (expand-file-name "drv.c" dir))
        (exe (expand-file-name (format "vm-%s" regalloc) dir))
        (nelisp-aot-compiler-regalloc regalloc)
        (nelisp-aot-compiler-threaded-dispatch-functions '(ra_run))
        (nelisp-aot-compiler-vm-register-functions '((ra_run pc sp raw code mem))))
    (nelisp-aot-compile-to-object regalloc-test--vm obj)
    (with-temp-file c (insert regalloc-test--driver))
    (should (= 0 (call-process "cc" nil nil nil "-O1" c obj "-o" exe)))
    (cons exe
          (with-temp-buffer
            (call-process "objdump" nil t nil "-d" "--no-show-raw-insn" "-M" "intel" obj)
            (goto-char (point-min))
            (let ((n 0) (in nil))
              (while (not (eobp))
                (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
                  (cond ((string-match-p "<ra_run>:" line) (setq in t))
                        ((string-match-p "^[0-9a-f]+ <" line) (setq in nil))
                        ((and in (string-match-p "^ +[0-9a-f]+:\t" line)) (setq n (1+ n)))))
                (forward-line 1))
              n)))))

(ert-deftest regalloc/native-vm-equals-reference-both-settings ()
  "The VM gives the C reference's results on 4001 programs with regalloc off and on,
and the regalloc lowering is shorter."
  (skip-unless (and (eq system-type 'gnu/linux) (string-match-p "x86_64" system-configuration)
                    (executable-find "cc") (executable-find "objdump")))
  (let ((d (make-temp-file "regalloc-" t)))
    (unwind-protect
        (let ((old (regalloc-test--build nil d))
              (new (regalloc-test--build t d)))
          (should (= 0 (call-process (car old) nil nil nil)))
          (should (= 0 (call-process (car new) nil nil nil)))
          (message "regalloc ra_run instructions: off %d on %d" (cdr old) (cdr new))
          (should (< (cdr new) (cdr old))))
      (delete-directory d t))))

(ert-deftest regalloc/negative-control-wrong-table-entries-caught ()
  "With the table-start addend dropped from the entries the driver must fail."
  (skip-unless (and (eq system-type 'gnu/linux) (string-match-p "x86_64" system-configuration)
                    (executable-find "cc") (executable-find "objdump")))
  (let ((d (make-temp-file "regalloc-neg-" t))
        (orig (symbol-function 'nelisp-asm-x86_64-emit-fixup)))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-asm-x86_64-emit-fixup)
                   (lambda (buf slot label) (funcall orig buf slot (if (consp label) (car label) label)))))
          (let ((bad (regalloc-test--build t d)))
            ;; A crash returns a signal description string, a wrong answer exit status 1.
            (should-not (eql 0 (call-process (car bad) nil nil nil)))))
      (delete-directory d t))))

(ert-deftest regalloc/path-counts-most-used-path-and-expected ()
  "An `if' counts its test plus the larger arm per slot (or, expected, each arm at 1/2);
a plain node sums its children."
  (let* ((ref (lambda (s) (nelisp-aot-compiler--make-ir 'ref :var 'v :slot s :class 'gp)))
         (node (nelisp-aot-compiler--make-ir
                'if :id 'x :test (funcall ref 1)
                :then (nelisp-aot-compiler--make-ir 'value-seq :forms (list (funcall ref 2) (funcall ref 2) (funcall ref 3)))
                :else (nelisp-aot-compiler--make-ir 'value-seq :forms (list (funcall ref 2) (funcall ref 3) (funcall ref 3) (funcall ref 3)))))
         (counts (nelisp-aot-compiler--x86-path-slot-counts node)))
    (should (equal (sort (copy-sequence counts) (lambda (a b) (< (car a) (car b))))
                   '((1 . 1) (2 . 2) (3 . 3))))
    (should (equal (sort (copy-sequence (nelisp-aot-compiler--x86-path-slot-counts node t))
                         (lambda (a b) (< (car a) (car b))))
                   '((1 . 1) (2 . 1.5) (3 . 2.0))))
    ;; Slots 2 and 3 are used in both arms and slot 1 in the test: the region is not split.
    (should-not (nelisp-aot-compiler--x86-branch-confined-p node '(2 3)))
    (should-not (nelisp-aot-compiler--x86-branch-confined-p node '(1)))
    ;; Each arm's own slot: split, so each arm plans its own registers.
    (should (nelisp-aot-compiler--x86-branch-confined-p
             (nelisp-aot-compiler--make-ir 'if :id 'y :test (funcall ref 1)
                                           :then (funcall ref 2) :else (funcall ref 3))
             '(2 3)))))

(provide 'nelisp-aot-regalloc-test)
;;; nelisp-aot-regalloc-test.el ends here
