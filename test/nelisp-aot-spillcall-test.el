;;; nelisp-aot-spillcall-test.el --- callee-saved registers across calls (spillcall) -*- lexical-binding: t; -*-

;; The vm-register plan keeps opted-in variables in callee-saved registers for the whole body,
;; calls included.  With `nelisp-aot-compiler-spillcall' a variable the raw proof refuses (it may
;; hold a Lisp reference) is admitted too, because the collector flushes the callee-saved
;; registers before its conservative scan (`nl_gc_scan_regs'); its frame word is then a stale
;; copy, so every call site's stack-map live set must leave that word out.  These tests compile a
;; two-function unit with the unit compiler the standalone build uses and read the decision and
;; the per-call-site maps.  Run:
;;   emacs -Q --batch -L lisp -L src -L scripts -L test -l test/nelisp-aot-spillcall-test.el \
;;     -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(setenv "NELISP_STANDALONE_TARGET" "linux-x86_64")
(require 'nelisp-standalone-build)

(defconst nelisp-aot-spillcall-test--forms
  '((defun spc_callee (a i)
      ;; Recursive, so no pass inlines it: the call stays a call site with a map.
      (if (> i 1000000) (spc_callee a (- i 1)) (+ (ptr-read-u64 a 0) i)))
    (defun spc_probe (p n)
      ;; KEEP is a 64-bit load (raw-proof class 1: may be a reference) that stays live across
      ;; the call in the loop and is returned after it; I and S are raw.
      (let* ((keep (ptr-read-u64 p 0)) (i 0) (s 0))
        (seq
         (while (< i n)
           (seq (setq s (+ s (spc_callee keep i)))
                (setq i (+ i 1))))
         (+ keep s)))))
  "A unit whose opted-in variable KEEP holds an unknown word across a call.")

(defun nelisp-aot-spillcall-test--compile (spillcall)
  "Compile the probe unit with SPILLCALL; return (DECISION STACKMAP-ENTRY KEEP-SLOT)."
  (let* ((max-lisp-eval-depth 40000)
         (nelisp-aot-compiler-spillcall spillcall)
         (nelisp-aot-compiler-vm-register-functions '((spc_probe keep i s)))
         (nelisp-aot-compiler--vm-reg-decisions nil)
         (unit (nelisp-standalone--compile-to-unit
                "spillcall-probe" (cons 'seq nelisp-aot-spillcall-test--forms)))
         (decision (cdr (assoc "spc_probe" (mapcar (lambda (d) (cons (format "%s" (car d)) (cdr d)))
                                                   nelisp-aot-compiler--vm-reg-decisions))))
         (smap (cl-find "spc_probe" (plist-get unit :stackmap)
                        :key (lambda (f) (plist-get f :name)) :test #'equal)))
    (list decision smap unit)))

(defun nelisp-aot-spillcall-test--keep-slot (decision)
  "The slot the vm-register plan gave KEEP: the admitted pair that is not raw-provable."
  (car (car (cdr decision))))

(ert-deftest nelisp-aot-spillcall-admits-reference-across-call ()
  "With spillcall the plan admits KEEP (refused by the raw proof) into a callee-saved register."
  (let* ((on (car (nelisp-aot-spillcall-test--compile t)))
         (off (car (nelisp-aot-spillcall-test--compile nil))))
    (should (eq (car on) :admit))
    (should (= (length (cdr on)) 3))
    (should (cl-every (lambda (p) (memq (cdr p) '(r12 r13 r14 r15 rbx))) (cdr on)))
    ;; Without spillcall the raw proof drops KEEP and S (a sum with a call result) and keeps I.
    (should (eq (car off) :admit))
    (should (= (length (cdr off)) 1))
    (should-not (assq (nelisp-aot-spillcall-test--keep-slot on) (cdr off)))))

(ert-deftest nelisp-aot-spillcall-map-drops-register-slots ()
  "Every call site of the spillcall unit leaves the register-held slots out of its live set."
  (let* ((r (nelisp-aot-spillcall-test--compile t))
         (pairs (cdr (nth 0 r))) (smap (nth 1 r))
         (sites (plist-get smap :sites)))
    (should smap)
    (should-not (plist-get smap :reason))
    (should sites)
    (dolist (s sites)
      (should (integerp (cdr s)))
      (dolist (p pairs)
        (should (zerop (logand (cdr s) (ash 1 (car p)))))))))

(ert-deftest nelisp-aot-spillcall-off-keeps-live-word ()
  "Negative control: without spillcall KEEP stays in its frame word, which is live at the call."
  (let* ((on (nelisp-aot-spillcall-test--compile t))
         (keep (nelisp-aot-spillcall-test--keep-slot (nth 0 on)))
         (off (nelisp-aot-spillcall-test--compile nil))
         (sites (plist-get (nth 1 off) :sites)))
    (should sites)
    (should (cl-some (lambda (s) (and (integerp (cdr s)) (/= 0 (logand (cdr s) (ash 1 keep)))))
                     sites))))

(provide 'nelisp-aot-spillcall-test)
;;; nelisp-aot-spillcall-test.el ends here
