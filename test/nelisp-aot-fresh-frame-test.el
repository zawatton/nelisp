;;; nelisp-aot-fresh-frame-test.el --- No fresh-frame zeroing policy -*- lexical-binding: t; -*-
;; smapz (Wave 2.5 stage 3): the collector scans native frames by their stack maps, so the compiler no longer
;; zeroes fresh runtime slots.  These tests pin the deletion of the policy.
(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defconst nelisp-fresh-frame-test--rep-stos (unibyte-string #xfc #xf3 #x48 #xab)
  "CLD; REP STOSQ: the tail of the deleted 26-byte fresh-frame loop.")

(defun nelisp-fresh-frame-test--emit (source slots)
  (let* ((ir (nelisp-aot-compiler--ir-with
              (car (nelisp-aot-compiler--collect-defuns (nelisp-aot-compiler--parse source)))
              :rt-slot-count slots))
         (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--arch 'x86_64)
         (buf (nelisp-asm-x86_64-make-buffer 'sysv)))
    (nelisp-aot-compiler--emit-defun ir buf)
    (nelisp-asm-x86_64-resolve-fixups buf)))

(ert-deftest nelisp-fresh-frame-policy-is-deleted ()
  (should-not (boundp 'nelisp-aot-compiler-zero-fresh-frame-functions))
  (should-not (fboundp 'nelisp-aot-compiler--emit-zero-fresh-rt-slots))
  (should-not (fboundp 'nelisp-standalone--zero-fresh-frame-functions)))

(ert-deftest nelisp-fresh-frame-emits-no-rep-stos-for-formerly-selected-names ()
  ;; The names the policy used to select must now compile like any other function.
  (dolist (name '(nelisp_apply_function_w nl_apply_builtin_w nl_apply_builtin_leaf_42_w
                  bf_garbage_collect_w nl_gc_stats_run m5_fx_run_body))
    (let ((code (nelisp-fresh-frame-test--emit
                 `(defun ,name (a b c d e f g h) (let* ((x (+ a h))) (+ x (+ b (+ c (+ d (+ e (+ f g))))))))
                 4096)))
      (should (> (length code) 0))
      (should-not (string-match-p (regexp-quote nelisp-fresh-frame-test--rep-stos) code)))))
