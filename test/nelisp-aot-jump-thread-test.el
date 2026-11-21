;;; nelisp-aot-jump-thread-test.el --- Gated jump threading in the AOT compiler -*- lexical-binding: t; -*-

;; `nelisp-aot-compiler-jump-thread-functions' retargets jumps whose label
;; is bound at another unconditional jump.  Only the listed defuns change;
;; every other defun must be emitted byte for byte as before.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defconst nelisp-aot-jump-thread-test--forms
  '((defun jt_callee (a i)
      (if (> i 1000000) (jt_callee a (- i 1)) (+ (ptr-read-u64 a 0) i)))
    (defun jt_probe (p n)
      (let* ((i 0) (s 0))
        (seq (while (< i n)
               (seq (if (if (> i 3) (if (> i 5) (jt_callee p i) 1) 2)
                        (setq s (+ s 1))
                      (setq s (+ s 2)))
                    (if (> s 5)
                        (if (= s 7) (setq s (jt_callee p s))
                          (if (= s 9) (setq s 1) (setq s (+ s 3))))
                      (if (= s 2) (if (= s 1) (setq s 4) (setq s 5)) (setq s 6)))
                    (setq i (+ i 1))))
             s))))
  "Nested conditionals whose join points compile to jump-to-jump chains.")

(defun nelisp-aot-jump-thread-test--text (gate)
  "Return the text section of the probe unit compiled with GATE."
  (let* ((process-environment
          (cons "NELISP_STANDALONE_TARGET=linux-x86_64" process-environment))
         (max-lisp-eval-depth 40000)
         (nelisp-aot-compiler-jump-thread-functions gate)
         (unit (nelisp-standalone--compile-to-unit
                "jt-probe" (cons 'seq nelisp-aot-jump-thread-test--forms))))
    (cdr (assq 'text (plist-get unit :sections)))))

(ert-deftest nelisp-aot-jump-thread-gated-defun-changes ()
  "Threading a listed defun changes its jump targets, not the code size."
  (let ((on (nelisp-aot-jump-thread-test--text '(jt_probe)))
        (off (nelisp-aot-jump-thread-test--text nil)))
    (should (= (length on) (length off)))
    (should-not (equal on off))))

(ert-deftest nelisp-aot-jump-thread-unlisted-defun-identical ()
  "A gate that names no probe defun leaves the output unchanged."
  (should (equal (nelisp-aot-jump-thread-test--text nil)
                 (nelisp-aot-jump-thread-test--text '(other)))))

(provide 'nelisp-aot-jump-thread-test)
;;; nelisp-aot-jump-thread-test.el ends here
