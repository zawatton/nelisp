;;; nelisp-aot-zinit-test.el --- batched zero stores for zero-initialised lets -*- lexical-binding: t; -*-

;; A run of nested `let' locals initialised to 0 in a function named by
;; `nelisp-aot-compiler-zero-init-batch-functions' is stored with 16-byte zero
;; stores (pxor xmm0,xmm0 + movdqu).  Functions outside the list keep the
;; `mov rax,0; mov [slot],rax' lowering byte for byte.  Run:
;;   emacs -Q --batch -L lisp -L src -L scripts -L test -l test/nelisp-aot-zinit-test.el \
;;     -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(setenv "NELISP_STANDALONE_TARGET" "linux-x86_64")
(require 'nelisp-standalone-build)

(defconst nelisp-aot-zinit-test--forms
  '((defun zi_callee (a i)
      (if (> i 1000000) (zi_callee a (- i 1)) (+ (ptr-read-u64 a 0) i)))
    (defun zi_probe (p n)
      (let* ((a 0) (b 0) (c 0) (d 0) (e 0) (f 0) (g 0) (i 0) (s 0))
        (seq (while (< i n)
               (seq (setq s (+ s (zi_callee p i))) (setq a (+ a i)) (setq b (+ b a)) (setq c (+ c b))
                    (setq d (+ d c)) (setq e (+ e d)) (setq f (+ f e)) (setq g (+ g f)) (setq i (+ i 1))))
             (+ s (+ a (+ b (+ c (+ d (+ e (+ f g))))))))))
    (defun zi_rl (p n)
      (let* ((x (ptr-read-u64 p 0)) (y (+ x n)) (z (zi_callee p y)))
        (+ z (+ (* x y) (* y z)))))))

(defun nelisp-aot-zinit-test--text (gate)
  "Text bytes of the probe unit compiled with GATE as the batch list."
  (let* ((max-lisp-eval-depth 40000)
         (nelisp-aot-compiler-zero-init-batch-functions gate)
         (unit (nelisp-standalone--compile-to-unit
                "zinit-probe" (cons 'seq nelisp-aot-zinit-test--forms))))
    (cdr (assq 'text (plist-get unit :sections)))))

(defconst nelisp-aot-zinit-test--pxor (unibyte-string #x66 #x0F #xEF #xC0))

(ert-deftest nelisp-aot-zinit-batches-gated-function ()
  (let ((text (nelisp-aot-zinit-test--text '(zi_probe))))
    (should (stringp text))
    (should (string-search nelisp-aot-zinit-test--pxor text))))

(ert-deftest nelisp-aot-zinit-leaves-other-functions-alone ()
  "Negative control: with an empty gate the batch never appears."
  (let ((text (nelisp-aot-zinit-test--text nil)))
    (should (stringp text))
    (should-not (string-search nelisp-aot-zinit-test--pxor text))))

;; slot-reload peephole: the gated function drops reloads of a just-stored slot; a gate that
;; does not name it is byte-identical to an empty gate (the control).
(defun nelisp-aot-zinit-test--reload-text (gate)
  (let* ((max-lisp-eval-depth 40000)
         (nelisp-aot-compiler-zero-init-batch-functions nil)
         (nelisp-aot-compiler-slot-reload-functions gate)
         (unit (nelisp-standalone--compile-to-unit
                "zinit-probe" (cons 'seq nelisp-aot-zinit-test--forms))))
    (cdr (assq 'text (plist-get unit :sections)))))

(ert-deftest nelisp-aot-slot-reload-shrinks-gated-function ()
  (should (< (length (nelisp-aot-zinit-test--reload-text '(zi_rl)))
             (length (nelisp-aot-zinit-test--reload-text nil)))))

(ert-deftest nelisp-aot-slot-reload-gate-off-is-identical ()
  "Negative control: a gate naming another function changes nothing."
  (should (equal (nelisp-aot-zinit-test--reload-text '(other_fn))
                 (nelisp-aot-zinit-test--reload-text nil))))

(provide 'nelisp-aot-zinit-test)
;;; nelisp-aot-zinit-test.el ends here
