;;; nelisp-eln-leaf-code-test.el --- Leaf-code verifier tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-eln-leaf-code)
(require 'nelisp-eln-emitter)

(defun nelisp-eln-leaf-code-test--emitted-conditional ()
  "Return code emitted for a nested conditional leaf IR."
  (let* ((reference (lambda ()
                      (nelisp-aot-compiler--make-ir
                       'ref :var 'value :reg 'rdi :slot 0 :class 'gp)))
         (nested (nelisp-aot-compiler--make-ir
                  'if :test (funcall reference)
                  :then (nelisp-aot-compiler--make-ir 'imm :value nil)
                  :else (nelisp-aot-compiler--make-ir 'imm :value -1)))
         (body (nelisp-aot-compiler--make-ir
                'if :test (funcall reference) :then nested
                :else (nelisp-aot-compiler--make-ir 'imm :value 7))))
    (nelisp-eln-emitter--expression-bytecode body 'value)))

(defun nelisp-eln-leaf-code-test--u64 (word)
  (apply #'unibyte-string
         (mapcar (lambda (shift) (logand (ash word (- shift)) 255))
                 '(0 8 16 24 32 40 48 56))))

(defun nelisp-eln-leaf-code-test--branch (opcode displacement)
  (concat (unibyte-string opcode)
          (apply #'unibyte-string
                 (mapcar (lambda (shift)
                           (logand (ash displacement (- shift)) 255))
                         '(0 8 16 24)))))

(defun nelisp-eln-leaf-code-test--jz (displacement)
  "Return a conditional branch with signed DISPLACEMENT."
  (concat (unibyte-string #x0f #x84)
          (apply #'unibyte-string
                 (mapcar (lambda (shift)
                           (logand (ash displacement (- shift)) 255))
                         '(0 8 16 24)))))

(ert-deftest nelisp-eln-leaf-code-accepts-unary-leaves-and-tagged-values ()
  (should (nelisp-eln-leaf-code-valid-p
           (unibyte-string #x48 #x89 #xf8 #xc3)))
  (should (nelisp-eln-leaf-code-valid-p
           (unibyte-string #x31 #xc0 #xc3)))
  (should (nelisp-eln-leaf-code-valid-p
           (concat (unibyte-string #x48 #xb8)
                   (nelisp-eln-leaf-code-test--u64 2)
                   (unibyte-string #xc3))))
  ;; GNU fixnum -1 encoded as a signed machine word.
  (should (nelisp-eln-leaf-code-valid-p
           (concat (unibyte-string #x48 #xb8)
                   (nelisp-eln-leaf-code-test--u64 #xfffffffffffffffe)
                   (unibyte-string #xc3)))))

(ert-deftest nelisp-eln-leaf-code-accepts-nested-emitter-branch-shape ()
  ;; if arg (if arg nil -1) nil, followed by the one final ret.
  (let ((code (concat
               (unibyte-string #x48 #x89 #xf8 #x48 #x85 #xc0)
               (nelisp-eln-leaf-code-test--jz #x1f)
               (unibyte-string #x48 #x85 #xc0)
               (nelisp-eln-leaf-code-test--jz #x07)
               (unibyte-string #x31 #xc0)
               (nelisp-eln-leaf-code-test--branch #xe9 #x0a)
               (unibyte-string #x48 #xb8)
               (nelisp-eln-leaf-code-test--u64 #xfffffffffffffffe)
               (nelisp-eln-leaf-code-test--branch #xe9 #x02)
               (unibyte-string #x31 #xc0 #xc3))))
    (should (nelisp-eln-leaf-code-valid-p code))))

(ert-deftest nelisp-eln-leaf-code-accepts-real-emitter-output ()
  (should (nelisp-eln-leaf-code-valid-p
           (concat (nelisp-eln-leaf-code-test--emitted-conditional)
                   (unibyte-string #xc3)))))

(ert-deftest nelisp-eln-leaf-code-rejects-non-grammar-and-bad-control-flow ()
  (dolist (code
           (list
            nil ""
            (unibyte-string #xff #xd0)       ; call rax
            (unibyte-string #x48 #x8b #x07 #xc3) ; memory load
            (unibyte-string #x48 #xb8 #x01 #x00) ; truncated immediate
            (concat (unibyte-string #x48 #xb8)
                    (nelisp-eln-leaf-code-test--u64 3)
                    (unibyte-string #xc3))   ; invalid tag bits
            (unibyte-string #xc3 #x90)       ; ret is not final
            (nelisp-eln-leaf-code-test--branch #xe9 -5) ; backward
            (concat (nelisp-eln-leaf-code-test--branch #xe9 2)
                    (unibyte-string #x48 #xb8)
                    (nelisp-eln-leaf-code-test--u64 2)
                    (unibyte-string #xc3))   ; target inside immediate
            (concat (nelisp-eln-leaf-code-test--branch #xe9 100)
                    (unibyte-string #xc3))   ; out of range
            (unibyte-string #x0f #x84 0 0 0 0 #xc3) ; jz without test
            (unibyte-string #x48 #x85 #xc0 #xc3) ; test uninitialized RAX
            ;; One incoming edge jumps around the test feeding the common JZ.
            (concat (unibyte-string #x48 #x89 #xf8 #x48 #x85 #xc0)
                    (nelisp-eln-leaf-code-test--jz 8)
                    (unibyte-string #x48 #x85 #xc0)
                    (nelisp-eln-leaf-code-test--branch #xe9 5)
                    (nelisp-eln-leaf-code-test--branch #xe9 0)
                    (nelisp-eln-leaf-code-test--jz 0)
                    (unibyte-string #xc3))))
    (should-not (nelisp-eln-leaf-code-valid-p code))))

(provide 'nelisp-eln-leaf-code-test)

;;; nelisp-eln-leaf-code-test.el ends here
