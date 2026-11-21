;;; nelisp-aot-bytecode-opcodes-test.el --- AOT translator opcode coverage  -*- lexical-binding: t; -*-

;;; Commentary:

;; The build-time AOT translator accepts the numeric comparison opcodes 86-89
;; (>, <, <=, >=) and goto-if-non-nil-else-pop (134).  Each test compiles a
;; function with the host compiler, checks the opcode is really in its code, and
;; requires the translator to produce a DSL defun for it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bytecomp)
(require 'nelisp-aot-bytecode)

(defun nelisp-aot-opcodes-test--compile (form)
  (let ((byte-compile-warnings nil))
    (byte-compile form)))

(defun nelisp-aot-opcodes-test--has-op (fn op)
  "Non-nil when FN's code string decodes to an instruction with opcode OP."
  (let ((code (aref fn 1)) (pc 0) (found nil)
        (insns (nelisp-aot--decode (aref fn 1))))
    (while (< pc (length code))
      (let ((ins (aref insns pc)))
        (when (and ins (= (car ins) op)) (setq found t))
        (setq pc (+ pc (if ins (nth 2 ins) 1)))))
    found))

(defmacro nelisp-aot-opcodes-test--case (name op form)
  `(ert-deftest ,name ()
     (let* ((fn (nelisp-aot-opcodes-test--compile ,form)))
       (should (byte-code-function-p fn))
       (should (nelisp-aot-opcodes-test--has-op fn ,op))
       (let ((nelisp-aot--k 1) (nelisp-aot--direct nil))
         (should (eq (car-safe (nelisp-aot-translate 0 fn)) 'defun))))))

(nelisp-aot-opcodes-test--case nelisp-aot-opcode-gtr 86 '(lambda (a b) (> a b)))
(nelisp-aot-opcodes-test--case nelisp-aot-opcode-lss 87 '(lambda (a b) (< a b)))
(nelisp-aot-opcodes-test--case nelisp-aot-opcode-leq 88 '(lambda (a b) (<= a b)))
(nelisp-aot-opcodes-test--case nelisp-aot-opcode-geq 89 '(lambda (a b) (>= a b)))
(nelisp-aot-opcodes-test--case nelisp-aot-opcode-goto-if-non-nil-else-pop 134
                               '(lambda (a b) (or (car-safe a) (cdr-safe b))))

;; Lane F2 opcodes: stringp (59), nth (56), listN (175) and &rest functions.
(nelisp-aot-opcodes-test--case nelisp-aot-opcode-stringp 59 '(lambda (a) (stringp a)))
(nelisp-aot-opcodes-test--case nelisp-aot-opcode-nth 56 '(lambda (a b) (nth a b)))
(nelisp-aot-opcodes-test--case nelisp-aot-opcode-listn 175
                               '(lambda (a b c d e) (list a b c d e)))

(ert-deftest nelisp-aot-translate-rest-function ()
  ;; A &rest function takes its rest list as one extra argument word.
  (let* ((fn (nelisp-aot-opcodes-test--compile '(lambda (a &rest r) (cons a r))))
         (nelisp-aot--k 1) (nelisp-aot--direct nil)
         (def (nelisp-aot-translate 0 fn)))
    (should (/= (logand (aref fn 0) 128) 0))
    (should (eq (car-safe def) 'defun))
    ;; env + 2 argument words (A and the rest list).
    (should (= (length (nth 2 def)) 3))))

(provide 'nelisp-aot-bytecode-opcodes-test)
;;; nelisp-aot-bytecode-opcodes-test.el ends here
