;;; nelisp-x86-64-decode-test.el --- ERT tests for the x86-64 decoder  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs --batch -Q -L lisp -l test/nelisp-x86-64-decode-test.el \
;;     -f ert-run-tests-batch-and-exit
;; The exhaustive proof against capstone lives in tools/ai/x86-decode-diff/.

;;; Code:

(require 'ert)
(require 'nelisp-x86-64-decode)

(defun nelisp-x86-64-decode-test--decode (address &rest octets)
  "Decode the instruction made of OCTETS as if it were located at ADDRESS."
  (let ((bytes (apply #'unibyte-string octets)))
    (nelisp-x86-64-decode-insn bytes 0 (length bytes) address)))

(ert-deftest nelisp-x86-64-decode-call-rel32 ()
  (let ((insn (nelisp-x86-64-decode-test--decode #x1000 #xE8 #x10 0 0 0)))
    (should (equal (append insn nil)
                   '(5 "call" call imm 1 4 #x1015 #x1000)))))

(ert-deftest nelisp-x86-64-decode-jmp-forms ()
  (let ((short (nelisp-x86-64-decode-test--decode #x2000 #xEB #xFE))
        (near (nelisp-x86-64-decode-test--decode #x2000 #xE9 #xFB #xFF #xFF #xFF)))
    (should (equal (append short nil) '(2 "jmp" jump imm 1 1 #x2000 #x2000)))
    (should (equal (append near nil) '(5 "jmp" jump imm 1 4 #x2000 #x2000)))))

(ert-deftest nelisp-x86-64-decode-jcc-forms ()
  (let ((short (nelisp-x86-64-decode-test--decode #x3000 #x74 #x05))
        (near (nelisp-x86-64-decode-test--decode #x3000 #x0F #x8D #x00 #x01 0 0)))
    (should (equal (append short nil) '(2 "je" jump imm 1 1 #x3007 #x3000)))
    (should (equal (append near nil) '(6 "jge" jump imm 2 4 #x3106 #x3000)))))

(ert-deftest nelisp-x86-64-decode-indirect-branches ()
  ;; call rax, call [rip+0x10], jmp [rax*8+0x1000], jmp [rsp+8]
  (let ((call-reg (nelisp-x86-64-decode-test--decode 0 #xFF #xD0))
        (call-rip (nelisp-x86-64-decode-test--decode 0 #xFF #x15 #x10 0 0 0))
        (jmp-sib (nelisp-x86-64-decode-test--decode 0 #xFF #x24 #xC5 0 #x10 0 0))
        (jmp-disp8 (nelisp-x86-64-decode-test--decode 0 #xFF #x64 #x24 #x08)))
    (should (equal (list (aref call-reg 0) (aref call-reg 2) (aref call-reg 3)
                         (aref call-reg 6))
                   '(2 call indirect nil)))
    (should (equal (list (aref call-rip 0) (aref call-rip 2) (aref call-rip 3))
                   '(6 call indirect)))
    (should (equal (list (aref jmp-sib 0) (aref jmp-sib 1) (aref jmp-sib 2)
                         (aref jmp-sib 3))
                   '(7 "jmp" jump indirect)))
    (should (equal (list (aref jmp-disp8 0) (aref jmp-disp8 2)) '(4 jump)))))

(ert-deftest nelisp-x86-64-decode-non-branches ()
  ;; lea rax,[rip+0x10]; movabs rax,imm64; ret; mov eax,imm32 with 66 -> imm16.
  (let ((lea (nelisp-x86-64-decode-test--decode 0 #x48 #x8D #x05 #x10 0 0 0))
        (movabs (nelisp-x86-64-decode-test--decode 0 #x48 #xB8 1 2 3 4 5 6 7 8))
        (ret (nelisp-x86-64-decode-test--decode 0 #xC3))
        (mov16 (nelisp-x86-64-decode-test--decode 0 #x66 #xB8 1 2)))
    (should (equal (list (aref lea 0) (aref lea 2) (aref lea 3)) '(7 nil nil)))
    (should (equal (list (aref movabs 0) (aref movabs 2)) '(10 nil)))
    (should (equal (list (aref ret 0) (aref ret 2)) '(1 nil)))
    (should (= (aref mov16 0) 4))))

(ert-deftest nelisp-x86-64-decode-range-tiles-exactly ()
  (let* ((bytes (unibyte-string #x55 #x48 #x89 #xE5 #xE8 0 0 0 0 #x5D #xC3))
         (insns (nelisp-x86-64-decode-range bytes 0 (length bytes) #x400000)))
    (should (equal (mapcar (lambda (i) (aref i 0)) insns) '(1 3 5 1 1)))
    (should (= (aref (nth 2 insns) 6) #x400009))
    ;; A range boundary inside an instruction must be refused.
    (should-error (nelisp-x86-64-decode-range bytes 0 6 0)
                  :type 'nelisp-x86-64-decode-error)))

(ert-deftest nelisp-x86-64-decode-unknown-opcode-signals ()
  (should-error (nelisp-x86-64-decode-test--decode 0 #x06)
                :type 'nelisp-x86-64-decode-error)
  (should-error (nelisp-x86-64-decode-test--decode 0 #x0F #xFF #xC0)
                :type 'nelisp-x86-64-decode-error))

(ert-deftest nelisp-x86-64-decode-truncated-signals ()
  (should-error (nelisp-x86-64-decode-test--decode 0 #xE8 1 2)
                :type 'nelisp-x86-64-decode-error)
  (should-error (nelisp-x86-64-decode-test--decode 0 #x48)
                :type 'nelisp-x86-64-decode-error)
  ;; END, not the string length, bounds the read.
  (let ((bytes (unibyte-string #xE8 0 0 0 0)))
    (should-error (nelisp-x86-64-decode-insn bytes 0 4 0)
                  :type 'nelisp-x86-64-decode-error)))

(provide 'nelisp-x86-64-decode-test)
;;; nelisp-x86-64-decode-test.el ends here
