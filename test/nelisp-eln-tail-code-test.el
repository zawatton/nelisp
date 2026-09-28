;;; nelisp-eln-tail-code-test.el --- Tail-import verifier tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-eln-tail-code)

(defun nelisp-eln-tail-code-test--fixture ()
  "Return GNU 31.1's genuine `nelisp-gnu-increment' function bytes."
  (apply #'unibyte-string
         '(#x8d #x47 #xfe #xa8 #x03 #x75 #x29
           #x48 #xba #xff #xff #xff #xff #xff #xff #xff #x1f
           #x48 #x89 #xf8 #x48 #xc1 #xf8 #x02 #x48 #x39 #xd0
           #x74 #x13 #x48 #x8d #x04 #x85 #x06 #x00 #x00 #x00 #xc3
           #x66 #x2e #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00
           #x48 #x8b #x05 #xa1 #x2e #x00 #x00 #x48 #x8b #x00
           #xff #xa0 #xa8 #x28 #x00 #x00)))

(defun nelisp-eln-tail-code-test--analyze (bytes &optional slots)
  (nelisp-eln-tail-code-analyze bytes #x1100 (or slots '(1301))))

(ert-deftest nelisp-eln-tail-code-decodes-eax-immediate-and-truncation ()
  (let ((insn (nelisp-eln-tail-code--decode
               (unibyte-string #x8d #x47 #xfe) 0 3 #x1000)))
    (should (eq (nth 2 insn) 'eax-rdi-add))
    (should (= (nth 3 insn) #xfe)))
  (should-not (catch 'invalid
                (nelisp-eln-tail-code--decode
                 (unibyte-string #x8d #x47) 0 2 #x1000))))

(ert-deftest nelisp-eln-tail-code-zero-shift-does-not-initialize-flags ()
  (should-not
   (nelisp-eln-tail-code-test--analyze
    (unibyte-string #x48 #x89 #xf8       ; mov rax,rdi
                    #x48 #xc1 #xf8 #x00 ; sar rax,0 preserves flags
                    #x74 #x03           ; Both forward paths return RDI.
                    #x48 #x89 #xf8 #x48 #x89 #xf8 #xc3))))

(ert-deftest nelisp-eln-tail-code-verifies-genuine-arithmetic-cfg-and-import ()
  (let ((result (nelisp-eln-tail-code-test--analyze
                 (nelisp-eln-tail-code-test--fixture))))
    (should (eq (plist-get result :safe) t))
    (should (eq (plist-get result :proof) :forward-cfg))
    (should (equal (plist-get result :imports)
                   '((:slot 1301 :got-vaddr 16344)))))
  ;; The exact bignum threshold immediate and fixnum retag bias are
  ;; deliberately NOT authenticated here: those are per-descriptor
  ;; constants, checked instead by
  ;; `nelisp-eln-native-subr--tail-constants-match-p' against the
  ;; specific descriptor a caller has already selected.  This verifier
  ;; only proves the surrounding instruction/control-flow shape, so a
  ;; different (but still self-consistent) choice of those two operands
  ;; alone must still parse as the same safe shape.
  (let ((variant (copy-sequence (nelisp-eln-tail-code-test--fixture))))
    (aset variant 10 #xfe)      ; Different bignum threshold immediate.
    (aset variant 33 #x0a)      ; Different valid fixnum bias (mod 4 = 2).
    (should (nelisp-eln-tail-code-test--analyze variant)))
  ;; Corpus crash gate fix: the tag-test displacement and the tag mask
  ;; ARE universal ABI constants (the fixnum tag width and its bit
  ;; pattern never vary by descriptor), and used to be decoded but
  ;; never compared -- a single flipped byte in either field still
  ;; admitted genuine gnu-increment/gnu-decrement.  Each must now be
  ;; rejected on its own, not just in combination.
  (let ((variant (copy-sequence (nelisp-eln-tail-code-test--fixture))))
    (aset variant 2 #xfd)       ; Different argument tag arithmetic.
    (should-not (nelisp-eln-tail-code-test--analyze variant)))
  (let ((variant (copy-sequence (nelisp-eln-tail-code-test--fixture))))
    (aset variant 4 #x07)       ; Different low-tag test mask.
    (should-not (nelisp-eln-tail-code-test--analyze variant))))

(ert-deftest nelisp-eln-tail-code-rejects-wrong-untag-shift-amount ()
  "Corpus crash gate fix: the fixnum untag shift is always exactly 2."
  (let ((variant (copy-sequence (nelisp-eln-tail-code-test--fixture))))
    (aset variant 23 #x03)      ; sar $3 instead of sar $2.
    (should-not (nelisp-eln-tail-code-test--analyze variant))))

(ert-deftest nelisp-eln-tail-code-allows-only-nil-return-without-import ()
  (should (equal (nelisp-eln-tail-code-test--analyze
                  (unibyte-string #x31 #xc0 #xc3))
                 '(:safe t :imports nil :proof :forward-cfg)))
  (should-not (nelisp-eln-tail-code-test--analyze (unibyte-string #xc3))))

(ert-deftest nelisp-eln-tail-code-rejects-malformed-control-flow-and-memory ()
  (let* ((fixture (nelisp-eln-tail-code-test--fixture))
         (bad-target (copy-sequence fixture))
         (bad-return (copy-sequence fixture))
         (bad-memory (copy-sequence fixture))
         (bad-import (copy-sequence fixture))
         (bad-bypass (copy-sequence fixture))
         (bad-unreachable (copy-sequence fixture)))
    (aset bad-target 6 #x2a)     ; Branch enters the import's displacement.
    (aset bad-return 32 #x04)    ; Scalar address is not a returnable value.
    (aset bad-memory 50 #x07)    ; Arbitrary [rdi] read.
    (aset bad-import 60 #xb0)    ; Slot 1302, not authorized.
    (aset bad-bypass 6 #x1e)     ; Branch reaches the ret without tagging RAX.
    (aset bad-unreachable 39 #x90) ; Full stream decode includes dead padding.
    (should-not (nelisp-eln-tail-code-test--analyze bad-target))
    (should-not (nelisp-eln-tail-code-test--analyze bad-return))
    (should-not (nelisp-eln-tail-code-test--analyze bad-memory))
    (should-not (nelisp-eln-tail-code-test--analyze bad-import))
    (should-not (nelisp-eln-tail-code-test--analyze bad-bypass))
    (should-not (nelisp-eln-tail-code-test--analyze bad-unreachable))
    (should-not (nelisp-eln-tail-code-test--analyze (substring fixture 0 63)))
    (should-not (nelisp-eln-tail-code-test--analyze fixture '(1300)))))

(ert-deftest nelisp-eln-tail-code-rejects-unproven-table-and-machine-ops ()
  (should-not (nelisp-eln-tail-code-analyze
               (unibyte-string #x48 #x8b #x00 #xff #xa0 #x08 #x00 #x00 #x00)
               #x1000 '(1)))
  (dolist (bytes (list (unibyte-string #x48 #x89 #xc7 #xc3) ; writes RDI
                       (unibyte-string #xe8 #x00 #x00 #x00 #x00 #xc3) ; call
                       (unibyte-string #x48 #x83 #xc4 #x08 #xc3))) ; stack
    (should-not (nelisp-eln-tail-code-test--analyze bytes))))

(provide 'nelisp-eln-tail-code-test)

;;; nelisp-eln-tail-code-test.el ends here
