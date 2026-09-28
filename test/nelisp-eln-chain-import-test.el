;;; nelisp-eln-chain-import-test.el --- S4.6 chain-call admission tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Admission tests for the ledger S4.6 two-argument chain shape --
;; `(defun nelisp-gnu-chain (g x) (funcall g (1+ x)))' -- whose only
;; indirect calls are a non-tail authenticated CALL to the existing
;; Fadd1 tail-descriptor slot (the inline fixnum fast path's slow-path
;; fallback) and a further non-tail authenticated CALL to the new
;; `Ffuncall' MANY-convention descriptor slot.  Companion to
;; `test/nelisp-eln-tail-import-test.el' (tail-JMP, 1+/1-) and
;; `test/nelisp-eln-many-import-test.el' (MANY stack-call, `zerop').
;; These tests read the real, byte-identical `gnu-chain.eln' fixture
;; prepared under ~/.cache/tmp/s46-chain-artifact/ and run entirely
;; against it and its tampered copies and the genuine dynamic-binding
;; negative control; nothing here depends on a running native process.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)

(defconst nelisp-eln-chain-import-test--fixture
  (expand-file-name
   "~/.cache/tmp/s46-chain-artifact/overlay/eln/31.1-ba35c031/gnu-chain.eln")
  "The genuine, byte-identical GNU chain artifact prepared for S4.6.
sha256 f06b6249a26dea523833148e41c091c4142ea23a311345865f6104692bfb8c17,
authenticated against ~/.cache/tmp/slot-auth/freloc-ba35c031.tsv (sha256
3e8591ab81130c91375221fb3efeaa377c2cd90ccf45fcd58adb3f31c55f0758): its
two indirect calls are through freloc offsets 0x28a8 (row 1301, Fadd1)
and 0x1d88 (row 945, `Ffuncall').")

(defconst nelisp-eln-chain-import-test--dynamic-fixture
  (expand-file-name
   "~/.cache/tmp/s46-chain-artifact/overlay/dynamic-NEGATIVE-CONTROL/eln/31.1-ba35c031/gnu-chain-dynamic.eln")
  "Dynamic-binding negative control for the same source form.
Not admissible: its dynamic-binding prologue/epilogue does not match
the lexical-binding artifact's exact instruction sequence at all, so
`nelisp-eln-tail-code-analyze-chain-call' must reject it by simply
failing to parse, with no dynamic-binding-specific logic required.")

(defun nelisp-eln-chain-import-test--artifact-code (path symbol-fragment)
  "Read the function bytes named by SYMBOL-FRAGMENT from ELF PATH.
Returns (CODE VADDR ELF); mirrors
`nelisp-eln-many-import-test--artifact-code' for the chain fixture."
  (let* ((bytes (nelisp-eln-system-loader--read-file path))
         (elf (nelisp-eln-system-loader--file-symbols path bytes))
         (entry (catch 'found
                  (maphash (lambda (name value)
                             (when (string-match-p symbol-fragment name)
                               (throw 'found value)))
                           (plist-get elf :symbols))
                  nil))
         (value (plist-get entry :value))
         (load (catch 'found
                 (dolist (row (plist-get elf :loads))
                   (when (and (/= 0 (logand (nth 4 row) 1))
                              (>= value (nth 0 row))
                              (< (- value (nth 0 row)) (nth 2 row)))
                     (throw 'found row)))))
         (offset (+ (nth 1 load) (- value (nth 0 load))))
         (code (substring bytes offset (+ offset (plist-get entry :size)))))
    (list code value elf)))

(defun nelisp-eln-chain-import-test--analyze-artifact (code vaddr abi)
  "Run authenticated chain-call analysis over CODE at VADDR for ABI."
  (cl-letf (((symbol-function 'nelisp-eln-system-loader--state)
             (lambda (_) '(:bias 0)))
            ((symbol-function 'nelisp-eln-system-loader-validate-root-indirection)
             (lambda (&rest _) 12345)))
    (nelisp-eln-native-subr-chain-import-analysis
     'handle (list nil nil nil vaddr) code abi)))

(ert-deftest nelisp-eln-chain-import-admits-genuine-chain-artifact ()
  (skip-unless (file-exists-p nelisp-eln-chain-import-test--fixture))
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-chain-import-test--artifact-code
                 nelisp-eln-chain-import-test--fixture "nelisp_gnu_chain_0"))
               (analysis (nelisp-eln-chain-import-test--analyze-artifact
                          code value "ba35c031")))
    (should (equal (length code) 100))
    (should (equal (mapcar (lambda (i) (plist-get i :slot))
                           (plist-get analysis :imports))
                   '(1301 945)))
    (should (equal (nth 2 (plist-get analysis :unary-descriptor)) '1+))
    (should (equal (nth 2 (plist-get analysis :many-descriptor)) 'funcall))
    (should (equal (nth 3 (plist-get analysis :many-descriptor)) 2))
    ;; The chain-call CFG verifier alone already proves this shape,
    ;; independent of the descriptor lookups and root-indirection checks
    ;; layered on top of it in `--chain-import-analysis'.
    (should (eq (plist-get (nelisp-eln-tail-code-analyze-chain-call
                            code value '(1301) #x1fffffffffffffff 6 '(945) 2)
                           :safe)
                t))))

(ert-deftest nelisp-eln-chain-import-rejects-dynamic-binding-artifact ()
  "The genuine dynamic-binding variant of the same source is not admissible."
  (skip-unless (file-exists-p nelisp-eln-chain-import-test--dynamic-fixture))
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-chain-import-test--artifact-code
                 nelisp-eln-chain-import-test--dynamic-fixture
                 "nelisp_gnu_chain_0")))
    (should-not (equal (length code) 100))
    (should-not (nelisp-eln-tail-code-analyze-chain-call
                 code value '(1301) #x1fffffffffffffff 6 '(945) 2))
    (should-not (nelisp-eln-chain-import-test--analyze-artifact
                 code value "ba35c031"))))

(ert-deftest nelisp-eln-chain-import-rejects-unauthenticated-many-slot ()
  "Swapping the Ffuncall call's freloc slot to an unlisted index fails."
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-chain-import-test--artifact-code
                 nelisp-eln-chain-import-test--fixture "nelisp_gnu_chain_0"))
               (tampered (copy-sequence code)))
    (skip-unless (file-exists-p nelisp-eln-chain-import-test--fixture))
    ;; The Ffuncall call's disp32 (offset 0x1d88 little-endian) sits at
    ;; bytes 89..92 (call-slot-rbp at offset 0x57, disp32 at +2).
    (should (equal (substring tampered 89 93) (unibyte-string #x88 #x1d 0 0)))
    (let ((bogus (* 999 8)))
      (aset tampered 89 (logand bogus #xff))
      (aset tampered 90 (logand (ash bogus -8) #xff)))
    (should-not (nelisp-eln-tail-code-analyze-chain-call
                 tampered value '(1301) #x1fffffffffffffff 6 '(945) 2))
    ;; With the tampered slot explicitly allowed, the CFG shape alone
    ;; still holds, proving the rejection above comes from the slot
    ;; allow-list, not from malformed bytes.
    (should (eq (plist-get (nelisp-eln-tail-code-analyze-chain-call
                            tampered value '(1301) #x1fffffffffffffff 6
                            '(999) 2)
                           :safe)
                t))
    (should-not (nelisp-eln-chain-import-test--analyze-artifact
                 tampered value "ba35c031"))))

(ert-deftest nelisp-eln-chain-import-rejects-injected-branch ()
  "Any injected branch opcode breaks the loop-free straight-line grammar."
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-chain-import-test--artifact-code
                 nelisp-eln-chain-import-test--fixture "nelisp_gnu_chain_0")))
    (skip-unless (file-exists-p nelisp-eln-chain-import-test--fixture))
    ;; Overwrite two bytes inside the array-build tail (offsets chosen
    ;; to land inside a fixed-length instruction, away from any operand
    ;; this grammar already treats as a branch) with a short forward
    ;; jmp encoding.
    (let ((tampered (copy-sequence code)))
      (aset tampered 70 #xeb)
      (aset tampered 71 #x02)
      (should-not (nelisp-eln-tail-code-analyze-chain-call
                   tampered value '(1301) #x1fffffffffffffff 6 '(945) 2))
      (should-not (nelisp-eln-chain-import-test--analyze-artifact
                   tampered value "ba35c031")))))

(ert-deftest nelisp-eln-chain-import-rejects-unknown-abi ()
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-chain-import-test--artifact-code
                 nelisp-eln-chain-import-test--fixture "nelisp_gnu_chain_0")))
    (skip-unless (file-exists-p nelisp-eln-chain-import-test--fixture))
    (should-not (nelisp-eln-chain-import-test--analyze-artifact
                 code value "unknown-abi"))))

(provide 'nelisp-eln-chain-import-test)

;;; nelisp-eln-chain-import-test.el ends here
