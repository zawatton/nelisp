;;; nelisp-eln-many-import-test.el --- GNU MANY stack-call admission tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Admission tests for the non-tail, stack-array GNU MANY calling
;; convention that genuine vendor `zerop' uses to call `Feqlsign' (the C
;; implementation backing `=').  Companion to
;; `test/nelisp-eln-tail-import-test.el', which covers the tail-JMP
;; convention (1+/1-).  These tests read the real, byte-identical
;; `gnu-zerop.eln' fixture surveyed under
;; ~/.cache/tmp/s6-survey-lex/zerop/ and run entirely against it and its
;; tampered copies; nothing here depends on a running native process.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)

(defconst nelisp-eln-many-import-test--fixture
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/zerop/overlay/eln/31.1-ba35c031/gnu-zerop.eln")
  "The genuine, byte-identical GNU `zerop' artifact surveyed for stage S6.
sha256 bfbc0afbaec2321651f65a08ced1a66303af9c4f0746767681c22d3d4fafeb12,
authenticated against ~/.cache/tmp/slot-auth/freloc-ba35c031.tsv (sha256
3e8591ab81130c91375221fb3efeaa377c2cd90ccf45fcd58adb3f31c55f0758): its
only indirect call is through freloc offset 0x2940, row 1320, which
resolves to `Feqlsign in section .text of /usr/local/bin/emacs-31.1'.")

(defun nelisp-eln-many-import-test--artifact-code (path symbol-fragment)
  "Read the function bytes named by SYMBOL-FRAGMENT from ELF PATH.
Returns (CODE VADDR ELF); mirrors
`nelisp-eln-tail-import-test--artifact-code' for the MANY fixture."
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

(defun nelisp-eln-many-import-test--analyze-artifact (code vaddr abi)
  "Run authenticated MANY stack-call analysis over CODE at VADDR for ABI."
  (cl-letf (((symbol-function 'nelisp-eln-system-loader--state)
             (lambda (_) '(:bias 0)))
            ((symbol-function 'nelisp-eln-system-loader-validate-root-indirection)
             (lambda (&rest _) 12345)))
    (nelisp-eln-native-subr--many-import-analysis
     'handle (list nil nil nil vaddr) code abi)))

(ert-deftest nelisp-eln-many-import-admits-genuine-zerop-artifact ()
  (skip-unless (file-exists-p nelisp-eln-many-import-test--fixture))
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-many-import-test--artifact-code
                 nelisp-eln-many-import-test--fixture "zerop_0"))
               (analysis (nelisp-eln-many-import-test--analyze-artifact
                          code value "ba35c031")))
    (should (equal (length code) 46))
    (should (equal (plist-get (car (plist-get analysis :imports)) :slot) 1320))
    (should (equal (plist-get (car (plist-get analysis :imports)) :got-vaddr)
                    16344))
    (should (equal (nth 2 (plist-get analysis :descriptor)) '=))
    (should (equal (nth 3 (plist-get analysis :descriptor)) 2))
    ;; The straight-line CFG verifier alone already proves this shape,
    ;; independent of the descriptor lookup and root-indirection checks
    ;; layered on top of it in `--many-import-analysis'.
    (should (eq (plist-get (nelisp-eln-tail-code-analyze-stack-call
                            code value '(1320) 2) :safe) t))))

(ert-deftest nelisp-eln-many-import-rejects-unauthenticated-slot ()
  "Swapping the call's freloc slot to an index outside the MANY table fails."
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-many-import-test--artifact-code
                 nelisp-eln-many-import-test--fixture "zerop_0"))
               (tampered (copy-sequence code)))
    (skip-unless (file-exists-p nelisp-eln-many-import-test--fixture))
    ;; The call's disp32 (offset 0x2940 little-endian) sits at bytes
    ;; 37..40; retarget it to slot 1301 (a genuine, but unary tail-only,
    ;; slot never listed in `nelisp-eln-native-subr--many-descriptors').
    (should (equal (substring tampered 37 41) (unibyte-string #x40 #x29 0 0)))
    (aset tampered 37 (logand (* 1301 8) #xff))
    (aset tampered 38 (ash (logand (* 1301 8) #xff00) -8))
    ;; The CFG verifier alone still accepts this shape once 1301*8's slot is
    ;; explicitly allowed, proving the rejection below comes from the
    ;; MANY descriptor table, not from malformed bytes.
    (should (eq (plist-get (nelisp-eln-tail-code-analyze-stack-call
                            tampered value (list 1301) 2) :safe) t))
    (should-not (nelisp-eln-many-import-test--analyze-artifact
                 tampered value "ba35c031"))))

(ert-deftest nelisp-eln-many-import-rejects-injected-branch ()
  "Any injected branch opcode breaks the loop-free straight-line grammar."
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-many-import-test--artifact-code
                 nelisp-eln-many-import-test--fixture "zerop_0")))
    (skip-unless (file-exists-p nelisp-eln-many-import-test--fixture))
    (dolist (patch
             ;; Overwrite two consecutive body bytes with a short forward
             ;; jz/jmp encoding at a few different offsets; each corrupts
             ;; the fixed instruction sequence the grammar requires.
             '((11 . (#x74 #x02)) (18 . (#x75 #x04)) (32 . (#xeb #x02))))
      (let ((tampered (copy-sequence code)))
        (aset tampered (car patch) (nth 0 (cdr patch)))
        (aset tampered (1+ (car patch)) (nth 1 (cdr patch)))
        (should-not (nelisp-eln-tail-code-analyze-stack-call
                     tampered value '(1320) 2))
        (should-not (nelisp-eln-many-import-test--analyze-artifact
                     tampered value "ba35c031"))))))

(ert-deftest nelisp-eln-many-import-rejects-a-second-call ()
  "A second CALL through the table, even to the same slot, is rejected."
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-many-import-test--artifact-code
                 nelisp-eln-many-import-test--fixture "zerop_0")))
    (skip-unless (file-exists-p nelisp-eln-many-import-test--fixture))
    ;; Splice a duplicate `call *0x2940(%rax)' right after the first one,
    ;; before the epilogue; the grammar has no state that tolerates a
    ;; second call, so this must fail closed rather than silently admit
    ;; two indirect calls as one.
    (let* ((call-end 41)
           (duplicated (concat (substring code 0 call-end)
                                (substring code 35 call-end)
                                (substring code call-end))))
      (should-not (nelisp-eln-tail-code-analyze-stack-call
                   duplicated value '(1320) 2))
      (should-not (nelisp-eln-many-import-test--analyze-artifact
                   duplicated value "ba35c031")))))

(ert-deftest nelisp-eln-many-import-rejects-unknown-abi ()
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-many-import-test--artifact-code
                 nelisp-eln-many-import-test--fixture "zerop_0")))
    (skip-unless (file-exists-p nelisp-eln-many-import-test--fixture))
    (should-not (nelisp-eln-many-import-test--analyze-artifact
                 code value "unknown-abi"))))

(provide 'nelisp-eln-many-import-test)

;;; nelisp-eln-many-import-test.el ends here
