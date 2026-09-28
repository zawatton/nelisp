;;; nelisp-eln-cxr-import-test.el --- S6 caar/cadr admission tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Admission tests for the genuine, byte-identical `caar'/`cadr'
;; artifacts surveyed under ~/.cache/tmp/s6-survey-lex/{caar,cadr}/:
;; two cons-tag-guarded loads (the outer argument's car or cdr, then
;; the result's car) with a shared error path that loads the `listp'
;; predicate symbol from the artifact's own `d_reloc' data-relocation
;; table and makes one non-tail authenticated CALL to freloc slot 0
;; (`wrong_type_argument').  These tests read the real fixtures and run
;; entirely against them and tampered copies; nothing here depends on
;; a running native process (`nelisp-eln-metadata-read-with-backend'
;; and `nelisp-eln-system-loader-validate-root-indirection' are
;; mocked, exactly like the sibling MANY/chain import tests mock them,
;; since a real `nl-ffi' dlopen needs the standalone binary, not host
;; Emacs).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)

(defconst nelisp-eln-cxr-import-test--fixtures
  '(("caar" -3 "F63616172_caar_0")
    ("cadr" 5 "F63616472_cadr_0"))
  "(OP FIRST-DISP MANGLED-NAME) for each surveyed S6 fixture.")

(defun nelisp-eln-cxr-import-test--path (op)
  (expand-file-name
   (format "~/.cache/tmp/s6-survey-lex/%s/overlay/eln/31.1-ba35c031/gnu-%s.eln"
           op op)))

(defun nelisp-eln-cxr-import-test--artifact-code (path symbol-fragment)
  "Read the function bytes named by SYMBOL-FRAGMENT from ELF PATH."
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

(defun nelisp-eln-cxr-import-test--analyze-artifact
    (code vaddr abi first-disp d-reloc-slot &optional relocations)
  "Run authenticated cxr analysis over CODE at VADDR for ABI.
RELOCATIONS, when given, stands in for the artifact's own decoded
`:data-relocations' vector (default: a 6-element vector whose slot 5
is `listp', matching the genuine caar/cadr fixtures)."
  (cl-letf (((symbol-function 'nelisp-eln-system-loader--state)
             (lambda (_) '(:bias 0)))
            ((symbol-function 'nelisp-eln-system-loader-validate-root-indirection)
             (lambda (&rest _) 12345))
            ((symbol-function 'nelisp-eln-metadata-read-with-backend)
             (lambda (&rest _)
               (list :data-relocations
                     (or relocations [nil nil nil nil nil listp nil])))))
    (nelisp-eln-native-subr--cxr-import-analysis
     'handle (list nil nil nil vaddr) code abi first-disp d-reloc-slot)))

(ert-deftest nelisp-eln-cxr-import-admits-genuine-caar-and-cadr-artifacts ()
  (dolist (fixture nelisp-eln-cxr-import-test--fixtures)
    (pcase-let* ((`(,op ,first-disp ,name) fixture)
                 (path (nelisp-eln-cxr-import-test--path op)))
      (skip-unless (file-exists-p path))
      (pcase-let* ((`(,code ,value ,_)
                    (nelisp-eln-cxr-import-test--artifact-code path name))
                   (analysis (nelisp-eln-cxr-import-test--analyze-artifact
                              code value "ba35c031" first-disp 5)))
        (should (equal (length code) 75))
        (should (equal (plist-get (car (plist-get analysis :imports)) :slot) 0))
        (should (equal (plist-get (plist-get analysis :data-relocation) :slot) 5))
        (should (equal (plist-get (plist-get analysis :descriptor) :symbol)
                       "wrong_type_argument"))
        ;; The pure CFG/data-relocation shape alone already proves this,
        ;; independent of the freloc-descriptor and root-indirection
        ;; checks layered on top of it in `--cxr-import-analysis'.
        (should (eq (plist-get (nelisp-eln-tail-code-analyze-cxr-call
                                code value first-disp 5 '(0))
                               :safe)
                    t))))))

(ert-deftest nelisp-eln-cxr-import-rejects-wrong-d-reloc-identity ()
  "A `d_reloc' slot whose live decoded value is not `listp' is refused."
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-cxr-import-test--artifact-code
                 (nelisp-eln-cxr-import-test--path "caar") "F63616172_caar_0")))
    (skip-unless (file-exists-p (nelisp-eln-cxr-import-test--path "caar")))
    ;; The CFG shape alone still holds; only the semantic slot-identity
    ;; cross-check in `--cxr-import-analysis' must reject this.
    (should (eq (plist-get (nelisp-eln-tail-code-analyze-cxr-call
                            code value -3 5 '(0))
                           :safe)
                t))
    (should-not (nelisp-eln-cxr-import-test--analyze-artifact
                 code value "ba35c031" -3 5
                 [nil nil nil nil nil consp nil]))))

(ert-deftest nelisp-eln-cxr-import-rejects-unauthenticated-freloc-slot ()
  "Retargeting the error call's freloc slot away from slot 0 fails closed.
`call *(%rax)' (\"ff 10\", ModRM mod=00/rm=000, no displacement) is the
zero-displacement encoding slot 0 alone gets; splicing in an explicit
disp8 form (\"ff 50 08\", slot 1) still reaches the same table
dereference, but no longer matches this grammar's fixed zero-slot
encoding at all -- checked with slot 1 explicitly allowed too, so the
rejection below is the grammar mismatch, not an unrelated byte-count
error from widening the instruction by one byte."
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-cxr-import-test--artifact-code
                 (nelisp-eln-cxr-import-test--path "caar") "F63616172_caar_0"))
               (call-start 66)
               (retargeted
                (concat (substring code 0 call-start)
                        (unibyte-string #xff #x50 #x08)
                        (substring code (+ call-start 2)))))
    (skip-unless (file-exists-p (nelisp-eln-cxr-import-test--path "caar")))
    (should-not (nelisp-eln-tail-code-analyze-cxr-call retargeted value -3 5 '(0 1)))
    (should-not (nelisp-eln-cxr-import-test--analyze-artifact
                 retargeted value "ba35c031" -3 5))))

(ert-deftest nelisp-eln-cxr-import-rejects-wrong-first-displacement ()
  "cadr's shape does not admit as caar's (-3), and vice versa."
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-cxr-import-test--artifact-code
                 (nelisp-eln-cxr-import-test--path "caar") "F63616172_caar_0")))
    (skip-unless (file-exists-p (nelisp-eln-cxr-import-test--path "caar")))
    (should-not (nelisp-eln-tail-code-analyze-cxr-call code value 5 5 '(0)))))

(ert-deftest nelisp-eln-cxr-import-rejects-injected-branch ()
  "Any injected branch opcode breaks the loop-free straight-line grammar."
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-cxr-import-test--artifact-code
                 (nelisp-eln-cxr-import-test--path "caar") "F63616172_caar_0")))
    (skip-unless (file-exists-p (nelisp-eln-cxr-import-test--path "caar")))
    (let ((tampered (copy-sequence code)))
      ;; Offset 63 is `load-table' ("48 8b 00", an exact-match
      ;; instruction with no variable field); corrupting its first byte
      ;; cannot coincidentally still match some other legal instruction
      ;; in this grammar's small fixed vocabulary.
      (aset tampered 63 #xeb)
      (aset tampered 64 #x02)
      (should-not (nelisp-eln-tail-code-analyze-cxr-call tampered value -3 5 '(0))))))

(ert-deftest nelisp-eln-cxr-import-rejects-unknown-abi ()
  (pcase-let* ((`(,code ,value ,_)
                (nelisp-eln-cxr-import-test--artifact-code
                 (nelisp-eln-cxr-import-test--path "caar") "F63616172_caar_0")))
    (skip-unless (file-exists-p (nelisp-eln-cxr-import-test--path "caar")))
    (should-not (nelisp-eln-cxr-import-test--analyze-artifact
                 code value "unknown-abi" -3 5))))

(ert-deftest nelisp-eln-cxr-import-logic-matches-genuine-values ()
  "The authenticated `wrong_type_argument' implementation signals exactly
GNU semantics, and the ordinary car/cdr logic this shape implements
(never reached through the error call for these inputs) matches
`(caar '((1) 2)) => 1' / `(cadr '(1 2)) => 2' / the nil cases -- checked
directly against the real, unauthenticated Lisp forms, since evaluating
them needs no native call at all."
  (should (equal (caar '((1) 2)) 1))
  (should (equal (cadr '(1 2)) 2))
  (should (equal (caar nil) nil))
  (should (equal (cadr nil) nil))
  (should (equal (condition-case err (caar 5) (error err))
                 '(wrong-type-argument listp 5)))
  (should (equal (condition-case err
                     (funcall #'nelisp-eln-runtime-services-wrong-type-argument
                              'listp 5)
                   (error err))
                 '(wrong-type-argument listp 5))))

(provide 'nelisp-eln-cxr-import-test)

;;; nelisp-eln-cxr-import-test.el ends here
