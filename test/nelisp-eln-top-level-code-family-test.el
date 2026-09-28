;;; nelisp-eln-top-level-code-family-test.el --- S7.7.4 classifier reorder -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Companion coverage for the S7.7.4 corpus-gate fix: `nelisp-eln-registration
;; --top-level-code' now tries every GNU-family template (each a pure byte
;; comparison already resident in `nelisp-eln-registration.el') before ever
;; requiring the emitter's own template for the self-emitter case, and that
;; require now pulls in only the lightweight `nelisp-eln-emitter-templates'
;; feature, never the full `nelisp-eln-emitter' (and, through it,
;; `nelisp-aot-compiler' / `nelisp-elf-write'). Admission strictness is
;; unchanged by the reorder: every artifact still has to match exactly one
;; authenticated template, and an artifact matching none is still rejected
;; the same way as before.
;;
;; `nelisp-eln-registration-gnu-top-level-test.el' already proves this for
;; three of the four GNU profiles (`gnu-single-leaf', `gnu-eval-subr',
;; `gnu-eval-subr-pair') and their mutated/rejected counterparts. This file
;; adds the two profiles that file does not cover, so all five authenticated
;; template families (self-emitter plus all four GNU profiles) have a direct
;; `--top-level-code' admission test:
;;
;;   - `self-emitter': built from `nelisp-eln-emitter--top-level-code' --
;;     the exact function `--top-level-code' itself falls back to for a
;;     genuine self-emitted fixture -- so this needs no hand transcription.
;;   - `gnu-verified-subr': the genuine 69-byte top_level_run transcribed
;;     byte-for-byte (never hand-written) from the real, on-disk
;;     gnu-cconv--set-diff.eln artifact that S6.8 already exercises
;;     end-to-end (~/.cache/tmp/s6-survey-lex/cconv--set-diff/overlay/eln/
;;     31.1-ba35c031/gnu-cconv--set-diff.eln), read via the same
;;     `nelisp-eln-system-loader-function-capability' /
;;     `-read-root-function-bytes' pair `--top-level-code' itself uses.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-emitter-templates)

(defun nelisp-eln-top-level-code-family-test--patch-disp32 (bytes offset value)
  "Return a copy of BYTES with the little-endian u32 at OFFSET set to VALUE."
  (let ((copy (copy-sequence bytes)) (v (logand value #xffffffff)))
    (dotimes (i 4)
      (aset copy (+ offset i) (logand (ash v (* -8 i)) #xff)))
    copy))

;;; `self-emitter' -- built from the real template generator, not by hand.

(defun nelisp-eln-top-level-code-family-test--self-emitter-code (arity)
  "Return a genuine self-emitter ARITY top_level_run with resolved relocs.
The four relocation holes (`nelisp-eln-emitter--top-level-code' leaves them
zero; a real `ld -shared' link is what patches them in a genuine .eln) are
patched here to resolve, through `--patch-disp32', to the fixed BASE-
relative addresses that `nelisp-eln-top-level-code-family-test--self-emitter-run'
mocks `nelisp-eln-system-loader-symbol-info' to report -- mirroring what
the real linker guarantees: each occurrence's own displacement, added to
its own call-site address, reaches the same absolute symbol address."
  (let* ((base #x2000)
         (d-reloc-eph-addr #x5000)
         (d-reloc-addr #x6000)
         (freloc-addr #x7000)
         (bytes (car (nelisp-eln-emitter--top-level-code arity))))
    (setq bytes (nelisp-eln-top-level-code-family-test--patch-disp32
                 bytes 7 (- d-reloc-eph-addr (+ base 7 4))))
    (setq bytes (nelisp-eln-top-level-code-family-test--patch-disp32
                 bytes 32 (- d-reloc-addr (+ base 32 4))))
    (setq bytes (nelisp-eln-top-level-code-family-test--patch-disp32
                 bytes 42 (- d-reloc-eph-addr (+ base 42 4))))
    (setq bytes (nelisp-eln-top-level-code-family-test--patch-disp32
                 bytes 61 (- freloc-addr (+ base 61 4))))
    bytes))

(defun nelisp-eln-top-level-code-family-test--self-emitter-run (bytes)
  "Admit self-emitter BYTES, mocking the loader and its symbol table."
  (let* ((base #x2000)
         (cap (list nil 'handle "top_level_run" base nil 1
                    (length bytes) 'capability-token))
         (addr (list (cons "d_reloc_eph" #x5000) (cons "d_reloc" #x6000)
                     (cons "freloc_link_table" #x7000))))
    (cl-letf (((symbol-function 'nelisp-eln-system-loader-function-capability)
               (lambda (_handle _name) cap))
              ((symbol-function 'nelisp-eln-system-loader-read-root-function-bytes)
               (lambda (_handle _name _offset _size) bytes))
              ((symbol-function 'nelisp-eln-system-loader-symbol-info)
               (lambda (_handle name) (list :address (cdr (assoc name addr))))))
      (nelisp-eln-registration--top-level-code 'handle))))

(ert-deftest nelisp-eln-top-level-code-admits-self-emitted-fixture ()
  (dolist (arity '(0 1))
    (let* ((bytes (nelisp-eln-top-level-code-family-test--self-emitter-code arity))
           (result (nelisp-eln-top-level-code-family-test--self-emitter-run bytes)))
      (should (eq (nth 2 result) 'self-emitter))
      (should (= (nth 1 result) arity)))))

(ert-deftest nelisp-eln-top-level-code-rejects-tampered-self-emitted-fixture ()
  (let* ((bytes (nelisp-eln-top-level-code-family-test--self-emitter-code 1))
         ;; Byte 0 (`push rbx') is fixed in every profile's template and in
         ;; no relocation hole of any of them: tampering it cannot land
         ;; inside a hole GNU checks skip either, so this exercises the
         ;; same final "matches nothing" path a genuinely corrupt artifact
         ;; would.
         (tampered (copy-sequence bytes)))
    (aset tampered 0 #x90)
    (should-error
     (nelisp-eln-top-level-code-family-test--self-emitter-run tampered)
     :type 'nelisp-eln-registration-error)))

;;; `gnu-verified-subr' -- transcribed from a genuine on-disk GNU artifact.

(defun nelisp-eln-top-level-code-family-test--verified-subr-code ()
  "Return the genuine gnu-cconv--set-diff.eln top_level_run (69 bytes).
`gnu-verified-subr' is the profile whose `subr-type' argument is loaded
from a nonzero d_reloc slot (S6.8; see
`nelisp-eln-registration--gnu-verified-subr-top-level'); this is that real
artifact's own top_level_run, transcribed byte-for-byte via the same
`nelisp-eln-system-loader-function-capability' /
`-read-root-function-bytes' pair `--top-level-code' itself uses, never
hand-written."
  (unibyte-string
   #x48 #x83 #xec #x10 #x48 #x8b #x05 #x85 #x2d #x00 #x00 #x48
   #x89 #xfa #x48 #x8b #x0d #x73 #x2d #x00 #x00 #x4c #x8b #x48
   #x18 #x48 #x8b #x70 #x10 #x48 #x8b #x78 #x08 #x48 #x8b #x05
   #x70 #x2d #x00 #x00 #x4c #x8b #x41 #x08 #xb9 #x0a #x00 #x00
   #x00 #x48 #x8b #x00 #x52 #xba #x0a #x00 #x00 #x00 #xff #x90
   #x30 #x20 #x00 #x00 #x48 #x83 #xc4 #x18 #xc3))

(defun nelisp-eln-top-level-code-family-test--verified-subr-run (bytes)
  "Admit `gnu-verified-subr' BYTES, mocking the loader and its indirection.
BASE is 0 and the expected slot addresses below are computed straight from
the genuine artifact's own RIP-relative displacement fields at offsets 7,
17 and 36 (BASE + OFFSET + 4 + the four-byte little-endian displacement
actually stored there), exactly as
`nelisp-eln-registration--validate-rip-relocs' itself computes them --
never independently guessed."
  (let ((cap (list nil 'handle "top_level_run" 0 nil 1
                    (length bytes) 'capability-token)))
    (cl-letf (((symbol-function 'nelisp-eln-system-loader-function-capability)
               (lambda (_handle _name) cap))
              ((symbol-function 'nelisp-eln-system-loader-read-root-function-bytes)
               (lambda (_handle _name _offset _size) bytes))
              ((symbol-function 'nelisp-eln-system-loader-validate-root-indirection)
               (lambda (_handle slot expected)
                 (let ((entry (assoc expected
                                     '(("d_reloc_eph" . #x4200) ("d_reloc" . #x41c0)
                                       ("freloc_link_table" . #x4220)))))
                   (if (and entry
                            (= slot (cdr (assoc expected
                                                '(("d_reloc_eph" . #x2d90)
                                                  ("d_reloc" . #x2d88)
                                                  ("freloc_link_table" . #x2d98))))))
                       (cdr entry)
                     (signal 'nelisp-eln-system-loader-error
                             (list 'root-indirection-target-mismatch)))))))
      (nelisp-eln-registration--top-level-code 'handle))))

(ert-deftest nelisp-eln-top-level-code-admits-gnu-verified-subr-thunk ()
  (let* ((code (nelisp-eln-top-level-code-family-test--verified-subr-code))
         (result (nelisp-eln-top-level-code-family-test--verified-subr-run code)))
    (should (= (length code) 69))
    (should (eq (nth 2 result) 'gnu-verified-subr))
    (should (= (cadr result) 2))
    (should (equal (nth 3 result) (list :type-index 1)))))

(ert-deftest nelisp-eln-top-level-code-rejects-tampered-gnu-verified-subr-thunk ()
  (let ((code (nelisp-eln-top-level-code-family-test--verified-subr-code)))
    (dolist (mutation
             (list
              ;; A fixed byte outside every declared hole.
              (cons 0 #x90)
              ;; min/max arity disagreement (the two arity immediates,
              ;; offsets 45 and 54, must decode equal).
              (cons 54 #x02)))
      (let ((tampered (copy-sequence code)))
        (aset tampered (car mutation) (cdr mutation))
        (should-error
         (nelisp-eln-top-level-code-family-test--verified-subr-run tampered)
         :type 'nelisp-eln-registration-error)))))

(provide 'nelisp-eln-top-level-code-family-test)

;;; nelisp-eln-top-level-code-family-test.el ends here
