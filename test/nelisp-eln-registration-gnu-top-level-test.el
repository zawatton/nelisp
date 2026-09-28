;;; nelisp-eln-registration-gnu-top-level-test.el --- GNU thunk admission -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)

(defun nelisp-eln-registration-gnu-top-level-test--code ()
  "Return the exact 68-byte GNU Emacs 31.1 single-identity thunk."
  (unibyte-string
   #x48 #x83 #xec #x10
   #x48 #x8b #x05 #xb5 #x2e #x00 #x00
   #x48 #x89 #xfa
   #x48 #x8b #x0d #xa3 #x2e #x00 #x00
   #x4c #x8b #x48 #x18
   #x48 #x8b #x70 #x10
   #x48 #x8b #x78 #x08
   #x48 #x8b #x05 #xa0 #x2e #x00 #x00
   #x4c #x8b #x01
   #xb9 #x06 #x00 #x00 #x00
   #x48 #x8b #x00
   #x52
   #xba #x06 #x00 #x00 #x00
   #xff #x90 #x30 #x20 #x00 #x00
   #x48 #x83 #xc4 #x18 #xc3))

(defun nelisp-eln-registration-gnu-top-level-test--run (bytes)
  "Admit BYTES with strict synthetic GOT-slot targets and return result."
  (let ((calls nil)
        (cap (list nil 'handle "top_level_run" #x1110 nil 1
                   (length bytes) 'capability-token)))
    (cl-letf (((symbol-function 'nelisp-eln-system-loader-function-capability)
               (lambda (_handle _name) cap))
              ((symbol-function 'nelisp-eln-system-loader-read-root-function-bytes)
               (lambda (_handle _name _offset _size) bytes))
              ((symbol-function 'nelisp-eln-emitter--top-level-code)
               (lambda (_arity) (list (make-string 71 0) nil)))
              ((symbol-function 'nelisp-eln-system-loader-validate-root-indirection)
               (lambda (_handle slot expected)
                 (push (list slot expected) calls)
                 (let ((entry (assoc expected
                                     '(("d_reloc_eph" . #x4200)
                                       ("d_reloc" . #x41c0)
                                       ("freloc_link_table" . #x4220)))))
                   (if (and entry
                            (= slot (cdr (assoc expected
                                               '(("d_reloc_eph" . #x3fd0)
                                                 ("d_reloc" . #x3fc8)
                                                 ("freloc_link_table" . #x3fd8))))))
                       (cdr entry)
                     (signal 'nelisp-eln-system-loader-error
                             (list 'root-indirection-target-mismatch)))))))
      (let ((result (nelisp-eln-registration--top-level-code 'handle)))
        (list result (nreverse calls))))))

(defun nelisp-eln-registration-gnu-top-level-test--change (code index byte)
  "Return a copy of CODE with INDEX changed to BYTE."
  (let ((copy (copy-sequence code)))
    (aset copy index byte)
    copy))

(ert-deftest nelisp-eln-registration-admits-gnu-single-register-thunk ()
  (dolist (arity '(1 0))
    (let* ((code (nelisp-eln-registration-gnu-top-level-test--code))
           (_ (when (= arity 0)
                (aset code 44 2)
                (aset code 53 2)))
           (observed
            (nelisp-eln-registration-gnu-top-level-test--run code))
           (result (car observed)))
      (should (= (length code) 68))
      (should (= (cadr result) arity))
      (should (eq (nth 2 result) 'gnu-single-leaf))
      (should (equal (mapcar #'cadr (cadr observed))
                     '("d_reloc_eph" "d_reloc" "freloc_link_table"))))))

(ert-deftest nelisp-eln-registration-admits-gnu-arity-2-thunk-stub ()
  "Doc 207: the arity-2 stub passes the top-level check; the leaf body must
then be the authenticated chain shape (checked in preflight, not here)."
  (let ((code (nelisp-eln-registration-gnu-top-level-test--code)))
    (aset code 44 10)
    (aset code 53 10)
    (should (= (cadr (car (nelisp-eln-registration-gnu-top-level-test--run
                           code)))
               2))))

(ert-deftest nelisp-eln-registration-rejects-mutated-gnu-thunks ()
  (let ((code (nelisp-eln-registration-gnu-top-level-test--code)))
    (dolist (mutated
             (list
              (nelisp-eln-registration-gnu-top-level-test--change code 1 #x82)
              (nelisp-eln-registration-gnu-top-level-test--change code 3 #x08)
              (nelisp-eln-registration-gnu-top-level-test--change code 51 #x53)
              (nelisp-eln-registration-gnu-top-level-test--change code 66 #x10)
              (nelisp-eln-registration-gnu-top-level-test--change code 59 #x31)
              (nelisp-eln-registration-gnu-top-level-test--change code 7 #xb4)
              (let ((copy (copy-sequence code)))
                (aset copy 53 2) copy) ; min/max disagreement
              (let ((copy (copy-sequence code)))
                (aset copy 44 14) (aset copy 53 14) copy))) ; arity not 0/1/2
      (should-error
       (nelisp-eln-registration-gnu-top-level-test--run mutated)
       :type 'nelisp-eln-registration-error))))

;;; Genuine GNU 31.1 (register_subr, Feval[, register_subr]) shapes.
;;
;; The byte sequences below are byte-for-byte transcriptions (never
;; hand-written) of real top_level_run functions from genuine GNU-compiled
;; small-tier .eln artifacts: gnu-caar.eln (S6.16-21: same shape admits
;; cadr/fixnump/bignump/frame-configuration-p, differing only in the
;; d_reloc slot offsets a negative control below tampers) and
;; gnu-zerop.eln (S6.24: the two-registration compiler-macro shape). See
;; ~/.cache/tmp/preflight/disasm/{caar,zerop}.fixed.txt for the annotated
;; disassembly they were transcribed from.

(defun nelisp-eln-registration-gnu-top-level-test--caar-code ()
  "Return the genuine gnu-caar.eln top_level_run (register_subr, Feval)."
  (unibyte-string
   #x41 #x54 #x48 #x8b #x05 #x7f #x2e #x00 #x00 #x48 #x89 #xfa #xb9 #x06
   #x00 #x00 #x00 #x55 #x48 #x8b #x2d #x5f #x2e #x00 #x00 #x53 #x4c #x8b
   #x20 #x48 #x8b #x05 #x5c #x2e #x00 #x00 #x4c #x8b #x45 #x08 #x48 #x83
   #xec #x08 #x4c #x8b #x48 #x18 #x48 #x8b #x70 #x10 #x48 #x8b #x78 #x08
   #x52 #xba #x06 #x00 #x00 #x00 #x41 #xff #x94 #x24 #x30 #x20 #x00 #x00
   #x48 #x8b #x75 #x18 #x48 #x8b #x7d #x00 #x48 #x89 #xc3 #x41 #xff #x94
   #x24 #x98 #x1d #x00 #x00 #x58 #x48 #x89 #xd8 #x5a #x5b #x5d #x41 #x5c
   #xc3))

(defun nelisp-eln-registration-gnu-top-level-test--zerop-code ()
  "Return the genuine gnu-zerop.eln top_level_run (register_subr, Feval,
register_subr)."
  (unibyte-string
   #x41 #x55 #xb9 #x06 #x00 #x00 #x00 #xba #x06 #x00 #x00 #x00 #x41 #x54
   #x55 #x48 #x89 #xfd #x53 #x48 #x83 #xec #x10 #x48 #x8b #x1d #x42 #x2e
   #x00 #x00 #x4c #x8b #x25 #x33 #x2e #x00 #x00 #x48 #x8b #x05 #x3c #x2e
   #x00 #x00 #x4c #x8b #x4b #x18 #x4d #x8b #x44 #x24 #x20 #x4c #x8b #x28
   #x48 #x8b #x73 #x10 #x48 #x8b #x7b #x08 #x55 #x41 #xff #x95 #x30 #x20
   #x00 #x00 #x49 #x8b #x74 #x24 #x30 #x49 #x8b #x7c #x24 #x18 #x41 #xff
   #x95 #x98 #x1d #x00 #x00 #x4c #x8b #x4b #x38 #x4d #x8b #x44 #x24 #x28
   #xb9 #x0a #x00 #x00 #x00 #x48 #x8b #x73 #x30 #x48 #x8b #x7b #x28 #xba
   #x0a #x00 #x00 #x00 #x48 #x89 #x2c #x24 #x41 #xff #x95 #x30 #x20 #x00
   #x00 #x48 #x83 #xc4 #x18 #x5b #x5d #x41 #x5c #x41 #x5d #xc3))

(defun nelisp-eln-registration-gnu-top-level-test--run-eval (bytes addresses)
  "Admit BYTES for the eval-subr(-pair) profiles with mocked ADDRESSES.

Genuine GNU artifacts reach each RIP-relative symbol through one level of
GOT-style indirection, authenticated by
`nelisp-eln-system-loader-validate-root-indirection' (see
`nelisp-eln-registration--validate-rip-relocs'); this mocks exactly that
entry point rather than `-symbol-info' directly.  ADDRESSES maps a symbol
name string to the exact computed slot address its RIP-relative load must
produce; any other slot address, or a symbol missing from ADDRESSES,
signals `nelisp-eln-system-loader-error', matching an unauthenticated or
tampered import."
  (let ((cap (list nil 'handle "top_level_run" #x1110 nil 1
                   (length bytes) 'capability-token)))
    (cl-letf (((symbol-function 'nelisp-eln-system-loader-function-capability)
               (lambda (_handle _name) cap))
              ((symbol-function 'nelisp-eln-system-loader-read-root-function-bytes)
               (lambda (_handle _name _offset _size) bytes))
              ((symbol-function 'nelisp-eln-emitter--top-level-code)
               (lambda (_arity) (list (make-string (1+ (length bytes)) 0) nil)))
              ((symbol-function 'nelisp-eln-system-loader-validate-root-indirection)
               (lambda (_handle slot-address name)
                 (let ((entry (assoc name addresses)))
                   (if (and entry (= slot-address (cdr entry)))
                       1
                     (signal 'nelisp-eln-system-loader-error
                             (list 'root-indirection-target-mismatch)))))))
      (nelisp-eln-registration--top-level-code 'handle))))

(ert-deftest nelisp-eln-registration-admits-gnu-eval-subr-thunk ()
  (let* ((code (nelisp-eln-registration-gnu-top-level-test--caar-code))
         (result
          (nelisp-eln-registration-gnu-top-level-test--run-eval
           code '(("d_reloc_eph" . #x3f90) ("d_reloc" . #x3f88)
                  ("freloc_link_table" . #x3f98)))))
    (should (= (length code) 99))
    (should (eq (nth 2 result) 'gnu-eval-subr))
    (should (= (cadr result) 1))
    (should (equal (nth 3 result)
                   '(:type-index 1 :lexenv-index 3 :form-index 0)))))

(ert-deftest nelisp-eln-registration-admits-gnu-eval-subr-pair-thunk ()
  (let* ((code (nelisp-eln-registration-gnu-top-level-test--zerop-code))
         (result
          (nelisp-eln-registration-gnu-top-level-test--run-eval
           code '(("d_reloc_eph" . #x3f70) ("d_reloc" . #x3f68)
                  ("freloc_link_table" . #x3f78)))))
    (should (= (length code) 138))
    (should (eq (nth 2 result) 'gnu-eval-subr-pair))
    (should (= (cadr result) 1))
    (should (equal (nth 3 result)
                   '(:type-index 4 :lexenv-index 6 :form-index 3
                     :type2-index 5 :arity2 2)))))

(ert-deftest nelisp-eln-registration-rejects-mutated-gnu-eval-subr-thunks ()
  (let ((code (nelisp-eln-registration-gnu-top-level-test--caar-code))
        (good '(("d_reloc_eph" . #x3f90) ("d_reloc" . #x3f88)
                ("freloc_link_table" . #x3f98))))
    ;; Sanity: the untampered artifact and address map must admit first,
    ;; so every case below is known to fail because of its own mutation.
    (should (nelisp-eln-registration-gnu-top-level-test--run-eval code good))
    (dolist (case
             (list
              ;; Tampered call target: register_subr's slot moved off 1030.
              (list (nelisp-eln-registration-gnu-top-level-test--change
                     code 66 #x31)
                    good)
              ;; Tampered call target: Feval's slot moved off 947.
              (list (nelisp-eln-registration-gnu-top-level-test--change
                     code 85 #x10)
                    good)
              ;; Tampered fixed byte outside every declared hole.
              (list (nelisp-eln-registration-gnu-top-level-test--change
                     code 0 #x55)
                    good)
              ;; Unknown import: freloc_link_table resolves to the wrong
              ;; address (a substituted, unauthenticated symbol).
              (list code
                    '(("d_reloc_eph" . #x3f90) ("d_reloc" . #x3f88)
                      ("freloc_link_table" . #x9999)))
              ;; Unknown import: the symbol is not authenticated at all.
              (list code
                    '(("d_reloc_eph" . #x3f90) ("d_reloc" . #x3f88)))
              ;; Altered constant index: the register_subr `type' slot
              ;; offset is no longer a multiple of eight.
              (list (nelisp-eln-registration-gnu-top-level-test--change
                     code 39 5)
                    good)
              ;; min/max arity disagreement.
              (list (nelisp-eln-registration-gnu-top-level-test--change
                     code 58 10)
                    good)))
      (should-error
       (nelisp-eln-registration-gnu-top-level-test--run-eval
        (nth 0 case) (nth 1 case))
       :type 'nelisp-eln-registration-error))))

(ert-deftest nelisp-eln-registration-rejects-mutated-gnu-eval-subr-pair-thunks ()
  (let ((code (nelisp-eln-registration-gnu-top-level-test--zerop-code))
        (good '(("d_reloc_eph" . #x3f70) ("d_reloc" . #x3f68)
                ("freloc_link_table" . #x3f78))))
    (should (nelisp-eln-registration-gnu-top-level-test--run-eval code good))
    (dolist (case
             (list
              ;; Tampered call target: the second register_subr call
              ;; (offset 120) moved off slot 1030.
              (list (nelisp-eln-registration-gnu-top-level-test--change
                     code 124 #x31)
                    good)
              ;; Tampered call target: Feval moved off slot 947.
              (list (nelisp-eln-registration-gnu-top-level-test--change
                     code 86 #x10)
                    good)
              ;; Unknown import: d_reloc resolves to the wrong address.
              (list code
                    '(("d_reloc_eph" . #x3f70) ("d_reloc" . #x1111)
                      ("freloc_link_table" . #x3f78)))
              ;; Second registration's min/max arity disagreement.
              (list (nelisp-eln-registration-gnu-top-level-test--change
                     code 112 6)
                    good)))
      (should-error
       (nelisp-eln-registration-gnu-top-level-test--run-eval
        (nth 0 case) (nth 1 case))
       :type 'nelisp-eln-registration-error))))

;;; `nelisp-eln-registration--eval-effect' -- the Feval call's own decoded
;;; effect, authenticated from already-decoded metadata, never executed.

(defun nelisp-eln-registration-gnu-top-level-test--caar-eval-metadata ()
  "Return a metadata plist shaped like genuine gnu-caar.eln's own."
  (list :data-relocations
        (vector '(byte-code "\300\301\302\303#\300\207"
                   [function-put caar compiler-macro
                    internal--compiler-macro-cXXr]
                   4)
                '(function (t) t) nil t 'consp 'listp 'symbol-with-pos-p)))

(ert-deftest nelisp-eln-registration-admits-genuine-eval-effect ()
  (should (equal (nelisp-eln-registration--eval-effect
                  (nelisp-eln-registration-gnu-top-level-test--caar-eval-metadata)
                  0 3 'caar)
                 '((compiler-macro . internal--compiler-macro-cXXr)))))

(ert-deftest nelisp-eln-registration-admits-genuine-multi-property-eval-effect ()
  ;; Shaped like genuine gnu-bignump.eln / gnu-frame-configuration-p.eln:
  ;; one byte-code object installing two properties on the same NAME.
  (let ((metadata
         (list :data-relocations
               (vector nil 'fixnump
                       '(byte-code "\300\301\302\303#\300\301\304\305#\300\207"
                         [function-put bignump function-type
                          (function (t) boolean) side-effect-free error-free]
                         5)
                       '(function (t) boolean) t 'consp 'listp
                       'symbol-with-pos-p))))
    (should (equal (nelisp-eln-registration--eval-effect metadata 2 4 'bignump)
                   '((function-type . (function (t) boolean))
                     (side-effect-free . error-free))))))

(ert-deftest nelisp-eln-registration-rejects-mutated-eval-effect ()
  (let ((good (nelisp-eln-registration-gnu-top-level-test--caar-eval-metadata)))
    (should (nelisp-eln-registration--eval-effect good 0 3 'caar))
    ;; LEXICAL argument must be `t' (an empty lexical environment); GNU
    ;; top_level_run never passes `nil' or anything else here.  Copy the
    ;; relocation vector itself before mutating -- `copy-sequence' on the
    ;; plist alone would still share (and corrupt) GOOD's own vector.
    (let* ((bad (copy-sequence good))
           (reloc (copy-sequence (plist-get good :data-relocations))))
      (aset reloc 3 nil)
      (plist-put bad :data-relocations reloc)
      (should-error (nelisp-eln-registration--eval-effect bad 0 3 'caar)
                     :type 'nelisp-eln-registration-error))
    ;; NAME mismatch: the byte-code object must tag the same symbol this
    ;; call site's own `Fcomp__register_subr' call already registers.
    (should-error (nelisp-eln-registration--eval-effect good 0 3 'cadr)
                   :type 'nelisp-eln-registration-error)
    ;; Callee other than `function-put'.
    (let* ((bad (copy-sequence good))
           (reloc (copy-sequence (plist-get good :data-relocations)))
           (form (copy-sequence (aref reloc 0)))
           (consts (copy-sequence (nth 2 form))))
      (aset consts 0 'put)
      (setf (nth 2 form) consts)
      (aset reloc 0 form)
      (plist-put bad :data-relocations reloc)
      (should-error (nelisp-eln-registration--eval-effect bad 0 3 'caar)
                     :type 'nelisp-eln-registration-error))
    ;; VALUE constant is not inert data (a vector, never decoded further).
    (let* ((bad (copy-sequence good))
           (reloc (copy-sequence (plist-get good :data-relocations)))
           (form (copy-sequence (aref reloc 0)))
           (consts (copy-sequence (nth 2 form))))
      (aset consts 3 (vector 1 2))
      (setf (nth 2 form) consts)
      (aset reloc 0 form)
      (plist-put bad :data-relocations reloc)
      (should-error (nelisp-eln-registration--eval-effect bad 0 3 'caar)
                     :type 'nelisp-eln-registration-error))
    ;; Corrupted opcode: the call byte no longer means Bcall3.
    (let* ((bad (copy-sequence good))
           (reloc (copy-sequence (plist-get good :data-relocations)))
           (form (copy-sequence (aref reloc 0)))
           (code (copy-sequence (nth 1 form))))
      (aset code 4 36)
      (setf (nth 1 form) code)
      (aset reloc 0 form)
      (plist-put bad :data-relocations reloc)
      (should-error (nelisp-eln-registration--eval-effect bad 0 3 'caar)
                     :type 'nelisp-eln-registration-error))))

;;; Call-sequence dispatch (`--call-role' / `--register-ordinal') -- the
;;; S6.16-24 mechanism that lets one shared import slot honestly route a
;;; genuine top_level_run's ordered register_subr/Feval/register_subr
;;; calls, from the active owner's own preflight-admitted role sequence
;;; and a call counter, never from anything a call claims about itself.

(defun nelisp-eln-registration-gnu-top-level-test--owner (role-sequence)
  "Return a minimal 19-slot owner vector carrying ROLE-SEQUENCE at index 18."
  (let ((owner (make-vector nelisp-eln-registration--owner-size nil)))
    (aset owner 18 (list :role-sequence role-sequence))
    owner))

(ert-deftest nelisp-eln-registration-call-role-legacy-single-call ()
  ;; No :role-sequence (self-emitter / gnu-single-leaf): exactly one
  ;; `register' call admitted; anything past it is not admitted.
  (let ((owner (nelisp-eln-registration-gnu-top-level-test--owner nil)))
    (let ((nelisp-eln-registration--call-index 1))
      (should (eq (nelisp-eln-registration--call-role owner) 'register)))
    (let ((nelisp-eln-registration--call-index 2))
      (should (null (nelisp-eln-registration--call-role owner))))))

(ert-deftest nelisp-eln-registration-call-role-eval-subr ()
  (let ((owner (nelisp-eln-registration-gnu-top-level-test--owner
                '(register eval))))
    (let ((nelisp-eln-registration--call-index 1))
      (should (eq (nelisp-eln-registration--call-role owner) 'register))
      (should (= (nelisp-eln-registration--register-ordinal owner) 1)))
    (let ((nelisp-eln-registration--call-index 2))
      (should (eq (nelisp-eln-registration--call-role owner) 'eval)))
    (let ((nelisp-eln-registration--call-index 3))
      (should (null (nelisp-eln-registration--call-role owner))))))

(ert-deftest nelisp-eln-registration-call-role-eval-subr-pair ()
  (let ((owner (nelisp-eln-registration-gnu-top-level-test--owner
                '(register eval register))))
    (let ((nelisp-eln-registration--call-index 1))
      (should (eq (nelisp-eln-registration--call-role owner) 'register))
      (should (= (nelisp-eln-registration--register-ordinal owner) 1)))
    (let ((nelisp-eln-registration--call-index 2))
      (should (eq (nelisp-eln-registration--call-role owner) 'eval)))
    (let ((nelisp-eln-registration--call-index 3))
      (should (eq (nelisp-eln-registration--call-role owner) 'register))
      (should (= (nelisp-eln-registration--register-ordinal owner) 2)))
    (let ((nelisp-eln-registration--call-index 4))
      (should (null (nelisp-eln-registration--call-role owner))))))

(ert-deftest nelisp-eln-registration-callback-dispatch-rejects-extra-call ()
  ;; A fourth call against a three-call admitted plan -- e.g. a corrupted
  ;; or unexpectedly re-entrant native path -- must signal, never guess.
  (let ((nelisp-eln-registration--active-owner
         (nelisp-eln-registration-gnu-top-level-test--owner
          '(register eval register)))
        (nelisp-eln-registration--call-index 3))
    (should-error
     (nelisp-eln-registration--callback (make-string 56 0))
     :type 'nelisp-eln-registration-error)
    (should (= nelisp-eln-registration--call-index 4))))

(ert-deftest nelisp-eln-registration-apply-eval-callback-requires-effects ()
  ;; `--apply-eval-callback' must signal, never silently no-op, when the
  ;; active owner's effects or prior registration are unavailable.
  (let ((nelisp-eln-registration--active-owner
         (nelisp-eln-registration-gnu-top-level-test--owner '(register eval)))
        (nelisp-eln-registration--registered-word nil)
        (nelisp-eln-registration--registered-callable nil))
    (should-error
     (nelisp-eln-registration--apply-eval-callback (make-string 56 0))
     :type 'nelisp-eln-registration-error)))

(provide 'nelisp-eln-registration-gnu-top-level-test)

;;; nelisp-eln-registration-gnu-top-level-test.el ends here
