;;; nelisp-eln-native-subr.el --- managed GNU native function values -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Admit authenticated scalar0 integers and bounded unary expression leaves.
;; Unary calls cross the GNU ABI through leased object views and a raw-word
;; trampoline; a NeLisp object address is never treated as a GNU Lisp_Object.

;;; Code:

(require 'nelisp-eln-system-loader)
(require 'nelisp-eln-objects)
(require 'nelisp-eln-raw-call)
(require 'nelisp-eln-leaf-code)
(require 'nelisp-eln-tail-code)
(require 'nelisp-eln-runtime-services)
(require 'cl-lib)

;; `nelisp-eln-callable-import' is NOT required eagerly: every reference
;; below is inside one of the `nelisp-eln-native-subr-create*' subr-
;; construction functions, `--chain-bridge' (the call-time bridge a
;; created chain subr's closure invokes, never at construction),
;; `-import-entries' or `-import-entry-address' -- all reached only once
;; preflight classification has already decided an artifact is
;; admissible, never during classification itself (`--tail-import-
;; analysis', `--many-import-analysis', `--cxr-import-analysis' and
;; `-multi-import-analysis' call none of it). A rejected artifact -- the
;; corpus gate's S7.7.4 corrupt/truncated/ABI-mismatched checks, or
;; simply a name already bound -- never reaches subr construction, so
;; never pays this module's load cost either. Each site's own
;; `(require 'nelisp-eln-callable-import)' mirrors the same pattern
;; `nelisp-eln-registration.el' uses for `nelisp-native-load'.
(declare-function nelisp-eln-callable-import-entry-address
  "nelisp-eln-callable-import" ())
(declare-function nelisp-eln-callable-import-port-tag
  "nelisp-eln-callable-import" (port))
(declare-function nelisp-eln-callable-import-port-entry-address
  "nelisp-eln-callable-import" (port))
(declare-function nelisp-eln-callable-import-wide-entry-address
  "nelisp-eln-callable-import" ())
(declare-function nelisp-eln-callable-import--call-unary
  "nelisp-eln-callable-import"
  (capability implementation argument &optional convention arity constants
              ports extra-arguments))
(declare-function nelisp-eln-callable-import--call-chain
  "nelisp-eln-callable-import"
  (capability implementation arguments &optional convention arity constants
              ports))

(define-error 'nelisp-eln-native-subr-error
  "Invalid managed GNU native function" 'nelisp-eln-system-loader-error)

(defconst nelisp-eln-native-subr--tail-descriptors
  '(("ba35c031" 1301 1+ 1 #x1fffffffffffffff 6)
    ("ba35c031" 1300 1- 1 #xe000000000000000 -2))
  "Authenticated (ABI SLOT BUILTIN ARITY BOUND TAGGED-DELTA) descriptors.")
(defconst nelisp-eln-native-subr--tail-lease-marker
  (make-symbol "nelisp-eln-tail-import-lease"))
(defvar nelisp-eln-native-subr--tail-import-context nil
  "Dynamically bound, registration-owned lease during subr construction.")

(defun nelisp-eln-native-subr--tail-descriptor (abi-hash slot)
  "Return the verified descriptor for ABI-HASH and SLOT, or nil."
  (cl-find-if (lambda (descriptor)
                (and (equal abi-hash (nth 0 descriptor))
                     (equal slot (nth 1 descriptor))))
              nelisp-eln-native-subr--tail-descriptors))

(defconst nelisp-eln-native-subr--many-descriptors
  '(("ba35c031" 1320 = 2)
    ("ba35c031" 945 funcall 2)
    ("ba35c031" 946 apply 2)
    ("ba35c031" 1317 <= 2)
    ;; S6.9 (`byte-compile-lambda'): 946 `Fapply' (1, MANY) called with four
    ;; arguments, 1117 `Fvector' (MANY) with two and 1236 `Fappend' (MANY)
    ;; with two or three -- NeLisp's own `apply'/`vector'/`append'.
    ("ba35c031" 946 apply 4)
    ("ba35c031" 1117 vector 2)
    ("ba35c031" 1236 append 2)
    ;; S6.11 (`byte-compile-make-closure'): 946 `Fapply' with six arguments
    ;; (`make-byte-code' and its five) and 1318 `Fgtr' (`>') with two.
    ("ba35c031" 946 apply 6)
    ("ba35c031" 1318 > 2))
  "Authenticated (ABI SLOT BUILTIN ARITY) GNU MANY-convention descriptors.

Verified against the authenticated slot table
\"~/.cache/tmp/slot-auth/freloc-ba35c031.tsv\" (sha256
3e8591ab81130c91375221fb3efeaa377c2cd90ccf45fcd58adb3f31c55f0758): row
1320, offset 0x2940, resolves to `Feqlsign in section .text of
/usr/local/bin/emacs-31.1', the C implementation backing `=', which is
genuine vendor subr.el `zerop's only indirect call.  Unlike
`nelisp-eln-native-subr--tail-descriptors', these entries describe a
non-tail CALL through a stack-built GNU MANY (argc, argv) pair rather
than a tail JMP, so they carry no fixnum bound or tagged delta.

Row 945, offset 0x1d88, resolves to `Ffuncall' itself (src/eval.c).
Unlike the `=' entry above, whose second array element is always the
compiled-in fixnum literal 0 so the only variable input is the
descriptor's own unary argument, this entry's BUILTIN names the
generic dispatch gateway, not a fixed 2-argument computation: every
element of the MANY array (including array[0], the callee) is data the
caller supplied, and `--canonical-builtin' below authenticates only
that slot 945 truly reaches genuine, unredefined `funcall' -- it makes
no claim about the identity of whatever callable that data names.
`(symbol-function \\='funcall)' is confirmed (S4.6 slice A probe) to be
`equal' to `(builtin funcall)' and `apply'-able as such, exactly like
`=' above, so no change to `--canonical-builtin' or to the generic
`nelisp-eln-callable-import--dispatch' apply step was needed to admit
this entry.

Row 1317, offset 0x2928, resolves to `Fleq' (src/data.c), the C
implementation backing `<='; genuine vendor subr.el `fixnump' makes two
MANY (2, argv) calls through it (see
`nelisp-eln-native-subr-multi-import-analysis').

Row 946, offset 0x1d90, resolves to `Fapply' (src/eval.c, S6.4): genuine
`macroexpand-1' calls it through a MANY (2, argv) pair, (FUNCTION ARGLIST).
As for `Ffuncall', `--canonical-builtin' authenticates only that the slot
reaches genuine, unredefined `apply'.")

(defun nelisp-eln-native-subr--many-descriptor (abi-hash slot &optional arity)
  "Return the verified MANY descriptor for ABI-HASH and SLOT, or nil.
A slot may own several rows with different argument counts (`Fapply');
when ARITY is a number the row must carry exactly that count."
  (cl-find-if (lambda (descriptor)
                (and (equal abi-hash (nth 0 descriptor))
                     (equal slot (nth 1 descriptor))
                     (or (not (natnump arity)) (equal arity (nth 3 descriptor)))))
              nelisp-eln-native-subr--many-descriptors))

(defun nelisp-eln-native-subr--tail-import-context-abi-hash ()
  "Return the ABI hash of the live registration lease, or nil.
Shared by the tail-JMP and MANY stack-call import analyses; both must
authenticate against the same owner's recorded ABI before either
descriptor table is consulted."
  (let ((lease nelisp-eln-native-subr--tail-import-context))
    (and (vectorp lease) (> (length lease) 2)
         (let ((owner (aref lease 2)))
           (and (vectorp owner) (> (length owner) 7)
                (plist-get (aref owner 7) :abi-hash))))))

(defun nelisp-eln-native-subr--tail-constants-match-p (code descriptor)
  "Return non-nil when CODE carries DESCRIPTOR's bound and tagged delta."
  (catch 'invalid
    (let* ((bound (nth 4 descriptor)) (delta (nth 5 descriptor))
           ;; Decode the machine bound as two unsigned words.  NeLisp's
           ;; signed 64-bit immediate representation reads 0xe000... as a
           ;; negative integer, while the x86 decoder assembles it unsigned.
           (bound-low (logand bound #xffffffff))
           (bound-high (logand (ash bound -32) #xffffffff))
          (bound-seen nil) (delta-seen nil) (i 0) (size (length code)))
      (while (< i size)
        (let* ((insn (nelisp-eln-tail-code--decode code i size 0))
               (end (nth 1 insn)) (kind (nth 2 insn)) (arg (nth 3 insn)))
          (when (and (eq kind 'mov-rdx-imm)
                     (= (nelisp-eln-leaf-code--u32 code (+ i 2)) bound-low)
                     (= (nelisp-eln-leaf-code--u32 code (+ i 6)) bound-high))
            (setq bound-seen t))
          (when (and (eq kind 'lea-fixnum) (= arg delta))
            (setq delta-seen t))
          (setq i end)))
      (and (= i size) bound-seen delta-seen))))

(defun nelisp-eln-native-subr--tail-import-analysis
    (handle capability code &optional abi-hash)
  "Return verified tail-import analysis for CODE in CAPABILITY, or nil.
ABI-HASH selects the descriptor table entry; unknown ABIs and slots fail
closed.  The GOT indirection must resolve through the authenticated
`freloc_link_table' root object."
  (let* ((state (condition-case nil
                    (nelisp-eln-system-loader--state handle)
                  (nelisp-eln-system-loader-error nil)))
         (bias (plist-get state :bias))
         (address (nth 3 capability))
         (abi-hash (or abi-hash (plist-get state :abi-hash)))
         (allowed-slots (mapcar (lambda (entry) (nth 1 entry))
                                (cl-remove-if-not
                                 (lambda (entry) (equal abi-hash (nth 0 entry)))
                                 nelisp-eln-native-subr--tail-descriptors)))
         ;; The CFG verifier classifies LEA fixnums by signed remainder.  -2
         ;; is congruent to 2 modulo four, so verify that one descriptor using
         ;; an equivalent displacement in a private copy; authenticate the
         ;; original -2 bytes below before admitting the result.
         ;;
         ;; `nelisp-eln-tail-code--decode' throws \\='invalid, uncaught, on any
         ;; byte sequence it does not recognize -- fine inside
         ;; `nelisp-eln-tail-code-analyze''s own `catch', which this loop is
         ;; not.  This function must return nil, gracefully, for every leaf
         ;; shape other than the tail-JMP one -- CODE is not yet known to be
         ;; that shape here, only that *some* ABI-matching descriptor
         ;; happens to use -2 -- so this walk must not let that throw
         ;; escape uncaught; falling back to CODE unchanged is exactly as
         ;; correct as never having patched it, since
         ;; `nelisp-eln-tail-code-analyze' below still authenticates every
         ;; byte itself and rejects a genuine mismatch on its own.
         (verify-code (if (cl-some (lambda (entry)
                                    (and (equal abi-hash (nth 0 entry))
                                         (= (nth 5 entry) -2)))
                                  nelisp-eln-native-subr--tail-descriptors)
                          (catch 'invalid
                            (let ((copy (copy-sequence code)) (i 0))
                              (while (< i (length copy))
                                (let* ((insn (nelisp-eln-tail-code--decode
                                              copy i (length copy) 0))
                                       (end (nth 1 insn)))
                                  (when (and (eq (nth 2 insn) 'lea-fixnum)
                                             (= (nth 3 insn) -2))
                                    (aset copy (+ i 4) 2)
                                    (aset copy (+ i 5) 0)
                                    (aset copy (+ i 6) 0)
                                    (aset copy (+ i 7) 0))
                                  (setq i end)))
                              copy))
                        code))
         (verify-code (or verify-code code))
         (vaddr (and (integerp bias) (integerp address) (- address bias)))
         (analysis (and (integerp vaddr) (>= vaddr 0)
                        (nelisp-eln-tail-code-analyze
                         verify-code vaddr
                         allowed-slots)))
         (imports (plist-get analysis :imports)))
    (when (and (eq (plist-get analysis :safe) t)
               (listp imports) (= (length imports) 1)
               (let ((descriptor
                      (nelisp-eln-native-subr--tail-descriptor
                       abi-hash (plist-get (car imports) :slot))))
                 (and descriptor
                      (= (nth 3 descriptor) 1)
                      (nelisp-eln-native-subr--tail-constants-match-p
                       code descriptor))))
      (let* ((got-vaddr (plist-get (car imports) :got-vaddr))
             (slot-address (and (integerp got-vaddr)
                                (>= got-vaddr 0) (+ bias got-vaddr)))
             (target
              (and slot-address
                   (condition-case nil
                       (nelisp-eln-system-loader-validate-root-indirection
                        handle slot-address "freloc_link_table")
                     (nelisp-eln-system-loader-error nil)))))
        (when (and (integerp target) (> target 0))
          (plist-put analysis :descriptor
                     (nelisp-eln-native-subr--tail-descriptor
                      abi-hash (plist-get (car imports) :slot))))))))

(defun nelisp-eln-native-subr--many-import-analysis
    (handle capability code &optional abi-hash)
  "Return verified MANY stack-call analysis for CODE in CAPABILITY, or nil.
Mirrors `nelisp-eln-native-subr--tail-import-analysis' for the non-tail,
stack-array GNU MANY calling convention (the genuine `zerop' shape): a
single authenticated CALL, never a tail JMP, through one freloc slot
whose native argument count matches the descriptor's ARITY.  ABI-HASH
selects `nelisp-eln-native-subr--many-descriptors'; unknown ABIs and
slots fail closed."
  (let* ((state (condition-case nil
                    (nelisp-eln-system-loader--state handle)
                  (nelisp-eln-system-loader-error nil)))
         (bias (plist-get state :bias))
         (address (nth 3 capability))
         (abi-hash (or abi-hash (plist-get state :abi-hash)))
         (candidates (cl-remove-if-not
                      (lambda (entry) (equal abi-hash (nth 0 entry)))
                      nelisp-eln-native-subr--many-descriptors))
         (allowed-slots (mapcar (lambda (entry) (nth 1 entry)) candidates))
         (vaddr (and (integerp bias) (integerp address) (- address bias)))
         (analysis
          (and (integerp vaddr) (>= vaddr 0)
               (catch 'admitted
                 (dolist (entry candidates)
                   (let ((result (nelisp-eln-tail-code-analyze-stack-call
                                  code vaddr allowed-slots (nth 3 entry))))
                     (when (eq (plist-get result :safe) t)
                       (throw 'admitted result))))
                 nil)))
         (imports (plist-get analysis :imports)))
    (when (and analysis (listp imports) (= (length imports) 1))
      (let* ((got-vaddr (plist-get (car imports) :got-vaddr))
             (slot-address (and (integerp got-vaddr)
                                (>= got-vaddr 0) (+ bias got-vaddr)))
             (target
              (and slot-address
                   (condition-case nil
                       (nelisp-eln-system-loader-validate-root-indirection
                        handle slot-address "freloc_link_table")
                     (nelisp-eln-system-loader-error nil))))
             (descriptor
              (nelisp-eln-native-subr--many-descriptor
               abi-hash (plist-get (car imports) :slot))))
        (when (and (integerp target) (> target 0) descriptor)
          (plist-put analysis :descriptor descriptor))))))

(defun nelisp-eln-native-subr-chain-import-analysis
    (handle capability code &optional abi-hash)
  "Return verified S4.6 chain-call analysis for CODE in CAPABILITY, or nil.
Ledger S4.6: admits exactly the genuine two-argument
`(defun nelisp-gnu-chain (g x) (funcall g (1+ x)))' shape -- an inline
fixnum fast path, a non-tail authenticated CALL to a unary tail-
descriptor slot (the Fadd1 slow path) that joins the fast path, and a
further non-tail authenticated CALL to a MANY-convention descriptor
slot (the `Ffuncall' hop).  Tries every ABI-matching unary descriptor
as the slow-path candidate (each carries its own fixnum bound/tagged
delta) crossed with every ABI-matching MANY descriptor as the funcall
candidate, admitting the first combination
`nelisp-eln-tail-code-analyze-chain-call' accepts; unknown ABIs and
slot combinations fail closed, exactly like the sibling tail-JMP and
MANY stack-call analyses above."
  (let* ((state (condition-case nil
                    (nelisp-eln-system-loader--state handle)
                  (nelisp-eln-system-loader-error nil)))
         (bias (plist-get state :bias))
         (address (nth 3 capability))
         (abi-hash (or abi-hash (plist-get state :abi-hash)))
         (unary-candidates
          (cl-remove-if-not (lambda (entry) (equal abi-hash (nth 0 entry)))
                             nelisp-eln-native-subr--tail-descriptors))
         (many-candidates
          (cl-remove-if-not (lambda (entry) (and (equal abi-hash (nth 0 entry))
                                                 (= (nth 3 entry) 2)))
                             nelisp-eln-native-subr--many-descriptors))
         (vaddr (and (integerp bias) (integerp address) (- address bias)))
         (analysis
          (and (integerp vaddr) (>= vaddr 0)
               (catch 'admitted
                 (dolist (unary unary-candidates)
                   (dolist (many many-candidates)
                     (let ((result
                            (nelisp-eln-tail-code-analyze-chain-call
                             code vaddr (list (nth 1 unary)) (nth 4 unary)
                             (nth 5 unary) (list (nth 1 many)) (nth 3 many))))
                       (when (eq (plist-get result :safe) t)
                         (throw 'admitted result)))))
                 nil)))
         (imports (plist-get analysis :imports)))
    (when (and analysis (listp imports) (= (length imports) 2))
      (let* ((unary-slot (plist-get (nth 0 imports) :slot))
             (many-slot (plist-get (nth 1 imports) :slot))
             (unary-descriptor
              (nelisp-eln-native-subr--tail-descriptor abi-hash unary-slot))
             (many-descriptor
              (nelisp-eln-native-subr--many-descriptor abi-hash many-slot))
             (validated
              (cl-every
               (lambda (import)
                 (let* ((got-vaddr (plist-get import :got-vaddr))
                        (slot-address (and (integerp got-vaddr)
                                           (>= got-vaddr 0) (+ bias got-vaddr)))
                        (target
                         (and slot-address
                              (condition-case nil
                                  (nelisp-eln-system-loader-validate-root-indirection
                                   handle slot-address "freloc_link_table")
                                (nelisp-eln-system-loader-error nil)))))
                   (and (integerp target) (> target 0))))
               imports)))
        (when (and validated unary-descriptor many-descriptor)
          (plist-put analysis :unary-descriptor unary-descriptor)
          (plist-put analysis :many-descriptor many-descriptor))))))

(defun nelisp-eln-native-subr--canonical-builtin (descriptor)
  "Return a snapshot of DESCRIPTOR's canonical builtin descriptor.
Validated for both the unary tail-JMP descriptors and the MANY
stack-call descriptors; DESCRIPTOR's ARITY field is accepted as-is and
is not required to be 1, since a MANY-convention `zerop' calls a
2-argument builtin (`=') even though `zerop' itself stays unary."
  (let* ((builtin (nth 2 descriptor))
         (cell (and (fboundp builtin) (symbol-function builtin))))
    ;; The dispatcher uses this static builtin name directly. Snapshot the
    ;; exact two-cell descriptor without traversing arbitrary/circular data.
    (unless (and descriptor (integerp (nth 3 descriptor)) (> (nth 3 descriptor) 0)
                 (consp cell) (eq (car cell) 'builtin)
                 (consp (cdr cell)) (eq (cadr cell) builtin)
                 (null (cddr cell)))
      (signal 'nelisp-eln-native-subr-error
              (list 'noncanonical-tail-builtin builtin)))
    (list 'builtin builtin)))

(defun nelisp-eln-native-subr--tail-lease-valid-p
    (lease handle capability &optional require-active)
  "Return non-nil only for LEASE's retained installed callback table.
Shared by both the unary tail-JMP import and the MANY stack-call
import: the lease/table/owner mechanics it checks are convention-
agnostic, so it accepts a proof tagged either :forward-cfg or
:straight-line-call, resolving the imported slot against whichever of
`nelisp-eln-native-subr--tail-descriptors' or
`nelisp-eln-native-subr--many-descriptors' names it."
  (let* ((owner (and (vectorp lease) (= (length lease) 8)
                     (eq (aref lease 0)
                         nelisp-eln-native-subr--tail-lease-marker)
                     (aref lease 2)))
         (table-owner (and owner (aref lease 3)))
         (table-address (and owner (aref lease 4)))
         (link-address (and owner (aref lease 5)))
         (entry-address (and owner (aref lease 6)))
         (proof (and owner (aref lease 7)))
         ;; Registration owners grew from 18 to `nelisp-eln-registration--owner-size'
         ;; slots (dual registration added 18-19); only slots below 18 are read here.
         (owner-cap (and (vectorp owner) (>= (length owner) 18)
                         (aref owner 8)))
         (owner-metadata (and (vectorp owner) (>= (length owner) 18)
                              (aref owner 7)))
         (unit (and owner (aref owner 1)))
         (owner-handle (and (vectorp unit) (> (length unit) 1)
                            (aref unit 1))))
    (and owner owner-cap (listp capability) (>= (length capability) 8)
         (listp owner-cap) (>= (length owner-cap) 8)
         (eq (aref lease 1) handle)
         (eq owner-handle handle)
         (eq (aref owner 12) table-owner)
         (eq (aref owner 17) lease)
         (or (not require-active)
             (and (boundp 'nelisp-eln-registration--active-owner)
                  (eq nelisp-eln-registration--active-owner owner)))
         (boundp 'nelisp-eln-registration--owners)
         (memq owner nelisp-eln-registration--owners)
         (stringp (plist-get owner-metadata :abi-hash))
         (eq (plist-get proof :safe) t)
         (memq (plist-get proof :proof) '(:forward-cfg :straight-line-call))
         (consp (plist-get proof :imports))
         (null (cdr (plist-get proof :imports)))
         (let ((import (car (plist-get proof :imports))))
           (and (or (nelisp-eln-native-subr--tail-descriptor
                     (plist-get owner-metadata :abi-hash)
                     (plist-get import :slot))
                    (nelisp-eln-native-subr--many-descriptor
                     (plist-get owner-metadata :abi-hash)
                     (plist-get import :slot)))
                (integerp (plist-get import :got-vaddr))
                (>= (plist-get import :got-vaddr) 0)))
         (equal (nth 2 capability) (nth 2 owner-cap))
         (equal (nth 3 capability) (nth 3 owner-cap))
         (eq (nth 7 capability) (nth 7 owner-cap))
         (integerp table-address) (> table-address 0)
         (integerp link-address) (> link-address 0)
         (= table-address (nl-ffi-memory-address table-owner))
         (= (ptr-read-u64 link-address 0) table-address)
         (integerp entry-address) (> entry-address 0)
         (= (ptr-read-u64 table-address
                          (* 8 (plist-get (car (plist-get proof :imports)) :slot)))
            entry-address)
         ;; The exact entry this proof's convention needs (callback1 for
         ;; a unary tail JMP, the seven-word entry for a MANY call).
         (= entry-address (nelisp-eln-native-subr-import-entry-address proof)))))

(defun nelisp-eln-native-subr--chain-lease-valid-p
    (lease handle capability &optional require-active)
  "Return non-nil only for LEASE's retained installed callback table.
Ledger S4.6 sibling of `nelisp-eln-native-subr--tail-lease-valid-p',
generalized from exactly one authenticated import to exactly two: the
proof must be tagged :chain-call and carry both the Fadd1 slow-path
import and the `Ffuncall' import, and BOTH slots' live freloc table
entries -- not just one -- must resolve to the shared callback
trampoline before the lease is trusted."
  (let* ((owner (and (vectorp lease) (= (length lease) 8)
                     (eq (aref lease 0)
                         nelisp-eln-native-subr--tail-lease-marker)
                     (aref lease 2)))
         (table-owner (and owner (aref lease 3)))
         (table-address (and owner (aref lease 4)))
         (link-address (and owner (aref lease 5)))
         (entry-address (and owner (aref lease 6)))
         (proof (and owner (aref lease 7)))
         (owner-cap (and (vectorp owner) (>= (length owner) 18)
                         (aref owner 8)))
         (owner-metadata (and (vectorp owner) (>= (length owner) 18)
                              (aref owner 7)))
         (unit (and owner (aref owner 1)))
         (owner-handle (and (vectorp unit) (> (length unit) 1)
                            (aref unit 1)))
         (imports (and owner (plist-get proof :imports))))
    (and owner owner-cap (listp capability) (>= (length capability) 8)
         (listp owner-cap) (>= (length owner-cap) 8)
         (eq (aref lease 1) handle)
         (eq owner-handle handle)
         (eq (aref owner 12) table-owner)
         (eq (aref owner 17) lease)
         (or (not require-active)
             (and (boundp 'nelisp-eln-registration--active-owner)
                  (eq nelisp-eln-registration--active-owner owner)))
         (boundp 'nelisp-eln-registration--owners)
         (memq owner nelisp-eln-registration--owners)
         (stringp (plist-get owner-metadata :abi-hash))
         (eq (plist-get proof :safe) t)
         (eq (plist-get proof :proof) :chain-call)
         (listp imports) (= (length imports) 2)
         (equal (nth 2 capability) (nth 2 owner-cap))
         (equal (nth 3 capability) (nth 3 owner-cap))
         (eq (nth 7 capability) (nth 7 owner-cap))
         (integerp table-address) (> table-address 0)
         (integerp link-address) (> link-address 0)
         (= table-address (nl-ffi-memory-address table-owner))
         (= (ptr-read-u64 link-address 0) table-address)
         ;; Doc 207: ENTRY-ADDRESS holds ((SLOT . PORT-ENTRY) ...), one
         ;; slot-identifying port per import, and each live table slot must
         ;; still hold its own port.
         (consp entry-address)
         (equal entry-address (nelisp-eln-native-subr-import-entries proof))
         (let ((unary (nth 0 imports)) (many (nth 1 imports))
               (abi-hash (plist-get owner-metadata :abi-hash)))
           (and (nelisp-eln-native-subr--tail-descriptor
                 abi-hash (plist-get unary :slot))
                (nelisp-eln-native-subr--many-descriptor
                 abi-hash (plist-get many :slot))
                (cl-every (lambda (import)
                            (and (integerp (plist-get import :got-vaddr))
                                 (>= (plist-get import :got-vaddr) 0)))
                          imports)
                (cl-every (lambda (entry)
                            (and (integerp (cdr entry)) (> (cdr entry) 0)
                                 (= (ptr-read-u64 table-address
                                                  (* 8 (car entry)))
                                    (cdr entry))))
                          entry-address))))))

(defun nelisp-eln-native-subr--unary-bridge (address argument)
  "Invoke the authenticated unary leaf at ADDRESS.
ARGUMENT is encoded as a genuine GNU object word for the native call and the
result is decoded while the per-call object activation is still leased."
  (let (objects context activation result call-failure cleanup-failure)
    (unwind-protect
        (condition-case err
            (progn
              (setq objects (nelisp-eln-objects-create)
                    context (nelisp-eln-raw-call-context-create))
              (let ((argument-word
                     (nelisp-eln-objects-encode objects argument)))
                (setq activation
                      (nelisp-eln-objects-activation-acquire objects))
                (setq result
                      (nelisp-eln-objects-activation-decode
                       activation
                       (nelisp-eln-raw-call-word
                        context address (list argument-word))))))
          (error (setq call-failure err)))
      ;; Always attempt every release, even during throw or quit. Release
      ;; failures are retained by their owners; suppress them here so they do
      ;; not replace an error or nonlocal exit from the call.
      (dolist (cleanup
               (list (and activation
                          (list #'nelisp-eln-objects-activation-release
                                activation))
                     (and objects
                          (list #'nelisp-eln-objects-release objects))
                     (and context
                          (list #'nelisp-eln-raw-call-context-release
                                context))))
        (when cleanup
          (condition-case err
              (funcall (car cleanup) (cadr cleanup))
            ((error quit)
             (unless cleanup-failure (setq cleanup-failure err)))))))
    (cond (call-failure (signal (car call-failure) (cdr call-failure)))
          (cleanup-failure
           (signal (car cleanup-failure) (cdr cleanup-failure)))
          (t result))))

(defun nelisp-eln-native-subr--chain-bridge
    (lease handle capability unary-implementation many-implementation g x)
  "Invoke the authenticated S4.6 chain leaf at CAPABILITY with G and X.
Re-validates LEASE against HANDLE/CAPABILITY on every call, exactly like
the tail-import and many-import bridges above, then makes the outer
two-argument raw call through `nelisp-eln-callable-import--call-chain'.

The one native activation carries both authenticated identities
\(Doc 207): UNARY-IMPLEMENTATION (the canonical `1+' snapshot) for the
non-tail Fadd1 slow-path callback, which arrives through port 0, and
MANY-IMPLEMENTATION (the canonical `funcall' snapshot) for the non-tail
`Ffuncall' hop, which arrives through port 1.  A
non-local exit from either callback suppresses every later callback of
the same activation and is resumed after the native frame returns."
  ;; `fboundp', not just `featurep': a test's `cl-letf' mock of this
  ;; exact function must survive instead of being clobbered by a real
  ;; load triggered here.
  (unless (fboundp 'nelisp-eln-callable-import--call-chain)
    (require 'nelisp-eln-callable-import))
  (unless (nelisp-eln-native-subr--chain-lease-valid-p lease handle capability)
    (signal 'nelisp-eln-native-subr-error (list 'expired-import-table)))
  (nelisp-eln-callable-import--call-chain
   capability nil (list g x) nil nil nil
   (list (cons (nelisp-eln-callable-import-port-tag 0)
               (list :convention 'fixed :arity 1 :arguments '(lisp)
                     :return 'lisp :implementation unary-implementation))
         (cons (nelisp-eln-callable-import-port-tag 1)
               (list :convention 'many :arity 2 :arguments '(lisp lisp)
                     :return 'lisp :implementation many-implementation)))))

(defun nelisp-eln-native-subr-create-chain (handle name &optional function-name)
  "Create a managed genuine two-argument S4.6 chain-import subr for NAME.
Ledger S4.6 sibling of `nelisp-eln-native-subr-create', scoped to
exactly the admitted `nelisp-gnu-chain'-shaped leaf: a function whose
only indirect calls are the authenticated Fadd1 slow path and the
authenticated `Ffuncall' MANY hop, verified end to end by
`nelisp-eln-native-subr-chain-import-analysis'.  FUNCTION-NAME follows
the same string/symbol contract as `nelisp-eln-native-subr-create'."
  (let* ((capability
          (nelisp-eln-system-loader-function-capability handle name))
         (size (nth 6 capability))
         (code (and (integerp size) (> size 0)
                    (nelisp-eln-system-loader-read-root-function-bytes
                     handle name 0 size)))
         (chain-analysis (and code (= (length code) size)
                              (nelisp-eln-native-subr-chain-import-analysis
                               handle capability code
                               (nelisp-eln-native-subr--tail-import-context-abi-hash))))
         (lease nelisp-eln-native-subr--tail-import-context)
         (unary-implementation
          (and chain-analysis
               (nelisp-eln-native-subr--canonical-builtin
                (plist-get chain-analysis :unary-descriptor))))
         (many-implementation
          (and chain-analysis
               (nelisp-eln-native-subr--canonical-builtin
                (plist-get chain-analysis :many-descriptor))))
         (module-id (nelisp-eln-system-loader-module-id handle)))
    (unless (and chain-analysis unary-implementation many-implementation
                 (nelisp-eln-native-subr--chain-lease-valid-p
                  lease handle capability t)
                 (or (null function-name)
                     (symbolp function-name)
                     (and (stringp function-name)
                          (= (length function-name)
                             (string-bytes function-name)))))
      (signal 'nelisp-eln-native-subr-error
              (list 'unsupported-native-abi name size)))
    (nelisp-eln-system-loader-validate-function-capability capability)
    (setq function-name
          (cond ((and function-name (symbolp function-name)) function-name)
                ((stringp function-name) (intern function-name))
                (t (intern name))))
    (let ((bridge
           (lambda (g x)
             (nelisp-eln-native-subr--chain-bridge
              lease handle capability unary-implementation
              many-implementation g x))))
      (unless (functionp bridge)
        (signal 'nelisp-eln-native-subr-error
                (list 'invalid-chain-bridge name)))
      (nelisp--native-subr-create capability function-name module-id bridge 2))))

(defun nelisp-eln-native-subr--runtime-services-descriptor (index)
  "Return the authenticated runtime-services descriptor at freloc INDEX."
  (cl-find-if (lambda (d) (equal (plist-get d :index) index))
              nelisp-eln-runtime-services-descriptors))

(defun nelisp-eln-native-subr--cxr-import-analysis
    (handle capability code &optional abi-hash first-disp d-reloc-slot)
  "Return verified S6 caar/cadr analysis for CODE in CAPABILITY, or nil.
FIRST-DISP and D-RELOC-SLOT select the shape (see
`nelisp-eln-tail-code-analyze-cxr-call'); unknown ABIs, an
unauthenticated `wrong_type_argument' freloc slot, or a `d_reloc' slot
whose live decoded identity is not the expected predicate symbol all
fail closed."
  (let* ((state (condition-case nil
                    (nelisp-eln-system-loader--state handle)
                  (nelisp-eln-system-loader-error nil)))
         (bias (plist-get state :bias))
         (address (nth 3 capability))
         (abi-hash (or abi-hash (plist-get state :abi-hash)))
         (descriptor (and (equal abi-hash "ba35c031")
                          (nelisp-eln-native-subr--runtime-services-descriptor 0)))
         (vaddr (and (integerp bias) (integerp address) (- address bias)))
         (analysis
          (and descriptor (integerp vaddr) (>= vaddr 0)
               (nelisp-eln-tail-code-analyze-cxr-call
                code vaddr first-disp d-reloc-slot '(0))))
         (freloc-import (car (plist-get analysis :imports)))
         (data-relocation (plist-get analysis :data-relocation)))
    (when (and (eq (plist-get analysis :safe) t) freloc-import data-relocation
               (equal (plist-get freloc-import :slot) 0)
               (equal (plist-get data-relocation :slot) d-reloc-slot))
      (let* ((freloc-got (plist-get freloc-import :got-vaddr))
             (freloc-slot-address (and (integerp freloc-got) (>= freloc-got 0)
                                       (+ bias freloc-got)))
             (freloc-target
              (and freloc-slot-address
                   (condition-case nil
                       (nelisp-eln-system-loader-validate-root-indirection
                        handle freloc-slot-address "freloc_link_table")
                     (nelisp-eln-system-loader-error nil))))
             (d-reloc-got (plist-get data-relocation :got-vaddr))
             (d-reloc-slot-address (and (integerp d-reloc-got) (>= d-reloc-got 0)
                                       (+ bias d-reloc-got)))
             (d-reloc-target
              (and d-reloc-slot-address
                   (condition-case nil
                       (nelisp-eln-system-loader-validate-root-indirection
                        handle d-reloc-slot-address "d_reloc")
                     (nelisp-eln-system-loader-error nil))))
             (metadata
              (and d-reloc-target
                   (condition-case nil
                       (nelisp-eln-metadata-read-with-backend
                        handle #'nelisp-eln-system-loader-symbol-info
                        #'nelisp-eln-system-loader-read-root-object-bytes)
                     (error nil))))
             (relocations (and metadata (plist-get metadata :data-relocations))))
        (when (and (integerp freloc-target) (> freloc-target 0)
                   (integerp d-reloc-target) (> d-reloc-target 0)
                   (or (vectorp relocations) (listp relocations))
                   (> (length relocations) d-reloc-slot)
                   (eq (elt relocations d-reloc-slot) 'listp))
          (setq analysis (plist-put analysis :descriptor descriptor))
          (plist-put analysis :constant-address
                     (+ d-reloc-target (* 8 d-reloc-slot))))))))

(defun nelisp-eln-native-subr--port-number (analysis index)
  "Return the callback port number of ANALYSIS's INDEXth import.
That is INDEX itself, unless ANALYSIS carries `:port-numbers' (S6.11): a
profile whose bodies share one link table gives every freloc slot the same
port number in every body, so one table entry serves each slot for all of
them (see `nelisp-eln-registration--assign-port-numbers')."
  (let ((numbers (plist-get analysis :port-numbers)))
    (if numbers
        (or (nth index numbers)
            (signal 'nelisp-eln-native-subr-error
                    (list 'missing-port-number index)))
      index)))

(defun nelisp-eln-native-subr--port-tags (analysis)
  "Return ((PORT-TAG . SPEC) ...) for ANALYSIS's :PORT-SPECS, in import order."
  (let ((index -1))
    (mapcar (lambda (spec)
              (setq index (1+ index))
              (cons (nelisp-eln-callable-import-port-tag
                     (nelisp-eln-native-subr--port-number analysis index))
                    spec))
            (plist-get analysis :port-specs))))

(defun nelisp-eln-native-subr--resolve-constant-cells (cells lease)
  "Return the (WORD . VALUE) constants of CELLS, read live for this call.
CELLS is ((ADDRESS . VALUE) ...).  A VALUE of the form (:registered-lambda
. INDEX) names the d_reloc slot INDEX that `Fcomp__register_lambda'
overwrote; it resolves to the owner's registered native subr, and only when
the slot still holds exactly the word the owner published there when
registration finished.  LEASE is the calling body's own lease."
  (mapcar
   (lambda (cell)
     (let ((word (nelisp-eln-abi-read-word (car cell) 0))
           (value (cdr cell)))
       (if (eq (car-safe value) :registered-lambda)
           (let* ((index (cdr value))
                  (owner (and (vectorp lease) (aref lease 2)))
                  (plist (and (vectorp owner) (>= (length owner) 19)
                              (aref owner 18)))
                  (callable (cdr (assq index
                                       (plist-get plist :lambda-callables))))
                  (expected (cdr (assq index
                                       (plist-get plist :lambda-slot-words)))))
             (unless (and callable (integerp expected) (eql word expected))
               (signal 'nelisp-eln-native-subr-error
                       (list 'registered-lambda-slot-not-live index word (and callable t) expected)))
             (cons word callable))
         (cons word value))))
   cells))

(defun nelisp-eln-native-subr-import-entries (analysis)
  "Return ((SLOT . ENTRY-ADDRESS) ...) for every import ANALYSIS proves.
A proof with several distinct freloc slots gives each slot its own
slot-identifying callback port (`nelisp-eln-callable-import-port-entry-
address'), in import order, so the dispatcher can tell which
authenticated slot a callback came from; a single-import proof keeps its
existing single entry (`nelisp-eln-native-subr-import-entry-address')."
  ;; No blanket `require' here: the two branches below need different
  ;; `nelisp-eln-callable-import' functions, and each already guards its
  ;; own load precisely (the single-import branch's own
  ;; `nelisp-eln-native-subr-import-entry-address' guards on the exact
  ;; function it calls). A guard here on just one representative symbol
  ;; would incorrectly force-load the module -- clobbering a test's
  ;; `cl-letf' mock of a DIFFERENT one of this module's functions -- even
  ;; on the branch that never uses the symbol being checked.
  (let ((imports (plist-get analysis :imports)))
    ;; Doc 207: the two-import chain leaf uses ports too (Fadd1 is port 0,
    ;; the `Ffuncall' MANY hop port 1).
    (if (memq (plist-get analysis :proof) '(:multi-import-call :chain-call))
        (let ((index 0) (entries nil))
          (unless (fboundp 'nelisp-eln-callable-import-port-entry-address)
            (require 'nelisp-eln-callable-import))
          (dolist (import imports)
            (push (cons (plist-get import :slot)
                        (nelisp-eln-callable-import-port-entry-address
                         (nelisp-eln-native-subr--port-number analysis index)))
                  entries)
            (setq index (1+ index)))
          (nreverse entries))
      (list (cons (plist-get (car imports) :slot)
                  (nelisp-eln-native-subr-import-entry-address analysis))))))

(defun nelisp-eln-native-subr-import-entry-address (analysis)
  "Return the callback entry address ANALYSIS's single import slot uses.
A `:cxr-call' proof imports the fixed two-argument `wrong_type_argument',
which needs the seven-word entry, and so does a `:straight-line-call'
MANY stack call (S6.16 `zerop'): callback1 forwards only %rdi and would
zero-fill the argv pointer in %rsi.  Every other single-import proof (a
unary tail JMP) is served by the callback1 entry."
  (unless (fboundp 'nelisp-eln-callable-import-entry-address)
    (require 'nelisp-eln-callable-import))
  (if (memq (plist-get analysis :proof) '(:cxr-call :straight-line-call))
      (nelisp-eln-callable-import-wide-entry-address)
    (nelisp-eln-callable-import-entry-address)))

(defun nelisp-eln-native-subr--cxr-lease-valid-p
    (lease handle capability &optional require-active)
  "Return non-nil only for LEASE's retained installed callback table.
S6 sibling of `nelisp-eln-native-subr--tail-lease-valid-p', for the
single authenticated `wrong_type_argument' freloc import a `:cxr-call'
proof carries (the `d_reloc' data load is immutable, load-time
constant data, not a live jump table, so it has no analogous
liveness check here -- its identity was already authenticated once,
for good, by `--cxr-import-analysis' at admission time)."
  (let* ((owner (and (vectorp lease) (= (length lease) 8)
                     (eq (aref lease 0)
                         nelisp-eln-native-subr--tail-lease-marker)
                     (aref lease 2)))
         (table-owner (and owner (aref lease 3)))
         (table-address (and owner (aref lease 4)))
         (link-address (and owner (aref lease 5)))
         (entry-address (and owner (aref lease 6)))
         (proof (and owner (aref lease 7)))
         (owner-cap (and (vectorp owner) (>= (length owner) 18)
                         (aref owner 8)))
         (owner-metadata (and (vectorp owner) (>= (length owner) 18)
                              (aref owner 7)))
         (unit (and owner (aref owner 1)))
         (owner-handle (and (vectorp unit) (> (length unit) 1)
                            (aref unit 1)))
         (import (and owner (car (plist-get proof :imports)))))
    (and owner owner-cap (listp capability) (>= (length capability) 8)
         (listp owner-cap) (>= (length owner-cap) 8)
         (eq (aref lease 1) handle)
         (eq owner-handle handle)
         (eq (aref owner 12) table-owner)
         (eq (aref owner 17) lease)
         (or (not require-active)
             (and (boundp 'nelisp-eln-registration--active-owner)
                  (eq nelisp-eln-registration--active-owner owner)))
         (boundp 'nelisp-eln-registration--owners)
         (memq owner nelisp-eln-registration--owners)
         (stringp (plist-get owner-metadata :abi-hash))
         (eq (plist-get proof :safe) t)
         (eq (plist-get proof :proof) :cxr-call)
         (listp (plist-get proof :imports)) (= (length (plist-get proof :imports)) 1)
         (equal (plist-get import :slot) 0)
         (integerp (plist-get import :got-vaddr))
         (>= (plist-get import :got-vaddr) 0)
         (equal (nth 2 capability) (nth 2 owner-cap))
         (equal (nth 3 capability) (nth 3 owner-cap))
         (eq (nth 7 capability) (nth 7 owner-cap))
         (integerp table-address) (> table-address 0)
         (integerp link-address) (> link-address 0)
         (= table-address (nl-ffi-memory-address table-owner))
         (= (ptr-read-u64 link-address 0) table-address)
         (integerp entry-address) (> entry-address 0)
         (= (ptr-read-u64 table-address (* 8 (plist-get import :slot)))
            entry-address)
         (= entry-address
            (nelisp-eln-native-subr-import-entry-address proof)))))

(defun nelisp-eln-native-subr-create-cxr
    (handle name first-disp d-reloc-slot &optional function-name)
  "Create a managed genuine unary S6 caar/cadr subr for NAME.
FIRST-DISP is -3 for `caar', 5 for `cadr'.  D-RELOC-SLOT is the
expected `d_reloc' index of the `listp' predicate constant (5 for both
in the surveyed artifacts)."
  ;; No `require' here: the only `nelisp-eln-callable-import' reference
  ;; below is inside the deferred `bridge' closure (`--call-unary'),
  ;; which is invoked only when the created subr is actually called, not
  ;; at construction time; `--call-unary' guards its own load.
  (let* ((capability
          (nelisp-eln-system-loader-function-capability handle name))
         (size (nth 6 capability))
         (code (and (integerp size) (> size 0)
                    (nelisp-eln-system-loader-read-root-function-bytes
                     handle name 0 size)))
         (cxr-analysis (and code (= (length code) size)
                            (nelisp-eln-native-subr--cxr-import-analysis
                             handle capability code
                             (nelisp-eln-native-subr--tail-import-context-abi-hash)
                             first-disp d-reloc-slot)))
         (lease nelisp-eln-native-subr--tail-import-context)
         (implementation
          (and cxr-analysis
               (let ((impl (plist-get (plist-get cxr-analysis :descriptor)
                                       :implementation)))
                 (if (symbolp impl) (symbol-function impl) impl))))
         (constant-address (plist-get cxr-analysis :constant-address))
         (module-id (nelisp-eln-system-loader-module-id handle)))
    (unless (and cxr-analysis (functionp implementation)
                 (integerp constant-address) (> constant-address 0)
                 (not (symbolp implementation))
                 (nelisp-eln-native-subr--cxr-lease-valid-p
                  lease handle capability t)
                 (or (null function-name)
                     (symbolp function-name)
                     (and (stringp function-name)
                          (= (length function-name)
                             (string-bytes function-name)))))
      (signal 'nelisp-eln-native-subr-error
              (list 'unsupported-native-abi name size)))
    (nelisp-eln-system-loader-validate-function-capability capability)
    (setq function-name
          (cond ((and function-name (symbolp function-name)) function-name)
                ((stringp function-name) (intern function-name))
                (t (intern name))))
    (let ((bridge
           (lambda (argument)
             (unless (nelisp-eln-native-subr--cxr-lease-valid-p
                      lease handle capability)
               (signal 'nelisp-eln-native-subr-error
                       (list 'expired-import-table)))
             ;; The body passes the authenticated `d_reloc' `listp'
             ;; constant to `wrong_type_argument'; read its live word
             ;; (written at registration from the authenticated data
             ;; relocations) so the error callback decodes it by identity.
             (nelisp-eln-callable-import--call-unary
              capability implementation argument 'fixed 2
              (list (cons (nelisp-eln-abi-read-word constant-address 0)
                          'listp))))))
      (unless (functionp bridge)
        (signal 'nelisp-eln-native-subr-error
                (list 'invalid-cxr-bridge name)))
      (nelisp--native-subr-create capability function-name module-id bridge 1))))

(defconst nelisp-eln-native-subr--multi-import-specs
  '((fixnum-range
     :ports ((1317 many 2 (lisp lisp) lisp)
             (1 fixed 2 (lisp raw) bool))
     :constants ((7 . t)))
    (bignum
     :ports ((945 many 2 (lisp lisp) lisp)
             (1 fixed 2 (lisp raw) bool)
             (7 fixed 2 (lisp lisp) bool))
     :constants ((1 . fixnump) (4 . t)))
    (car-eq-constant
     :ports ((7 fixed 2 (lisp lisp) bool))
     :constants ((1 . frame-configuration) (4 . t)))
    (cons-form-constant
     :ports ((1119 fixed 2 (lisp lisp) lisp))
     :constants ((1 . =))
     :arity 2)
    (set-difference
     :ports ((0 fixed 2 (lisp lisp) void)
             (13 fixed 0 () void)
             (14 fixed 0 () void)
             (1217 fixed 2 (lisp lisp) lisp)
             (1119 fixed 2 (lisp lisp) lisp)
             (1209 fixed 1 (lisp) lisp))
     :constants ((0 . nil) (4 . listp))
     :module-counter "quitcounter"
     :arity 2)
    (parse-body
     :ports ((0 fixed 2 (lisp lisp) void)
             (13 fixed 0 () void)
             (14 fixed 0 () void)
             (1376 fixed 1 (lisp) lisp)
             (1354 fixed 1 (lisp) lisp)
             (1119 fixed 2 (lisp lisp) lisp)
             (1209 fixed 1 (lisp) lisp)
             (1217 fixed 2 (lisp lisp) lisp))
     :constants ((0 . nil)
                 (1 . (:documentation declare interactive cl-declare))
                 (5 . listp))
     :module-counter "quitcounter"
     :opaque-argument-symbols t)
    (for-effect-constant
     :ports ((1335 fixed 1 (lisp) lisp)
             (945 many 2 (lisp lisp) lisp)
             (10 fixed 4 (lisp lisp lisp raw) void))
     :constants ((0 . byte-compile--for-effect)
                 (2 . byte-compile-push-constant)))
    (setq-form
     :ports ((1250 fixed 1 (lisp) lisp)
             (1320 many 2 (lisp lisp) lisp)
             (1220 fixed 2 (lisp lisp) lisp)
             (945 many (2 3) (lisp lisp lisp) lisp)
             (1335 fixed 1 (lisp) lisp)
             (10 fixed 4 (lisp lisp lisp raw) void)
             (0 fixed 2 (lisp lisp) void))
     :constants ((3 . byte-compile-form)
                 (4 . byte-compile--for-effect)
                 (5 . byte-compile-variable-set)
                 (6 . byte-compile-out)
                 (7 . byte-dup)
                 (9 . cl--assertion-failed)
                 (10 . (= (length form) 3))
                 (15 . listp))
     :opaque-argument-symbols t)
    (lambda-nth-form
     :ports ((1220 fixed 2 (lisp lisp) lisp)
             (1335 fixed 1 (lisp) lisp)
             (945 many (3) (lisp lisp lisp) lisp))
     :constants ((0 . byte-compile-form)
                 (3 . byte-compile--for-effect))
     :arity 0)
    (lambda-cdr-form
     :ports ((0 fixed 2 (lisp lisp) void)
             (1335 fixed 1 (lisp) lisp)
             (945 many (3) (lisp lisp lisp) lisp))
     :constants ((3 . byte-compile--for-effect)
                 (4 . byte-compile-body)
                 (24 . listp))
     :arity 0)
    (if-form
     :ports ((945 many (1 2 3) (lisp lisp lisp) lisp)
             (1113 many 2 (lisp lisp) handle)
             (1119 fixed 2 (lisp lisp) lisp)
             (10 fixed 4 (lisp lisp lisp raw) void)
             (1335 fixed 1 (lisp) lisp)
             (0 fixed 2 (lisp lisp) void))
     :constants ((0 . byte-compile-form)
                 (3 . byte-compile--for-effect)
                 (5 . byte-compile-make-tag)
                 (7 . byte-compile-goto)
                 (8 . byte-goto-if-nil)
                 (9 . byte-compile--maybe-guarded)
                 (11 . (:bytecode 0 (194 195 192 56 9 34 135)
                                  [V0 byte-compile--for-effect
                                      byte-compile-form 2]
                                  3))
                 (12 . byte-goto)
                 (13 . byte-compile-out-tag)
                 (14 . not)
                 (15 . (:bytecode 0 (194 192 65 65 65 9 34 135)
                                  [V0 byte-compile--for-effect
                                      byte-compile-body]
                                  3))
                 (16 . byte-goto-if-nil-else-pop)
                 (24 . listp))
     :opaque-argument-symbols t)
    (accumulate-forms
     :ports ((1320 many 2 (lisp lisp) lisp)
             (945 many 2 (lisp lisp) lisp)
             (7 fixed 2 (lisp lisp) bool)
             (1354 fixed 1 (lisp) lisp)
             (1119 fixed 2 (lisp lisp) lisp)
             (1209 fixed 1 (lisp) lisp)
             (1196 many 2 (lisp lisp) lisp)
             (1300 fixed 1 (lisp) lisp)
             (0 fixed 2 (lisp lisp) void)
             (13 fixed 0 () void)
             (14 fixed 0 () void))
     :constants ((0 . nil) (1 . macroexp--expand-all) (5 . t) (7 . listp))
     :module-counter "quitcounter"
     :arity 2
     :min-arity 1
     :symbols-with-pos t
     :opaque-argument-symbols t))
  "Per exact multi-import shape (`nelisp-eln-tail-code--multi-import-shapes'):
:PORTS lists, in port order, (SLOT CONVENTION ARITY ARGUMENT-KINDS
RETURN-KIND) for each freloc import -- a MANY slot must have an
authenticated `nelisp-eln-native-subr--many-descriptors' row, a fixed
slot a supported runtime-services descriptor of the same convention and
arity (both tables are cross-checked against the freloc slot table
~/.cache/tmp/slot-auth/freloc-ba35c031.tsv, sha256 3e8591ab..f0758:
1317 `Fleq', 945 `Ffuncall', 1 `helper_PSEUDOVECTOR_TYPEP_XUNTAG', 7
`slow_eq', 1119 `Fcons', 0 `wrong_type_argument', 13 `maybe_gc', 14
`maybe_quit', 1217 `Fmemq', 1209 `Fnreverse', 1335 `Fsymbol_value', 10
`set_internal', 1250 `Flength', 1220 `Fnth').  A MANY port's ARITY may
be a list of argc values when the body calls that one slot with several
argument counts; only the variadic `Ffuncall' row admits that.  Argument
kind `raw' is a C int; return kind `bool' a C bool, `void' no value (%rax is left 0).
:MODULE-COUNTER names the module-local `.bss' counter object the body
reaches by direct RIP-relative access (see
`nelisp-eln-native-subr--module-counter-valid-p'); a shape without it
must not access one.  :ARITY is the body's own fixed Lisp arity (default 1);
only `nelisp-eln-native-subr-create-multi-binary' builds an arity-2 body.
:CONSTANTS lists (D-RELOC-INDEX . VALUE) for every `d_reloc' constant the
body reads; each must decode to exactly VALUE in the artifact's own data
relocations (a symbol or immediate by identity, a quoted list constant,
which no admitted body mutates, by `equal'), and the live words are then
decoded by identity when the body passes or returns them.
:MIN-ARITY (default :ARITY) is the smallest argument count the Lisp
function accepts: a lower one makes it `(MIN-ARITY &optional ...)' up to
:ARITY, the missing ones being nil exactly as GNU pads them.
:SYMBOLS-WITH-POS non-nil admits the body's reads of
f_symbols_with_pos_enabled_reloc in a binary shape (the cell stays zero).
:OPAQUE-ARGUMENT-SYMBOLS non-nil states that the exact body never reads a
symbol's cells inline (it only compares, stores and passes symbol words
to authenticated ports), so interned symbols with non-empty global state
reachable from its arguments or port results may cross as opaque views
\(see `nelisp-eln-objects--admit-opaque-interned-symbols').
A constant written (:bytecode ARGS BYTES CONSTANTS DEPTH) is a byte-code
function object whose four components must equal those values (BYTES a
list of byte values); the analysis then carries the live object.  Return
kind `handle' answers an opaque, identity-preserving word for an object
that only ever flows on to other authenticated ports (see
`nelisp-eln-callable-import--dispatch-port').")

;; S6.6 (`cconv-closure-convert'): kept in its own constant, appended to the
;; spec list, so concurrent lanes adding specs do not touch the same lines.
;; Slot 12 `specbind' and slot 4 `helper_unbind_n' are the runtime-services
;; unwind-stack services; the body's three specbinds are undone by its own
;; `helper_unbind_n' (3) on a normal return and, on any non-local exit past
;; the native frame, by `nelisp-eln-callable-import--call-unary'.
(defconst nelisp-eln-native-subr--multi-import-specs-cconv
  '((closure-convert
     :ports ((12 fixed 2 (lisp lisp) void)
             (945 many (2 3 4) (lisp lisp lisp lisp) lisp)
             (1335 fixed 1 (lisp) lisp)
             (1209 fixed 1 (lisp) lisp)
             (10 fixed 4 (lisp lisp lisp raw) void)
             (4 fixed 1 (lisp) void))
     :constants ((1 . cconv-var-classification)
                 (2 . cconv-freevars-alist)
                 (3 . cconv--dynbound-variables)
                 (4 . cconv-analyze-form)
                 (5 . cconv-convert)
                 (7 . cl--assertion-failed)
                 (8 . (null cconv-freevars-alist)))
     ;; `(FORM &optional DYNBOUND-VARS)': one required argument (S6.4's
     ;; preflight compares this with the registration's own MINARGS).
     :arity 2 :min-arity 1
     :opaque-argument-symbols t))
  "Dispatcher specs for the vendor cconv.el shapes of
`nelisp-eln-tail-code--multi-import-shapes-cconv'; see
`nelisp-eln-native-subr--multi-import-specs' for the format.")

(unless (assq 'closure-convert nelisp-eln-native-subr--multi-import-specs)
  (setq nelisp-eln-native-subr--multi-import-specs
        (append nelisp-eln-native-subr--multi-import-specs
                nelisp-eln-native-subr--multi-import-specs-cconv)))

;; S6.4 (`macroexpand-1'): kept in its own constant, appended to the spec
;; list, like the cconv one above.  :MIN-ARITY 1 with :ARITY 2 is the
;; `(FORM &optional ENVIRONMENT)' registration (see
;; `nelisp-eln-native-subr-multi-min-arity'); :SYMBOLS-WITH-POS declares that
;; the body reads the `symbols_with_pos_enabled' byte through
;; f_symbols_with_pos_enabled_reloc (see
;; `nelisp-eln-native-subr-multi-swp-declared-p').
(defconst nelisp-eln-native-subr--multi-import-specs-macroexpand
  '((macroexpand-1
     :ports ((0 fixed 2 (lisp lisp) void)
             (7 fixed 2 (lisp lisp) bool)
             (945 many 2 (lisp lisp) lisp)
             (946 many 2 (lisp lisp) lisp)
             (948 fixed 3 (lisp lisp lisp) lisp)
             (1119 fixed 2 (lisp lisp) lisp)
             (1215 fixed 2 (lisp lisp) lisp)
             (1339 fixed 1 (lisp) lisp)
             (1350 fixed 1 (lisp) lisp)
             (1378 fixed 1 (lisp) lisp))
     :constants ((3 . macro) (5 . macrop) (7 . t) (9 . listp))
     :arity 2 :min-arity 1 :symbols-with-pos t
     :opaque-argument-symbols t))
  "Dispatcher specs for the vendor macroexp.el shapes of
`nelisp-eln-tail-code--multi-import-shapes-macroexpand'; see
`nelisp-eln-native-subr--multi-import-specs' for the format.  Verified
against ~/.cache/tmp/slot-auth/freloc-ba35c031.tsv (sha256 3e8591ab..f0758):
948 `Fautoload_do_load', 1215 `Fassq', 1339 `Ffboundp', 1350
`Fsymbol_function', 1378 `Fsymbolp', 946 `Fapply', 945 `Ffuncall', 1119
`Fcons', 7 `slow_eq', 0 `wrong_type_argument'.")

(unless (assq 'macroexpand-1 nelisp-eln-native-subr--multi-import-specs)
  (setq nelisp-eln-native-subr--multi-import-specs
        (append nelisp-eln-native-subr--multi-import-specs
                nelisp-eln-native-subr--multi-import-specs-macroexpand)))

;; S6.9 (`byte-compile-lambda'): kept in its own constant, appended to the
;; spec list.  Ports 13 `maybe_gc' and 14 `maybe_quit' are reached only
;; through the body's exact local `maybe_gc_quit' helper (see the shape's
;; :HELPER); the body reads GNU's `symbols_with_pos_enabled' cell through
;; f_symbols_with_pos_enabled_reloc and never enables symbols with position.
(defconst nelisp-eln-native-subr--multi-import-specs-lambda
  '((lambda-form
     :ports ((1354 fixed 1 (lisp) lisp)
             (945 many (2 3 4 5 6) (lisp lisp lisp lisp lisp lisp) lisp)
             (7 fixed 2 (lisp lisp) bool)
             (1335 fixed 1 (lisp) lisp)
             (1236 many (2 3) (lisp lisp lisp) lisp)
             (12 fixed 2 (lisp lisp) void)
             (1376 fixed 1 (lisp) lisp)
             (1215 fixed 2 (lisp lisp) lisp)
             (1217 fixed 2 (lisp lisp) lisp)
             (1119 fixed 2 (lisp lisp) lisp)
             (1218 fixed 2 (lisp lisp) lisp)
             (1117 many 2 (lisp lisp) lisp)
             (946 many 4 (lisp lisp lisp lisp) handle)
             (1263 fixed 3 (lisp lisp lisp) lisp)
             (1392 fixed 1 (lisp) lisp)
             (1323 fixed 3 (lisp lisp lisp) lisp)
             (4 fixed 1 (lisp) void)
             (10 fixed 4 (lisp lisp lisp raw) void)
             (951 fixed 2 (lisp lisp) void)
             (13 fixed 0 () void)
             (14 fixed 0 () void))
     :constants ((0 . lambda) (1 . nil) (2 . byte-compile-check-lambda-list)
                 (3 . byte-run-strip-symbol-positions)
                 (4 . byte-compile-arglist-vars) (6 . lexical-binding)
                 (7 . byte-compile-bound-variables) (8 . interactive)
                 (9 . byte-compile-top-level) (10 . progn) (11 . byte-code)
                 (13 . make-byte-code) (14 . bytecomp--code-strings)
                 (15 . byte-native-compiling) (18 . byte-to-native-lambdas-h)
                 (20 . cl-struct-byte-to-native-lambda-tags)
                 (22 . wrong-type-argument) (23 . byte-to-native-lambda)
                 (25 . byte-compile-make-args-desc)
                 (26 . cl--assertion-failed)
                 (27 . (eq 'byte-code (car-safe compiled)))
                 (28 . byte-compile-make-lambda-lexenv)
                 (29 . byte-compile-warn-x)
                 (30 . "malformed interactive spec: %s")
                 (31 . seq-every-p) (32 . symbolp)
                 (33 . (let let* progn save-excursion)) (34 . list)
                 (35 . "malformed `interactive' specification: %s")
                 (36 . byte-compile--docstring) (37 . "")
                 (38 . is-a-value) (39 . byte-compile--known-dynamic-vars)
                 (40 . byte-compile--warn-lexical-dynamic)
                 (41 . help-add-fundoc-usage) (42 . error)
                 (43 . "Not a lambda list: %S") (46 . t) (48 . listp))
     :arity 2 :min-arity 1 :symbols-with-pos t
     :opaque-argument-symbols t :opaque-vectors t
     :helper-counter "quitcounter"))
  "Dispatcher spec for `nelisp-eln-tail-code--multi-import-shapes-lambda';
see `nelisp-eln-native-subr--multi-import-specs' for the format.
:HELPER-COUNTER names the module-local counter the shape's :HELPER bumps.")

(unless (assq 'lambda-form nelisp-eln-native-subr--multi-import-specs)
  (setq nelisp-eln-native-subr--multi-import-specs
        (append nelisp-eln-native-subr--multi-import-specs
                nelisp-eln-native-subr--multi-import-specs-lambda)))

;; S6.11 (`byte-compile-make-closure'): the body and its three registered
;; native lambdas, in their own constant appended to the spec list.  Port
;; return kind `handle-nil' answers Qnil (word 0) for a nil result and an
;; opaque, identity-preserving handle otherwise; it is used for every port
;; whose result the exact body only tests for nil, passes on to a later port
;; or returns (see `nelisp-eln-tail-code--multi-import-shapes-closure').  The
;; ports whose result the body reads inline -- `Flength' and `Fsub1'
;; (fixnums), `Fgtr' and `Fbyte_code_function_p' (booleans), `Fsymbol_value'
;; and the `Fmapcar' list whose `cdr' the docstring path reads -- keep the
;; ordinary `lisp' encoding.  A constant `:registered-lambda' names a d_reloc
;; slot GNU's `Fcomp__register_lambda' overwrites: it is resolved per call to
;; the owner's registered native subr (see
;; `nelisp-eln-native-subr--resolve-constant-cells').
(defconst nelisp-eln-native-subr--multi-import-specs-closure
  '((lambda-intern-format
     :ports ((704 many 2 (lisp lisp) handle-nil)
             (1006 fixed 2 (lisp lisp) handle-nil))
     :constants ((2 . "V%d"))
     :arity 1)
    (lambda-aref-form
     :ports ((1324 fixed 2 (lisp lisp) handle-nil))
     :constants nil
     :arity 1)
    (lambda-cons-form
     :ports ((1119 fixed 2 (lisp lisp) handle-nil))
     :constants ((4 . quote))
     :arity 1
     :opaque-argument-symbols t)
    (make-closure-form
     :ports ((1335 fixed 1 (lisp) lisp)
             (10 fixed 4 (lisp lisp lisp raw) void)
             (1220 fixed 2 (lisp lisp) handle-nil)
             (1221 fixed 2 (lisp lisp) handle-nil)
             (1119 fixed 2 (lisp lisp) handle-nil)
             (1250 fixed 1 (lisp) lisp)
             (945 many (2 3) (lisp lisp lisp) handle-nil)
             (1318 many 2 (lisp lisp) lisp)
             (1364 fixed 1 (lisp) lisp)
             (1300 fixed 1 (lisp) lisp)
             (1195 fixed 2 (lisp lisp) lisp)
             (1113 many 2 (lisp lisp) handle)
             (1324 fixed 2 (lisp lisp) handle-nil)
             (1234 many 2 (lisp lisp) handle-nil)
             (946 many 6 (lisp lisp lisp lisp lisp lisp) handle)
             (1236 many 2 (lisp lisp) handle-nil)
             (0 fixed 2 (lisp lisp) void))
     :constants ((6 . byte-compile--for-effect) (10 . byte-compile-lambda)
                 (11 . lambda) (13 . byte-compile-form)
                 (14 . macroexp-const-p) (15 . make-byte-code)
                 (17 . vconcat) (18 . vector)
                 (19 . byte-run-strip-symbol-positions)
                 (21 . :registered-lambda) (24 . :registered-lambda)
                 (25 . number-sequence) (26 . make-closure)
                 (27 . (:bytecode 257 (192 1 72 135) [V0] 3 "

(fn I)"))
                 (29 . eval) (30 . t) (31 . cl--assertion-failed)
                 (32 . (byte-code-function-p fun))
                 (33 . (or (> (length env) 0) docstring-exp))
                 (39 . listp))
     :arity 1
     :opaque-argument-symbols t :opaque-vectors t))
  "Dispatcher spec for `nelisp-eln-tail-code--multi-import-shapes-closure';
see `nelisp-eln-native-subr--multi-import-specs' for the format.")

(unless (assq 'make-closure-form nelisp-eln-native-subr--multi-import-specs)
  (setq nelisp-eln-native-subr--multi-import-specs
        (append nelisp-eln-native-subr--multi-import-specs
                nelisp-eln-native-subr--multi-import-specs-closure)))

(defun nelisp-eln-native-subr--multi-reject (reason &rest detail)
  "Reject an exactly matched multi-import body for REASON with DETAIL.
Once CODE matches a `nelisp-eln-tail-code--multi-import-shapes' template
the body is that shape, so every later authentication failure is an
explicit, reasoned rejection -- never a quiet nil."
  (signal 'nelisp-eln-native-subr-error
          (cons 'multi-import-not-admitted (cons reason detail))))

(defconst nelisp-eln-native-subr--many-service-descriptors
  '(("ba35c031" 1113 2) ("ba35c031" 1196 2)
    ;; S6.11: `Fformat' (1, MANY) and `Fvconcat' (0, MANY), both called with
    ;; two arguments; NeLisp's own `format'/`vconcat' are Lisp functions,
    ;; not builtin cells, so they are runtime services.
    ("ba35c031" 704 2) ("ba35c031" 1234 2))
  "Authenticated (ABI SLOT ARITY) MANY slots answered by a runtime service
implementation rather than a canonical builtin: 1113 is `Fmake_closure' and
1196 is `Fnconc' \(src/alloc.c, 1 MANY; src/fns.c, 0 MANY), whose NeLisp implementations
are `nelisp-eln-runtime-services-fmake-closure' and `-fnconc'.  Cross-checked against
freloc-ba35c031.tsv row 1113 and the runtime-services descriptor table.")

(defun nelisp-eln-native-subr--many-service-implementation (abi-hash slot arity)
  "Return the runtime-services implementation of MANY service SLOT, or nil.
Nil when ABI-HASH and SLOT are not an authenticated
`nelisp-eln-native-subr--many-service-descriptors' row; a row whose
ARITY or runtime-services descriptor does not authenticate is rejected."
  (let ((row (cl-find-if (lambda (r) (and (equal abi-hash (nth 0 r))
                                          (equal slot (nth 1 r))))
                         nelisp-eln-native-subr--many-service-descriptors)))
    (when row
      (let* ((d (nelisp-eln-native-subr--runtime-services-descriptor slot))
             (impl (plist-get d :implementation)))
        (unless (and (equal (nth 2 row) arity) d
                     (eq (plist-get d :status) 'supported)
                     (eq (plist-get d :convention) 'many))
          (nelisp-eln-native-subr--multi-reject
           'unauthenticated-many-service-slot slot))
        (if (symbolp impl) (symbol-function impl) impl)))))

(defun nelisp-eln-native-subr--multi-port-spec (abi-hash port)
  "Return the authenticated dispatcher spec for PORT, or reject."
  (let* ((slot (nth 0 port)) (convention (nth 1 port)) (arity (nth 2 port))
         (implementation
          (if (eq convention 'many)
              (or
               (nelisp-eln-native-subr--many-service-implementation
                abi-hash slot arity)
              (let ((d (nelisp-eln-native-subr--many-descriptor
                        abi-hash slot arity)))
                ;; A list ARITY (several argc values through one slot) is
                ;; admitted only for the variadic `Ffuncall' row, with one
                ;; `lisp' argument kind per position of the largest argc.
                (unless (and d (if (consp arity)
                                   (and (memq (nth 2 d) '(funcall append))
                                        (cl-every (lambda (n)
                                                    (and (natnump n) (<= 1 n 8)))
                                                  arity)
                                        (equal (nth 3 port)
                                               (make-list (apply #'max arity)
                                                          'lisp)))
                                 (equal (nth 3 d) arity)))
                  (nelisp-eln-native-subr--multi-reject
                   'unauthenticated-many-slot slot))
                (nelisp-eln-native-subr--canonical-builtin d)))
            (let* ((d (nelisp-eln-native-subr--runtime-services-descriptor
                       slot))
                   (impl (plist-get d :implementation)))
              (unless (and d (eq (plist-get d :status) 'supported)
                           (eq (plist-get d :convention) convention)
                           (equal (plist-get d :arity) arity))
                (nelisp-eln-native-subr--multi-reject
                 'unauthenticated-fixed-slot slot))
              (if (symbolp impl) (symbol-function impl) impl)))))
    (list :slot slot :convention convention :arity arity
          :arguments (nth 3 port) :return (nth 4 port)
          :implementation implementation)))

(defun nelisp-eln-native-subr--elf-sections (bytes)
  "Return every section header of ELF BYTES as (NAME TYPE FLAGS ADDR
OFFSET SIZE LINK), in section-index order, or nil if they are malformed."
  (let* ((u #'nelisp-eln-system-loader--file-u)
         (n (length bytes))
         (shoff (and (>= n 64) (funcall u bytes 40 8)))
         (shentsize (and shoff (funcall u bytes 58 2)))
         (shnum (and shoff (funcall u bytes 60 2)))
         (shstrndx (and shoff (funcall u bytes 62 2))))
    (when (and shoff (= shentsize 64) (< 0 shnum) (< shstrndx shnum)
               (<= (+ shoff (* shnum 64)) n))
      (let* ((names (funcall u bytes (+ shoff (* shstrndx 64) 24) 8))
             (sections nil) (i 0))
        (while (< i shnum)
          (let* ((h (+ shoff (* i 64)))
                 (name-off (+ names (funcall u bytes h 4)))
                 (end name-off))
            (while (and (< end n) (/= (aref bytes end) 0))
              (setq end (1+ end)))
            (push (list (and (< name-off n) (substring bytes name-off end))
                        (funcall u bytes (+ h 4) 4) (funcall u bytes (+ h 8) 8)
                        (funcall u bytes (+ h 16) 8) (funcall u bytes (+ h 24) 8)
                        (funcall u bytes (+ h 32) 8) (funcall u bytes (+ h 40) 4))
                  sections))
          (setq i (1+ i)))
        (nreverse sections)))))

(defun nelisp-eln-native-subr--module-counter-valid-p
    (bytes vaddr name dynamic-symbols)
  "Return non-nil only if VADDR is exactly the artifact's own counter NAME.
BYTES is the handle's integrity-checked file image.  A GNU native body
bumps its module-local `static int quitcounter' by direct RIP-relative
access, never through a GOT slot, and the symbol is LOCAL, so it lives
only in `.symtab'.  Admitted exactly when: `.symtab' holds one and only
one symbol NAME, a LOCAL OBJECT of size 4 whose section is the
writable, allocated, non-executable NOBITS `.bss'; its value is VADDR,
four-byte aligned and wholly inside `.bss'; and no `.dynsym' object in
DYNAMIC-SYMBOLS (the loader's own root symbol table) overlaps those
four bytes, so the body can never write into d_reloc, the freloc link
cell or any other authenticated root object through it."
  (let* ((u #'nelisp-eln-system-loader--file-u)
         (sections (and (stringp bytes) (integerp vaddr)
                        (nelisp-eln-native-subr--elf-sections bytes)))
         (bss-index (cl-position ".bss" sections
                                 :key #'car :test #'equal))
         (bss (and bss-index (nth bss-index sections)))
         (symtab (cl-find 2 sections :key #'cadr))
         (strtab (and symtab (< (nth 6 symtab) (length sections))
                      (nth (nth 6 symtab) sections)))
         (matches nil))
    (when (and bss strtab
               (= (nth 1 bss) 8)                ; SHT_NOBITS
               (= (logand (nth 2 bss) 7) 3)     ; SHF_WRITE|SHF_ALLOC, no EXECINSTR
               (= (nth 1 strtab) 3)             ; SHT_STRTAB
               (= (% (nth 5 symtab) 24) 0)
               (<= (+ (nth 4 symtab) (nth 5 symtab)) (length bytes))
               (<= (+ (nth 4 strtab) (nth 5 strtab)) (length bytes)))
      (let ((i 0) (count (/ (nth 5 symtab) 24)))
        (while (< i count)
          (let* ((sym (+ (nth 4 symtab) (* i 24)))
                 (name-off (+ (nth 4 strtab) (funcall u bytes sym 4)))
                 (want (length name)))
            (when (and (< (+ name-off want) (+ (nth 4 strtab) (nth 5 strtab)))
                       (equal (substring bytes name-off (+ name-off want)) name)
                       (= (aref bytes (+ name-off want)) 0))
              (push (list (funcall u bytes (+ sym 4) 1)
                          (funcall u bytes (+ sym 6) 2)
                          (funcall u bytes (+ sym 8) 8)
                          (funcall u bytes (+ sym 16) 8))
                    matches)))
          (setq i (1+ i))))
      (let ((sym (car matches)))
        (and (= (length matches) 1)
             (= (nth 0 sym) 1)                  ; STB_LOCAL, STT_OBJECT
             (= (nth 1 sym) bss-index)
             (= (nth 2 sym) vaddr)
             (= (nth 3 sym) 4)
             (= (% vaddr 4) 0)
             (<= (nth 3 bss) vaddr)
             (<= (+ vaddr 4) (+ (nth 3 bss) (nth 5 bss)))
             (hash-table-p dynamic-symbols)
             (catch 'overlap
               (maphash (lambda (_ entry)
                          (let ((start (plist-get entry :value))
                                (size (max 1 (or (plist-get entry :size) 0))))
                            (when (and (integerp start)
                                       (< start (+ vaddr 4))
                                       (< vaddr (+ start size)))
                              (throw 'overlap nil))))
                        dynamic-symbols)
               t))))))

(defun nelisp-eln-native-subr--bytecode-spec-p (expected)
  "Non-nil when EXPECTED is a (:bytecode ARGS BYTES CONSTANTS DEPTH) spec."
  (and (consp expected) (eq (car expected) :bytecode)))

(defun nelisp-eln-native-subr--closure-slot (closure index)
  "Return byte-code function CLOSURE's slot INDEX.
Called through the `aref' function cell, never the inlined VM opcode: the
compiled `aref' opcode does not read byte-code function objects in this
runtime, while the function itself does."
  (funcall (symbol-function 'aref) closure index))

(defun nelisp-eln-native-subr--deep-equal (a b)
  "Return non-nil when A and B are `equal', vectors compared element-wise.
Called through the `equal' function cell: the compiled `equal' opcode
compares vectors only by identity in this runtime, while the function
recurses into them."
  (funcall (symbol-function 'equal) a b))

(defun nelisp-eln-native-subr--bytecode-matches-p (actual spec)
  "Non-nil when ACTUAL is a byte-code function object equal to SPEC.
SPEC is (ARGS BYTES CONSTANTS DEPTH), BYTES a list of byte values, or
\(ARGS BYTES CONSTANTS DEPTH DOC) for an object that carries a fifth
documentation slot (S6.11's closure prototype)."
  (and (byte-code-function-p actual)
       (proper-list-p spec) (memq (length spec) '(4 5))
       (nelisp-eln-native-subr--deep-equal
        (nelisp-eln-native-subr--closure-slot actual 0) (nth 0 spec))
       (stringp (nelisp-eln-native-subr--closure-slot actual 1))
       (equal (mapcar (lambda (c) (if (>= c #x3fff80) (- c #x3fff00) c))
                      (append (nelisp-eln-native-subr--closure-slot actual 1)
                              nil))
              (nth 1 spec))
       (nelisp-eln-native-subr--deep-equal
        (nelisp-eln-native-subr--closure-slot actual 2) (nth 2 spec))
       (nelisp-eln-native-subr--deep-equal
        (nelisp-eln-native-subr--closure-slot actual 3) (nth 3 spec))
       ;; Exactly four (or, with a documentation slot, five) slots.
       (eql (funcall (symbol-function 'length) actual) (length spec))
       ;; The documentation slot, when the spec has one, equals it exactly.
       (or (= (length spec) 4)
           (let ((doc (nelisp-eln-native-subr--closure-slot actual 4)))
             (and (stringp doc) (equal doc (nth 4 spec)))))))

(defun nelisp-eln-native-subr--multi-constant-matches-p (actual expected)
  "Non-nil when d_reloc constant ACTUAL is EXPECTED.
Symbols and immediates by identity; a byte-code constant by its
components; any other quoted list constant (never mutated by an admitted
body) by `equal'."
  (cond ((eq actual expected) t)
        ;; A d_reloc slot `Fcomp__register_lambda' overwrites (S6.11): the
        ;; file holds GNU's own "#$" placeholder there.
        ((eq expected :registered-lambda)
         (and (stringp actual) (equal actual "#$")))
        ;; A string constant (never mutated by an admitted body) by `equal'.
        ((stringp expected) (and (stringp actual) (equal actual expected)))
        ((nelisp-eln-native-subr--bytecode-spec-p expected)
         (nelisp-eln-native-subr--bytecode-matches-p actual (cdr expected)))
        ((consp expected) (equal actual expected))))

;; S6.9: the local `maybe_gc_quit' helper preceding a body.
(defun nelisp-eln-native-subr--helper-analysis
    (handle analysis spec function-vaddr freloc-got)
  "Authenticate ANALYSIS's :HELPER region, exactly :BACK bytes before the
body at FUNCTION-VADDR: its on-file bytes must match the helper template
\(`nelisp-eln-tail-code-analyze-helper'), its one freloc GOT load must
reach FREELOC-GOT (the body's own), and its module-counter accesses must
all reach one and the same object that
`nelisp-eln-native-subr--module-counter-valid-p' authenticates as SPEC's
:HELPER-COUNTER.  The registration preflight separately requires the live
bytes to equal the file's (`nelisp-eln-registration--raw-read-bytes' on
the returned range).  Return (ADDRESS . SIZE) of the region."
  (let* ((helper (plist-get analysis :helper))
         (state (nelisp-eln-system-loader--state handle))
         (bias (plist-get state :bias))
         (file-bytes (plist-get state :file-bytes))
         (back (plist-get helper :back))
         (vaddr (and (integerp back) (- function-vaddr back)))
         ;; Executable PT_LOAD (flags bit 0) containing the whole region.
         (row (and vaddr
                   (catch 'found
                     (dolist (r (plist-get (plist-get state :elf) :loads))
                       (when (and (/= 0 (logand (nth 4 r) 1))
                                  (>= vaddr (nth 0 r))
                                  (<= (+ (- vaddr (nth 0 r)) back) (nth 2 r)))
                         (throw 'found r)))
                     nil))))
    (unless (and row (stringp (plist-get spec :helper-counter)))
      (nelisp-eln-native-subr--multi-reject 'malformed-helper-spec vaddr))
    (let* ((file-offset (+ (nth 1 row) (- vaddr (nth 0 row))))
           (bytes (substring file-bytes file-offset (+ file-offset back)))
           (result (nelisp-eln-tail-code-analyze-helper
                    bytes helper function-vaddr)))
      (unless result
        (nelisp-eln-native-subr--multi-reject 'helper-instructions vaddr))
      (unless (and (= (plist-get result :freloc) freloc-got)
                   (nelisp-eln-native-subr--module-counter-valid-p
                    file-bytes (plist-get result :counter)
                    (plist-get spec :helper-counter)
                    (plist-get (plist-get state :elf) :symbols)))
        (nelisp-eln-native-subr--multi-reject
         'unauthenticated-helper (plist-get result :counter)
         (plist-get result :freloc) freloc-got))
      (cons (+ bias vaddr) back))))

(defun nelisp-eln-native-subr-multi-import-analysis
    (handle capability code &optional abi-hash)
  "Return verified S6 multi-import analysis for CODE in CAPABILITY.
Return nil only when CODE matches no
`nelisp-eln-tail-code--multi-import-shapes' template (not this shape at
all).  A matching body must then authenticate completely -- every import
slot and `d_reloc' constant against its shape's
`nelisp-eln-native-subr--multi-import-specs' entry, and the freloc,
`d_reloc' and (when read) f_symbols_with_pos_enabled_reloc GOT slots as
root indirections -- or this signals `nelisp-eln-native-subr-error'
\(multi-import-not-admitted REASON ...), or the loader's own error for
an invalid root slot.  The result adds :PORT-SPECS, :CONSTANTS,
:D-RELOC-ADDRESS and :SYMBOLS-WITH-POS-ADDRESS to the tail-code analysis."
  (let* ((state (nelisp-eln-system-loader--state handle))
         (bias (plist-get state :bias))
         (address (nth 3 capability))
         (abi-hash (or abi-hash (plist-get state :abi-hash)))
         (vaddr (and (integerp bias) (integerp address) (- address bias)))
         (analysis (and (equal abi-hash "ba35c031")
                        (integerp vaddr) (>= vaddr 0)
                        (nelisp-eln-tail-code-analyze-multi-import-call
                         code vaddr))))
    (when analysis
      (let* ((spec (cdr (assq (plist-get analysis :shape)
                              nelisp-eln-native-subr--multi-import-specs)))
             (imports (plist-get analysis :imports))
             (data (plist-get analysis :data-relocations))
             (constants (plist-get spec :constants))
             (freloc-got (plist-get (car imports) :got-vaddr))
             (d-reloc-got (plist-get (car data) :got-vaddr))
             (swp-got (plist-get analysis :symbols-with-pos-got))
             (counter-vaddr (plist-get analysis :module-counter-vaddr))
             (counter-name (plist-get spec :module-counter)))
        (unless (and (eq (plist-get analysis :safe) t) spec)
          (nelisp-eln-native-subr--multi-reject
           'shape-without-spec (plist-get analysis :shape)))
        ;; A direct module-local counter access is admitted only for a
        ;; shape whose spec names it, and only at that exact object.
        (unless (if counter-name
                    (and counter-vaddr
                         (nelisp-eln-native-subr--module-counter-valid-p
                          (plist-get state :file-bytes) counter-vaddr
                          counter-name
                          (plist-get (plist-get state :elf) :symbols)))
                  (null counter-vaddr))
          (nelisp-eln-native-subr--multi-reject
           'unauthenticated-module-counter counter-vaddr counter-name))
        (unless (equal (mapcar (lambda (i) (plist-get i :slot)) imports)
                       (mapcar #'car (plist-get spec :ports)))
          (nelisp-eln-native-subr--multi-reject 'import-slots-mismatch imports))
        (unless (and (cl-every (lambda (i) (equal (plist-get i :got-vaddr)
                                                  freloc-got))
                               imports)
                     (cl-every (lambda (d) (equal (plist-get d :got-vaddr)
                                                  d-reloc-got))
                               data)
                     (equal (mapcar (lambda (d) (plist-get d :slot)) data)
                            (mapcar #'car constants)))
          (nelisp-eln-native-subr--multi-reject 'got-shape-mismatch
                                                imports data))
        (let* ((port-specs
                (mapcar (lambda (port)
                          (nelisp-eln-native-subr--multi-port-spec
                           abi-hash port))
                        (plist-get spec :ports)))
               ;; Each validator signals its own reasoned loader error.
               (freloc-target
                (nelisp-eln-system-loader-validate-root-indirection
                 handle (+ bias freloc-got) "freloc_link_table"))
               (d-reloc-target
                ;; A shape that reads no d_reloc constant has no d_reloc GOT.
                (and d-reloc-got
                     (nelisp-eln-system-loader-validate-root-indirection
                      handle (+ bias d-reloc-got) "d_reloc")))
               (swp-target
                (and swp-got
                     (nelisp-eln-system-loader-validate-root-indirection
                      handle (+ bias swp-got)
                      "f_symbols_with_pos_enabled_reloc")))
               (metadata (nelisp-eln-metadata-read-with-backend
                          handle #'nelisp-eln-system-loader-symbol-info
                          #'nelisp-eln-system-loader-read-root-object-bytes))
               (relocations (plist-get metadata :data-relocations)))
          (unless (and (integerp freloc-target) (> freloc-target 0)
                       (or (and (null d-reloc-got) (null data))
                           (and (integerp d-reloc-target) (> d-reloc-target 0)))
                       (or (null swp-got)
                           (and (integerp swp-target) (> swp-target 0))))
            (nelisp-eln-native-subr--multi-reject
             'invalid-root-target freloc-target d-reloc-target swp-target))
          (unless (and (or (vectorp relocations) (listp relocations))
                       (cl-every (lambda (c)
                                   (and (< (car c) (length relocations))
                                        (nelisp-eln-native-subr--multi-constant-matches-p
                                         (elt relocations (car c)) (cdr c))))
                                 constants))
            (nelisp-eln-native-subr--multi-reject 'constant-identity-mismatch
                                                  constants))
          ;; A byte-code constant travels as the live object.
          (setq constants
                (mapcar (lambda (c)
                          (if (nelisp-eln-native-subr--bytecode-spec-p (cdr c))
                              (cons (car c) (elt relocations (car c)))
                            c))
                        constants))
          (when (plist-get analysis :helper)
            (setq analysis (plist-put analysis :helper-range
                                      (nelisp-eln-native-subr--helper-analysis
                                       handle analysis spec vaddr freloc-got))))
          (setq analysis (plist-put analysis :port-specs port-specs))
          (setq analysis (plist-put analysis :constants constants))
          (setq analysis (plist-put analysis :d-reloc-address d-reloc-target))
          (setq analysis (plist-put analysis :module-counter-address
                                    (and counter-vaddr (+ bias counter-vaddr))))
          (plist-put analysis :symbols-with-pos-address swp-target))))))

(defun nelisp-eln-native-subr--multi-lease-valid-p
    (lease handle capability &optional require-active)
  "Return non-nil only for LEASE's retained multi-port callback table.
Like `nelisp-eln-native-subr--cxr-lease-valid-p', but for a proof with
several imports: LEASE's entry field holds ((SLOT . ENTRY) ...) exactly
as `nelisp-eln-native-subr-import-entries' computes it for the proof,
and every one of those live table slots must still hold its own port.
A body that reads f_symbols_with_pos_enabled_reloc additionally needs
that cell to still point at the owner's own zero byte (symbols with
position are never enabled in NeLisp)."
  (let* ((owner (and (vectorp lease) (= (length lease) 8)
                     (eq (aref lease 0)
                         nelisp-eln-native-subr--tail-lease-marker)
                     (aref lease 2)))
         (table-owner (and owner (aref lease 3)))
         (table-address (and owner (aref lease 4)))
         (link-address (and owner (aref lease 5)))
         (entries (and owner (aref lease 6)))
         (proof (and owner (aref lease 7)))
         ;; A pair profile's second body holds its own lease, retained in
         ;; the owner's role plist as :lease2 next to its :leaf-cap2.
         (second (and (vectorp owner) (>= (length owner) 19)
                      (listp (aref owner 18))
                      (eq (plist-get (aref owner 18) :lease2) lease)))
         ;; S6.11: each registered native lambda holds its own lease over the
         ;; same retained table, kept as (LEASE . CAPABILITY) in :lambda-leases.
         (lambda-entry (and (vectorp owner) (>= (length owner) 19)
                            (listp (aref owner 18))
                            (assq lease (plist-get (aref owner 18)
                                                   :lambda-leases))))
         (owner-cap (and (vectorp owner) (>= (length owner) 18)
                         (cond (lambda-entry (cdr lambda-entry))
                               (second
                                (plist-get (aref owner 18) :leaf-cap2))
                               (t (aref owner 8)))))
         (unit (and owner (aref owner 1)))
         (owner-handle (and (vectorp unit) (> (length unit) 1)
                            (aref unit 1)))
         (swp-address (plist-get proof :symbols-with-pos-address))
         (swp-memory (and owner (vectorp owner) (>= (length owner) 19)
                          (plist-get (aref owner 18)
                                     :symbols-with-pos-memory))))
    (and owner owner-cap (listp capability) (>= (length capability) 8)
         (listp owner-cap) (>= (length owner-cap) 8)
         (eq (aref lease 1) handle)
         (eq owner-handle handle)
         (eq (aref owner 12) table-owner)
         (or second lambda-entry (eq (aref owner 17) lease))
         (or (not require-active)
             (and (boundp 'nelisp-eln-registration--active-owner)
                  (eq nelisp-eln-registration--active-owner owner)))
         (boundp 'nelisp-eln-registration--owners)
         (memq owner nelisp-eln-registration--owners)
         (eq (plist-get proof :safe) t)
         (eq (plist-get proof :proof) :multi-import-call)
         (equal (nth 2 capability) (nth 2 owner-cap))
         (equal (nth 3 capability) (nth 3 owner-cap))
         (eq (nth 7 capability) (nth 7 owner-cap))
         (integerp table-address) (> table-address 0)
         (integerp link-address) (> link-address 0)
         (= table-address (nl-ffi-memory-address table-owner))
         (= (ptr-read-u64 link-address 0) table-address)
         (consp entries)
         (equal entries (nelisp-eln-native-subr-import-entries proof))
         (cl-every (lambda (entry)
                     (and (integerp (cdr entry)) (> (cdr entry) 0)
                          (= (ptr-read-u64 table-address (* 8 (car entry)))
                             (cdr entry))))
                   entries)
         (or (null swp-address)
             (and swp-memory
                  (= (ptr-read-u64 swp-address 0)
                     (nl-ffi-memory-address swp-memory))
                  (= (ptr-read-u64 (nl-ffi-memory-address swp-memory) 0)
                     0))))))

(defun nelisp-eln-native-subr--lease-port-numbers (analysis)
  "Return ANALYSIS carrying the port numbering of the live lease's proof.
A profile whose bodies share one link table (S6.11) numbers the ports of
every body's proof alike (`:port-numbers', see
`nelisp-eln-registration--assign-port-numbers'); a body's constructor
re-derives its analysis from the file's own bytes, so that numbering is
re-applied here from the lease's proof -- and only when that proof imports
exactly the same slots in the same order.  Without `:port-numbers' in the
proof ANALYSIS is returned unchanged."
  (let* ((lease nelisp-eln-native-subr--tail-import-context)
         (proof (and (vectorp lease) (= (length lease) 8) (aref lease 7)))
         (numbers (plist-get proof :port-numbers)))
    (cond
     ((or (null analysis) (null numbers)) analysis)
     ((equal (mapcar (lambda (i) (plist-get i :slot))
                     (plist-get analysis :imports))
             (mapcar (lambda (i) (plist-get i :slot))
                     (plist-get proof :imports)))
      (plist-put (copy-sequence analysis) :port-numbers numbers))
     (t (nelisp-eln-native-subr--multi-reject
         'lease-proof-imports-mismatch (plist-get analysis :imports))))))

(defun nelisp-eln-native-subr-create-multi (handle name &optional function-name)
  "Create a managed genuine unary S6 multi-import subr for root NAME.
Its body calls several distinct authenticated freloc slots (see
`nelisp-eln-native-subr--multi-import-specs'); each reaches the
dispatcher through its own callback port, so every slot is decoded and
answered by its own convention, and the body's authenticated `d_reloc'
constants decode by identity."
  ;; Unlike `-create-cxr'/`-create' below, this one genuinely needs the
  ;; module now, not just inside a deferred bridge: `port-tag' (in the
  ;; `ports' binding just below) runs at construction time.
  (unless (fboundp 'nelisp-eln-callable-import-port-tag)
    (require 'nelisp-eln-callable-import))
  (let* ((capability
          (nelisp-eln-system-loader-function-capability handle name))
         (size (nth 6 capability))
         (code (and (integerp size) (> size 0)
                    (nelisp-eln-system-loader-read-root-function-bytes
                     handle name 0 size)))
         (analysis (nelisp-eln-native-subr--lease-port-numbers
                    (and code (= (length code) size)
                         (nelisp-eln-native-subr-multi-import-analysis
                          handle capability code
                          (nelisp-eln-native-subr--tail-import-context-abi-hash)))))
         (lease nelisp-eln-native-subr--tail-import-context)
         (d-reloc-address (plist-get analysis :d-reloc-address))
         (module-id (nelisp-eln-system-loader-module-id handle)))
    (unless (and analysis
                 (or (null (plist-get analysis :constants))
                     (and (integerp d-reloc-address) (> d-reloc-address 0)))
                 (= (nelisp-eln-native-subr-multi-arity analysis) 1)
                 (nelisp-eln-native-subr--multi-lease-valid-p
                  lease handle capability t)
                 (or (null function-name)
                     (symbolp function-name)
                     (and (stringp function-name)
                          (= (length function-name)
                             (string-bytes function-name)))))
      (signal 'nelisp-eln-native-subr-error
              (list 'unsupported-native-abi name size)))
    (nelisp-eln-system-loader-validate-function-capability capability)
    (setq function-name
          (cond ((and function-name (symbolp function-name)) function-name)
                ((stringp function-name) (intern function-name))
                (t (intern name))))
    (let* ((ports (nelisp-eln-native-subr--port-tags analysis))
           ;; Only an exact shape whose body never reads symbol cells
           ;; inline admits opaque interned symbol views (see
           ;; `nelisp-eln-native-subr--multi-import-specs').
           (opaque-symbols
            (and (plist-get (cdr (assq (plist-get analysis :shape)
                                       nelisp-eln-native-subr--multi-import-specs))
                            :opaque-argument-symbols)
                 t))
           ;; Likewise vectors: only a shape declaring :OPAQUE-VECTORS admits
           ;; opaque identity-only vector views.
           (opaque-vectors
            (and (plist-get (cdr (assq (plist-get analysis :shape)
                                       nelisp-eln-native-subr--multi-import-specs))
                            :opaque-vectors)
                 t))
           (constant-cells
            (mapcar (lambda (c)
                      (cons (+ d-reloc-address (* 8 (car c)))
                            (if (eq (cdr c) :registered-lambda)
                                (cons :registered-lambda (car c))
                              (cdr c))))
                    (plist-get analysis :constants)))
           (bridge
            (lambda (argument)
              (unless (nelisp-eln-native-subr--multi-lease-valid-p
                       lease handle capability)
                (signal 'nelisp-eln-native-subr-error
                        (list 'expired-import-table)))
              ;; Read each authenticated constant's live word (written at
              ;; registration from the authenticated data relocations).
              ;; Symbol constants also encode to exactly that word, and an
              ;; argument may carry interned symbols whose global state is
              ;; entirely empty (see `nelisp-eln-objects--admit-empty-
              ;; interned-symbols'); both only for the duration of this call.
              (let* ((constants
                      (nelisp-eln-native-subr--resolve-constant-cells
                       constant-cells lease))
                     (symbol-words
                      (delq nil
                            (mapcar (lambda (c)
                                      (and (cdr c) (not (eq (cdr c) t))
                                           (symbolp (cdr c))
                                           (cons (cdr c) (car c))))
                                    constants))))
                (nelisp-eln-objects-call-with-artifact-symbols
                 symbol-words
                 (lambda ()
                   (nelisp-eln-callable-import--call-unary
                    capability nil argument nil nil constants ports))
                 opaque-symbols opaque-vectors)))))
      (nelisp--native-subr-create capability function-name module-id
                                  bridge 1))))

;;; Binary multi-import bodies (S6.16): a `gnu-eval-subr-pair' profile's
;;; compiler-macro body takes two Lisp arguments.

(defun nelisp-eln-native-subr-multi-arity (analysis)
  "Return the fixed Lisp arity of multi-import ANALYSIS's exact shape.
This is its `nelisp-eln-native-subr--multi-import-specs' :ARITY, or 1."
  (or (plist-get (cdr (assq (plist-get analysis :shape)
                            nelisp-eln-native-subr--multi-import-specs))
                 :arity)
      1))

(defun nelisp-eln-native-subr-multi-min-arity (analysis)
  "Return the number of required arguments of multi-import ANALYSIS's shape.
This is its spec's :MIN-ARITY (S6.4: 1 for the `&optional' `macroexpand-1'),
defaulting to its `nelisp-eln-native-subr-multi-arity'."
  (or (plist-get (cdr (assq (plist-get analysis :shape)
                            nelisp-eln-native-subr--multi-import-specs))
                 :min-arity)
      (nelisp-eln-native-subr-multi-arity analysis)))

(defun nelisp-eln-native-subr-multi-registered-lambda-slots (analysis)
  "Return the d_reloc slots ANALYSIS's exact shape reads as registered lambdas.
These are the slots its spec declares as `:registered-lambda' constants
\(S6.11): d_reloc slots that `Fcomp__register_lambda' overwrites."
  (let ((slots nil))
    (dolist (c (plist-get (cdr (assq (plist-get analysis :shape)
                                     nelisp-eln-native-subr--multi-import-specs))
                          :constants))
      (when (eq (cdr c) :registered-lambda)
        (push (car c) slots)))
    (nreverse slots)))

(defun nelisp-eln-native-subr-multi-swp-declared-p (analysis)
  "Non-nil when ANALYSIS's shape spec declares :SYMBOLS-WITH-POS."
  (and (plist-get (cdr (assq (plist-get analysis :shape)
                             nelisp-eln-native-subr--multi-import-specs))
                  :symbols-with-pos)
       t))

(defun nelisp-eln-native-subr-create-multi-binary
    (handle name &optional function-name min-arity)
  "Create a managed genuine binary S6 multi-import subr for root NAME.
Exactly like `nelisp-eln-native-subr-create-multi' -- the same exact
template, per-slot port, `d_reloc' constant and live-lease
authentication -- but only for a shape whose spec declares :ARITY 2 and
reads f_symbols_with_pos_enabled_reloc iff it declares :SYMBOLS-WITH-POS;
the bridge passes both
Lisp arguments to the native body (%rdi, %rsi).  MIN-ARITY 1 (S6.6)
makes the second argument `&optional' -- an arity (1 . 2) subr whose
omitted second argument reaches the body as nil, as in GNU's native
calling convention; any other non-nil MIN-ARITY signals."
  ;; Same reason as `-create-multi': `port-tag' runs at construction time.
  (unless (fboundp 'nelisp-eln-callable-import-port-tag)
    (require 'nelisp-eln-callable-import))
  (let* ((capability
          (nelisp-eln-system-loader-function-capability handle name))
         (size (nth 6 capability))
         (code (and (integerp size) (> size 0)
                    (nelisp-eln-system-loader-read-root-function-bytes
                     handle name 0 size)))
         (analysis (nelisp-eln-native-subr--lease-port-numbers
                    (and code (= (length code) size)
                         (nelisp-eln-native-subr-multi-import-analysis
                          handle capability code
                          (nelisp-eln-native-subr--tail-import-context-abi-hash)))))
         (lease nelisp-eln-native-subr--tail-import-context)
         (d-reloc-address (plist-get analysis :d-reloc-address))
         (module-id (nelisp-eln-system-loader-module-id handle)))
    (unless (and analysis
                 (or (null (plist-get analysis :constants))
                     (and (integerp d-reloc-address) (> d-reloc-address 0)))
                 (= (nelisp-eln-native-subr-multi-arity analysis) 2)
                 (or (null min-arity) (eql min-arity 1))
                 ;; The constructor's MIN-ARITY must be the spec's own.
                 (= (or min-arity 2)
                    (nelisp-eln-native-subr-multi-min-arity analysis))
                 ;; The `symbols_with_pos_enabled' byte is read iff the
                 ;; exact shape declares it.
                 (eq (and (plist-get analysis :symbols-with-pos-address) t)
                     (nelisp-eln-native-subr-multi-swp-declared-p analysis))
                 (nelisp-eln-native-subr--multi-lease-valid-p
                  lease handle capability t)
                 (or (null function-name)
                     (symbolp function-name)
                     (and (stringp function-name)
                          (= (length function-name)
                             (string-bytes function-name)))))
      (signal 'nelisp-eln-native-subr-error
              (list 'unsupported-native-abi name size)))
    (nelisp-eln-system-loader-validate-function-capability capability)
    (setq function-name
          (cond ((and function-name (symbolp function-name)) function-name)
                ((stringp function-name) (intern function-name))
                (t (intern name))))
    (let* ((ports (nelisp-eln-native-subr--port-tags analysis))
           ;; As in `nelisp-eln-native-subr-create-multi': only an exact
           ;; shape whose body never reads symbol cells inline admits
           ;; opaque interned symbol views (S6.6 `closure-convert').
           (opaque-symbols
            (and (plist-get (cdr (assq (plist-get analysis :shape)
                                       nelisp-eln-native-subr--multi-import-specs))
                            :opaque-argument-symbols)
                 t))
           ;; Likewise vectors: only a shape declaring :OPAQUE-VECTORS admits
           ;; opaque identity-only vector views.
           (opaque-vectors
            (and (plist-get (cdr (assq (plist-get analysis :shape)
                                       nelisp-eln-native-subr--multi-import-specs))
                            :opaque-vectors)
                 t))
           (constant-cells
            (mapcar (lambda (c)
                      (cons (+ d-reloc-address (* 8 (car c)))
                            (if (eq (cdr c) :registered-lambda)
                                (cons :registered-lambda (car c))
                              (cdr c))))
                    (plist-get analysis :constants)))
           (bridge-fn
            (lambda (first second)
              (unless (nelisp-eln-native-subr--multi-lease-valid-p
                       lease handle capability)
                (signal 'nelisp-eln-native-subr-error
                        (list 'expired-import-table)))
              ;; Same per-call constant and artifact-symbol handling as
              ;; the unary `nelisp-eln-native-subr-create-multi' bridge.
              (let* ((constants
                      (nelisp-eln-native-subr--resolve-constant-cells
                       constant-cells lease))
                     (symbol-words
                      (delq nil
                            (mapcar (lambda (c)
                                      (and (cdr c) (not (eq (cdr c) t))
                                           (symbolp (cdr c))
                                           (cons (cdr c) (car c))))
                                    constants))))
                (nelisp-eln-objects-call-with-artifact-symbols
                 symbol-words
                 (lambda ()
                   (nelisp-eln-callable-import--call-unary
                    capability nil first nil nil constants ports
                    (list second)))
                 opaque-symbols opaque-vectors))))
           (bridge (if min-arity
                       (lambda (first &optional second)
                         (funcall bridge-fn first second))
                     bridge-fn)))
      (if min-arity
          (nelisp--native-subr-create capability function-name module-id
                                      bridge 2 min-arity)
        (nelisp--native-subr-create capability function-name module-id
                                    bridge 2)))))

(defun nelisp-eln-native-subr-create-lambda-placeholder
    (handle name &optional function-name)
  "Create a managed arity-0 subr for the registered anonymous lambda NAME.
S6.12: GNU's `comp--register-lambda' stores each native anonymous lambda's
subr into a `d_reloc' slot that no admitted body reads.  The subr's body is
admitted (a `jmp' thunk, or an exact arity-0 multi-import template with every
import and constant authenticated), but calling it is refused: the bridge
signals `registered-lambda-not-callable'.  FUNCTION-NAME only names the
subr; it is never published (a string is used for an uninterned symbol)."
  (let* ((capability
          (nelisp-eln-system-loader-function-capability handle name))
         (size (nth 6 capability))
         (code (and (integerp size) (> size 0)
                    (nelisp-eln-system-loader-read-root-function-bytes
                     handle name 0 size)))
         (thunk (and code (= (length code) size) (= size 2)
                     (= (aref code 0) #xeb)))
         (analysis (and code (= (length code) size) (not thunk)
                        (nelisp-eln-native-subr-multi-import-analysis
                         handle capability code
                         (nelisp-eln-native-subr--tail-import-context-abi-hash))))
         (module-id (nelisp-eln-system-loader-module-id handle)))
    (unless (and (or thunk
                     (and analysis
                          (= (nelisp-eln-native-subr-multi-arity analysis) 0)))
                 (or (null function-name) (symbolp function-name)
                     (and (stringp function-name)
                          (= (length function-name)
                             (string-bytes function-name)))))
      (signal 'nelisp-eln-native-subr-error
              (list 'unsupported-native-abi name size)))
    (nelisp-eln-system-loader-validate-function-capability capability)
    (setq function-name
          (cond ((and function-name (symbolp function-name)) function-name)
                ((stringp function-name) (make-symbol function-name))
                (t (make-symbol name))))
    (nelisp--native-subr-create
     capability function-name module-id
     (lambda ()
       (signal 'nelisp-eln-native-subr-error
               (list 'registered-lambda-not-callable name)))
     0)))

(defun nelisp-eln-native-subr-create (handle name &optional function-name)
  "Create a managed scalar0 or unary leaf subr for root NAME in HANDLE.
FUNCTION-NAME, when non-nil, is a symbol or a string naming the canonical
Lisp registration symbol.  A string is interned only after capability and
code validation; otherwise a symbol named NAME is interned for the direct API.
Scalar0 retains its exact x86-64 `mov imm32,%eax; ret' fast path.  Unary
functions must pass the bounded x86-64 leaf-code verifier.  A function whose
only indirect call is a tail JMP through an authenticated slot (1+/1-) must
pass `nelisp-eln-native-subr--tail-import-analysis'; a function whose only
indirect call is a non-tail CALL through a GNU MANY (argc, argv) stack array
(e.g. genuine vendor `zerop', which calls `=') must pass
`nelisp-eln-native-subr--many-import-analysis'.  `zerop' itself stays a
unary Lisp function; only its internal native call is MANY-convention."
  ;; No `require' here either, for the same reason as `-create-cxr':
  ;; every `nelisp-eln-callable-import' reference below is inside a
  ;; deferred bridge closure, never at construction time.
  (let* ((capability
          (nelisp-eln-system-loader-function-capability handle name))
         (size (nth 6 capability))
         (code (and (integerp size) (> size 0)
                    (nelisp-eln-system-loader-read-root-function-bytes
                     handle name 0 size)))
         (word (and code (= (length code) size) (= size 6)
                    (+ (aref code 1)
                       (lsh (aref code 2) 8)
                       (lsh (aref code 3) 16)
                       (lsh (aref code 4) 24))))
         (scalar0 (and code (= (length code) size) (= size 6)
                       (= (aref code 0) #xb8)
                       (= (aref code 5) #xc3) (= (logand word 3) 2)))
         (unary-leaf (and code (= (length code) size)
                          (nelisp-eln-leaf-code-valid-p code)))
         (tail-analysis (and code (= (length code) size)
                            (not scalar0) (not unary-leaf)
                            (nelisp-eln-native-subr--tail-import-analysis
                             handle capability code
                             (nelisp-eln-native-subr--tail-import-context-abi-hash))))
         (tail-import (and tail-analysis
                           (let ((lease
                                  nelisp-eln-native-subr--tail-import-context)
                                 (implementation
                                 (nelisp-eln-native-subr--canonical-builtin
                                  (plist-get tail-analysis :descriptor))))
                             (unless (nelisp-eln-native-subr--tail-lease-valid-p
                                      lease handle capability t)
                               (signal 'nelisp-eln-native-subr-error
                                       (list 'missing-live-import-table)))
                             (cons lease implementation))))
         (many-analysis (and code (= (length code) size)
                             (not scalar0) (not unary-leaf) (not tail-import)
                             (nelisp-eln-native-subr--many-import-analysis
                              handle capability code
                              (nelisp-eln-native-subr--tail-import-context-abi-hash))))
         (many-import (and many-analysis
                           (let ((lease
                                  nelisp-eln-native-subr--tail-import-context)
                                 (implementation
                                 (nelisp-eln-native-subr--canonical-builtin
                                  (plist-get many-analysis :descriptor))))
                             (unless (nelisp-eln-native-subr--tail-lease-valid-p
                                      lease handle capability t)
                               (signal 'nelisp-eln-native-subr-error
                                       (list 'missing-live-import-table)))
                             (cons lease implementation))))
         (module-id (nelisp-eln-system-loader-module-id handle)))
    (unless (and (or scalar0 unary-leaf tail-import many-import)
                 (or (null function-name)
                     (symbolp function-name)
                     (and (stringp function-name)
                          (= (length function-name)
                             (string-bytes function-name)))))
      (signal 'nelisp-eln-native-subr-error
              (list 'unsupported-native-abi name size)))
    (nelisp-eln-system-loader-validate-function-capability capability)
    (setq function-name
          (cond ((and function-name (symbolp function-name)) function-name)
                ((stringp function-name) (intern function-name))
                (t (intern name))))
    (if scalar0
        (nelisp--native-subr-create capability function-name module-id)
      (let ((bridge
             (cond
              (tail-import
               (lambda (argument)
                 (unless (nelisp-eln-native-subr--tail-lease-valid-p
                          (car tail-import) handle capability)
                   (signal 'nelisp-eln-native-subr-error
                           (list 'expired-import-table)))
                 (nelisp-eln-callable-import--call-unary
                  capability (cdr tail-import) argument)))
              (many-import
               (let ((call-arity (nth 3 (plist-get many-analysis :descriptor))))
                 (lambda (argument)
                   (unless (nelisp-eln-native-subr--tail-lease-valid-p
                            (car many-import) handle capability)
                     (signal 'nelisp-eln-native-subr-error
                             (list 'expired-import-table)))
                   (nelisp-eln-callable-import--call-unary
                    capability (cdr many-import) argument 'many call-arity))))
              (t
               (lambda (argument)
                 (nelisp-eln-native-subr--unary-bridge
                  (nth 3 capability) argument))))))
        (unless (functionp bridge)
          (signal 'nelisp-eln-native-subr-error
                  (list 'invalid-unary-bridge name)))
        (nelisp--native-subr-create
         capability function-name module-id bridge 1)))))

(provide 'nelisp-eln-native-subr)

;;; nelisp-eln-native-subr.el ends here
