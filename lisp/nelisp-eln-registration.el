;;; nelisp-eln-registration.el --- bounded GNU .eln registration adapter -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Execute only the pinned emitter's one-call registration path.  Native
;; comp-unit hash tables remain NeLisp-owned state; the admitted top-level
;; code and comp--register-subr callback treat the unit word as opaque.

;;; Code:

(defvar nelisp-eln-registration-debug nil)

(defun nelisp-eln-registration--trace (text)
  (when nelisp-eln-registration-debug (princ text)))

;; The full `nelisp-eln-emitter' is NOT required here at all: it pulls in
;; `nelisp-aot-compiler' (21k+ lines) and `nelisp-elf-write' from source,
;; which dominated this file's own load cost in a fresh process (S7.7.4
;; corpus-gate investigation: ~1.9s of ~4.4s). Only two of its functions are
;; used below, in `nelisp-eln-registration--top-level-code' and
;; `nelisp-eln-registration--metadata-data-count' -- `--top-level-code' and
;; `--symbol-name', which are pure byte/string construction with no
;; dependency on either of those two heavy requires -- so both now live in
;; the standalone `nelisp-eln-emitter-templates' feature (moved there, not
;; duplicated: `nelisp-eln-emitter.el' requires it too, for the same
;; functions under the same names, and stays the sole owner of everything
;; that DOES need the AOT compiler / ELF writer). `--top-level-code' further
;; reorders its own GNU-family template checks ahead of the self-emitter
;; one specifically so that requiring even this lightweight feature is
;; skipped entirely for every genuine or corrupted GNU-compiled artifact --
;; see that function's own commentary. Each remaining call site below still
;; calls `(require 'nelisp-eln-emitter-templates)' itself right before use,
;; the same require-at-use pattern `nelisp-eln-switchover-migrate-neln'
;; already uses for the full module. NOTE: plain `(autoload ...)' was tried
;; first and rejected: on this runtime, calling an unresolved autoload
;; object signals `invalid-function' instead of loading and re-dispatching
;; (verified empirically), so autoload here would be silently non-lazy-safe
;; -- require-at-use is the only mechanism that actually works.
(nelisp-eln-registration--trace "REGTRACE require nelisp-eln-metadata\n")
(require 'nelisp-eln-metadata)
(nelisp-eln-registration--trace "REGTRACE require nelisp-eln-system-loader\n")
(require 'nelisp-eln-system-loader)
(nelisp-eln-registration--trace "REGTRACE require nelisp-eln-native-subr\n")
(require 'nelisp-eln-native-subr)
(nelisp-eln-registration--trace "REGTRACE require nelisp-eln-registration-objects\n")
(require 'nelisp-eln-registration-objects)
(nelisp-eln-registration--trace "REGTRACE require nelisp-eln-registration-vectors\n")
(require 'nelisp-eln-registration-vectors)
;; `nelisp-native-load' (3048 lines) is NOT required eagerly here either,
;; for the same reason as `nelisp-eln-emitter' above: every use below is
;; confined to the deep activation/rollback path (constructing or tearing
;; down the native callback-pin context), which is only reached once an
;; artifact has already passed every preflight check -- a rejected
;; artifact (corrupt/truncated/ABI-mismatched, or simply an already-bound
;; name) never gets there, so `--cleanup' and `--finish-success' guard
;; each call behind the exact non-nil check (`callback-token'/`pin-env')
;; that already proves activation had begun, and `require' there is (in
;; the overwhelmingly common case where activation already required it
;; in `--load-1') a no-op safety net, not a real load.
(nelisp-eln-registration--trace "REGTRACE require nl-ffi\n")
(require 'nl-ffi)
(nelisp-eln-registration--trace "REGTRACE require nl-ffi-memory\n")
(require 'nl-ffi-memory)
(nelisp-eln-registration--trace "REGTRACE require nl-ffi-loader\n")
(require 'nl-ffi-loader)

(define-error 'nelisp-eln-registration-error
  "Unsupported pinned .eln registration path")

(defconst nelisp-eln-registration--slot 1030)
(defconst nelisp-eln-registration--eval-slot 947
  "The ba35c031 freloc import table index for `Feval'.

Authenticated against ~/.cache/tmp/slot-auth/freloc-ba35c031.tsv (sha256
3e8591ab81130c91375221fb3efeaa377c2cd90ccf45fcd58adb3f31c55f0758), whose
row 947 reads \"Feval in section .text of /usr/local/bin/emacs-31.1\" --
the same table that authenticates slot 1030 as `Fcomp__register_subr'
below.  Genuine GNU top_level_run units call this slot once, on a
compiled `(byte-code ...)' object, to install `function-put' properties
for the subr `nelisp-eln-registration--slot' just registered (S6.16-24;
see `nelisp-eln-registration--eval-effect').")
(defconst nelisp-eln-registration--lambda-slot 1031
  "The ba35c031 freloc import table index for `Fcomp__register_lambda'.

Authenticated against ~/.cache/tmp/slot-auth/freloc-ba35c031.tsv (sha256
3e8591ab81130c91375221fb3efeaa377c2cd90ccf45fcd58adb3f31c55f0758), whose
row 1031 (offset 0x2038) reads \"Fcomp__register_lambda in section .text of
/usr/local/bin/emacs-31.1\"; GNU src/comp.c registers an anonymous lambda
as a subr and stores it into `d_reloc[RELOC_IDX]'.  Only the S6.12
`gnu-lambda-require-subr' profile calls it.")
(defconst nelisp-eln-registration--table-slots 1031)
(defconst nelisp-eln-registration--owner-marker 'nelisp-eln-registration-owner)
(defconst nelisp-eln-registration--owner-size 20
  "The exact length every owner vector must have: indices 0-17 are the
original single-registration fields; 18 is the S6.16-24 role-sequence/
eval-effect/second-registration-expectation plist
\(`nelisp-eln-registration--role-plist'); 19 is the second (compiler-macro)
registration's own (WORD . CALLABLE), or nil.  Single-sourced here so
every real or fixture owner vector -- `nelisp-eln-registration-load''s
own construction below, and every test that builds one by hand -- stays
the same length; a fixture built with a stale, smaller length is exactly
what made `nelisp-eln-registration--cleanup''s unconditional `(aset owner
19 ...)' signal `args-out-of-range' on an unrelated, pre-existing test.")
(defvar nelisp-eln-registration--owners nil)
(defvar nelisp-eln-registration--active-owner nil)
(defvar nelisp-eln-registration--registered-word nil)
(defvar nelisp-eln-registration--registered-callable nil)
(defvar nelisp-eln-registration--registered-word-2 nil
  "The `gnu-eval-subr-pair' profile's second (compiler-macro) registration
word, set only after its own `Fcomp__register_subr' callback succeeds.")
(defvar nelisp-eln-registration--registered-callable-2 nil)
(defvar nelisp-eln-registration--call-index 0
  "How many admitted top-level import calls have fired so far this load.
Reset to 0 at the start of `nelisp-eln-registration-load'; incremented by
`nelisp-eln-registration--callback' on entry.  Combined with the active
owner's `:role-sequence' (S6.16-24), this is what lets one shared import
slot dispatch a genuine GNU top_level_run's ordered sequence of
register_subr / Feval / register_subr calls to the right admitted
handler -- never by trusting anything the call itself claims to be.")
(defvar nelisp-eln-registration--last-callback-error nil)
;; Isolated registration namespaces (S6 measurement harness).
;;
;; A normal registration publishes the genuine artifact's subr into the
;; global function cell of its interned name, and only when that name is
;; entirely unbound.  For a name the runtime itself already defines and
;; uses (`zerop', `caar', `cadr', ...), replacing that global binding
;; breaks the runtime: the loader, the object codec, nl-ffi and the
;; callable-import bridge all call those very names.  An isolated
;; namespace publishes the same authenticated registration -- same
;; preflight, same interned name identity, same register_subr / Feval
;; callback validation -- into a private function/plist table instead,
;; leaving the runtime's global binding untouched.  Only the publication
;; target changes; nothing about which bytes, imports or callback
;; arguments are admitted does.

(defconst nelisp-eln-registration--namespace-marker
  'nelisp-eln-registration-isolated-namespace)

(defvar nelisp-eln-registration-isolated-namespace nil
  "When non-nil, an isolated namespace for `nelisp-eln-registration-load'.
Let-bind it to a value from `nelisp-eln-registration-make-isolated-namespace'
around a normal `load' of a .eln to publish the admitted registration(s)
into that namespace instead of the global function cells.  Read the
result back with `nelisp-eln-registration-isolated-function' and
`nelisp-eln-registration-isolated-get'.")

(defvar nelisp-eln-registration--load-namespace nil
  "The validated isolated namespace of the registration in progress, or nil.")

(defun nelisp-eln-registration-make-isolated-namespace ()
  "Return a fresh, empty isolated registration namespace."
  (vector nelisp-eln-registration--namespace-marker nil nil))

(defun nelisp-eln-registration--namespace-p (value)
  (and (vectorp value) (= (length value) 3)
       (eq (aref value 0) nelisp-eln-registration--namespace-marker)))

(defun nelisp-eln-registration--check-namespace (value)
  "Return VALUE when it is nil or a valid isolated namespace, else fail."
  (unless (or (null value) (nelisp-eln-registration--namespace-p value))
    (nelisp-eln-registration--fail 'invalid-isolated-namespace value))
  value)

(defun nelisp-eln-registration-isolated-function (namespace name)
  "Return NAME's callable published into isolated NAMESPACE, or nil."
  (unless (nelisp-eln-registration--namespace-p namespace)
    (signal 'wrong-type-argument (list 'isolated-namespace namespace)))
  (cdr (assq name (aref namespace 1))))

(defun nelisp-eln-registration-isolated-get (namespace name prop)
  "Return NAME's PROP as published into isolated NAMESPACE, or nil."
  (unless (nelisp-eln-registration--namespace-p namespace)
    (signal 'wrong-type-argument (list 'isolated-namespace namespace)))
  (plist-get (cdr (assq name (aref namespace 2))) prop))

(defun nelisp-eln-registration--target-fboundp (name)
  "Non-nil when NAME already has a function in the publication target."
  (let ((ns nelisp-eln-registration--load-namespace))
    (if ns
        (assq name (aref ns 1))
      (fboundp name))))

(defun nelisp-eln-registration--target-plist-empty-p (name)
  "Non-nil when NAME has no plist in an isolated publication target.
The global target's plist is checked by symbol admission itself."
  (let ((ns nelisp-eln-registration--load-namespace))
    (or (null ns) (null (cdr (assq name (aref ns 2)))))))

(defun nelisp-eln-registration--target-function (name)
  "Return NAME's function in the publication target, or nil."
  (let ((ns nelisp-eln-registration--load-namespace))
    (if ns
        (cdr (assq name (aref ns 1)))
      (and (fboundp name) (symbol-function name)))))

(defun nelisp-eln-registration--publish (name callable)
  "Publish CALLABLE as NAME's function in the publication target."
  (let ((ns nelisp-eln-registration--load-namespace))
    (if ns
        (aset ns 1 (cons (cons name callable)
                         (assq-delete-all name (aref ns 1))))
      (fset name callable))))

(defun nelisp-eln-registration--target-put (name prop value)
  "Set NAME's PROP to VALUE in the publication target."
  (let ((ns nelisp-eln-registration--load-namespace))
    (if ns
        (let ((cell (assq name (aref ns 2))))
          (if cell
              (setcdr cell (plist-put (cdr cell) prop value))
            (aset ns 2 (cons (cons name (list prop value)) (aref ns 2)))))
      (function-put name prop value))))

(defun nelisp-eln-registration--unpublish (name callable)
  "Retract NAME's publication when it is still exactly CALLABLE."
  (let ((ns nelisp-eln-registration--load-namespace))
    (when (and name (symbolp name) callable
               (eq (nelisp-eln-registration--target-function name) callable))
      (if ns
          (progn
            (aset ns 1 (assq-delete-all name (aref ns 1)))
            (aset ns 2 (assq-delete-all name (aref ns 2))))
        (fset name nil)))))

(defvar nelisp-eln-registration--pending-cleanups nil)
(defvar nelisp-eln-registration--boundary-enabled t
  "Non-nil enables the general crash-containment boundary (S7.7).

When enabled (the default), any non-local exit from
`nelisp-eln-registration-load' -- an ordinary error, a `throw', or a
`quit' signal -- is recorded by `nelisp-eln-registration--containment-boundary'
as either a rolled-back attempt (silent, no pending owner) or a quarantined
pending owner that blocks all subsequent registration, per the Doc 206 P5
contract. This is a test-only kill switch for the negative control in
test/nelisp-eln-crash-general-smoke.sh; production code must never rebind
it, and doing so leaks the partially registered native resources of any
attempt that fails while it is nil.")

(declare-function nelisp-eln-registration-metadata-create
  "nelisp-eln-registration-metadata" (profile data docs))
(declare-function nelisp-eln-registration-metadata-data-word
  "nelisp-eln-registration-metadata" (token))
(declare-function nelisp-eln-registration-metadata-docs-word
  "nelisp-eln-registration-metadata" (token))
(declare-function nelisp-eln-registration-metadata-slot-word
  "nelisp-eln-registration-metadata" (token vector index))
(declare-function nelisp-eln-registration-metadata-type-word
  "nelisp-eln-registration-metadata" (token))
(declare-function nelisp-eln-registration-metadata-decode
  "nelisp-eln-registration-metadata" (token word))
(declare-function nelisp-eln-registration-metadata-release
  "nelisp-eln-registration-metadata" (token))
(declare-function nelisp-eln-emitter--top-level-code
  "nelisp-eln-emitter-templates" (arity))
(declare-function nelisp-eln-emitter--symbol-name
  "nelisp-eln-emitter-templates" (name))
(declare-function nelisp-native-load--symbol-addr "nelisp-native-load" (name))
(declare-function nelisp-native-load--pin-begin "nelisp-native-load" (env))
(declare-function nelisp-native-load--pin-reserve
  "nelisp-native-load" (env marker))
(declare-function nelisp-native-load--pin-end "nelisp-native-load" (env marker))
(declare-function nelisp-native-load-box
  "nelisp-native-load" (addr value &optional env pin-frame))

(defun nelisp-eln-registration--fail (reason &optional detail)
  (signal 'nelisp-eln-registration-error (list reason detail)))

(defun nelisp-eln-registration--writable-object (handle name size)
  "Return root object address for writable NAME with exact SIZE."
  (let* ((state (nelisp-eln-system-loader--state handle))
         (elf (plist-get state :elf))
         (entry (gethash name (plist-get elf :symbols)))
         (info (nelisp-eln-system-loader-symbol-info handle name))
         (value (and entry (plist-get entry :value)))
         (load (and entry
                    (catch 'found
                      (dolist (row (plist-get elf :loads))
                        (when (and (= (plist-get entry :type)
                                      nelisp-eln-system-loader--stt-object)
                                   (= (plist-get entry :size) size)
                                   (/= 0 (logand (nth 4 row) 2))
                                   (>= value (nth 0 row))
                                   (<= size (- (nth 3 row) (- value (nth 0 row)))))
                          (throw 'found row)))
                      nil))))
    (unless (and info load
                 (= (plist-get info :size) size)
                 (equal (plist-get info :source-path)
                        (plist-get state :path)))
      (nelisp-eln-registration--fail
       'root-object-not-writable (list name size info)))
    (plist-get info :address)))

(defun nelisp-eln-registration--scalar0-code-p (code)
  "Recognize the native factory's exact tagged scalar0 byte sequence."
  (and (stringp code) (= (length code) 6) (= (aref code 0) #xb8)
       (= (aref code 5) #xc3)
       (= (logand (+ (aref code 1) (ash (aref code 2) 8)
                     (ash (aref code 3) 16) (ash (aref code 4) 24))
                  3)
          2)))

(defun nelisp-eln-registration--gnu-single-top-level-arity (handle cap actual)
  "Return (ARITY . t) for the checked GNU31 single-register thunk, else nil."
  (let* ((size (length actual))
         (base (nth 3 cap))
         (pattern (unibyte-string
                   #x48 #x83 #xec #x10
                   #x48 #x8b #x05 0 0 0 0
                   #x48 #x89 #xfa
                   #x48 #x8b #x0d 0 0 0 0
                   #x4c #x8b #x48 #x18
                   #x48 #x8b #x70 #x10
                   #x48 #x8b #x78 #x08
                   #x48 #x8b #x05 0 0 0 0
                   #x4c #x8b #x01
                   #xb9 0 0 0 0
                   #x48 #x8b #x00
                   #x52
                   #xba 0 0 0 0
                   #xff #x90 #x30 #x20 0 0
                   #x48 #x83 #xc4 #x18 #xc3))
         (ok (= size 68))
         (i 0)
         (minarg nil) (maxarg nil) (targets-ok t))
    (while (and ok (< i size))
      (unless (or (and (>= i 7) (< i 11))
                  (and (>= i 17) (< i 21))
                  (and (>= i 36) (< i 40))
                  (and (>= i 44) (< i 48))
                  (and (>= i 53) (< i 57))
                  (= (aref actual i) (aref pattern i)))
        (setq ok nil))
      (setq i (1+ i)))
    (when ok
      (setq minarg (+ (aref actual 44)
                      (ash (aref actual 45) 8)
                      (ash (aref actual 46) 16)
                      (ash (aref actual 47) 24))
            maxarg (+ (aref actual 53)
                      (ash (aref actual 54) 8)
                      (ash (aref actual 55) 16)
                      (ash (aref actual 56) 24)))
      ;; Tagged fixnums 2, 6 and 10 are arities 0, 1 and 2.  Arity 2 is
      ;; admitted only for the Doc 207 chain body (see preflight).
      (unless (and (= minarg maxarg) (memq minarg '(2 6 10)))
        (setq ok nil)))
    (when ok
      (dolist (entry '((4 . "d_reloc_eph")
                       (14 . "d_reloc")
                       (33 . "freloc_link_table")))
        (let* ((start (car entry))
               (disp (+ (aref actual (+ start 3))
                        (ash (aref actual (+ start 4)) 8)
                        (ash (aref actual (+ start 5)) 16)
                        (ash (aref actual (+ start 6)) 24)))
               (signed (if (>= disp #x80000000)
                           (- disp #x100000000) disp))
               (slot-address (+ base start 7 signed))
               (target
                (condition-case nil
                    (nelisp-eln-system-loader-validate-root-indirection
                     handle slot-address (cdr entry))
                  (nelisp-eln-system-loader-error nil))))
          (unless (and (integerp target) (> target 0))
            (setq targets-ok nil))))
      (when targets-ok
        (cons (ash (- minarg 2) -2) t)))))

;; GNU real-artifact top-level shapes (S6.16-S6.24).  A genuine GNU top
;;_level_run emits one `Fcomp__register_subr' call per top-level `defsubr'
;; and, when the defun's properties need installing (a compiler-macro, a
;; `function-type', `pure'/`side-effect-free', ...), one `Feval' call on a
;; small compiled `(byte-code ...)' object that calls `function-put'.  Both
;; call targets are authenticated against the same ba35c031 freloc import
;; table already cited by `nelisp-eln-registration--slot' and
;; `nelisp-eln-registration--eval-slot' above
;; (~/.cache/tmp/slot-auth/freloc-ba35c031.tsv, sha256
;; 3e8591ab81130c91375221fb3efeaa377c2cd90ccf45fcd58adb3f31c55f0758).  The
;; templates below are byte-for-byte transcriptions of genuine GNU31 x86-64
;; artifacts (~/.cache/tmp/s6-survey-lex/<fn>/overlay/eln/31.1-ba35c031/),
;; never hand-written; see ~/.cache/tmp/preflight/disasm/ for the annotated
;; disassembly each was transcribed from.

(defun nelisp-eln-registration--offset-holed-p (offset holes)
  "Return non-nil if OFFSET falls in one of HOLES's (START . END) ranges."
  (catch 'found
    (dolist (range holes)
      (when (and (>= offset (car range)) (< offset (cdr range)))
        (throw 'found t)))
    nil))

(defun nelisp-eln-registration--match-holed-template (actual template holes)
  "Return non-nil if ACTUAL equals TEMPLATE outside HOLES byte ranges."
  (and (= (length actual) (length template))
       (let ((i 0) (ok t) (n (length template)))
         (while (and ok (< i n))
           (unless (or (nelisp-eln-registration--offset-holed-p i holes)
                       (= (aref actual i) (aref template i)))
             (setq ok nil))
           (setq i (1+ i)))
         ok)))

(defun nelisp-eln-registration--read-u32-le (actual offset)
  "Read a four-byte little-endian unsigned integer from ACTUAL at OFFSET."
  (+ (aref actual offset) (ash (aref actual (1+ offset)) 8)
     (ash (aref actual (+ offset 2)) 16) (ash (aref actual (+ offset 3)) 24)))

(defun nelisp-eln-registration--validate-rip-relocs (handle base actual relocs)
  "Validate ACTUAL's RIP-relative loads at each (OFFSET . SYMBOL) in RELOCS.
OFFSET is the byte position of a four-byte displacement field belonging to
a `mov SYM(%rip),REG' encoding.  Genuine GNU artifacts reach SYMBOL through
one level of GOT-style indirection: the computed address is a root slot
whose own eight bytes, once read, must equal SYMBOL's authenticated loaded
address -- exactly what
`nelisp-eln-system-loader-validate-root-indirection' already validates (the
same mechanism `nelisp-eln-registration--gnu-single-top-level-arity' above
uses); this only computes each slot address and delegates to it, so the
one exported entry point owns every dereference and file-integrity check."
  (catch 'fail
    (dolist (entry relocs)
      (let* ((offset (car entry))
             (disp (nelisp-eln-registration--read-u32-le actual offset))
             (signed (if (>= disp #x80000000) (- disp #x100000000) disp))
             (slot-address (+ base offset 4 signed)))
        (unless (condition-case nil
                    (nelisp-eln-system-loader-validate-root-indirection
                     handle slot-address (cdr entry))
                  (nelisp-eln-system-loader-error nil))
          (throw 'fail nil))))
    t))

(defun nelisp-eln-registration--gnu-arity (actual offset)
  "Decode the fixed-arity immediate at ACTUAL's four bytes from OFFSET.
Returns the small nonnegative integer arity, or nil if the encoded word
is not an admitted fixnum in [0, 8]."
  (let* ((raw (nelisp-eln-registration--read-u32-le actual offset))
         (value (nelisp-eln-abi-decode-immediate raw)))
    (and (integerp value) (<= 0 value 8) value)))

(defun nelisp-eln-registration--d-reloc-index (actual offset)
  "Decode ACTUAL's single-byte d_reloc slot offset at OFFSET to its index.
Returns nil unless the raw byte is a nonnegative multiple of eight."
  (let ((raw (aref actual offset)))
    (and (integerp raw) (<= 0 raw) (= (mod raw 8) 0) (/ raw 8))))

(defconst nelisp-eln-registration--gnu-verified-subr-template
  (unibyte-string
   #x48 #x83 #xec #x10 #x48 #x8b #x05 0 0 0 0 #x48
   #x89 #xfa #x48 #x8b #x0d 0 0 0 0 #x4c #x8b #x48
   #x18 #x48 #x8b #x70 #x10 #x48 #x8b #x78 #x08 #x48 #x8b #x05
   0 0 0 0 #x4c #x8b #x41 0 #xb9 0 0 0 0 #x48
   #x8b #x00 #x52 #xba 0 0 0 0 #xff #x90 #x30 #x20
   0 0 #x48 #x83 #xc4 #x18 #xc3)
  "Genuine GNU 31.1 x86-64 single-registration top_level_run whose
`subr-type' argument is loaded from a nonzero d_reloc slot (S6.8).
Byte-for-byte identical to gnu-cconv--set-diff.eln's own 69-byte
top_level_run (sha256 of that artifact: 7f4cedf5..fb1392) outside its
variable fields, see `nelisp-eln-registration--gnu-verified-subr-holes'.
It is the 68-byte `gnu-single-leaf' thunk (see
`nelisp-eln-registration--gnu-single-top-level-arity') except that the
type is read by the four-byte `mov DISP8(%rcx),%r8' instead of the
three-byte `mov (%rcx),%r8'; GNU emits it when the type constant is not
the first d_reloc entry.")

(defconst nelisp-eln-registration--gnu-verified-subr-holes
  '((7 . 11) (17 . 21) (24 . 25) (28 . 29) (32 . 33) (36 . 40) (43 . 44)
    (45 . 49) (54 . 58))
  "Variable byte ranges in
`nelisp-eln-registration--gnu-verified-subr-template': the three
RIP-relative pointer loads (d_reloc_eph, d_reloc, freloc_link_table, in
that order), the three one-byte d_reloc_eph slot offsets (offsets 24, 28
and 32: the registration's rest, c-name and name words, see
`nelisp-eln-registration--gnu-verified-subr-top-level'), the `subr-type'
d_reloc slot offset, and the register_subr call's max and min arity
immediates.")

(defun nelisp-eln-registration--gnu-verified-subr-top-level (handle cap actual)
  "Return (ARITY TYPE-INDEX MIN-ARITY) for the checked `gnu-verified-subr' thunk.
ARITY is the maximum argument count; MIN-ARITY equals it for a fixed
arity, or is 1 for the one admitted variable arity 1..2 (`(A &optional
B)', S6.4).  Any other min/max pair is rejected.
Return nil when ACTUAL is not that exact shape; signal when it is, but
its RIP-relative loads, arity immediates or type slot offset do not
authenticate.  The type slot offset must name a nonzero d_reloc index
\(index 0 is the three-byte `gnu-single-leaf' encoding, never this one);
whether that slot really holds the registered function's type is
checked against the artifact's own data relocations by
`nelisp-eln-registration--verified-subr-type-p' during preflight."
  (when (nelisp-eln-registration--match-holed-template
         actual nelisp-eln-registration--gnu-verified-subr-template
         nelisp-eln-registration--gnu-verified-subr-holes)
    ;; `Fcomp__register_subr' takes MINARGS in %edx (offset 54) and MAXARGS
    ;; in %ecx (offset 45).
    (let ((maxarg (nelisp-eln-registration--gnu-arity actual 45))
          (minarg (nelisp-eln-registration--gnu-arity actual 54))
          (type-index (nelisp-eln-registration--d-reloc-index actual 43)))
      (unless (and (nelisp-eln-registration--validate-rip-relocs
                    handle (nth 3 cap) actual
                    '((7 . "d_reloc_eph") (17 . "d_reloc")
                      (36 . "freloc_link_table")))
                   minarg maxarg
                   (or (= minarg maxarg)
                       (and (= minarg 1) (= maxarg 2)))
                   ;; The d_reloc_eph words the registration reads sit one
                   ;; word later when MIN and MAX both precede the name
                   ;; (variable arity): 0x18/0x10/0x08 for a fixed arity,
                   ;; 0x20/0x18/0x10 for 1..2.
                   (let ((shift (if (= minarg maxarg) 0 8)))
                     (and (= (aref actual 24) (+ #x18 shift))
                          (= (aref actual 28) (+ #x10 shift))
                          (= (aref actual 32) (+ #x08 shift))))
                   type-index (> type-index 0))
        (nelisp-eln-registration--fail 'top-level-instructions-not-admitted
                                       (list (length actual) actual
                                             'gnu-verified-subr)))
      (list maxarg type-index minarg))))

(defconst nelisp-eln-registration--gnu-require-subr-template
  (unibyte-string
   #x48 #x8b #x05 0 0 0 0 #x41 #x54 #x55 #x48 #x8b
   #x2d 0 0 0 0 #x53 #x4c #x8b #x20 #x48 #x89 #xfb
   #x48 #x8b #x75 0 #x48 #x8b #x7d 0 #x41 #xff #x94 #x24
   #x98 #x1d 0 0 #x48 #x8b #x05 0 0 0 0 #x4c
   #x8b #x45 0 #xb9 0 0 0 0 #x48 #x83 #xec #x08
   #xba 0 0 0 0 #x4c #x8b #x48 #x18 #x48 #x8b #x70
   #x10 #x48 #x8b #x78 #x08 #x53 #x41 #xff #x94 #x24 #x30 #x20
   0 0 #x5a #x59 #x5b #x5d #x41 #x5c #xc3)
  "Genuine GNU 31.1 x86-64 (Feval, register_subr) top_level_run skeleton
whose Feval form is a file-level `(require \\='FEATURE)' (S6.15).
Byte-for-byte identical to gnu-byte-compile-constant.eln's own 93-byte
top_level_run (sha256 of that artifact: f837d057..86f5de) and
gnu-byte-compile-setq.eln's (869a0c42..5b96d8d38) outside its variable fields, see
`nelisp-eln-registration--gnu-require-subr-holes'.  GNU emits it for a
file whose only top-level forms are `(require \\='FEATURE)' and one
fixed-arity `defun': the require is evaluated first, then the subr is
registered with its `subr-type' read from a d_reloc slot.")

(defconst nelisp-eln-registration--gnu-require-subr-holes
  '((3 . 7) (13 . 17) (27 . 28) (31 . 32) (43 . 47) (50 . 51)
    (52 . 56) (61 . 65))
  "Variable byte ranges in
`nelisp-eln-registration--gnu-require-subr-template': the RIP-relative
freloc_link_table and d_reloc loads, the Feval call's lexenv and form
d_reloc slot offsets, the RIP-relative d_reloc_eph load, the
register_subr call's `subr-type' d_reloc slot offset, and its max and
min arity immediates.")

(defconst nelisp-eln-registration--admitted-require-features
  '(bytecomp cconv macroexp)
  "Features a `gnu-require-subr' top_level_run may `require'.
The artifact's Feval form is never evaluated; after it statically
decodes to exactly `(require \\='FEATURE)' with FEATURE in this list,
the admitted call site runs NeLisp's own `require' of FEATURE.")

(defun nelisp-eln-registration--gnu-require-subr-top-level (handle cap actual)
  "Return (ARITY TYPE-INDEX LEXENV-INDEX FORM-INDEX) for the checked
`gnu-require-subr' thunk, or nil when ACTUAL is not that exact shape.
Signal when it is, but its RIP-relative loads, arity immediates or slot
offsets do not authenticate.  What the three d_reloc slots hold is
checked against the artifact's own data relocations during preflight
\(`nelisp-eln-registration--metadata-data-count' and
`nelisp-eln-registration--require-effect')."
  (when (nelisp-eln-registration--match-holed-template
         actual nelisp-eln-registration--gnu-require-subr-template
         nelisp-eln-registration--gnu-require-subr-holes)
    (let ((maxarg (nelisp-eln-registration--gnu-arity actual 52))
          (minarg (nelisp-eln-registration--gnu-arity actual 61))
          (lexenv-index (nelisp-eln-registration--d-reloc-index actual 27))
          (form-index (nelisp-eln-registration--d-reloc-index actual 31))
          (type-index (nelisp-eln-registration--d-reloc-index actual 50)))
      (unless (and (nelisp-eln-registration--validate-rip-relocs
                    handle (nth 3 cap) actual
                    '((3 . "freloc_link_table") (13 . "d_reloc")
                      (43 . "d_reloc_eph")))
                   minarg maxarg (= minarg maxarg)
                   type-index lexenv-index form-index
                   (/= type-index lexenv-index) (/= type-index form-index)
                   (/= lexenv-index form-index))
        (nelisp-eln-registration--fail 'top-level-instructions-not-admitted
                                       (list (length actual) actual
                                             'gnu-require-subr)))
      (list minarg type-index lexenv-index form-index))))

;; S6.6: the same skeleton for a function with `&optional' arguments
;; (min < max arity).  GNU then keeps BOTH arities in d_reloc_eph
;; ([MIN MAX NAME C-NAME REST], 40 bytes) so the register_subr call's
;; name/c-name/rest loads sit one word higher (eph+0x10/0x18/0x20).
(defconst nelisp-eln-registration--gnu-require-subr-opt-template
  (let ((template (copy-sequence
                   nelisp-eln-registration--gnu-require-subr-template)))
    (aset template 68 #x20)
    (aset template 72 #x18)
    (aset template 76 #x10)
    template)
  "Genuine GNU 31.1 (Feval, register_subr) top_level_run skeleton for a
`(require \\='FEATURE)' file whose one `defun' has `&optional' arguments
\(S6.6, gnu-cconv-closure-convert.eln: sha256 of that artifact is
recorded in tools/ai/eln-progress.org).  Byte-for-byte
`nelisp-eln-registration--gnu-require-subr-template' except that the
three ephemeral-relocation loads (name, c-name, rest) are one word
higher; the holes are the same.")

(defun nelisp-eln-registration--gnu-require-subr-opt-top-level
    (handle cap actual)
  "Return (MAXARG TYPE-INDEX LEXENV-INDEX FORM-INDEX MINARG) for the
checked optional-argument `gnu-require-subr' thunk, or nil when ACTUAL is
not that exact shape.  Signal when it is, but its RIP-relative loads,
arity immediates or slot offsets do not authenticate.  Requires
0 <= MINARG < MAXARG <= 8; the fixed-arity case is
`nelisp-eln-registration--gnu-require-subr-top-level'."
  (when (nelisp-eln-registration--match-holed-template
         actual nelisp-eln-registration--gnu-require-subr-opt-template
         nelisp-eln-registration--gnu-require-subr-holes)
    (let ((maxarg (nelisp-eln-registration--gnu-arity actual 52))
          (minarg (nelisp-eln-registration--gnu-arity actual 61))
          (lexenv-index (nelisp-eln-registration--d-reloc-index actual 27))
          (form-index (nelisp-eln-registration--d-reloc-index actual 31))
          (type-index (nelisp-eln-registration--d-reloc-index actual 50)))
      (unless (and (nelisp-eln-registration--validate-rip-relocs
                    handle (nth 3 cap) actual
                    '((3 . "freloc_link_table") (13 . "d_reloc")
                      (43 . "d_reloc_eph")))
                   minarg maxarg (< minarg maxarg)
                   type-index lexenv-index form-index
                   (/= type-index lexenv-index) (/= type-index form-index)
                   (/= lexenv-index form-index))
        (nelisp-eln-registration--fail 'top-level-instructions-not-admitted
                                       (list (length actual) actual
                                             'gnu-require-subr-opt)))
      (list maxarg type-index lexenv-index form-index minarg))))

;; S6.9 (`byte-compile-lambda'): the same skeleton once the function's
;; d_reloc slot indices exceed 15, so GNU encodes every d_reloc load with a
;; four-byte displacement (`mov 0x170(%rbp),%rsi' is `48 8b b5 disp32'),
;; and the function has `&optional' arguments (min < max) while its
;; ephemeral vector keeps the plain [1 NAME C-NAME REST] layout (name at
;; +8, c-name +0x10, rest +0x18, all fixed bytes below).
(defconst nelisp-eln-registration--gnu-require-subr-wide-template
  (unibyte-string
   #x48 #x8b #x05 0 0 0 0 #x41 #x54 #x55 #x48 #x8b
   #x2d 0 0 0 0 #x53 #x4c #x8b #x20 #x48 #x89 #xfb
   #x48 #x8b #xb5 0 0 0 0 #x48 #x8b #xbd 0 0 0 0
   #x41 #xff #x94 #x24 #x98 #x1d #x00 #x00
   #x48 #x8b #x05 0 0 0 0 #x4c #x8b #x85 0 0 0 0
   #xb9 0 0 0 0 #x48 #x83 #xec #x08
   #xba 0 0 0 0 #x4c #x8b #x48 #x18 #x48 #x8b #x70 #x10
   #x48 #x8b #x78 #x08 #x53 #x41 #xff #x94 #x24 #x30 #x20 #x00 #x00
   #x5a #x59 #x5b #x5d #x41 #x5c #xc3)
  "Genuine GNU 31.1 x86-64 (Feval, register_subr) top_level_run skeleton
for a `(require \\='FEATURE)' file whose one `defun' has `&optional'
arguments and whose d_reloc slots need four-byte displacements (S6.9,
gnu-byte-compile-lambda.eln; 102 bytes).")

(defconst nelisp-eln-registration--gnu-require-subr-wide-holes
  '((3 . 7) (13 . 17) (27 . 31) (34 . 38) (49 . 53) (56 . 60)
    (61 . 65) (70 . 74))
  "Variable byte ranges of
`nelisp-eln-registration--gnu-require-subr-wide-template': the RIP loads
of freloc_link_table (3), d_reloc (13) and d_reloc_eph (49), the Feval
lexenv (27) and form (34) and register_subr type (56) d_reloc slot
displacements, and the max (61) and min (70) arity immediates.")

(defun nelisp-eln-registration--gnu-require-subr-wide-top-level
    (handle cap actual)
  "Return (MAXARG TYPE-INDEX LEXENV-INDEX FORM-INDEX MINARG) for the
checked wide `gnu-require-subr' thunk, or nil when ACTUAL is not that
exact shape.  Signal when it is, but a RIP load, arity immediate or slot
offset does not authenticate.  Requires 0 <= MINARG < MAXARG <= 8."
  (when (nelisp-eln-registration--match-holed-template
         actual nelisp-eln-registration--gnu-require-subr-wide-template
         nelisp-eln-registration--gnu-require-subr-wide-holes)
    (let ((maxarg (nelisp-eln-registration--gnu-arity actual 61))
          (minarg (nelisp-eln-registration--gnu-arity actual 70))
          (lexenv-index (nelisp-eln-registration--d-reloc-index32 actual 27))
          (form-index (nelisp-eln-registration--d-reloc-index32 actual 34))
          (type-index (nelisp-eln-registration--d-reloc-index32 actual 56)))
      (unless (and (nelisp-eln-registration--validate-rip-relocs
                    handle (nth 3 cap) actual
                    '((3 . "freloc_link_table") (13 . "d_reloc")
                      (49 . "d_reloc_eph")))
                   minarg maxarg (< minarg maxarg)
                   type-index lexenv-index form-index
                   (/= type-index lexenv-index) (/= type-index form-index)
                   (/= lexenv-index form-index))
        (nelisp-eln-registration--fail 'top-level-instructions-not-admitted
                                       (list (length actual) actual
                                             'gnu-require-subr-wide)))
      (list maxarg type-index lexenv-index form-index minarg))))

(defconst nelisp-eln-registration--gnu-lambda-require-subr-template
  (unibyte-string
   #x41 #x55 #xb9 0 0 0 0 #xba 0 0 0 0
   #x41 #x54 #x55 #x48 #x89 #xfd #x53 #x48 #x83 #xec #x10 #x48
   #x8b #x1d 0 0 0 0 #x4c #x8b #x25 0 0 0
   0 #x48 #x8b #x05 0 0 0 0 #x4c #x8b #x4b #x08
   #x4d #x8b #x84 #x24 0 0 0 0 #x4c #x8b #x28 #x48
   #x8b #x33 #x57 #xbf 0 0 0 0 #x41 #xff #x95 #x38
   #x20 #x00 #x00 #x4c #x8b #x4b #x18 #x48 #x8b #x73 #x10 #xb9
   0 0 0 0 #x4d #x8b #x84 #x24 0 0 0 0
   #xba 0 0 0 0 #x48 #x89 #x2c #x24 #xbf 0 0
   0 0 #x41 #xff #x95 #x38 #x20 #x00 #x00 #x49 #x8b #xb4
   #x24 0 0 0 0 #x49 #x8b #xbc #x24 0 0 0
   0 #x41 #xff #x95 #x98 #x1d #x00 #x00 #x4c #x8b #x4b #x38
   #x48 #x8b #x73 #x30 #xb9 0 0 0 0 #x4d #x8b #x84
   #x24 0 0 0 0 #x48 #x8b #x7b #x28 #xba 0 0
   0 0 #x48 #x89 #x2c #x24 #x41 #xff #x95 #x30 #x20 #x00
   #x00 #x48 #x83 #xc4 #x18 #x5b #x5d #x41 #x5c #x41 #x5d #xc3)
  "Genuine GNU 31.1 x86-64 top_level_run skeleton (S6.12) that registers
two native anonymous lambdas (two `Fcomp__register_lambda' calls, slot
1031), evaluates a file-level `(require \\='FEATURE)' (slot 947 `Feval'),
then registers one fixed-arity subr (slot 1030 `Fcomp__register_subr').
It is byte-for-byte the 192-byte top_level_run of gnu-byte-compile-if.eln
outside `nelisp-eln-registration--gnu-lambda-require-subr-holes'.  Every
import slot, every callee-saved-register move and every d_reloc_eph field
offset is fixed; only the RIP-relative loads, the two lambda d_reloc
indices, the arities and the d_reloc slot offsets are variable.")

(defconst nelisp-eln-registration--gnu-lambda-require-subr-holes
  '((3 . 7) (8 . 12) (26 . 30) (33 . 37) (40 . 44) (52 . 56) (64 . 68)
    (84 . 88) (92 . 96) (97 . 101) (106 . 110) (121 . 125) (129 . 133)
    (149 . 153) (157 . 161) (166 . 170))
  "Variable byte ranges of
`nelisp-eln-registration--gnu-lambda-require-subr-template': the first
lambda's max and min arity (3, 8), the RIP-relative d_reloc_eph,
d_reloc and freloc_link_table loads (26, 33, 40), the lambda type slot
offset (52) and first lambda d_reloc index (64), the second lambda's
max arity (84), type slot offset (92), min arity (97) and d_reloc index
\(106), the Feval lexenv and form slot offsets (121, 129), and the
registered subr's max arity (149), type slot offset (157) and min arity
\(166).")

(defconst nelisp-eln-registration--lambda-c-name-prefix
  "F616e6f6e796d6f75732d6c616d626461_anonymous_lambda_"
  "GNU's mangled C name prefix for anonymous native lambdas.")

(defun nelisp-eln-registration--lambda-c-name-p (name &optional suffix)
  "Non-nil when NAME is the anonymous-lambda C name (with SUFFIX if given)."
  (and (stringp name)
       (string-prefix-p nelisp-eln-registration--lambda-c-name-prefix name)
       (let ((tail (substring name
                              (length nelisp-eln-registration--lambda-c-name-prefix))))
         (and (> (length tail) 0)
              (string-match-p "\\`[0-9]+\\'" tail)
              (or (null suffix) (equal tail suffix))))))

(defun nelisp-eln-registration--d-reloc-index32 (actual offset)
  "Decode the four-byte d_reloc slot offset at ACTUAL's OFFSET to its index.
Return nil unless it is a multiple of eight below 65536."
  (let ((raw (nelisp-eln-registration--read-u32-le actual offset)))
    (and (= (mod raw 8) 0) (< raw 65536) (/ raw 8))))

(defun nelisp-eln-registration--gnu-fixnum-immediate (actual offset)
  "Decode the fixnum immediate at ACTUAL's four bytes from OFFSET.
Return the integer in [0, 8191], or nil."
  (let ((value (nelisp-eln-abi-decode-immediate
                (nelisp-eln-registration--read-u32-le actual offset))))
    (and (integerp value) (<= 0 value 8191) value)))

(defun nelisp-eln-registration--gnu-lambda-require-subr-top-level
    (handle cap actual)
  "Return the decoded fields of the checked `gnu-lambda-require-subr' thunk,
or nil when ACTUAL is not that exact shape: (ARITY TYPE-INDEX LEXENV-INDEX
FORM-INDEX LAMBDA-TYPE-INDEX LAMBDA-IDX1 LAMBDA-IDX2 LAMBDA-ARITY).  Signal
when it is, but its RIP-relative loads, arities or slot offsets do not
authenticate.  What the d_reloc slots hold is checked against the
artifact's own data relocations during preflight."
  (when (nelisp-eln-registration--match-holed-template
         actual nelisp-eln-registration--gnu-lambda-require-subr-template
         nelisp-eln-registration--gnu-lambda-require-subr-holes)
    (let ((max1 (nelisp-eln-registration--gnu-arity actual 3))
          (min1 (nelisp-eln-registration--gnu-arity actual 8))
          (max2 (nelisp-eln-registration--gnu-arity actual 84))
          (min2 (nelisp-eln-registration--gnu-arity actual 97))
          (maxm (nelisp-eln-registration--gnu-arity actual 149))
          (minm (nelisp-eln-registration--gnu-arity actual 166))
          (idx1 (nelisp-eln-registration--gnu-fixnum-immediate actual 64))
          (idx2 (nelisp-eln-registration--gnu-fixnum-immediate actual 106))
          (ltype1 (nelisp-eln-registration--d-reloc-index32 actual 52))
          (ltype2 (nelisp-eln-registration--d-reloc-index32 actual 92))
          (lexenv (nelisp-eln-registration--d-reloc-index32 actual 121))
          (form (nelisp-eln-registration--d-reloc-index32 actual 129))
          (type (nelisp-eln-registration--d-reloc-index32 actual 157)))
      (unless (and (nelisp-eln-registration--validate-rip-relocs
                    handle (nth 3 cap) actual
                    '((26 . "d_reloc_eph") (33 . "d_reloc")
                      (40 . "freloc_link_table")))
                   min1 max1 (= min1 max1) min2 max2 (= min2 max2)
                   (= min1 min2) minm maxm (= minm maxm)
                   idx1 idx2 (/= idx1 idx2)
                   ltype1 ltype2 (= ltype1 ltype2)
                   lexenv form type
                   (/= type lexenv) (/= type form) (/= lexenv form)
                   (/= ltype1 type) (/= ltype1 lexenv) (/= ltype1 form))
        (nelisp-eln-registration--fail 'top-level-instructions-not-admitted
                                       (list (length actual) actual
                                             'gnu-lambda-require-subr)))
      (list minm type lexenv form ltype1 idx1 idx2 min1))))

(defun nelisp-eln-registration--require-effect (metadata form-index lexenv-index)
  "Return the FEATURE a `gnu-require-subr' Feval call site requires.
FORM-INDEX and LEXENV-INDEX are the d_reloc slots the admitted
top_level_run passes to Feval.  The constants come only from the
artifact's own statically decoded data relocations: the lexical
environment must be `t' and the form exactly `(require \\='FEATURE)'
with FEATURE in `nelisp-eln-registration--admitted-require-features'.
Anything else signals `eval-form-not-admitted'; the form itself is never
evaluated."
  (let* ((reloc (plist-get metadata :data-relocations))
         (count (and (vectorp reloc) (length reloc))))
    (unless (and count (integerp form-index) (integerp lexenv-index)
                 (< -1 form-index count) (< -1 lexenv-index count))
      (nelisp-eln-registration--fail 'eval-form-not-admitted 'index))
    (let ((lexenv (aref reloc lexenv-index))
          (form (aref reloc form-index)))
      (unless (eq lexenv t)
        (nelisp-eln-registration--fail 'eval-form-not-admitted 'lexenv))
      (unless (and (proper-list-p form) (= (length form) 2)
                   (eq (car form) 'require)
                   (proper-list-p (nth 1 form)) (= (length (nth 1 form)) 2)
                   (eq (car (nth 1 form)) 'quote)
                   (symbolp (nth 1 (nth 1 form)))
                   (memq (nth 1 (nth 1 form))
                         nelisp-eln-registration--admitted-require-features))
        (nelisp-eln-registration--fail 'eval-form-not-admitted
                                       (list 'require form)))
      (nth 1 (nth 1 form)))))

(defun nelisp-eln-registration--verified-optional-subr-type-p
    (type min-arity max-arity)
  "Return non-nil if TYPE is a genuine (function ARGS VALUE) constant whose
ARGS is exactly MIN-ARITY types, one `&optional', then MAX-ARITY minus
MIN-ARITY types (no `&rest')."
  (and (consp type) (eq (car type) 'function)
       (proper-list-p type) (= (length type) 3)
       (proper-list-p (nth 1 type))
       (< min-arity max-arity)
       (= (length (nth 1 type)) (1+ max-arity))
       (eq (nth min-arity (nth 1 type)) '&optional)
       (= (cl-count '&optional (nth 1 type)) 1)
       (not (memq '&rest (nth 1 type)))))

(defun nelisp-eln-registration--verified-subr-type-p (type arity)
  "Return non-nil if TYPE is a genuine fixed-ARITY `subr-type' constant.
GNU's native compiler records a defun's type as (function ARGS VALUE);
for the fixed-arity `gnu-verified-subr' profile ARGS must be a proper
list of exactly ARITY types with no `&optional' / `&rest' marker.  A
top_level_run pointing its type load at any other d_reloc slot (nil,
`t', a predicate symbol, ...) is rejected before anything runs."
  (and (consp type) (eq (car type) 'function)
       (proper-list-p type) (= (length type) 3)
       (proper-list-p (nth 1 type))
       (= (length (nth 1 type)) arity)
       (not (memq '&optional (nth 1 type)))
       (not (memq '&rest (nth 1 type)))))

(defconst nelisp-eln-registration--gnu-eval-subr-template
  (unibyte-string
   #x41 #x54 #x48 #x8b #x05 0 0 0 0 #x48 #x89 #xfa
   #xb9 0 0 0 0 #x55 #x48 #x8b #x2d 0 0 0
   0 #x53 #x4c #x8b #x20 #x48 #x8b #x05 0 0 0 0
   #x4c #x8b #x45 0 #x48 #x83 #xec #x08 #x4c #x8b #x48 #x18
   #x48 #x8b #x70 #x10 #x48 #x8b #x78 #x08 #x52 #xba 0 0
   0 0 #x41 #xff #x94 #x24 #x30 #x20 0 0 #x48 #x8b
   #x75 0 #x48 #x8b #x7d 0 #x48 #x89 #xc3 #x41 #xff #x94
   #x24 #x98 #x1d 0 0 #x58 #x48 #x89 #xd8 #x5a #x5b #x5d
   #x41 #x5c #xc3)
  "Genuine GNU 31.1 x86-64 (register_subr, Feval) top_level_run skeleton.
Byte-for-byte identical to gnu-caar/cadr/fixnump/bignump/
frame-configuration-p.eln's own top_level_run outside its variable
fields; see `nelisp-eln-registration--gnu-eval-subr-holes'.")

(defconst nelisp-eln-registration--gnu-eval-subr-holes
  '((5 . 9) (13 . 17) (21 . 25) (32 . 36) (39 . 40) (58 . 62) (73 . 74) (77 . 78))
  "Variable byte ranges in `nelisp-eln-registration--gnu-eval-subr-template':
the three RIP-relative pointer loads (freloc_link_table, d_reloc,
d_reloc_eph, in that order), the register_subr call's shared min/max
arity immediate, the register_subr call's `type' d_reloc slot offset,
then the Feval call's lexenv and form d_reloc slot offsets.")

(defconst nelisp-eln-registration--gnu-eval-subr-pair-template
  (unibyte-string
   #x41 #x55 #xb9 0 0 0 0 #xba 0 0 0 0
   #x41 #x54 #x55 #x48 #x89 #xfd #x53 #x48 #x83 #xec #x10 #x48
   #x8b #x1d 0 0 0 0 #x4c #x8b #x25 0 0 0
   0 #x48 #x8b #x05 0 0 0 0 #x4c #x8b #x4b #x18
   #x4d #x8b #x44 #x24 0 #x4c #x8b #x28 #x48 #x8b #x73 #x10
   #x48 #x8b #x7b #x08 #x55 #x41 #xff #x95 #x30 #x20 0 0
   #x49 #x8b #x74 #x24 0 #x49 #x8b #x7c #x24 0 #x41 #xff
   #x95 #x98 #x1d 0 0 #x4c #x8b #x4b #x38 #x4d #x8b #x44
   #x24 0 #xb9 0 0 0 0 #x48 #x8b #x73 #x30 #x48
   #x8b #x7b #x28 #xba 0 0 0 0 #x48 #x89 #x2c #x24
   #x41 #xff #x95 #x30 #x20 0 0 #x48 #x83 #xc4 #x18 #x5b
   #x5d #x41 #x5c #x41 #x5d #xc3)
  "Genuine GNU 31.1 x86-64 (register_subr, Feval, register_subr) pair
top_level_run skeleton.  Byte-for-byte identical to gnu-zerop.eln's own
top_level_run outside its variable fields; see
`nelisp-eln-registration--gnu-eval-subr-pair-holes'.  GNU emits this
shape when a defun's compiler-macro is itself compiled to a second
native subr and registered right after the main one.")

(defconst nelisp-eln-registration--gnu-eval-subr-pair-holes
  '((3 . 7) (8 . 12) (26 . 30) (33 . 37) (40 . 44) (52 . 53) (76 . 77)
    (81 . 82) (97 . 98) (99 . 103) (112 . 116))
  "Variable byte ranges in
`nelisp-eln-registration--gnu-eval-subr-pair-template': the first
register_subr call's min/max arity immediate and `type' d_reloc slot
offset, the three RIP-relative pointer loads (d_reloc_eph, d_reloc,
freloc_link_table, in that order), the Feval call's lexenv and form
d_reloc slot offsets, and the second register_subr call's `type' d_reloc
slot offset and min/max arity immediate.")

(defconst nelisp-eln-registration--bytecode-constant0 192
  "Byte-code opcode for `push constant #0' (Bconstant); GNU's own base.")
(defconst nelisp-eln-registration--bytecode-call3 35
  "Byte-code opcode for a 3-argument call (Bcall3).")
(defconst nelisp-eln-registration--bytecode-return 135
  "Byte-code opcode for `return top of stack' (Breturn).")

(defun nelisp-eln-registration--safe-constant-p (value)
  "Return non-nil if VALUE is inert data: no closures, subrs, or vectors."
  (cond
   ((or (symbolp value) (integerp value) (stringp value)) t)
   ((consp value)
    (and (nelisp-eln-registration--safe-constant-p (car value))
         (nelisp-eln-registration--safe-constant-p (cdr value))))
   (t nil)))

(defun nelisp-eln-registration--eval-effect (metadata form-index lexenv-index name)
  "Authenticate METADATA's Feval call and return its exact effect.

FORM-INDEX and LEXENV-INDEX are `:data-relocations' slot indices already
extracted from an admitted `nelisp-eln-registration--top-level-code' call
site; NAME is the symbol that same top_level_run call site already
registers via `Fcomp__register_subr'.  Both constants come only from the
artifact's own already-decoded data relocations (`nelisp-eln-metadata';
static reading, never execution).

Genuine GNU top_level_run units evaluate a compiled call to
`function-put' (one or more times, always tagging the same NAME) with
`Feval''s LEXICAL argument set to `t' -- an empty lexical environment,
since the compiled form never references a lexical variable.  This
recognizes exactly that shape and returns the list of (PROP . VALUE)
pairs it installs.  Anything else -- a different LEXICAL value, a
different opcode sequence, a callee other than `function-put', a NAME
mismatch, or a VALUE that is not inert data -- signals
`eval-form-not-admitted' instead of ever calling `eval' or `byte-code' on
the artifact's own bytes."
  (let* ((reloc (plist-get metadata :data-relocations))
         (count (and (vectorp reloc) (length reloc))))
    (unless (and count (integerp form-index) (integerp lexenv-index)
                 (<= 0 form-index) (< form-index count)
                 (<= 0 lexenv-index) (< lexenv-index count))
      (nelisp-eln-registration--fail 'eval-form-not-admitted 'index))
    (let ((lexenv (aref reloc lexenv-index))
          (form (aref reloc form-index)))
      (unless (eq lexenv t)
        (nelisp-eln-registration--fail 'eval-form-not-admitted 'lexenv))
      (unless (and (listp form) (= (length form) 4)
                   (eq (nth 0 form) 'byte-code)
                   (stringp (nth 1 form)) (vectorp (nth 2 form))
                   (integerp (nth 3 form)))
        (nelisp-eln-registration--fail 'eval-form-not-admitted 'shape))
      (let* ((code (nth 1 form)) (consts (nth 2 form)) (n (length code))
             (nconsts (length consts)) (effects nil) (i 0))
        (unless (and (>= n 7) (= (mod (- n 2) 5) 0)
                     (>= nconsts 2) (eq (aref consts 0) 'function-put)
                     (eq (aref consts 1) name)
                     (= (aref code (- n 2))
                        nelisp-eln-registration--bytecode-constant0)
                     (= (aref code (1- n))
                        nelisp-eln-registration--bytecode-return))
          (nelisp-eln-registration--fail 'eval-form-not-admitted 'header))
        (while (< i (- n 2))
          (let ((c0 (aref code i)) (c1 (aref code (1+ i)))
                (prop-idx (aref code (+ i 2))) (val-idx (aref code (+ i 3)))
                (call (aref code (+ i 4))))
            (unless (and (= c0 nelisp-eln-registration--bytecode-constant0)
                         (= c1 (1+ nelisp-eln-registration--bytecode-constant0))
                         (= call nelisp-eln-registration--bytecode-call3)
                         (>= prop-idx nelisp-eln-registration--bytecode-constant0)
                         (>= val-idx nelisp-eln-registration--bytecode-constant0)
                         (< (- prop-idx nelisp-eln-registration--bytecode-constant0)
                            nconsts)
                         (< (- val-idx nelisp-eln-registration--bytecode-constant0)
                            nconsts))
              (nelisp-eln-registration--fail 'eval-form-not-admitted 'block))
            (let ((prop (aref consts (- prop-idx
                                        nelisp-eln-registration--bytecode-constant0)))
                  (value (aref consts (- val-idx
                                         nelisp-eln-registration--bytecode-constant0))))
              (unless (symbolp prop)
                (nelisp-eln-registration--fail 'eval-form-not-admitted 'prop))
              (unless (nelisp-eln-registration--safe-constant-p value)
                (nelisp-eln-registration--fail 'eval-form-not-admitted 'value))
              (push (cons prop value) effects)))
          (setq i (+ i 5)))
        (nreverse effects)))))

(defun nelisp-eln-registration--top-level-code (handle)
  "Validate a pinned emitter profile or GNU31 single-leaf top_level_run."
  (let* ((cap (nelisp-eln-system-loader-function-capability
               handle "top_level_run"))
         (size (nth 6 cap))
         (actual (nelisp-eln-system-loader-read-root-function-bytes
                  handle "top_level_run" 0 size))
         (relocs '((7 . "d_reloc_eph") (32 . "d_reloc")
                   (42 . "d_reloc_eph") (61 . "freloc_link_table")))
         (arity nil) (expected nil) (profile nil) (relocs-ok t) (extra nil)
         (base (nth 3 cap)))
    ;; S7.7.4: try every GNU-family template before ever touching the
    ;; self-emitter's own template or requiring `nelisp-eln-emitter'.
    ;; Each of the four checks below (this one and the next three) is a
    ;; pure byte comparison against a template already resident in this
    ;; file -- a fixed static pattern, or `--match-holed-template' -- and
    ;; each returns/no-ops immediately once ACTUAL's own SIZE (already
    ;; read above, straight from the artifact's authenticated ELF
    ;; function-capability lookup, before any template is even
    ;; consulted) does not match that template's own fixed length. So
    ;; for every genuine or corrupted GNU-compiled artifact in the S7.7.4
    ;; corpus -- the overwhelming majority of one gate run's (artifact,
    ;; check) matrix -- this block alone decides the profile, or rules
    ;; out every GNU shape, without ever loading `nelisp-eln-emitter'
    ;; (and, through it, `nelisp-aot-compiler' and `nelisp-elf-write',
    ;; ~24,600 lines / ~1MB of source): only an artifact none of these
    ;; four admit -- this process's own self-emitted fixture, or
    ;; something genuinely unrecognized -- ever reaches the self-emitter
    ;; fallback below, which is the sole remaining consumer of the
    ;; emitter in this function.
    (unless expected
      (let ((gnu (nelisp-eln-registration--gnu-single-top-level-arity
                  handle cap actual)))
        (when gnu
          (setq arity (car gnu) expected t profile 'gnu-single-leaf))))
    (unless expected
      (let ((verified (nelisp-eln-registration--gnu-verified-subr-top-level
                       handle cap actual)))
        (when verified
          (setq arity (nth 0 verified) expected t profile 'gnu-verified-subr
                extra (if (= (nth 2 verified) (nth 0 verified))
                          (list :type-index (nth 1 verified))
                        ;; S6.4: `(A &optional B)', 1..2 -- the same
                        ;; `:min-arity'/`:eph-offset' extras S6.6's
                        ;; optional `gnu-require-subr' uses.
                        (list :type-index (nth 1 verified)
                              :min-arity (nth 2 verified)
                              :eph-offset 1))))))
    (unless expected
      (let ((required (nelisp-eln-registration--gnu-require-subr-top-level
                       handle cap actual)))
        (when required
          (setq arity (nth 0 required) expected t profile 'gnu-require-subr
                extra (list :type-index (nth 1 required)
                            :lexenv-index (nth 2 required)
                            :form-index (nth 3 required))))))
    (unless expected
      (let ((required (nelisp-eln-registration--gnu-require-subr-opt-top-level
                       handle cap actual)))
        (when required
          (setq arity (nth 0 required) expected t profile 'gnu-require-subr
                extra (list :type-index (nth 1 required)
                            :lexenv-index (nth 2 required)
                            :form-index (nth 3 required)
                            :min-arity (nth 4 required)
                            :eph-offset 1)))))
    (unless expected
      (let ((required (nelisp-eln-registration--gnu-require-subr-wide-top-level
                       handle cap actual)))
        (when required
          (setq arity (nth 0 required) expected t profile 'gnu-require-subr
                extra (list :type-index (nth 1 required)
                            :lexenv-index (nth 2 required)
                            :form-index (nth 3 required)
                            :min-arity (nth 4 required))))))
    (unless expected
      (let ((lreq (nelisp-eln-registration--gnu-lambda-require-subr-top-level
                   handle cap actual)))
        (when lreq
          (setq arity (nth 0 lreq) expected t profile 'gnu-lambda-require-subr
                extra (list :type-index (nth 1 lreq)
                            :lexenv-index (nth 2 lreq)
                            :form-index (nth 3 lreq)
                            :lambda-type-index (nth 4 lreq)
                            :lambda-idx1 (nth 5 lreq)
                            :lambda-idx2 (nth 6 lreq)
                            :lambda-arity (nth 7 lreq))))))
    (unless expected
      (when (nelisp-eln-registration--match-holed-template
             actual nelisp-eln-registration--gnu-eval-subr-template
             nelisp-eln-registration--gnu-eval-subr-holes)
        (unless (nelisp-eln-registration--validate-rip-relocs
                 handle base actual
                 '((32 . "d_reloc_eph") (21 . "d_reloc") (5 . "freloc_link_table")))
          (nelisp-eln-registration--fail 'top-level-instructions-not-admitted
                                         (list size actual 'gnu-eval-subr)))
        (let ((minarg (nelisp-eln-registration--gnu-arity actual 13))
              (maxarg (nelisp-eln-registration--gnu-arity actual 58))
              (type-index (nelisp-eln-registration--d-reloc-index actual 39))
              (lexenv-index (nelisp-eln-registration--d-reloc-index actual 73))
              (form-index (nelisp-eln-registration--d-reloc-index actual 77)))
          (unless (and minarg maxarg (= minarg maxarg)
                       type-index lexenv-index form-index)
            (nelisp-eln-registration--fail 'top-level-instructions-not-admitted
                                           (list size actual 'gnu-eval-subr)))
          (setq arity minarg expected t profile 'gnu-eval-subr
                extra (list :type-index type-index
                            :lexenv-index lexenv-index
                            :form-index form-index)))))
    (unless expected
      (when (nelisp-eln-registration--match-holed-template
             actual nelisp-eln-registration--gnu-eval-subr-pair-template
             nelisp-eln-registration--gnu-eval-subr-pair-holes)
        (unless (nelisp-eln-registration--validate-rip-relocs
                 handle base actual
                 '((26 . "d_reloc_eph") (33 . "d_reloc") (40 . "freloc_link_table")))
          (nelisp-eln-registration--fail 'top-level-instructions-not-admitted
                                         (list size actual 'gnu-eval-subr-pair)))
        (let ((minarg (nelisp-eln-registration--gnu-arity actual 3))
              (maxarg (nelisp-eln-registration--gnu-arity actual 8))
              (minarg2 (nelisp-eln-registration--gnu-arity actual 99))
              (maxarg2 (nelisp-eln-registration--gnu-arity actual 112))
              (type-index (nelisp-eln-registration--d-reloc-index actual 52))
              (lexenv-index (nelisp-eln-registration--d-reloc-index actual 76))
              (form-index (nelisp-eln-registration--d-reloc-index actual 81))
              (type2-index (nelisp-eln-registration--d-reloc-index actual 97)))
          (unless (and minarg maxarg (= minarg maxarg)
                       minarg2 maxarg2 (= minarg2 maxarg2)
                       type-index lexenv-index form-index type2-index)
            (nelisp-eln-registration--fail 'top-level-instructions-not-admitted
                                           (list size actual 'gnu-eval-subr-pair)))
          (setq arity minarg expected t profile 'gnu-eval-subr-pair
                extra (list :type-index type-index
                            :lexenv-index lexenv-index
                            :form-index form-index
                            :type2-index type2-index
                            :arity2 minarg2)))))
    ;; No GNU-family template matched: this is either this process's own
    ;; self-emitted fixture or an artifact admitted under no authenticated
    ;; template at all. Only now does admission need the lightweight
    ;; `nelisp-eln-emitter-templates' feature -- required lazily here,
    ;; guarded by `fboundp' (not just `featurep'): a test that mocks this
    ;; exact function via `cl-letf' (without loading the real module) must
    ;; not have that mock clobbered by a `require' here re-defining it.
    (unless expected
      (unless (fboundp 'nelisp-eln-emitter--top-level-code)
        (require 'nelisp-eln-emitter-templates))
      (dolist (candidate '(0 1))
        (unless arity
          (let* ((template (car (nelisp-eln-emitter--top-level-code candidate)))
                 (i 0) (ok (= size (length template))))
            (while (and ok (< i size))
              (unless (or (and (>= i 7) (< i 11))
                          (and (>= i 32) (< i 36))
                          (and (>= i 42) (< i 46))
                          (and (>= i 61) (< i 65))
                          (= (aref actual i) (aref template i)))
                (setq ok nil))
              (setq i (1+ i)))
            (when ok (setq arity candidate expected template
                           profile 'self-emitter))))))
    (unless expected
      (nelisp-eln-registration--fail
       'top-level-instructions-not-admitted
       (list size actual
             (car (nelisp-eln-emitter--top-level-code 0))
             (car (nelisp-eln-emitter--top-level-code 1)))))
    (when (eq profile 'self-emitter)
      (dolist (rel relocs)
        (let* ((offset (car rel))
               (disp (+ (aref actual offset)
                        (ash (aref actual (1+ offset)) 8)
                        (ash (aref actual (+ offset 2)) 16)
                        (ash (aref actual (+ offset 3)) 24)))
               (signed (if (>= disp #x80000000)
                           (- disp #x100000000) disp))
               (info (nelisp-eln-system-loader-symbol-info handle (cdr rel))))
          (unless (and info (= (+ base offset 4 signed)
                               (plist-get info :address)))
            (setq relocs-ok nil)))))
    (unless relocs-ok
      (nelisp-eln-registration--fail 'top-level-instructions-not-admitted
                                     (list size actual expected)))
    (list cap arity profile extra)))

(defconst nelisp-eln-registration--cxr-shapes
  '(("caar" . (-3 . 5)) ("cadr" . (5 . 5)))
  "Genuine S6 caar/cadr (FIRST-DISP . D-RELOC-SLOT) pairs, by function
name; see `nelisp-eln-tail-code-analyze-cxr-call'.")

(defun nelisp-eln-registration--admit-leaf-body (handle cap code abi-hash name)
  "Return (:KIND KIND :ANALYSIS ANALYSIS ...) for an admitted leaf body
at CAP with bytes CODE, or nil.  Tries, in order: the plain scalar0/
unary-leaf shapes (:kind \\='simple, no ANALYSIS); a tail-JMP import
\(`nelisp-eln-native-subr--tail-import-analysis', :kind \\='tail\\=); a
non-tail MANY stack-call import
\(`nelisp-eln-native-subr--many-import-analysis', :kind \\='many\\=, the
genuine `zerop' shape\\=); and, only for a NAME
`nelisp-eln-registration--cxr-shapes' recognizes, the S6 caar/cadr shape
\(`nelisp-eln-native-subr--cxr-import-analysis', :kind \\='cxr\\=, plus
:FIRST-DISP/:D-RELOC-SLOT\\=). Data-driven and fail-closed: never signals
itself on an outright miss (returns nil); the caller decides how to
report that."
  (cond
   ((or (nelisp-eln-registration--scalar0-code-p code)
        (nelisp-eln-leaf-code-valid-p code))
    (list :kind 'simple))
   (t
    (let ((tail (nelisp-eln-native-subr--tail-import-analysis
                handle cap code abi-hash)))
      (if tail
          (list :kind 'tail :analysis tail)
        (let ((many (nelisp-eln-native-subr--many-import-analysis
                    handle cap code abi-hash)))
          (if many
              (list :kind 'many :analysis many)
            (let* ((shape (assoc (symbol-name name)
                                 nelisp-eln-registration--cxr-shapes))
                   (first-disp (and shape (car (cdr shape))))
                   (d-reloc-slot (and shape (cdr (cdr shape))))
                   (cxr (and shape
                             (nelisp-eln-native-subr--cxr-import-analysis
                              handle cap code abi-hash
                              first-disp d-reloc-slot))))
              (if cxr
                  (list :kind 'cxr :analysis cxr
                        :first-disp first-disp
                        :d-reloc-slot d-reloc-slot)
                ;; A body importing several distinct slots: the exact
                ;; genuine `fixnump' shape, dispatched through one
                ;; slot-identifying callback port per import.
                (let ((multi
                       (nelisp-eln-native-subr-multi-import-analysis
                        handle cap code abi-hash)))
                  (and multi (list :kind 'multi :analysis multi))))))))))))

(defun nelisp-eln-registration--native-constructor-for (shape &optional min-arity)
  "Return a NATIVE-CONSTRUCTOR for
`nelisp-eln-registration-objects-subr-view', or nil for its default, from
a preflight-admitted SHAPE (see
`nelisp-eln-registration--admit-leaf-body'). Only the :cxr kind needs
one: `nelisp-eln-native-subr-create-cxr' takes its own extra FIRST-DISP/
D-RELOC-SLOT arguments the default `nelisp-eln-native-subr-create' has
no place for."
  (cond
   ((eq (plist-get shape :kind) 'cxr)
    (let ((first-disp (plist-get shape :first-disp))
          (d-reloc-slot (plist-get shape :d-reloc-slot)))
      (lambda (handle c-name name)
        (nelisp-eln-native-subr-create-cxr
         handle c-name first-disp d-reloc-slot name))))
   ((and (eq (plist-get shape :kind) 'multi)
         (= (nelisp-eln-native-subr-multi-arity (plist-get shape :analysis))
            2)
         min-arity)
    (lambda (handle c-name name)
      (nelisp-eln-native-subr-create-multi-binary
       handle c-name name min-arity)))
   ((and (eq (plist-get shape :kind) 'multi)
         (= (nelisp-eln-native-subr-multi-arity (plist-get shape :analysis))
            2))
    (lambda (handle c-name name)
      (nelisp-eln-native-subr-create-multi-binary handle c-name name)))
   ((eq (plist-get shape :kind) 'multi)
    (lambda (handle c-name name)
      (nelisp-eln-native-subr-create-multi handle c-name name)))
   ;; Doc 207: the two-import chain leaf re-verifies its own admission.
   ((eq (plist-get shape :kind) 'chain)
    (lambda (handle c-name name)
      (nelisp-eln-native-subr-create-chain handle c-name name)))))

(defun nelisp-eln-registration--pair-second-imports (shape2)
  "Return the pair profile's second body import proof from SHAPE2, or nil.
A second body without imports needs none.  One with imports must be an
exact multi-import proof (its own lease is then retained as :lease2 and
checked by `nelisp-eln-native-subr--multi-lease-valid-p'); any other
import shape, or one reading f_symbols_with_pos_enabled_reloc, fails
closed."
  (let ((analysis (plist-get shape2 :analysis)))
    (cond
     ((null (nelisp-eln-registration--leaf-shape-slot shape2)) nil)
     ((and (eq (plist-get shape2 :kind) 'multi)
           (eq (plist-get analysis :proof) :multi-import-call)
           (null (plist-get analysis :symbols-with-pos-address)))
      analysis)
     (t (nelisp-eln-registration--fail 'pair-second-import-shape-not-admitted
                                       (plist-get shape2 :kind))))))

(defun nelisp-eln-registration--expected-table-slots (preflight)
  "Return the exact import-table size PREFLIGHT's proofs require: the
register/Feval slots, or one past the highest import of either body."
  (let ((highest (1- nelisp-eln-registration--table-slots)))
    (dolist (analysis (list (plist-get preflight :tail-imports)
                            (plist-get preflight :tail-imports2)))
      (let ((slot (and analysis
                       (nelisp-eln-registration--leaf-shape-slot
                        (list :analysis analysis)))))
        (when slot (setq highest (max highest slot)))))
    (1+ highest)))

(defun nelisp-eln-registration--install-second-imports (preflight table slots)
  "Install PREFLIGHT's pair second-body import ports into TABLE of SLOTS.
Each entry must be a live port, below SLOTS, and collide neither with the
register/Feval slots nor with any slot of the first body's own proof."
  (let* ((second (plist-get preflight :tail-imports2))
         (first (plist-get preflight :tail-imports))
         (taken (append (list nelisp-eln-registration--slot
                              nelisp-eln-registration--eval-slot)
                        (and first
                             (mapcar #'car
                                     (nelisp-eln-native-subr-import-entries
                                      first))))))
    (when second
      (unless (= slots (nelisp-eln-registration--expected-table-slots
                        preflight))
        (nelisp-eln-registration--fail 'invalid-tail-import-table-size slots))
      (dolist (entry (nelisp-eln-native-subr-import-entries second))
        (unless (and (integerp (cdr entry)) (> (cdr entry) 0)
                     (integerp (car entry)) (< -1 (car entry) slots))
          (nelisp-eln-registration--fail 'tail-import-callback-unavailable))
        (when (memq (car entry) taken)
          (nelisp-eln-registration--fail 'tail-import-slot-collision
                                         (car entry)))
        (ptr-write-u64 table (* 8 (car entry)) (cdr entry))))))

(defun nelisp-eln-registration--leaf-shape-slot (shape)
  "Return the highest freloc import slot SHAPE's own analysis uses, or nil
for a :kind other than tail/many/cxr/multi (no import at all)."
  (let ((analysis (plist-get shape :analysis)) (highest nil))
    (dolist (import (and analysis (plist-get analysis :imports)))
      (let ((slot (plist-get import :slot)))
        (when (and (integerp slot) (or (null highest) (> slot highest)))
          (setq highest slot))))
    highest))

(defun nelisp-eln-registration--admit-lambda-bodies (handle metadata extra main)
  "Admit the two native anonymous lambdas a `gnu-lambda-require-subr'
artifact registers, and return (:LAMBDAS INFO :CAPS (CAP0 CAP1 CAP2)
:ANALYSES (A0 A2)).

The first registered lambda (eph[0], suffix 1) must be GNU's 2-byte
`jmp rel8' identical-code-folding thunk to the exported lambda with
suffix 0, whose 102-byte body must be exactly the `lambda-nth-form'
template; the second (eph[2], suffix 2) must be exactly the 90-byte
`lambda-cdr-form' template.  Both are authenticated by
`nelisp-eln-native-subr-multi-import-analysis' (every import slot and
d_reloc constant), are fixed-arity 0, and read no
f_symbols_with_pos_enabled_reloc cell.  MAIN is the registered subr's own
multi-import proof: none of the three bodies may read either d_reloc slot
`Fcomp__register_lambda' overwrites, so the registered lambda subrs are
provably unreachable from admitted code."
  (let* ((eph (plist-get metadata :ephemeral-data-relocations))
         (abi-hash (plist-get metadata :abi-hash))
         (prefix nelisp-eln-registration--lambda-c-name-prefix)
         (c1 (aref eph 0)) (c2 (aref eph 2))
         (c0 (concat prefix "0"))
         (idx1 (plist-get extra :lambda-idx1))
         (idx2 (plist-get extra :lambda-idx2))
         (cap1 (nelisp-eln-system-loader-function-capability handle c1))
         (cap2 (nelisp-eln-system-loader-function-capability handle c2))
         (cap0 (nelisp-eln-system-loader-function-capability handle c0))
         (code1 (and (eql (nth 6 cap1) 2)
                     (nelisp-eln-system-loader-read-root-function-bytes
                      handle c1 0 2)))
         (code0 (and (integerp (nth 6 cap0)) (> (nth 6 cap0) 0)
                     (nelisp-eln-system-loader-read-root-function-bytes
                      handle c0 0 (nth 6 cap0))))
         (code2 (and (integerp (nth 6 cap2)) (> (nth 6 cap2) 0)
                     (nelisp-eln-system-loader-read-root-function-bytes
                      handle c2 0 (nth 6 cap2))))
         (a0 nil) (a2 nil))
    (unless (and (nelisp-eln-registration--lambda-c-name-p c1 "1")
                 (nelisp-eln-registration--lambda-c-name-p c2 "2"))
      (nelisp-eln-registration--fail 'lambda-c-name-not-admitted (list c1 c2)))
    ;; The thunk: `jmp rel8' (EB xx) landing exactly on the lambda-0 body.
    (unless (and (stringp code1) (= (length code1) 2) (= (aref code1 0) #xeb)
                 (= (+ (nth 3 cap1) 2
                       (let ((rel (aref code1 1)))
                         (if (>= rel 128) (- rel 256) rel)))
                    (nth 3 cap0)))
      (nelisp-eln-registration--fail 'lambda-thunk-not-admitted c1))
    (setq a0 (and (= (length code0) (nth 6 cap0))
                  (nelisp-eln-native-subr-multi-import-analysis
                   handle cap0 code0 abi-hash))
          a2 (and (= (length code2) (nth 6 cap2))
                  (nelisp-eln-native-subr-multi-import-analysis
                   handle cap2 code2 abi-hash)))
    (dolist (entry (list (cons a0 'lambda-nth-form) (cons a2 'lambda-cdr-form)))
      (let ((a (car entry)))
        (unless (and a (eq (plist-get a :shape) (cdr entry))
                     (eq (plist-get a :proof) :multi-import-call)
                     (= (nelisp-eln-native-subr-multi-arity a) 0)
                     (eql (plist-get extra :lambda-arity) 0)
                     (null (plist-get a :symbols-with-pos-got)))
          (nelisp-eln-registration--fail 'leaf-instructions-not-admitted
                                         (cdr entry)))))
    ;; The two overwritten d_reloc slots are read by no admitted body.
    (dolist (a (list main a0 a2))
      (dolist (d (plist-get a :data-relocations))
        (when (memq (plist-get d :slot) (list idx1 idx2))
          (nelisp-eln-registration--fail 'lambda-slot-read-by-body
                                         (plist-get d :slot)))))
    (list :lambdas
          (list (list :idx idx1 :c-name c1 :rest (aref eph 1) :cap cap1
                      :arity 0)
                (list :idx idx2 :c-name c2 :rest (aref eph 3) :cap cap2
                      :arity 0))
          :caps (list cap0 cap1 cap2)
          :analyses (list a0 a2))))

(defun nelisp-eln-registration--preflight (handle)
  "Validate all emitted metadata, root extents, and imported call sites."
  (nelisp-eln-registration--trace "REGTRACE metadata-start\n")
  (let* ((metadata (nelisp-eln-system-loader-read handle))
         ;; Validate top_level_run before admitting any profile-specific metadata.
         (top-info (nelisp-eln-registration--top-level-code handle))
         (top-cap (car top-info))
         (arity (cadr top-info))
         (profile (nth 2 top-info))
         (extra (nth 3 top-info))
         (pair (eq profile 'gnu-eval-subr-pair))
         (lambda-p (eq profile 'gnu-lambda-require-subr))
         (eph-size (cond ((or pair lambda-p) 64)
                         ((plist-get extra :eph-offset) 40)
                         (t 32)))
         (eph (plist-get metadata :ephemeral-data-relocations))
         (data-size (plist-get metadata :d-reloc-size))
         (reloc-count
          (nelisp-eln-registration--metadata-data-count
           metadata profile arity extra))
         (eoff (or (plist-get extra :eph-offset) 0))
         (name (and (vectorp eph) (> (length eph) (+ 1 eoff))
                    (aref eph (if lambda-p 5 (+ 1 eoff)))))
         (c-name (and (vectorp eph) (> (length eph) (+ 2 eoff))
                      (aref eph (if lambda-p 6 (+ 2 eoff)))))
         (rest (and (vectorp eph) (> (length eph) (+ 3 eoff))
                    (aref eph (if lambda-p 7 (+ 3 eoff)))))
         (name2 (and pair (vectorp eph) (> (length eph) 5) (aref eph 5)))
         (c-name2 (and pair (vectorp eph) (> (length eph) 6) (aref eph 6)))
         (rest2 (and pair (vectorp eph) (> (length eph) 7) (aref eph 7)))
         (role-sequence
          (cond ((eq profile 'gnu-eval-subr) '(register eval))
                ((eq profile 'gnu-require-subr) '(require register))
                (lambda-p '(lambda lambda require register))
                (pair '(register eval register))))
         (expected-type
          (and (memq profile '(gnu-eval-subr gnu-eval-subr-pair
                               gnu-verified-subr gnu-require-subr
                               gnu-lambda-require-subr))
               (aref (plist-get metadata :data-relocations)
                     (plist-get extra :type-index))))
         (expected-type2
          (and pair
               (aref (plist-get metadata :data-relocations)
                     (plist-get extra :type2-index))))
         (leaf-cap2 nil)
         (data-address
          (nelisp-eln-registration--writable-object
           handle "d_reloc"
           (if (and (memq profile '(gnu-single-leaf gnu-eval-subr
                                     gnu-eval-subr-pair gnu-verified-subr
                                     gnu-require-subr gnu-lambda-require-subr))
                    (integerp data-size) (<= 8 data-size 65536))
               data-size 8)))
         (eph-address (nelisp-eln-registration--writable-object
                       handle "d_reloc_eph" eph-size))
         (link-address (nelisp-eln-registration--writable-object handle "freloc_link_table" 8))
         (unit-cell-address (nelisp-eln-registration--writable-object handle "comp_unit" 8))
         (link-table nil) (link-table-address nil)
         (leaf-cap nil) (leaf-code nil) (tail-analysis nil)
         (leaf-shape nil) (leaf-shape2 nil) (tail-imports2 nil)
         (table-slots (if lambda-p
                          (1+ nelisp-eln-registration--lambda-slot)
                        nelisp-eln-registration--table-slots))
         (lambda-info nil) (lambda-caps nil)
         (swp-address nil)
         (eval-effects nil)
         (saved-data nil))
    (nelisp-eln-registration--trace "REGTRACE metadata-read\n")
    (nelisp-eln-registration--trace "REGTRACE admission-start\n")
    (setq leaf-cap
          (nelisp-eln-system-loader-function-capability handle c-name))
    (when (eq profile 'gnu-single-leaf)
      (let ((size (nth 6 leaf-cap)))
        (unless (and (integerp size) (> size 0))
          (nelisp-eln-registration--fail 'invalid-leaf-code-size c-name))
        (setq leaf-code
              (nelisp-eln-system-loader-read-root-function-bytes
               handle c-name 0 size))
        (if (eql arity 2)
            ;; Doc 207: a two-argument GNU leaf is admitted only as the
            ;; authenticated non-tail chain shape (Fadd1 slow path plus
            ;; `Ffuncall' hop); no unary shape may be registered at arity 2.
            (let ((chain (and (= (length leaf-code) size)
                              (nelisp-eln-native-subr-chain-import-analysis
                               handle leaf-cap leaf-code
                               (plist-get metadata :abi-hash)))))
              (unless chain
                (nelisp-eln-registration--fail
                 'leaf-instructions-not-admitted c-name))
              (setq tail-analysis chain
                    leaf-shape (list :kind 'chain :analysis chain)
                    table-slots
                    (max table-slots
                         (1+ (apply #'max
                                    (mapcar (lambda (import)
                                              (plist-get import :slot))
                                            (plist-get chain :imports)))))))
          (setq tail-analysis
                (and (= (length leaf-code) size)
                     (nelisp-eln-native-subr--tail-import-analysis
                      handle leaf-cap leaf-code (plist-get metadata :abi-hash))))
          (unless (and (= (length leaf-code) size)
                       (or (nelisp-eln-registration--scalar0-code-p leaf-code)
                           (nelisp-eln-leaf-code-valid-p leaf-code)
                           tail-analysis))
            (nelisp-eln-registration--fail
             'leaf-instructions-not-admitted c-name))
          (when tail-analysis
            (setq table-slots
                  (1+ (nth 1 (plist-get tail-analysis :descriptor))))))))
    (when (memq profile '(gnu-verified-subr gnu-require-subr
                          gnu-lambda-require-subr))
      ;; S6.8 (and S6.15's `gnu-require-subr'): the body must be exactly one genuine multi-import template
      ;; (`nelisp-eln-tail-code--multi-import-shapes') whose every import
      ;; slot, d_reloc constant and module counter authenticates (see
      ;; `nelisp-eln-native-subr-multi-import-analysis'), at exactly the
      ;; arity the admitted registration code passes, reading no
      ;; f_symbols_with_pos_enabled_reloc cell.  Nothing else is admitted.
      (let ((size (nth 6 leaf-cap)))
        (unless (and (integerp size) (> size 0))
          (nelisp-eln-registration--fail 'invalid-leaf-code-size c-name))
        (setq leaf-code
              (nelisp-eln-system-loader-read-root-function-bytes
               handle c-name 0 size))
        (let ((multi (and (= (length leaf-code) size)
                          (nelisp-eln-native-subr-multi-import-analysis
                           handle leaf-cap leaf-code
                           (plist-get metadata :abi-hash)))))
          (unless (and multi
                       (eq (plist-get multi :proof) :multi-import-call)
                       (= (nelisp-eln-native-subr-multi-arity multi) arity)
                       (or (not lambda-p)
                           (eq (plist-get multi :shape) 'if-form))
                       ;; A variable arity must match the registration's
                       ;; own MINARGS exactly (a fixed arity has none).
                       (= (nelisp-eln-native-subr-multi-min-arity multi)
                          (or (plist-get extra :min-arity) arity))
                       ;; The `symbols_with_pos_enabled' byte is read iff
                       ;; the exact shape declares it.
                       (eq (and (plist-get multi :symbols-with-pos-got) t)
                           (nelisp-eln-native-subr-multi-swp-declared-p
                            multi)))
            (nelisp-eln-registration--fail
             'leaf-instructions-not-admitted c-name))
          (setq tail-analysis multi
                leaf-shape (list :kind 'multi :analysis multi)
                table-slots
                (max table-slots
                     (1+ (nelisp-eln-registration--leaf-shape-slot
                          leaf-shape))))))
      (when lambda-p
        ;; S6.12: the two native lambdas registered before the require are
        ;; admitted by their own exact templates, and never read.
        (setq lambda-info
              (nelisp-eln-registration--admit-lambda-bodies
               handle metadata extra tail-analysis)
              lambda-caps (plist-get lambda-info :caps)))
      (when (memq profile '(gnu-require-subr gnu-lambda-require-subr))
        ;; S6.15: the Feval call site that precedes the registration must
        ;; statically decode to an admitted `(require \='FEATURE)'.
        (setq eval-effects
              (list (cons :require
                          (nelisp-eln-registration--require-effect
                           metadata (plist-get extra :form-index)
                           (plist-get extra :lexenv-index)))))))
    (when (memq profile '(gnu-eval-subr gnu-eval-subr-pair))
      ;; Same body admission the gnu-single-leaf profile already requires,
      ;; for this call site's own registered subr -- but data-driven over
      ;; every admitted S6 shape (see
      ;; `nelisp-eln-registration--admit-leaf-body'), not just the plain
      ;; scalar0/unary-leaf case: a tail-JMP import, a non-tail MANY
      ;; stack-call import (the genuine `zerop' shape), or, for a
      ;; recognized NAME, the S6 caar/cadr shape.
      (let ((size (nth 6 leaf-cap))
            (abi-hash (plist-get metadata :abi-hash)))
        (unless (and (integerp size) (> size 0))
          (nelisp-eln-registration--fail 'invalid-leaf-code-size c-name))
        (setq leaf-code
              (nelisp-eln-system-loader-read-root-function-bytes
               handle c-name 0 size))
        (setq leaf-shape
              (and (= (length leaf-code) size)
                   (nelisp-eln-registration--admit-leaf-body
                    handle leaf-cap leaf-code abi-hash name)))
        (unless leaf-shape
          (nelisp-eln-registration--fail
           'leaf-instructions-not-admitted c-name))
        (when (memq (plist-get leaf-shape :kind) '(tail many cxr multi))
          (setq tail-analysis (plist-get leaf-shape :analysis))
          (let ((slot (nelisp-eln-registration--leaf-shape-slot leaf-shape)))
            (when slot
              (setq table-slots (max table-slots (1+ slot)))))))
      (when pair
        ;; The compiler-macro's own native body must be admitted too, by
        ;; the identical rule, before this call site's second
        ;; registration is admitted at all.  (No shape this file's own
        ;; analyzers recognize currently covers a genuine compiler-macro
        ;; closure body -- e.g. vendor `zerop--anon-cmacro' builds a cons
        ;; tree via three non-tail `Fcons' calls, a shape none of
        ;; `nelisp-eln-registration--admit-leaf-body''s analyzers
        ;; recognize -- so this is expected to keep failing honestly for
        ;; every `gnu-eval-subr-pair' artifact today; admitting it would
        ;; need a new analyzer this task does not build.)
        (setq leaf-cap2
              (nelisp-eln-system-loader-function-capability handle c-name2))
        (let ((size2 (nth 6 leaf-cap2))
              (abi-hash (plist-get metadata :abi-hash)))
          (unless (and (integerp size2) (> size2 0))
            (nelisp-eln-registration--fail 'invalid-leaf-code-size c-name2))
          (let ((leaf-code2
                 (nelisp-eln-system-loader-read-root-function-bytes
                  handle c-name2 0 size2)))
            (setq leaf-shape2
                  (and (= (length leaf-code2) size2)
                       (nelisp-eln-registration--admit-leaf-body
                        handle leaf-cap2 leaf-code2 abi-hash name2)))
            (unless leaf-shape2
              (nelisp-eln-registration--fail
               'leaf-instructions-not-admitted c-name2))
            ;; S6.16: the second body's own authenticated import proof.
            (setq tail-imports2
                  (nelisp-eln-registration--pair-second-imports leaf-shape2))
            (when tail-imports2
              (setq table-slots
                    (max table-slots
                         (1+ (nelisp-eln-registration--leaf-shape-slot
                              leaf-shape2))))))))
      (setq eval-effects
            (nelisp-eln-registration--eval-effect
             metadata (plist-get extra :form-index)
             (plist-get extra :lexenv-index) name)))
    (nelisp-eln-registration--trace "REGTRACE admission-done\n")
    ;; An admitted body that reads GNU's `symbols_with_pos_enabled' through
    ;; the artifact's f_symbols_with_pos_enabled_reloc cell: that cell must
    ;; be this module's own writable 8-byte root object, exactly the one
    ;; the body's authenticated GOT slot reaches, and still zero.
    (let ((swp (plist-get tail-analysis :symbols-with-pos-address)))
      (when swp
        (unless (and (= swp (nelisp-eln-registration--writable-object
                             handle "f_symbols_with_pos_enabled_reloc" 8))
                     (= (ptr-read-u64 swp 0) 0))
          (nelisp-eln-registration--fail
           'unexpected-symbols-with-pos-cell swp))
        (setq swp-address swp)))
    (unless (and (equal (nelisp-eln-abi-classify-word
                         (nelisp-eln-abi-encode-nil)) 'nil)
                 (= (ptr-read-u64 link-address 0) 0)
                 (= (ptr-read-u64 unit-cell-address 0) 0))
      (nelisp-eln-registration--fail 'unexpected-initial-relocation-cells))
    (setq saved-data
          (nelisp-eln-registration--snapshot-data-relocations
           data-address reloc-count))
    (let ((i 0) (eph-words (/ eph-size 8)))
      (while (< i eph-words)
        (unless (= (ptr-read-u64 eph-address (* i 8)) 0)
          (nelisp-eln-registration--fail 'unexpected-ephemeral-cell i))
        (setq i (1+ i))))
    (setq link-table
          (nl-ffi-memory-allocate (* 8 table-slots))
          link-table-address (nl-ffi-memory-address link-table))
    (let ((helper (plist-get tail-analysis :helper-range)))
      ;; S6.9: the helper's on-file bytes matched its exact template during
      ;; analysis; its live bytes must equal the file's.
      (when helper
        (nelisp-eln-registration--raw-read-bytes
         handle (car helper) (cdr helper))))
    (nelisp-eln-registration--validate-executable-regions
     handle
     (list (cons (nth 3 top-cap) (nth 6 top-cap))
           (and leaf-cap (cons (nth 3 leaf-cap) (nth 6 leaf-cap)))
           (and leaf-cap2 (cons (nth 3 leaf-cap2) (nth 6 leaf-cap2)))
           (plist-get tail-analysis :helper-range)
           (and lambda-caps (cons (nth 3 (nth 0 lambda-caps))
                                  (nth 6 (nth 0 lambda-caps))))
           (and lambda-caps (cons (nth 3 (nth 1 lambda-caps))
                                  (nth 6 (nth 1 lambda-caps))))
           (and lambda-caps (cons (nth 3 (nth 2 lambda-caps))
                                  (nth 6 (nth 2 lambda-caps))))))
    (list :metadata metadata :name name :c-name c-name :arity arity
          :profile profile :data-reloc-count reloc-count
          :leaf-cap leaf-cap
          :tail-imports tail-analysis :tail-imports2 tail-imports2
          :link-table-slots table-slots
          :top-cap top-cap :d-reloc-address data-address
          :eph-address eph-address :link-address link-address
          :symbols-with-pos-address swp-address
          :unit-cell-address unit-cell-address :link-table link-table
          :link-table-address link-table-address
          :saved-data-relocs saved-data
          :eval-effects eval-effects
          :role-sequence role-sequence
          :expected-type expected-type :expected-type2 expected-type2
          :type-index (plist-get extra :type-index)
          :type2-index (plist-get extra :type2-index)
          :leaf-cap2 leaf-cap2 :name2 name2 :c-name2 c-name2 :rest2 rest2
          :arity2 (plist-get extra :arity2)
          :min-arity (plist-get extra :min-arity)
          :eph-offset (plist-get extra :eph-offset)
          :lambda-info lambda-info
          :lambda-type-index (plist-get extra :lambda-type-index)
          :rest1 rest
          :eph-word-count (/ eph-size 8)
          :leaf-shape leaf-shape :leaf-shape2 leaf-shape2
          :saved-relocs (list (nelisp-eln-abi-read-word data-address 0)
                              (nelisp-eln-abi-read-word link-address 0)
                              (nelisp-eln-abi-read-word unit-cell-address 0)
                              (let ((i 0) (n (/ eph-size 8)) (words nil))
                                (while (< i n)
                                  (push (nelisp-eln-abi-read-word
                                         eph-address (* i 8))
                                        words)
                                  (setq i (1+ i)))
                                (nreverse words))))))

(defun nelisp-eln-registration--args (descriptor)
  (let ((i 0) (words nil))
    (while (< i 7)
      (push (nelisp-eln-abi-read-word descriptor (* i 8)) words)
      (setq i (1+ i)))
    (nreverse words)))

(defun nelisp-eln-registration--metadata-data-count (metadata profile arity
                                                                &optional extra)
  "Validate profile-specific METADATA and return its data-relocation count.
EXTRA carries the `gnu-eval-subr-pair' profile's second registration
arity (see `nelisp-eln-registration--top-level-code'); other profiles
ignore it."
  ;; Guarded by `fboundp' for the same reason as `--top-level-code'
  ;; above: a `cl-letf' mock of this exact function must survive. This
  ;; authenticates every profile's `c-name' mangling alike (see `common'
  ;; below), so unlike `--top-level-code' it cannot skip the requirement
  ;; by profile -- but the required feature is the lightweight
  ;; `nelisp-eln-emitter-templates', not the full `nelisp-eln-emitter', so
  ;; even a GNU artifact's own metadata check here stays cheap.
  (unless (fboundp 'nelisp-eln-emitter--symbol-name)
    (require 'nelisp-eln-emitter-templates))
  (let* ((reloc (plist-get metadata :data-relocations))
         (eph (plist-get metadata :ephemeral-data-relocations))
         (docs (plist-get metadata :function-docs))
         (count (and (vectorp reloc) (length reloc)))
         (size (plist-get metadata :d-reloc-size))
         (pair (eq profile 'gnu-eval-subr-pair))
         ;; S6.6: an `&optional' function's ephemeral vector leads with MIN
         ;; and MAX arity ([MIN MAX NAME C-NAME REST]); EOFF shifts the rest.
         (eoff (or (plist-get extra :eph-offset) 0))
         (lambda-p (eq profile 'gnu-lambda-require-subr))
         (common (and (equal (plist-get metadata :abi-hash) "ba35c031")
                      (vectorp reloc)
                      (vectorp eph)
                      (= (length eph) (cond ((or pair lambda-p) 8)
                                            (t (+ 4 eoff))))
                      (if lambda-p
                          ;; S6.12: [C-NAME-1 REST-1 C-NAME-2 REST-2 1
                          ;; NAME C-NAME REST], the two lambdas first.
                          (and (nelisp-eln-registration--lambda-c-name-p
                                (aref eph 0))
                               (equal (aref eph 1) '(0 nil nil))
                               (nelisp-eln-registration--lambda-c-name-p
                                (aref eph 2))
                               (equal (aref eph 3) '(1 nil nil))
                               (eql (aref eph 4) 1)
                               (symbolp (aref eph 5)) (stringp (aref eph 6))
                               (equal (aref eph 6)
                                      (nelisp-eln-emitter--symbol-name
                                       (aref eph 5)))
                               (equal (aref eph 7) '(2 nil nil)))
                        (and (integerp (aref eph 0))
                             (or (= eoff 0)
                                 (and (integerp (aref eph 1))
                                      (eql (aref eph 0)
                                           (plist-get extra :min-arity))
                                      (eql (aref eph 1) arity)))
                             (symbolp (aref eph (+ 1 eoff)))
                             (stringp (aref eph (+ 2 eoff)))
                             (equal (aref eph (+ 3 eoff)) '(0 nil nil))
                             (equal (aref eph (+ 2 eoff))
                                    (nelisp-eln-emitter--symbol-name
                                     (aref eph (+ 1 eoff))))
                             (or (not pair)
                                 (and (integerp (aref eph 4))
                                      (symbolp (aref eph 5))
                                      (stringp (aref eph 6))
                                      (equal (aref eph 7) '(1 nil nil))
                                      (equal (aref eph 6)
                                             (nelisp-eln-emitter--symbol-name
                                              (aref eph 5)))))))
                      (integerp (plist-get metadata :d-reloc-eph-size))
                      (= (plist-get metadata :d-reloc-eph-size)
                         (cond ((or pair lambda-p) 64) ((= eoff 1) 40) (t 32))))))
    (unless (and common
                 (cond
                  ((eq profile 'self-emitter)
                   (and (= count 1) (null (aref reloc 0))
                        (= (aref eph 0) 0)
                        (integerp size) (= size 8)))
                  ((eq profile 'gnu-single-leaf)
                   (and (memq arity '(0 1 2)) (<= 1 count 8192)
                        (integerp size) (= size (* 8 count))
                        ;; The genuine two-argument chain artifact carries
                        ;; 2 in this top-level-unused word; one-argument
                        ;; and nullary GNU leaves carry 1.
                        (integerp (aref eph 0))
                        (= (aref eph 0) (if (eql arity 2) 2 1))
                        (vectorp docs) (<= 0 (length docs) 8192)
                        (<= (+ size (* 8 (length docs))) 65536)))
                  ((memq profile '(gnu-verified-subr gnu-require-subr))
                   ;; Same metadata envelope as `gnu-single-leaf' (the
                   ;; genuine arity-2 artifact carries 2 in this word),
                   ;; plus a type slot inside the data relocations that
                   ;; holds a genuine fixed-arity function type.
                   (let ((type-index (plist-get extra :type-index)))
                     (and (<= 0 arity 8) (<= 1 count 8192)
                          (integerp size) (= size (* 8 count))
                          (integerp (aref eph 0))
                          (if (plist-get extra :min-arity)
                              t
                            (= (aref eph 0) (if (eql arity 2) 2 1)))
                          (integerp type-index) (< 0 type-index count)
                          (if (plist-get extra :min-arity)
                              (nelisp-eln-registration--verified-optional-subr-type-p
                               (aref reloc type-index)
                               (plist-get extra :min-arity) arity)
                            (nelisp-eln-registration--verified-subr-type-p
                             (aref reloc type-index) arity))
                          (vectorp docs) (<= 0 (length docs) 8192)
                          (<= (+ size (* 8 (length docs))) 65536))))
                  ((eq profile 'gnu-lambda-require-subr)
                   ;; The require-subr envelope, plus the two lambdas'
                   ;; shared `(function nil t)' type slot and their two
                   ;; "#$" placeholder slots, all distinct from the
                   ;; subr's own type slot.
                   (let ((type-index (plist-get extra :type-index))
                         (ltype (plist-get extra :lambda-type-index))
                         (idx1 (plist-get extra :lambda-idx1))
                         (idx2 (plist-get extra :lambda-idx2)))
                     (and (<= 0 arity 8) (<= 1 count 8192)
                          (integerp size) (= size (* 8 count))
                          (eql (plist-get extra :lambda-arity) 0)
                          (integerp type-index) (< 0 type-index count)
                          (nelisp-eln-registration--verified-subr-type-p
                           (aref reloc type-index) arity)
                          (integerp ltype) (< 0 ltype count)
                          (equal (aref reloc ltype) '(function nil t))
                          (integerp idx1) (integerp idx2)
                          (< -1 idx1 count) (< -1 idx2 count)
                          (equal (aref reloc idx1) "#$")
                          (equal (aref reloc idx2) "#$")
                          (not (memq idx1 (list type-index ltype
                                                (plist-get extra :lexenv-index)
                                                (plist-get extra :form-index))))
                          (not (memq idx2 (list type-index ltype
                                                (plist-get extra :lexenv-index)
                                                (plist-get extra :form-index))))
                          (vectorp docs) (<= 3 (length docs) 8192)
                          (<= (+ size (* 8 (length docs))) 65536))))
                  ((memq profile '(gnu-eval-subr gnu-eval-subr-pair))
                   (and (<= 0 arity 8) (<= 1 count 8192)
                        (integerp size) (= size (* 8 count))
                        (integerp (aref eph 0)) (= (aref eph 0) 1)
                        (vectorp docs) (<= 0 (length docs) 8192)
                        (<= (+ size (* 8 (length docs))) 65536)
                        (or (not pair)
                            (and (= (aref eph 4) 2)
                                 (integerp (plist-get extra :arity2))
                                 (<= 0 (plist-get extra :arity2) 8)))))))
      (nelisp-eln-registration--fail
       'metadata-outside-emitter-slice (list profile metadata)))
    count))

(defun nelisp-eln-registration--snapshot-data-relocations (address count)
  "Return COUNT initially-zero data words at ADDRESS, in slot order."
  (unless (and (integerp count) (<= 1 count 8192))
    (nelisp-eln-registration--fail 'invalid-data-relocation-count count))
  (let ((i 0) (words nil))
    (while (< i count)
      (let ((word (nelisp-eln-abi-read-word address (* i 8))))
        (unless (= word 0)
          (nelisp-eln-registration--fail 'unexpected-data-relocation-cell i))
        (push word words))
      (setq i (1+ i)))
    (nreverse words)))

(defun nelisp-eln-registration--restore-data-relocations (address words)
  "Restore WORDS to consecutive eight-byte slots at ADDRESS."
  (unless (and (listp words) (<= 1 (length words) 8192))
    (nelisp-eln-registration--fail 'invalid-saved-data-relocations))
  (let ((i 0))
    (while words
      (nelisp-eln-abi-write-word address (* i 8) (car words))
      (setq words (cdr words) i (1+ i)))))

(defun nelisp-eln-registration--install-import-table
    (preflight registration-callback-address)
  "Initialize PREFLIGHT's retained table and install admitted callbacks."
  (let* ((table (plist-get preflight :link-table-address))
         (link-address (plist-get preflight :link-address))
         (slots (plist-get preflight :link-table-slots))
         (tail-imports (plist-get preflight :tail-imports))
         (lambda-p (eq (plist-get preflight :profile)
                       'gnu-lambda-require-subr))
         (base-slots (if lambda-p
                         (1+ nelisp-eln-registration--lambda-slot)
                       nelisp-eln-registration--table-slots))
         (i 0))
    (unless (and (integerp table) (> table 0)
                 (integerp link-address) (> link-address 0)
                 (integerp slots) (>= slots base-slots)
                 (integerp registration-callback-address)
                 (> registration-callback-address 0))
      (nelisp-eln-registration--fail 'invalid-import-table))
    (while (< i slots)
      (ptr-write-u64 table (* 8 i) 0)
      (setq i (1+ i)))
    (ptr-write-u64 table (* 8 nelisp-eln-registration--slot)
                   registration-callback-address)
    (when lambda-p
      ;; S6.12: `Fcomp__register_lambda' shares the same generic
      ;; seven-word trampoline; `--callback' dispatches by admitted role.
      (ptr-write-u64 table (* 8 nelisp-eln-registration--lambda-slot)
                     registration-callback-address))
    (when (plist-get preflight :role-sequence)
      ;; The S6.16-24 `gnu-eval-subr'(-pair) profiles' Feval call site
      ;; shares this same generic seven-word trampoline; `--callback'
      ;; itself, not this table, dispatches by admitted call sequence.
      (ptr-write-u64 table (* 8 nelisp-eln-registration--eval-slot)
                     registration-callback-address))
    (when tail-imports
      (let* ((analysis tail-imports)
             (slot (nelisp-eln-registration--leaf-shape-slot
                    (list :analysis analysis))))
       ;; The table is exactly as large as preflight sized it: the
       ;; register/Feval slots or the leaf's highest import, whichever
       ;; is higher (a cxr leaf imports the low slot 0).
       (unless (= slots (if (plist-get preflight :tail-imports2)
                            (nelisp-eln-registration--expected-table-slots
                             preflight)
                          (max base-slots (1+ slot))))
        (nelisp-eln-registration--fail 'invalid-tail-import-table-size slots))
       ;; One entry per authenticated import slot; a multi-import body
       ;; gets a distinct slot-identifying port in each.
       (dolist (entry (nelisp-eln-native-subr-import-entries analysis))
        (unless (and (integerp (cdr entry)) (> (cdr entry) 0))
          (nelisp-eln-registration--fail 'tail-import-callback-unavailable))
        (when (memq (car entry) (list nelisp-eln-registration--slot
                                      nelisp-eln-registration--eval-slot
                                      nelisp-eln-registration--lambda-slot))
          (nelisp-eln-registration--fail 'tail-import-slot-collision
                                         (car entry)))
        (ptr-write-u64 table (* 8 (car entry)) (cdr entry)))))
    (nelisp-eln-registration--install-second-imports preflight table slots)
    (nelisp-eln-abi-write-word link-address 0 table)
    t))

(defun nelisp-eln-registration--type-index (owner &optional second)
  "Return OWNER's authenticated `subr-type' data-relocation index.
The `gnu-single-leaf' profile keeps its type at index 0; the S6
`gnu-eval-subr'(-pair) and `gnu-verified-subr' profiles record the index
their admitted
top_level_run call site actually loads (SECOND selects the pair
profile's second registration)."
  (let ((plist (nelisp-eln-registration--role-plist owner)))
    (or (plist-get plist (if second :type2-index :type-index)) 0)))

(defun nelisp-eln-registration--callback-type (owner unit word &optional second)
  "Decode callback TYPE WORD through OWNER's metadata capability if present.
SECOND selects the pair profile's second registration's type slot."
  (let ((token (aref owner 16)))
    (if token
        (let* ((metadata (aref owner 7))
               (index (nelisp-eln-registration--type-index owner second))
               (source (aref (plist-get metadata :data-relocations) index))
               (expected (nelisp-eln-registration-metadata-type-word
                          token index))
               (type (nelisp-eln-registration-metadata-decode token word)))
          (unless (and (eq type source) (= word expected))
            (nelisp-eln-registration--fail 'callback-type-provenance-mismatch
                                            word))
          type)
      (nelisp-eln-registration-objects-decode-word unit word))))

(defun nelisp-eln-registration--write-metadata-relocations
    (address token count)
  "Write exactly COUNT authenticated data-relocation words at ADDRESS."
  (unless (and token (integerp count) (<= 1 count 8192))
    (nelisp-eln-registration--fail 'invalid-metadata-relocation-count count))
  (let ((i 0))
    (while (< i count)
      (let ((word (nelisp-eln-registration-metadata-slot-word token 'data i)))
        (unless (integerp word)
          (nelisp-eln-registration--fail 'invalid-metadata-relocation-word i))
        (nelisp-eln-abi-write-word address (* i 8) word))
      (setq i (1+ i)))))

(defun nelisp-eln-registration--role-plist (owner)
  "Return OWNER's S6.16-24 role-sequence/eval-effect plist, or nil."
  (and owner (> (length owner) 18) (aref owner 18)))

(defun nelisp-eln-registration--call-role (owner)
  "Return the admitted role for the call site currently invoking a callback.

`nelisp-eln-registration--call-index' (already incremented by the caller)
selects a position in OWNER's own `:role-sequence' (built once, during
preflight, purely from the artifact's admitted top-level byte shape --
never from anything a running call claims about itself).  A legacy
profile (no `:role-sequence') is always exactly one `register' call.  Any
position past the end of a real sequence, or any call at all past the
first for a legacy profile, returns nil: an extra, unadmitted call site."
  (let ((plan (plist-get (nelisp-eln-registration--role-plist owner)
                         :role-sequence)))
    (if plan
        (nth (1- nelisp-eln-registration--call-index) plan)
      (and (= nelisp-eln-registration--call-index 1) 'register))))

(defun nelisp-eln-registration--register-ordinal (owner)
  "Return which admitted `register' call this is: 1, or 2 for the
`gnu-eval-subr-pair' profile's second (compiler-macro) registration."
  (let* ((plan (plist-get (nelisp-eln-registration--role-plist owner)
                          :role-sequence))
         (upto (if plan
                   (let ((i 0) (n 0) (rest plan))
                     (while (and rest (< i nelisp-eln-registration--call-index))
                       (when (eq (car rest) 'register) (setq n (1+ n)))
                       (setq rest (cdr rest) i (1+ i)))
                     n)
                 1)))
    upto))

(defun nelisp-eln-registration--register-callback (descriptor ordinal)
  "Implement the authenticated comp--register-subr imported callback.
ORDINAL is 1 for every profile's first (and, for `gnu-single-leaf' /
`gnu-eval-subr', only) registration, or 2 for `gnu-eval-subr-pair''s
second, compiler-macro registration -- selecting which half of OWNER's
admitted expectations (plain fields for 1; the S6.16-24 role plist's
`:name2' / `:c-name2' / `:rest2' / `:arity2' / `:expected-type2' /
`:leaf-cap2' for 2) this call site's decoded words must match."
  (let* ((owner nelisp-eln-registration--active-owner)
         (unit (and owner (aref owner 1)))
         (activation (and owner (aref owner 2)))
         (plist (nelisp-eln-registration--role-plist owner))
         (words (nelisp-eln-registration--args descriptor))
         (name (and unit (nelisp-eln-registration-objects-decode-word
                          unit (nth 0 words))))
         (c-name (and unit (nelisp-eln-registration-objects-decode-word
                            unit (nth 1 words))))
         (min-args (nelisp-eln-abi-decode-immediate (nth 2 words)))
         (max-args (nelisp-eln-abi-decode-immediate (nth 3 words)))
         (type (and unit (nelisp-eln-registration--callback-type
                          owner unit (nth 4 words) (= ordinal 2))))
         (rest (and unit (nelisp-eln-registration-objects-decode-word
                          unit (nth 5 words))))
         (unit-word (and unit (nelisp-eln-registration-objects-unit-word unit)))
         (metadata (and owner (aref owner 7)))
         (second (= ordinal 2))
         (expected-name (if second (plist-get plist :name2) (aref owner 3)))
         (expected-c-name (if second (plist-get plist :c-name2) (aref owner 4)))
         (expected-name-word
          (if second (plist-get plist :name-word2) (aref owner 14)))
         (expected-arity
          (if second (plist-get plist :arity2) (aref owner 15)))
         (expected-rest (if second (plist-get plist :rest2)
                          (or (plist-get plist :rest1) '(0 nil nil))))
         (expected-leaf-cap (if second (plist-get plist :leaf-cap2)
                              (aref owner 8)))
         (expected-type-value
          (if second (plist-get plist :expected-type2)
            (plist-get plist :expected-type)))
         (already-registered
          (if second nelisp-eln-registration--registered-callable-2
            nelisp-eln-registration--registered-callable))
         (prior-registered
          (or (not second) nelisp-eln-registration--registered-callable))
         (view nil) (callable nil) (word nil))
    (nelisp-eln-registration--trace "REGTRACE callback-arguments-decoded\n")
    (nelisp-eln-registration--trace
     (format "REGTRACE callback-values ordinal=%S name=%S expected-name=%S c-name=%S expected-c-name=%S min=%S max=%S type=%S rest=%S unit=%S expected-unit=%S\n"
             ordinal name expected-name c-name expected-c-name
             min-args max-args type rest unit-word
             (and unit (nelisp-eln-registration-objects-unit-word unit))))
    (unless (and owner unit activation prior-registered (null already-registered)
             (eq name expected-name)
             (equal c-name expected-c-name)
             (equal (symbol-name name) (symbol-name expected-name))
             (= (nth 6 (nelisp-eln-system-loader-function-capability
                        (aref unit 1) c-name))
                (nth 6 expected-leaf-cap))
             (= min-args (or (and (not second) (plist-get plist :min-arity))
                             expected-arity))
             (= max-args expected-arity)
             (cond
              (expected-type-value (equal type expected-type-value))
              ((aref owner 16)
               (eq type (aref (plist-get metadata :data-relocations)
                              (nelisp-eln-registration--type-index
                               owner second))))
              (t (null type)))
             (equal rest expected-rest)
             (= (nth 6 words) unit-word)
             (= (nth 0 words) expected-name-word)
             (= (nth 1 words) (nelisp-eln-registration-objects-encode-word
                               unit c-name))
             (= (nth 5 words) (nelisp-eln-registration-objects-encode-word
                               unit rest)))
      (nelisp-eln-registration--fail
       'callback-arguments-not-admitted
       (list ordinal words name expected-name c-name expected-c-name
             min-args max-args type rest unit-word expected-name-word)))
    (nelisp-eln-registration--trace "REGTRACE callback-arguments-admitted\n")
    (let ((nelisp-eln-native-subr--tail-import-context
           (and owner (if second (plist-get plist :lease2) (aref owner 17)))))
      (setq view
        (nelisp-eln-registration-objects-subr-view
         activation (symbol-name name) c-name (nth 1 rest) (nth 2 rest)
         (car rest) type expected-arity (aref owner 16)
         (nelisp-eln-registration--native-constructor-for
          (plist-get plist (if second :leaf-shape2 :leaf-shape))
          (and (not second) (plist-get plist :min-arity)))
         ;; The pair's second registration authenticates its own type
         ;; slot through the same metadata capability (S6.16).
         (nelisp-eln-registration--type-index owner second)
         (and (not second) (plist-get plist :min-arity)))))
    (setq callable (plist-get view :callable) word (plist-get view :word))
    (nelisp-eln-registration--trace "REGTRACE callback-view-created\n")
    (if second
        (progn
          (aset owner 19 (cons word callable))
          (nelisp-eln-registration--publish name callable)
          (setq nelisp-eln-registration--registered-word-2 word
                nelisp-eln-registration--registered-callable-2 callable))
      (aset owner 9 callable)
      (nelisp-eln-registration--publish name callable)
      (setq nelisp-eln-registration--registered-word word
            nelisp-eln-registration--registered-callable callable))
    (nelisp-eln-registration--trace "REGTRACE callback-published\n")
    (cons (logand word #xffffffff) (logand (ash word -32) #xffffffff))))

(defun nelisp-eln-registration--lambda-ordinal (owner)
  "Return which admitted `lambda' call this is (1 or 2) for OWNER's role plan."
  (let* ((plan (plist-get (nelisp-eln-registration--role-plist owner)
                          :role-sequence))
         (i 0) (n 0) (rest plan))
    (while (and rest (< i nelisp-eln-registration--call-index))
      (when (eq (car rest) 'lambda) (setq n (1+ n)))
      (setq rest (cdr rest) i (1+ i)))
    n))

(defun nelisp-eln-registration--register-lambda-callback (descriptor ordinal)
  "Implement the authenticated `Fcomp__register_lambda' imported callback.
ORDINAL is 1 or 2, selecting the owner's own admitted lambda expectation
\(d_reloc index, C name, `rest' descriptor, extent and arity, all decoded
statically during preflight).  The seven decoded call words must match it
exactly, the shared `(function nil t)' type must come through the
metadata capability, and the lambdas must be registered in order, once
each.  It builds a NeLisp-managed subr view -- a callable that signals
`registered-lambda-not-callable' when called, since no admitted body ever
reads the slot -- and stores its word into d_reloc[RELOC-IDX], exactly what
GNU's `comp--register-lambda' does; the subr is published under no name."
  (let* ((owner nelisp-eln-registration--active-owner)
         (unit (and owner (aref owner 1)))
         (activation (and owner (aref owner 2)))
         (plist (nelisp-eln-registration--role-plist owner))
         (expected (nth (1- ordinal) (plist-get plist :lambdas)))
         (words (nelisp-eln-registration--args descriptor))
         (idx (nelisp-eln-abi-decode-immediate (nth 0 words)))
         (c-name (and unit (nelisp-eln-registration-objects-decode-word
                            unit (nth 1 words))))
         (min-args (nelisp-eln-abi-decode-immediate (nth 2 words)))
         (max-args (nelisp-eln-abi-decode-immediate (nth 3 words)))
         (token (and owner (aref owner 16)))
         (type-index (plist-get plist :lambda-type-index))
         (metadata (and owner (aref owner 7)))
         (type (and token unit
                    (nelisp-eln-registration-metadata-decode
                     token (nth 4 words))))
         (rest (and unit (nelisp-eln-registration-objects-decode-word
                          unit (nth 5 words))))
         (unit-word (and unit (nelisp-eln-registration-objects-unit-word unit)))
         (previous (nth 0 (plist-get plist :lambda-callables)))
         (view nil))
    (unless (and owner unit activation token expected (memq ordinal '(1 2))
                 ;; In order, once each.
                 (= (length (plist-get plist :lambda-callables))
                    (1- ordinal))
                 (or (= ordinal 1) previous)
                 (eql idx (plist-get expected :idx))
                 (equal c-name (plist-get expected :c-name))
                 (= (nth 6 (nelisp-eln-system-loader-function-capability
                            (aref unit 1) c-name))
                    (nth 6 (plist-get expected :cap)))
                 (eql min-args (plist-get expected :arity))
                 (eql max-args (plist-get expected :arity))
                 (integerp type-index)
                 (= (nth 4 words)
                    (nelisp-eln-registration-metadata-type-word
                     token type-index))
                 (eq type (aref (plist-get metadata :data-relocations)
                                type-index))
                 (equal type '(function nil t))
                 (equal rest (plist-get expected :rest))
                 (= (nth 6 words) unit-word)
                 (= (nth 1 words) (nelisp-eln-registration-objects-encode-word
                                   unit c-name))
                 (= (nth 5 words) (nelisp-eln-registration-objects-encode-word
                                   unit rest)))
      (nelisp-eln-registration--fail
       'callback-arguments-not-admitted
       (list 'lambda ordinal words idx c-name min-args max-args type rest
             unit-word)))
    (let ((nelisp-eln-native-subr--tail-import-context (aref owner 17)))
      (setq view
            (nelisp-eln-registration-objects-subr-view
             activation c-name c-name (nth 1 rest) (nth 2 rest) (car rest)
             type (plist-get expected :arity) token
             (lambda (handle name display-name)
               (nelisp-eln-native-subr-create-lambda-placeholder
                handle name display-name))
             type-index)))
    (let ((word (plist-get view :word)))
      (nelisp-eln-abi-write-word (plist-get plist :d-reloc-address)
                                 (* 8 idx) word)
      (aset owner 18
            (plist-put (aref owner 18) :lambda-callables
                       (append (plist-get (aref owner 18) :lambda-callables)
                               (list (plist-get view :callable)))))
      (cons (logand word #xffffffff) (logand (ash word -32) #xffffffff)))))

(defun nelisp-eln-registration--apply-eval-callback (_descriptor)
  "Implement the authenticated Feval (slot 947) imported callback.

Never decodes or evaluates _DESCRIPTOR; the native Feval call's own
arguments are ignored entirely.  Every (PROP . VALUE) pair applied here
was already authenticated statically, against the artifact's own data
relocations, by `nelisp-eln-registration--eval-effect' during preflight
-- before this call site's top-level shape was ever admitted.  This
performs exactly that decoded `function-put' effect and nothing else;
if OWNER or its effects are unavailable this signals rather than
silently doing nothing."
  (let* ((owner nelisp-eln-registration--active-owner)
         (name (and owner (aref owner 3)))
         (effects (plist-get (nelisp-eln-registration--role-plist owner)
                             :eval-effects))
         (word nelisp-eln-registration--registered-word))
    (unless (and owner name effects word
             nelisp-eln-registration--registered-callable)
      (nelisp-eln-registration--fail 'eval-callback-effects-unavailable))
    (dolist (pair effects)
      (nelisp-eln-registration--target-put name (car pair) (cdr pair)))
    (cons (logand word #xffffffff) (logand (ash word -32) #xffffffff))))

(defun nelisp-eln-registration--apply-require-callback (_descriptor)
  "Implement the `gnu-require-subr' profile's Feval (slot 947) callback.

Like `nelisp-eln-registration--apply-eval-callback', never decodes or
evaluates _DESCRIPTOR.  The FEATURE was authenticated statically by
`nelisp-eln-registration--require-effect' during preflight; this runs
NeLisp's own `require' of it, before the registration call site (the
admitted role sequence is (require register)), and answers Qnil, whose
value GNU's top_level_run never reads."
  (let* ((owner nelisp-eln-registration--active-owner)
         (feature (cdr (assq :require
                             (plist-get (nelisp-eln-registration--role-plist
                                         owner)
                                        :eval-effects)))))
    (unless (and owner feature
                 (memq feature nelisp-eln-registration--admitted-require-features)
                 (null nelisp-eln-registration--registered-callable))
      (nelisp-eln-registration--fail 'require-callback-effect-unavailable))
    (require feature)
    (cons 0 0)))

(defun nelisp-eln-registration--callback (descriptor)
  "Dispatch one admitted top-level import call to its authenticated role.

Every call into any import slot this module ever installs lands here.
`nelisp-eln-registration--call-role' decides, from the active owner's own
preflight-admitted `:role-sequence' and how many calls have already
fired, whether this is a `Fcomp__register_subr' call (role `register',
`nelisp-eln-registration--register-callback') or the S6.16-24 `Feval'
call (role `eval', `nelisp-eln-registration--apply-eval-callback'); any
other role -- including a legacy profile's second call, or a call past
the end of an admitted sequence -- signals `callback-sequence-not-admitted'.
DESCRIPTOR is opaque here; each role handler decides for itself whether
and how to decode it."
  (setq nelisp-eln-registration--last-callback-error nil)
  (setq nelisp-eln-registration--call-index
        (1+ nelisp-eln-registration--call-index))
  (nelisp-eln-registration--trace
   (format "REGTRACE callback-entry index=%S\n"
           nelisp-eln-registration--call-index))
  (condition-case failure
      (let ((role (nelisp-eln-registration--call-role
                   nelisp-eln-registration--active-owner)))
        (cond
         ((eq role 'register)
          (nelisp-eln-registration--register-callback
           descriptor
           (nelisp-eln-registration--register-ordinal
            nelisp-eln-registration--active-owner)))
         ((eq role 'lambda)
          (nelisp-eln-registration--register-lambda-callback
           descriptor
           (nelisp-eln-registration--lambda-ordinal
            nelisp-eln-registration--active-owner)))
         ((eq role 'eval)
          (nelisp-eln-registration--apply-eval-callback descriptor))
         ((eq role 'require)
          (nelisp-eln-registration--apply-require-callback descriptor))
         (t (nelisp-eln-registration--fail
             'callback-sequence-not-admitted
             nelisp-eln-registration--call-index))))
    (error
     (setq nelisp-eln-registration--last-callback-error failure)
     (nelisp-eln-registration--trace (format "REGTRACE callback-error %S\n" failure))
     (signal (car failure) (cdr failure)))))

(defun nelisp-eln-registration--call6 (address a b c d e f)
  (ptr-call address a b c d e f))

(defun nelisp-eln-registration--cleanup
    (handle preflight unit activation vector-unit vectors-activation owner
            pin-env pin-marker callback-token name)
  "Restore cells and retire one registration attempt, preserving its error."
  ;; Guarded by `fboundp' (not just an unconditional `require'): a test
  ;; that mocks these exact functions via `cl-letf' must not have that
  ;; mock clobbered by a real `load' of `nelisp-native-load' here.
  (when (and callback-token owner)
    (unless (fboundp 'nelisp-native-load--symbol-addr)
      (require 'nelisp-native-load))
    (nelisp-eln-registration--call6
     (nelisp-native-load--symbol-addr "nelisp_eln_callback_context_pop")
     callback-token 0 0 0 0 0))
  (when (and pin-env pin-marker)
    (unless (fboundp 'nelisp-native-load--pin-end)
      (require 'nelisp-native-load))
    (nelisp-native-load--pin-end pin-env pin-marker))
  (nelisp-eln-registration--unpublish
   name nelisp-eln-registration--registered-callable)
  (let ((name2 (and preflight (plist-get preflight :name2))))
    (nelisp-eln-registration--unpublish
     name2 nelisp-eln-registration--registered-callable-2))
  (setq nelisp-eln-registration--active-owner nil
        nelisp-eln-registration--registered-word nil
        nelisp-eln-registration--registered-callable nil
        nelisp-eln-registration--registered-word-2 nil
        nelisp-eln-registration--registered-callable-2 nil
        nelisp-eln-registration--call-index 0)
  (when (and preflight handle)
    (let ((saved (plist-get preflight :saved-relocs))
          (eph-address (plist-get preflight :eph-address))
          (data (or (plist-get preflight :saved-data-relocs)
                    (list (nth 0 (plist-get preflight :saved-relocs))))))
      (nelisp-eln-registration--restore-data-relocations
       (plist-get preflight :d-reloc-address) data)
      (nelisp-eln-abi-write-word (plist-get preflight :link-address)
                                 0 (nth 1 saved))
      (nelisp-eln-abi-write-word (plist-get preflight :unit-cell-address)
                                 0 (nth 2 saved))
      (let ((i 0) (values (nth 3 saved)))
        (while (< i (length values))
          (nelisp-eln-abi-write-word eph-address (* i 8) (nth i values))
          (setq i (1+ i))))))
  (let ((swp (and preflight (plist-get preflight :symbols-with-pos-address)))
        (memory (and owner (plist-get (aref owner 18)
                                      :symbols-with-pos-memory))))
    (when swp (ptr-write-u64 swp 0 0))
    (when memory
      (aset owner 18 (plist-put (aref owner 18)
                                :symbols-with-pos-memory nil))
      (nl-ffi-memory-release memory)))
  (when owner (aset owner 19 nil))
  (when (and unit activation)
    (nelisp-eln-registration-objects-retire-activation activation))
  (when (and vector-unit vectors-activation)
    (nelisp-eln-registration-vectors-end-activation vectors-activation))
  (when vector-unit
    (nelisp-eln-registration-vectors-release-unit vector-unit))
  (when owner (aset owner 9 nil))
  (when unit
    (unless (eq (aref unit 5) 'closed)
      (nelisp-eln-registration-objects-release-unit unit)))
  (when (and handle (not unit))
    (nelisp-eln-system-loader-close handle))
  (when (and owner (aref owner 16))
    (nelisp-eln-registration-metadata-release (aref owner 16))
    (aset owner 16 nil))
  (when preflight
    (let ((memory (plist-get preflight :link-table)))
      (when memory
        (nl-ffi-memory-release memory))))
  (when owner
    (setq nelisp-eln-registration--owners
          (delq owner nelisp-eln-registration--owners)))
  t)

(defun nelisp-eln-registration--finish-success
    (preflight unit activation vector-unit vectors-activation owner
               pin-env pin-marker callback-token)
  "Retire temporary registration views while retaining the loaded module."
  (when callback-token
    (unless (fboundp 'nelisp-native-load--symbol-addr)
      (require 'nelisp-native-load))
    (nelisp-eln-registration--call6
     (nelisp-native-load--symbol-addr "nelisp_eln_callback_context_pop")
     callback-token 0 0 0 0 0))
  (when (and pin-env pin-marker)
    (unless (fboundp 'nelisp-native-load--pin-end)
      (require 'nelisp-native-load))
    (nelisp-native-load--pin-end pin-env pin-marker))
  ;; This admitted scalar profile never reads these symbol-valued cells again.
  ;; Clear them before retiring their activation-backed views.
  (let ((eph-address (plist-get preflight :eph-address))
        (eph-words (or (plist-get preflight :eph-word-count) 4)))
    (let ((i 0))
      (while (< i eph-words)
        (nelisp-eln-abi-write-word eph-address (* i 8)
                                   (nelisp-eln-abi-encode-nil))
        (setq i (1+ i)))))
  ;; S6.12: `Fcomp__register_lambda' left transient subr-view words in its
  ;; two d_reloc slots; put the artifact's own placeholder words back before
  ;; those views retire, so no slot ever dangles.
  (let* ((plist (aref owner 18))
         (token (aref owner 16))
         (address (plist-get plist :d-reloc-address)))
    (dolist (lambda-entry (plist-get plist :lambdas))
      (let ((idx (plist-get lambda-entry :idx)))
        (nelisp-eln-abi-write-word
         address (* 8 idx)
         (nelisp-eln-registration-metadata-slot-word token 'data idx)))))
  (nelisp-eln-registration-objects-retire-activation activation)
  (nelisp-eln-registration-vectors-end-activation vectors-activation)
  (setq nelisp-eln-registration--active-owner nil
        nelisp-eln-registration--registered-word nil
        nelisp-eln-registration--registered-callable nil
        nelisp-eln-registration--registered-word-2 nil
        nelisp-eln-registration--registered-callable-2 nil
        nelisp-eln-registration--call-index 0)
  ;; Keep OWNER, UNIT, VECTOR-UNIT, loader handle, link table, and relocation
  ;; memory rooted in --owners for this process lifetime.
  owner)

(defun nelisp-eln-registration--containment-boundary
    (success finalizing handle preflight unit activation vector-unit
              vectors-activation owner pin-env pin-marker callback-token name)
  "Shared crash-containment boundary for one `nelisp-eln-registration-load' call.

Runs unconditionally from that function's `unwind-protect' cleanup clause,
so it observes every non-local exit from the protected registration body --
an ordinary `error', a `throw' to an enclosing `catch', or a `quit' signal
-- exactly as reliably as it observes a normal return. SUCCESS and
FINALIZING carry the same meaning the caller already computed; the
remaining arguments are the same partially built registration state
`nelisp-eln-registration--cleanup' and `nelisp-eln-registration--finish-success'
already accept.

When SUCCESS is non-nil this is a no-op: `nelisp-eln-registration--finish-success'
already ran to completion inside the protected body. Otherwise:

- if FINALIZING is non-nil, the native top-level call had already returned
  the registered callable before some later bookkeeping step signalled;
  per the Doc 206 P5 contract a successfully loaded owner stays rooted for
  this process's lifetime and is never torn down after the fact, so this
  records a `success-finalization' pending owner and returns without
  attempting `nelisp-eln-registration--cleanup';
- otherwise it attempts `nelisp-eln-registration--cleanup' to fully roll
  back the partial attempt. A clean rollback leaves no trace. If cleanup
  itself signals, this records a `failed-cleanup' pending owner instead.

Either kind of pending owner blocks all subsequent calls to
`nelisp-eln-registration-load' (checked at its top); there is no cleanup
retry API, so recovery requires a new process.

`nelisp-eln-registration--boundary-enabled' lets tests disable this
function entirely -- see the negative control in
test/nelisp-eln-crash-general-smoke.sh. Production code must never rebind
it: doing so silently drops both the rollback and the quarantine, leaking
whatever native resources the failed attempt had already acquired."
  (when (and (not success) nelisp-eln-registration--boundary-enabled)
    (if finalizing
        (setq nelisp-eln-registration--pending-cleanups
              (cons (list :phase 'success-finalization
                          :handle handle :preflight preflight :unit unit
                          :activation activation :vector-unit vector-unit
                          :vectors-activation vectors-activation :owner owner
                          :pin-env pin-env :pin-marker pin-marker
                          :callback-token callback-token :name name)
                    nelisp-eln-registration--pending-cleanups))
      (condition-case cleanup-error
          (nelisp-eln-registration--cleanup
           handle preflight unit activation vector-unit vectors-activation owner
           pin-env pin-marker callback-token name)
        (error
         (setq nelisp-eln-registration--pending-cleanups
               (cons (list :phase 'failed-cleanup :error cleanup-error
                           :handle handle :preflight preflight :unit unit
                           :activation activation :vector-unit vector-unit
                           :vectors-activation vectors-activation :owner owner
                           :pin-env pin-env :pin-marker pin-marker
                           :callback-token callback-token :name name)
                     nelisp-eln-registration--pending-cleanups)))))))

(defun nelisp-eln-registration-crash-boundary-report ()
  "Return a plist summarizing this process's native-eln registration state.

Merges this module's owner/pending-cleanup bookkeeping with
`nelisp-eln-registration-objects-crash-boundary-snapshot' (the lower-level
unit/memory-codec pending-cleanup list in nelisp-eln-registration-objects.el)
into one whole-process consistency snapshot:

  :live-units        count of live native-comp-unit views
  :owners            count of rooted registration owners
  :pending-cleanups  count of quarantined attempts, across both layers
  :inconsistencies   list of plists describing any structural problem found

This function only reads existing state; it never mutates it, and it is
safe to call at any time, including from a test driver after every
injected failure. It is the general-purpose S7.7 reporter that lets a
smoke test assert \"no inconsistency\" for injection points other than the
S7.6 pin-end fixture."
  (require 'nelisp-eln-registration-objects)
  (let* ((objects-snapshot
          (nelisp-eln-registration-objects-crash-boundary-snapshot))
         (problems (copy-sequence (plist-get objects-snapshot :inconsistencies)))
         (owners nelisp-eln-registration--owners)
         (pending nelisp-eln-registration--pending-cleanups)
         (seen nil))
    (dolist (owner owners)
      (unless (and (vectorp owner)
                   (eq (aref owner 0) nelisp-eln-registration--owner-marker))
        (push (list :kind 'malformed-owner :owner owner) problems))
      (if (memq owner seen)
          (push (list :kind 'duplicate-owner :owner owner) problems)
        (push owner seen)))
    (dolist (entry pending)
      (let ((phase (plist-get entry :phase))
            (owner (plist-get entry :owner)))
        (cond
         ((and (eq phase 'success-finalization) owner (not (memq owner owners)))
          (push (list :kind 'orphaned-success-finalization-owner :owner owner)
                problems))
         ((and (eq phase 'failed-cleanup) owner (not (memq owner owners)))
          (push (list :kind 'lost-failed-cleanup-owner :owner owner)
                problems)))))
    (list :live-units (plist-get objects-snapshot :live-units)
          :owners (length owners)
          :pending-cleanups (+ (length pending)
                                (plist-get objects-snapshot :pending-cleanups))
          :inconsistencies (nreverse problems))))

(defun nelisp-eln-registration-load (path)
  "Load PATH's admitted scalar .eln module and retain its registered callable.
When `nelisp-eln-registration-isolated-namespace' is non-nil, publish the
registration(s) into that namespace instead of the global function cells."
  (nelisp-eln-registration--trace "REGTRACE driver-entry\n")
  (let ((nelisp-eln-registration--load-namespace
         (nelisp-eln-registration--check-namespace
          nelisp-eln-registration-isolated-namespace)))
    (nelisp-eln-registration--load-1 path)))

(defun nelisp-eln-registration--load-1 (path)
  "Body of `nelisp-eln-registration-load'."
  (let* ((handle nil) (preflight nil) (unit nil) (activation nil)
         (owner nil) (vector-unit nil) (vectors-activation nil)
         (pin-env nil) (pin-marker nil) (function-slot nil)
         (args-slot nil) (out-slot nil) (callback-token nil)
         (callback-addr nil) (callable nil) (decoded nil) (returned 0)
         (metadata-token nil)
         (name nil) (success nil) (finalizing nil) (result nil))
    (when (or nelisp-eln-registration--active-owner
              nelisp-eln-registration--pending-cleanups)
      (nelisp-eln-registration--fail 'registration-reentry
                                     (or nelisp-eln-registration--active-owner
                                         nelisp-eln-registration--pending-cleanups)))
    (unless (and (stringp path) (file-exists-p path))
      (nelisp-eln-registration--fail 'missing-emitted-artifact path))
    (nelisp-eln-registration--trace "REGTRACE open-start\n")
    (unwind-protect
        (progn
          (setq handle (nelisp-eln-system-loader-open path)
                preflight (nelisp-eln-registration--preflight handle)
                name (plist-get preflight :name))
          (nelisp-eln-registration--trace "REGTRACE preflight-done\n")
          (nelisp-eln-registration--trace "REGTRACE binding-check-before\n")
          (when (or (nelisp-eln-registration--target-fboundp name)
                    (not (nelisp-eln-registration--target-plist-empty-p name)))
            (nelisp-eln-registration--fail 'registration-name-already-bound name))
          (let ((name2 (plist-get preflight :name2)))
            (when (and name2
                       (or (nelisp-eln-registration--target-fboundp name2)
                           (not (nelisp-eln-registration--target-plist-empty-p
                                 name2))))
              (nelisp-eln-registration--fail 'registration-name-already-bound
                                             name2)))
          (nelisp-eln-registration--trace "REGTRACE binding-check-after\n")
          (let* ((metadata (plist-get preflight :metadata))
                 (eq-table (progn
                             (nelisp-eln-registration--trace "REGTRACE eq-table-create\n")
                             (make-hash-table :test 'eq)))
                 (equal-table (progn
                               (nelisp-eln-registration--trace "REGTRACE equal-table-create\n")
                               (make-hash-table :test 'equal)))
                 (fields (list nil nil
                               nil nil nil nil)))
            (nelisp-eln-registration--trace "REGTRACE unit-create-before\n")
            (setq unit (nelisp-eln-registration-objects-create-unit handle fields))
            (nelisp-eln-registration--trace "REGTRACE unit-create-after\n")
            (setq vector-unit
                  (nelisp-eln-registration-vectors-create-unit (aref unit 2)))
            (nelisp-eln-registration--trace "REGTRACE vector-unit-create-after\n")
            (setq owner (vector nelisp-eln-registration--owner-marker unit activation
                                name (plist-get preflight :c-name)
                                eq-table equal-table metadata
                                (plist-get preflight :leaf-cap) nil
                                vector-unit vectors-activation nil
                                '(:opaque-private-fields
                                  (lambda-gc-guard-h lambda-c-name-idx-h))
                                nil (plist-get preflight :arity) nil nil
                                nil nil))
            (unless (= (length owner) nelisp-eln-registration--owner-size)
              (error "owner vector literal drifted from --owner-size: %d/%d"
                     (length owner) nelisp-eln-registration--owner-size))
            (nelisp-eln-registration--trace "REGTRACE owner-create-after\n")
            (aset owner 18
                  (list :role-sequence (plist-get preflight :role-sequence)
                        :eval-effects (plist-get preflight :eval-effects)
                        :expected-type (plist-get preflight :expected-type)
                        :expected-type2 (plist-get preflight :expected-type2)
                        :name2 (plist-get preflight :name2)
                        :c-name2 (plist-get preflight :c-name2)
                        :rest2 (plist-get preflight :rest2)
                        :arity2 (plist-get preflight :arity2)
                        :min-arity (plist-get preflight :min-arity)
                        :leaf-cap2 (plist-get preflight :leaf-cap2)
                        :leaf-shape (plist-get preflight :leaf-shape)
                        :leaf-shape2 (plist-get preflight :leaf-shape2)
                        :type-index (plist-get preflight :type-index)
                        :type2-index (plist-get preflight :type2-index)
                        :rest1 (plist-get preflight :rest1)
                        :lambdas (plist-get (plist-get preflight :lambda-info)
                                            :lambdas)
                        :lambda-type-index (plist-get preflight
                                                      :lambda-type-index)
                        :d-reloc-address (plist-get preflight
                                                    :d-reloc-address)))
            (aset owner 12 (plist-get preflight :link-table))
            (setq nelisp-eln-registration--owners
                  (cons owner nelisp-eln-registration--owners)
                  nelisp-eln-registration--active-owner owner)
            (when (plist-get preflight :tail-imports)
              (aset owner 17
                    (vector nelisp-eln-native-subr--tail-lease-marker
                            handle owner (plist-get preflight :link-table)
                            (plist-get preflight :link-table-address)
                            (plist-get preflight :link-address)
                            (let ((analysis (plist-get preflight :tail-imports)))
                              (if (memq (plist-get analysis :proof)
                                        '(:multi-import-call :chain-call))
                                  (nelisp-eln-native-subr-import-entries
                                   analysis)
                                (nelisp-eln-native-subr-import-entry-address
                                 analysis)))
                            (plist-get preflight :tail-imports))))
            (let ((analysis2 (plist-get preflight :tail-imports2)))
              ;; The pair's second body gets its own lease over the same
              ;; retained table, carrying its own proof and entries.
              (when analysis2
                (aset owner 18
                      (plist-put
                       (aref owner 18) :lease2
                       (vector nelisp-eln-native-subr--tail-lease-marker
                               handle owner (plist-get preflight :link-table)
                               (plist-get preflight :link-table-address)
                               (plist-get preflight :link-address)
                               (nelisp-eln-native-subr-import-entries
                                analysis2)
                               analysis2)))))
            (nelisp-eln-registration--trace "REGTRACE unit-created\n")
            ;; GNU-produced profiles carry arbitrary constant graphs (for
            ;; the S6 eval profiles, a compiled `function-put' form and a
            ;; `(function ...)' type) that only the metadata capability can
            ;; materialize; every d_reloc slot is then populated from it.
            (when (memq (plist-get preflight :profile)
                        '(gnu-single-leaf gnu-eval-subr gnu-eval-subr-pair
                          gnu-verified-subr gnu-require-subr
                          gnu-lambda-require-subr))
              (require 'nelisp-eln-registration-metadata)
              (setq metadata-token
                    (nelisp-eln-registration-metadata-create
                     (plist-get preflight :profile)
                     (plist-get metadata :data-relocations)
                     (plist-get metadata :function-docs)))
              (unless metadata-token
                (nelisp-eln-registration--fail 'metadata-capability-refused))
              (aset owner 16 metadata-token))
            ;; Admit the actual interned NAME, then allocate every other
            ;; callback value and metadata vector before activating the codec.
            (let* ((name2 (plist-get preflight :name2))
                   (lambda-p (eq (plist-get preflight :profile)
                                 'gnu-lambda-require-subr))
                   (unit-word (nelisp-eln-registration-objects-unit-word unit))
                   (name-entries (nelisp-eln-objects-admit-registration-symbols
                                  (aref unit 2) (if name2 (list name name2)
                                                   (list name))
                                  (and nelisp-eln-registration--load-namespace
                                       t)))
                   (name-word (cdr (nth 0 name-entries)))
                   (name-word2 (and name2 (cdr (nth 1 name-entries))))
                   (c-name-word (nelisp-eln-registration-objects-encode-word
                                 unit (plist-get preflight :c-name)))
                   (c-name2-word
                    (and name2
                         (nelisp-eln-registration-objects-encode-word
                          unit (plist-get preflight :c-name2))))
                   (rest-word
                    (nelisp-eln-registration-objects-encode-word
                     unit (aref (plist-get metadata :ephemeral-data-relocations)
                                (if lambda-p 7 (+ 3 (or (plist-get preflight :eph-offset) 0))))))
                   ;; S6.12: the two lambdas' own C names and descriptors.
                   (lambda-words
                    (and lambda-p
                         (mapcar
                          (lambda (i)
                            (nelisp-eln-registration-objects-encode-word
                             unit (aref (plist-get metadata
                                                   :ephemeral-data-relocations)
                                        i)))
                          '(0 1 2 3))))
                   (rest2-word
                    (and name2
                         (nelisp-eln-registration-objects-encode-word
                          unit (aref (plist-get metadata :ephemeral-data-relocations) 7))))
                   (docs (plist-get metadata :function-docs))
                   (data (plist-get metadata :data-relocations))
                   (docs-word (if metadata-token
                                  (nelisp-eln-registration-metadata-docs-word
                                   metadata-token)
                                (nelisp-eln-registration-vectors-register
                                 vector-unit docs)))
                   (data-word (if metadata-token
                                  (nelisp-eln-registration-metadata-data-word
                                   metadata-token)
                                (nelisp-eln-registration-vectors-register
                                 vector-unit data)))
                   (unit-address (aref unit 4))
                   (eph-address (plist-get preflight :eph-address))
                   (d-reloc-address (plist-get preflight :d-reloc-address))
                   (unit-cell-address (plist-get preflight :unit-cell-address))
                   (link-address (plist-get preflight :link-address))
                   (link-table-address (plist-get preflight :link-table-address))
                   (state (progn (unless (fboundp 'nelisp-native-load--symbol-addr)
                                   (require 'nelisp-native-load))
                                 (nelisp-native-load--symbol-addr
                                  "nl_eln_callback7_context")))
                   (push-address (nelisp-native-load--symbol-addr
                                  "nelisp_eln_callback_context_push"))
                   (gateway (nelisp-native-load--symbol-addr
                             "wf_bytecode_call_gateway"))
                   (env (nelisp--native-env)))
              (nelisp-eln-registration--trace "REGTRACE activation-inputs-built\n")
              (nelisp-eln-registration--trace
               (format "REGTRACE registration-words name=%S c-name=%S rest=%S unit=%S\n"
                       name-word c-name-word rest-word unit-word))
              (unless (and (vectorp docs) (vectorp data))
                (nelisp-eln-registration--fail
                 'unsupported-metadata-vectors (list docs data)))
              (nelisp-eln-registration--trace "REGTRACE docs-field-write-before\n")
              (nelisp-eln-abi-write-word unit-address 40 docs-word)
              (nelisp-eln-registration--trace "REGTRACE docs-field-write-after\n")
              (nelisp-eln-registration--trace "REGTRACE data-field-write-before\n")
              (nelisp-eln-abi-write-word unit-address 48 data-word)
              (nelisp-eln-registration--trace "REGTRACE data-field-write-after\n")
              (nelisp-eln-registration--trace "REGTRACE vector-activation-before\n")
              (setq vectors-activation
                    (nelisp-eln-registration-vectors-begin-activation vector-unit)
                    )
              (nelisp-eln-registration--trace "REGTRACE vector-activation-after\n")
              (nelisp-eln-registration--trace "REGTRACE object-activation-before\n")
              (setq activation
                    (nelisp-eln-registration-objects-begin-activation unit))
              (nelisp-eln-registration--trace "REGTRACE object-activation-after\n")
              (aset owner 2 activation)
              (nelisp-eln-registration--trace "REGTRACE owner-activation-set\n")
              (aset owner 14 name-word)
              (nelisp-eln-registration--trace "REGTRACE activation-ready\n")
              (nelisp-eln-registration--trace "REGTRACE pin-begin\n")
              (setq pin-env env
                    callback-addr (nelisp-native-load--symbol-addr
                                   "nelisp_eln_callback7_entry_word"))
              (unless (and (> state 0) (> push-address 0) (> gateway 0)
                           (> callback-addr 0) (= (ptr-read-u64 state 24) 0))
                (nelisp-eln-registration--fail 'callback-runtime-unavailable))
              (setq pin-marker (nelisp-native-load--pin-begin env)
                    function-slot (nelisp-native-load--pin-reserve env pin-marker)
                    args-slot (nelisp-native-load--pin-reserve env pin-marker)
                    out-slot (nelisp-native-load--pin-reserve env pin-marker))
              (nelisp-native-load-box function-slot
                                      'nelisp-eln-registration--callback)
              (setq callback-token
                    (nelisp-eln-registration--call6
                     push-address gateway env function-slot args-slot out-slot 1))
              (unless (> callback-token 0)
                (nelisp-eln-registration--fail 'callback-context-refused))
              (nelisp-eln-registration--trace "REGTRACE pin-ready\n")
              (nelisp-eln-registration--trace "REGTRACE callback-context-ready\n")
              ;; The exact-shape preflight admitted only these writable cells
              ;; and import slots 1030 / 947. No callback runs before this
              ;; point. Only the d_reloc slot(s) the admitted top-level shape
              ;; actually reads (the register_subr `type' argument(s)) are
              ;; populated; slots the admitted shape never dereferences (the
              ;; Feval call's form/lexenv arguments -- ignored entirely by
              ;; `nelisp-eln-registration--apply-eval-callback', which never
              ;; runs real `Feval') are left at their already-verified-zero
              ;; value.
              (cond
               (metadata-token
                (nelisp-eln-registration--write-metadata-relocations
                 d-reloc-address metadata-token
                 (plist-get preflight :data-reloc-count)))
               ((plist-get preflight :role-sequence)
                (nelisp-eln-abi-write-word
                 d-reloc-address (* 8 (plist-get preflight :type-index))
                 (nelisp-eln-registration-objects-encode-word
                  unit (plist-get preflight :expected-type)))
                (when (plist-get preflight :expected-type2)
                  (nelisp-eln-abi-write-word
                   d-reloc-address (* 8 (plist-get preflight :type2-index))
                   (nelisp-eln-registration-objects-encode-word
                    unit (plist-get preflight :expected-type2)))))
               (t
                (nelisp-eln-abi-write-word
                 d-reloc-address 0 (nelisp-eln-abi-encode-nil))))
              (if lambda-p
                  (let ((i 0))
                    ;; [C-NAME-1 REST-1 C-NAME-2 REST-2 1 NAME C-NAME REST]
                    (dolist (word lambda-words)
                      (nelisp-eln-abi-write-word eph-address (* 8 i) word)
                      (setq i (1+ i)))
                    (nelisp-eln-abi-write-word
                     eph-address 32
                     (nelisp-eln-abi-encode-fixnum
                      (aref (plist-get metadata :ephemeral-data-relocations)
                            4)))
                    (nelisp-eln-abi-write-word eph-address 40 name-word)
                    (nelisp-eln-abi-write-word eph-address 48 c-name-word)
                    (nelisp-eln-abi-write-word eph-address 56 rest-word))
                (progn
                (nelisp-eln-abi-write-word
                 eph-address 0
                 (nelisp-eln-abi-encode-fixnum
                  (aref (plist-get metadata :ephemeral-data-relocations) 0)))
                (when (eql (plist-get preflight :eph-offset) 1)
                  ;; S6.6: the `&optional' layout's second word is MAX arity.
                  (nelisp-eln-abi-write-word
                   eph-address 8
                   (nelisp-eln-abi-encode-fixnum
                    (aref (plist-get metadata :ephemeral-data-relocations) 1))))
                (nelisp-eln-abi-write-word
                 eph-address (* 8 (+ 1 (or (plist-get preflight :eph-offset) 0)))
                 name-word)
                (nelisp-eln-abi-write-word
                 eph-address (* 8 (+ 2 (or (plist-get preflight :eph-offset) 0)))
                 c-name-word)
                (nelisp-eln-abi-write-word
                 eph-address (* 8 (+ 3 (or (plist-get preflight :eph-offset) 0)))
                 rest-word)
                  ))
              (when name2
                (aset owner 18 (plist-put (aref owner 18)
                                          :name-word2 name-word2))
                (nelisp-eln-abi-write-word
                 eph-address 32
                 (nelisp-eln-abi-encode-fixnum
                  (aref (plist-get metadata :ephemeral-data-relocations) 4)))
                (nelisp-eln-abi-write-word eph-address 40 name-word2)
                (nelisp-eln-abi-write-word eph-address 48 c-name2-word)
                (nelisp-eln-abi-write-word eph-address 56 rest2-word))
              (nelisp-eln-abi-write-word unit-cell-address 0 unit-word)
              (unless (= (nelisp-eln-abi-read-word
                          eph-address
                          (if lambda-p
                              40
                            (* 8 (+ 1 (or (plist-get preflight :eph-offset) 0)))))
                         name-word)
                (nelisp-eln-registration--fail 'name-word-relocation-mismatch
                                               (list name-word
                                                     (nelisp-eln-abi-read-word
                                                      eph-address
                                                      (if lambda-p
                                                          40
                                                        (* 8 (+ 1 (or (plist-get preflight :eph-offset) 0))))))))
              (when (plist-get preflight :symbols-with-pos-address)
                ;; GNU points this cell at its `symbols_with_pos_enabled'
                ;; bool; NeLisp never enables symbols with position, so
                ;; point it at an owned zero byte, retained with OWNER.
                (let ((memory (nl-ffi-memory-allocate 8)))
                  (ptr-write-u64 (nl-ffi-memory-address memory) 0 0)
                  (aset owner 18 (plist-put (aref owner 18)
                                            :symbols-with-pos-memory memory))
                  (ptr-write-u64 (plist-get preflight :symbols-with-pos-address)
                                 0 (nl-ffi-memory-address memory))))
              (nelisp-eln-registration--install-import-table
               preflight callback-addr)
              ;; This is the sole top_level_run call. Never retry on error.
              (nelisp-eln-registration--trace "REGTRACE native-call-before\n")
              (setq returned
                    (ptr-call (nth 3 (plist-get preflight :top-cap))
                              unit-word 0 0 0 0 0))
              (nelisp-eln-registration--trace "REGTRACE native-call-after\n")
              (unless (= (ptr-read-u64 state 24) 0)
                (nelisp-eln-registration--fail 'callback-status
                                               (list (ptr-read-u64 state 24)
                                                     nelisp-eln-registration--last-callback-error)))
              (setq decoded
                    (nelisp-eln-registration-objects-decode
                     activation (nelisp-eln-abi-normalize-word returned))
                    callable (aref owner 9))
              ;; top_level_run's own machine code returns whichever call it
              ;; made last: the second (compiler-macro) registration for the
              ;; `gnu-eval-subr-pair' profile, otherwise the first (and
              ;; only) registration -- see `nelisp-eln-registration--callback'.
              (let* ((name2 (plist-get preflight :name2))
                     (callable2 (and name2 (cdr (aref owner 19))))
                     (final-callable (or callable2 callable))
                     (final-word (if name2 nelisp-eln-registration--registered-word-2
                                   nelisp-eln-registration--registered-word)))
                (unless (and callable final-callable (eq decoded final-callable)
                             (= (nelisp-eln-abi-normalize-word returned) final-word)
                             (eq (nelisp-eln-registration--target-function
                                  name)
                                 callable)
                             (or (not name2)
                                 (eq (nelisp-eln-registration--target-function
                                      name2)
                                     callable2)))
                  (nelisp-eln-registration--fail 'registered-return-mismatch
                                                 returned)))
              (nelisp-eln-registration--trace "REGTRACE registered-callable-retained\n")
              (setq finalizing t)
              (nelisp-eln-registration--finish-success
               preflight unit activation vector-unit vectors-activation owner
               pin-env pin-marker callback-token)
              (setq result (list :success t :name name
                                 :c-name (plist-get preflight :c-name)
                                 :callable callable
                                 :name2 (plist-get preflight :name2)
                                 :callable2 (cdr (aref owner 19)))
                    success t)))
          result)
      (setq callable nil decoded nil)
      (nelisp-eln-registration--containment-boundary
       success finalizing handle preflight unit activation vector-unit
       vectors-activation owner pin-env pin-marker callback-token name))))

;; Whole-file executable-region authentication (crash-corpus gate hardening).
;;
;; Every byte the dynamic loader can ever execute must be authenticated, not
;; only `top_level_run' and admitted function bodies: `.init'/`.fini' (run
;; by `nl-ffi--dlopen'/dlclose's INIT_ARRAY/FINI_ARRAY, not by any Lisp
;; here), `.plt'/`.plt.got', the GCC-emitted CRT stubs in `.text'
;; (deregister_tm_clones, register_tm_clones, __do_global_dtors_aux,
;; frame_dummy), and the alignment padding between functions.  A single
;; flipped byte inside __do_global_dtors_aux previously passed this gate
;; entirely unauthenticated.
;;
;; IMPORTANT ORDERING LIMIT, found while implementing this: `nl-ffi--dlopen'
;; already ran, inside `nelisp-eln-system-loader-open', by the time any of
;; this preflight code -- or any other Lisp -- gets to look at anything.
;; That means `.init'/`.init_array' (frame_dummy -> register_tm_clones) has
;; ALREADY EXECUTED NATIVELY before this check can reject a tampered file,
;; and `.fini'/`.fini_array' (__do_global_dtors_aux) WILL execute at the
;; eventual, unavoidable dlclose this module's own cleanup path performs
;; even when this check rejects the artifact -- releasing the mmap requires
;; that dlclose regardless of the Lisp-level verdict. This validation genuinely
;; prevents a tampered artifact from ever being registered (fset) for use,
;; and gives a loud, specific rejection instead of silent trust, but it
;; cannot retroactively prevent that one INIT_ARRAY/FINI_ARRAY execution --
;; only `nelisp-eln-system-loader-open' itself could, by running equivalent
;; static byte checks on the file it already parses BEFORE calling
;; `nl-ffi--dlopen' (system-loader.el is out of this file's ownership; that
;; reordering is the "exact minimal hook" this lane would need to add).


;;; S7.8: pre-dlopen file-byte authentication.
;;
;; `nl-ffi--dlopen' runs `.init'/DT_INIT and INIT_ARRAY (frame_dummy,
;; which calls register_tm_clones) immediately, as an ordinary part of
;; opening a shared object -- confirmed empirically while building the
;; S7.7 executable-region tests above: flipping a byte in `.init' or
;; `.plt.got' and then opening the file segfaulted the process outright,
;; before any preflight code, this file's or anyone else's, ever ran.
;; Anything checked only after `nelisp-eln-system-loader-open' returns is
;; too late for those two regions. Everything below reads only the plain
;; file BYTES `nelisp-eln-system-loader-open' already has in hand before
;; it ever calls `nl-ffi--dlopen' -- no handle, no live memory, no bias --
;; and `nelisp-eln-system-loader-preopen-validator' (defined in
;; nelisp-eln-system-loader.el, set below) is the hook that lets this
;; file's validator run there without system-loader.el requiring this
;; file back (that would be a circular `require').
;;
;; `top_level_run' and admitted leaf bodies are deliberately NOT
;; re-validated here: unlike `.init'/CRT stubs, they are never invoked by
;; `dlopen'/`dlclose' themselves -- only by this module's own explicit
;; `ptr-call' in `nelisp-eln-registration-load', which already runs
;; strictly after the existing, unchanged `--preflight'. That ordering
;; was already correct; only the CRT/dynamic-section surface dlopen
;; itself executes needed to move earlier.

(defconst nelisp-eln-registration--dt-needed 1)
(defconst nelisp-eln-registration--dt-strtab 5)
(defconst nelisp-eln-registration--dt-init 12)
(defconst nelisp-eln-registration--dt-fini 13)
(defconst nelisp-eln-registration--dt-rpath 15)
(defconst nelisp-eln-registration--dt-textrel 22)
(defconst nelisp-eln-registration--dt-init-array 25)
(defconst nelisp-eln-registration--dt-fini-array 26)
(defconst nelisp-eln-registration--dt-init-arraysz 27)
(defconst nelisp-eln-registration--dt-fini-arraysz 28)
(defconst nelisp-eln-registration--dt-runpath 29)
(defconst nelisp-eln-registration--dt-flags 30)
(defconst nelisp-eln-registration--dt-preinit-array 32)
(defconst nelisp-eln-registration--dt-preinit-arraysz 33)
(defconst nelisp-eln-registration--df-textrel #x4)
(defconst nelisp-eln-registration--r-x86-64-relative 8)
(defconst nelisp-eln-registration--allowed-needed-libraries
  '("libc.so.6")
  "The only DT_NEEDED library names a genuine small-tier .eln may declare.")

(defconst nelisp-eln-registration--init-template
  (unibyte-string
   #x48 #x83 #xec #x08 #x48 #x8b #x05 #xd5 #x2f #x00 #x00 #x48 #x85 #xc0
   #x74 #x02 #xff #xd0 #x48 #x83 #xc4 #x08 #xc3)
  "GNU 31.1 x86-64 `.init' (`_init'): byte-for-byte identical across every
genuine artifact this file's own S6/corpus survey has seen; the `nop'-free
`__gmon_start__' GOT check its only external reference always resolves to
the same fixed GOT slot, so this template has no holes.")

(defconst nelisp-eln-registration--plt-template
  (unibyte-string
   #xff #x35 #xca #x2f #x00 #x00 #xff #x25 #xcc #x2f #x00 #x00
   #x0f #x1f #x40 #x00)
  "GNU 31.1 x86-64 `.plt' PLT0 resolver stub template; no holes (see
`nelisp-eln-registration--init-template').")

(defconst nelisp-eln-registration--plt-got-template
  (unibyte-string #xff #x25 #x7a #x2f #x00 #x00 #x66 #x90)
  "GNU 31.1 x86-64 `.plt.got' single `__cxa_finalize@plt' entry template;
no holes.  Only admits a `.plt.got' of exactly this one entry's size --
an artifact needing more direct-GOT external calls would need a repeated-
pattern template this file does not yet build (see the corpus gate run
this was validated against).")

(defconst nelisp-eln-registration--fini-template
  (unibyte-string #x48 #x83 #xec #x08 #x48 #x83 #xc4 #x08 #xc3)
  "GNU 31.1 x86-64 `.fini' (`_fini') template; no holes.")

(defconst nelisp-eln-registration--crt-stub-template
  (unibyte-string
   #x48 #x8d #x3d 0 0 0 0 #x48 #x8d #x05 0 0 0 0
   #x48 #x39 #xf8 #x74 #x15 #x48 #x8b #x05 #x66 #x2f 0 0 #x48 #x85
   #xc0 #x74 #x09 #xff #xe0 #x0f #x1f #x80 0 0 0 0 #xc3 #x0f
   #x1f #x80 0 0 0 0 #x48 #x8d #x3d 0 0 0 0 #x48
   #x8d #x35 0 0 0 0 #x48 #x29 #xfe #x48 #x89 #xf0 #x48 #xc1
   #xee #x3f #x48 #xc1 #xf8 #x03 #x48 #x01 #xc6 #x48 #xd1 #xfe #x74 #x14
   #x48 #x8b #x05 #x1d #x2f 0 0 #x48 #x85 #xc0 #x74 #x08 #xff #xe0
   #x66 #x0f #x1f #x44 0 0 #xc3 #x0f #x1f #x80 0 0 0 0
   #xf3 #x0f #x1e #xfa #x80 #x3d 0 0 0 0 0 #x75 #x2b #x55
   #x48 #x83 #x3d #xea #x2e 0 0 0 #x48 #x89 #xe5 #x74 #x0c #x48
   #x8b #x3d #x2e #x2f 0 0 #xe8 #x59 #xff #xff #xff #xe8 #x64 #xff
   #xff #xff #xc6 #x05 0 0 0 0 #x01 #x5d #xc3 #x0f #x1f 0
   #xc3 #x0f #x1f #x80 0 0 0 0 #xf3 #x0f #x1e #xfa #xe9 #x77
   #xff #xff #xff #x0f #x1f #x80 0 0 0 0)
  "GNU 31.1 x86-64 `deregister_tm_clones' + `register_tm_clones' +
`__do_global_dtors_aux' + `frame_dummy', concatenated -- the fixed CRT
stub block GCC always emits at the start of `.text', right after
`.plt.got'.  Holes: `__TMC_END__' (x4) and `completed.0' (x2); see
`nelisp-eln-registration--crt-stub-holes'/`-crt-stub-relocs'.")

(defconst nelisp-eln-registration--crt-stub-holes
  '((3 . 7) (10 . 14) (51 . 55) (58 . 62) (118 . 122) (158 . 162)))

(defconst nelisp-eln-registration--crt-stub-reloc-groups
  '((3 10 51 58) (118 158))
  "Which `nelisp-eln-registration--crt-stub-holes' offsets reference the
exact same symbol: all four `__TMC_END__' loads, then both `completed.0'
loads.  See `nelisp-eln-registration--validate-direct-rip-relocs'.")

(defconst nelisp-eln-registration--nop-forms
  (list (unibyte-string #x90)
        (unibyte-string #x66 #x90)
        (unibyte-string #x0f #x1f #x00)
        (unibyte-string #x0f #x1f #x40 #x00)
        (unibyte-string #x0f #x1f #x44 #x00 #x00)
        (unibyte-string #x66 #x0f #x1f #x44 #x00 #x00)
        (unibyte-string #x0f #x1f #x80 #x00 #x00 #x00 #x00)
        (unibyte-string #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00)
        (unibyte-string #x66 #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00)
        (unibyte-string #x66 #x2e #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00)
        (unibyte-string #x66 #x66 #x2e #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00))
  "The standard x86-64 multi-byte NOP encodings binutils/GCC use for
alignment padding.  The first nine are the canonical one-per-length (1-9
bytes) forms.  The eleven-byte tenth entry (two `0x66' operand-size
prefixes plus one `0x2e' CS-segment-override prefix stacked in front of
the seven-byte `nop DWORD PTR [rax+rax*1+0x0]' encoding) is GAS's own
choice for a gap it cannot fill with one 1-9 byte form alone; confirmed
against a genuine GNU 31.1 toolchain artifact
\(~/.cache/tmp/eln-gnu-identity-investigation/gnu-identity.eln, a 12-byte
alignment gap GAS filled with this 11-byte form plus one 1-byte `nop').
The ten-byte `cs nopw 0x0(%rax,%rax,1)' form (`66 2e 0f 1f 84' plus a
four-byte zero displacement, the Intel-recommended 10-byte NOP; S6.12:
GAS fills the ten-byte gap after `byte-compile-if''s first lambda with
it).  Matching is never positional-by-length any more -- see
`nelisp-eln-registration--padding-decomposes-p', which tries every entry
at every position -- so entries need not be contiguous by length.")

(defun nelisp-eln-registration--padding-decomposes-p (bytes)
  "Return non-nil if BYTES divides, left to right with no leftover byte,
into a sequence of entries from `nelisp-eln-registration--nop-forms' --
each an exact, byte-for-byte previously-authenticated x86-64 multi-byte
`nop' encoding, never an unverified guess.  Tries candidates longest
first at each position: none of the canonical encodings above is a
prefix of a different, longer one at the same starting byte, so this
greedy scan is unambiguous and needs no backtracking."
  (let ((pos 0) (n (length bytes))
        (forms (sort (copy-sequence nelisp-eln-registration--nop-forms)
                      (lambda (a b) (> (length a) (length b))))))
    (catch 'stuck
      (while (< pos n)
        (let ((matched nil))
          (dolist (form forms)
            (unless matched
              (let ((flen (length form)))
                (when (and (<= (+ pos flen) n)
                           (equal (substring bytes pos (+ pos flen)) form))
                  (setq matched t)
                  (setq pos (+ pos flen))))))
          (unless matched (throw 'stuck nil))))
      t)))
(defun nelisp-eln-registration--read-u16-le (bytes offset)
  (+ (aref bytes offset) (ash (aref bytes (1+ offset)) 8)))

(defun nelisp-eln-registration--read-u64-le (bytes offset)
  (+ (nelisp-eln-registration--read-u32-le bytes offset)
     (ash (nelisp-eln-registration--read-u32-le bytes (+ offset 4)) 32)))

(defun nelisp-eln-registration--c-string-at (bytes offset)
  "Return the NUL-terminated ASCII string in BYTES starting at OFFSET."
  (let ((end offset) (n (length bytes)))
    (while (and (< end n) (/= (aref bytes end) 0))
      (setq end (1+ end)))
    (substring bytes offset end)))

(defun nelisp-eln-registration--elf-section-in-bytes (bytes name)
  "Return (ADDR OFFSET SIZE) for ELF section NAME in raw file BYTES, or nil.
Section headers carry no runtime significance to the loader itself (only
program headers and the symbol table do; `nelisp-eln-system-loader-open'
never parses them) -- this is the one place this file reads them, purely
to name `.init'/`.fini'/`.plt'/`.plt.got'/`.dynamic'/`.rela.dyn' boundaries
no symbol names.  Pure static parsing of BYTES; touches neither live
memory nor a loader handle, so this runs equally well before or after
`nelisp-eln-system-loader-open' -- and in particular from the pre-dlopen
validator hook it installs (see the file-level commentary above)."
  (let* ((shoff (nelisp-eln-registration--read-u64-le bytes #x28))
         (shentsize (nelisp-eln-registration--read-u16-le bytes #x3a))
         (shnum (nelisp-eln-registration--read-u16-le bytes #x3c))
         (shstrndx (nelisp-eln-registration--read-u16-le bytes #x3e))
         (strtab-off
          (nelisp-eln-registration--read-u64-le
           bytes (+ shoff (* shstrndx shentsize) 24)))
         (i 0) (found nil))
    (while (and (not found) (< i shnum))
      (let* ((base (+ shoff (* i shentsize)))
             (name-off (nelisp-eln-registration--read-u32-le bytes base))
             (candidate
              (nelisp-eln-registration--c-string-at
               bytes (+ strtab-off name-off))))
        (when (equal candidate name)
          (setq found
                (list (nelisp-eln-registration--read-u64-le bytes (+ base 16))
                      (nelisp-eln-registration--read-u64-le bytes (+ base 24))
                      (nelisp-eln-registration--read-u64-le bytes (+ base 32))))))
      (setq i (1+ i)))
    found))

(defun nelisp-eln-registration--elf-section (handle name)
  "Return (ADDR . SIZE) for ELF section NAME in HANDLE's own file, or nil.
Reads the already-opened, integrity-checked on-file bytes HANDLE's own
state already holds; see `nelisp-eln-registration--elf-section-in-bytes'."
  (let* ((state (nelisp-eln-system-loader--state handle))
         (found (nelisp-eln-registration--elf-section-in-bytes
                 (plist-get state :file-bytes) name)))
    (and found (cons (nth 0 found) (nth 2 found)))))

;; Layout shift.  GNU's linker places `.got'/`.got.plt' at the next page
;; boundary after the last executable byte, so an artifact whose `.text'
;; crosses a 4 KiB boundary (e.g. byte-compile-lambda, `.text' 0x10c6
;; bytes) has every GOT-relative displacement in the fixed `.init'/`.plt'/
;; `.plt.got'/CRT-stub bytes larger by a whole number of pages than the
;; smaller artifacts the templates were captured from (`.got.plt' at
;; 0x3fe8).  The templates stay byte-exact: each GOT-relative
;; displacement field (listed explicitly per template) is increased by
;; the file's own `.got.plt' shift, which must be a non-negative multiple
;; of 0x1000 and, when `.dynamic' exists, must agree with DT_PLTGOT.

(defconst nelisp-eln-registration--base-got-plt-address #x3fe8
  "`.got.plt' address of the artifacts the fixed templates were captured from.")

(defconst nelisp-eln-registration--init-got-fields '(7))
(defconst nelisp-eln-registration--plt-got-fields '(2))
(defconst nelisp-eln-registration--plt-fields '(2 8))
(defconst nelisp-eln-registration--crt-stub-got-fields '(22 87 129 142))

(defun nelisp-eln-registration--layout-shift-in-bytes (bytes)
  "Return the authenticated GOT page shift of the ELF in BYTES, or nil."
  (let* ((got (nelisp-eln-registration--elf-section-in-bytes bytes ".got.plt"))
         (entries (nelisp-eln-registration--parse-dynamic-entries bytes))
         (pltgot (nelisp-eln-registration--dynamic-values entries 3)))
    (and got
         (let ((shift (- (nth 0 got)
                         nelisp-eln-registration--base-got-plt-address)))
           (and (>= shift 0) (< shift #x100000) (= (% shift #x1000) 0)
                (or (null entries) (equal pltgot (list (nth 0 got))))
                shift)))))

(defun nelisp-eln-registration--shift-template (template fields shift)
  "Return a copy of TEMPLATE with each u32 field offset in FIELDS raised by SHIFT."
  (if (= shift 0)
      template
    (let ((out (copy-sequence template)))
      (dolist (f fields)
        (let ((v (+ (nelisp-eln-registration--read-u32-le out f) shift)))
          (dotimes (i 4)
            (aset out (+ f i) (logand (ash v (* -8 i)) #xff)))))
      out)))

(defun nelisp-eln-registration--fixed-templates (shift)
  "Return (INIT PLT PLT-GOT CRT-STUB) templates shifted by SHIFT."
  (list (nelisp-eln-registration--shift-template
         nelisp-eln-registration--init-template
         nelisp-eln-registration--init-got-fields shift)
        (nelisp-eln-registration--shift-template
         nelisp-eln-registration--plt-template
         nelisp-eln-registration--plt-fields shift)
        (nelisp-eln-registration--shift-template
         nelisp-eln-registration--plt-got-template
         nelisp-eln-registration--plt-got-fields shift)
        (nelisp-eln-registration--shift-template
         nelisp-eln-registration--crt-stub-template
         nelisp-eln-registration--crt-stub-got-fields shift)))

(defun nelisp-eln-registration--validate-crt-stubs (handle)
  "Validate the CRT stub block at the very start of `.text', if this
artifact has one.  `deregister_tm_clones' and its siblings have no
`.dynsym' entry (see `nelisp-eln-registration--raw-read-bytes'), so
presence is decided the same way `nelisp-eln-registration--elf-section'
already lets `.init'/`.plt'/`.plt.got'/`.fini' validation decide it: a
genuine GCC-toolchain artifact always emits `.init' together with these
CRT stubs, in that exact fixed order at the start of `.text'; a minimal
self-emitted fixture has neither. `.init' absent is admitted (skip);
`.init' present makes the CRT stub block's own presence and exact
content mandatory, never optional."
  (let ((init (nelisp-eln-registration--elf-section handle ".init"))
        (text (nelisp-eln-registration--elf-section handle ".text")))
    (or (null init)
        (let* ((bias (plist-get (nelisp-eln-system-loader--state handle) :bias))
               (address (+ bias (car text)))
               (shift (nelisp-eln-registration--layout-shift-in-bytes
                       (plist-get (nelisp-eln-system-loader--state handle)
                                  :file-bytes)))
               (crt (and shift (nth 3 (nelisp-eln-registration--fixed-templates
                                       shift))))
               (actual (and crt
                            (nelisp-eln-registration--raw-read-bytes
                             handle address (length crt)))))
          (and crt
               (nelisp-eln-registration--match-holed-template
                actual crt nelisp-eln-registration--crt-stub-holes)
               (nelisp-eln-registration--validate-direct-rip-relocs
                address actual
                nelisp-eln-registration--crt-stub-reloc-groups))))))

(defun nelisp-eln-registration--parse-dynamic-entries (bytes)
  "Return every (TAG . VALUE) entry of `.dynamic' in BYTES, or nil if
BYTES has no such section (a self-emitted fixture, say)."
  (let ((section (nelisp-eln-registration--elf-section-in-bytes
                  bytes ".dynamic")))
    (and section
         (let ((off (nth 1 section)) (end (+ (nth 1 section) (nth 2 section)))
               (entries nil) (done nil))
           (while (and (not done) (< off end))
             (let ((tag (nelisp-eln-registration--read-u64-le bytes off))
                   (val (nelisp-eln-registration--read-u64-le bytes (+ off 8))))
               (push (cons tag val) entries)
               (when (= tag 0) (setq done t)))
             (setq off (+ off 16)))
           (nreverse entries)))))

(defun nelisp-eln-registration--parse-rela-entries (bytes section-name)
  "Return every (OFFSET TYPE ADDEND) entry of SECTION-NAME in BYTES."
  (let ((section (nelisp-eln-registration--elf-section-in-bytes
                  bytes section-name)))
    (and section
         (let ((off (nth 1 section)) (end (+ (nth 1 section) (nth 2 section)))
               (entries nil))
           (while (< off end)
             (push (list (nelisp-eln-registration--read-u64-le bytes off)
                         (logand (nelisp-eln-registration--read-u64-le
                                  bytes (+ off 8))
                                 #xffffffff)
                         (nelisp-eln-registration--read-u64-le bytes (+ off 16)))
                   entries)
             (setq off (+ off 24)))
           (nreverse entries)))))

(defun nelisp-eln-registration--dynamic-values (entries tag)
  "Return every VALUE in ENTRIES (see `-parse-dynamic-entries') for TAG."
  (delq nil (mapcar (lambda (e) (and (= (car e) tag) (cdr e))) entries)))

(defun nelisp-eln-registration--rela-addend-at (relocations offset)
  "Return the R_X86_64_RELATIVE addend targeting OFFSET in RELOCATIONS,
or nil if none or more than one does (either is inadmissible: the slot
either is never written by a relocation, matching its own static file
content exactly, or is written exactly once, authenticated below)."
  (let ((matches
         (delq nil
               (mapcar (lambda (r)
                         (and (= (nth 0 r) offset)
                              (= (nth 1 r)
                                 nelisp-eln-registration--r-x86-64-relative)
                              (nth 2 r)))
                       relocations))))
    (and (= (length matches) 1) (car matches))))

(defun nelisp-eln-registration--validate-dynamic-section (bytes crt-text-addr)
  "Validate `.dynamic', if BYTES has one, against CRT-TEXT-ADDR: the
`.text' section's own address when a genuine CRT stub block is present
there (see `nelisp-eln-registration--validate-crt-stubs'), or nil for a
self-emitted layout with no CRT stubs and no dynamic-loader-executed
surface at all. Authenticates: DT_INIT/DT_FINI point at `.init'/`.fini'
exactly; DT_INIT_ARRAY/DT_FINI_ARRAY exist iff CRT-TEXT-ADDR is non-nil,
hold exactly one pointer each, and that pointer -- read from the
authoritative R_X86_64_RELATIVE relocation targeting it when one exists,
otherwise its own static file content -- is exactly frame_dummy's /
__do_global_dtors_aux's address within the CRT stub block; no
DT_RPATH/DT_RUNPATH/DT_PREINIT_ARRAY/DT_TEXTREL/DF_TEXTREL; every
DT_NEEDED name is in `nelisp-eln-registration--allowed-needed-libraries'."
  (let ((entries (nelisp-eln-registration--parse-dynamic-entries bytes)))
    (or (null entries)
        (let* ((init-section (nelisp-eln-registration--elf-section-in-bytes
                              bytes ".init"))
               (fini-section (nelisp-eln-registration--elf-section-in-bytes
                              bytes ".fini"))
               (relocations (nelisp-eln-registration--parse-rela-entries
                            bytes ".rela.dyn"))
               (dt-init (nelisp-eln-registration--dynamic-values
                        entries nelisp-eln-registration--dt-init))
               (dt-fini (nelisp-eln-registration--dynamic-values
                        entries nelisp-eln-registration--dt-fini))
               (dt-init-array (nelisp-eln-registration--dynamic-values
                              entries nelisp-eln-registration--dt-init-array))
               (dt-init-arraysz
                (nelisp-eln-registration--dynamic-values
                 entries nelisp-eln-registration--dt-init-arraysz))
               (dt-fini-array (nelisp-eln-registration--dynamic-values
                              entries nelisp-eln-registration--dt-fini-array))
               (dt-fini-arraysz
                (nelisp-eln-registration--dynamic-values
                 entries nelisp-eln-registration--dt-fini-arraysz))
               (dt-flags (nelisp-eln-registration--dynamic-values
                         entries nelisp-eln-registration--dt-flags))
               (needed (nelisp-eln-registration--dynamic-values
                       entries nelisp-eln-registration--dt-needed))
               (strtab-section (nelisp-eln-registration--elf-section-in-bytes
                               bytes ".dynstr")))
          (and
           ;; No unauthenticated dynamic-linker search-path or preinit
           ;; surface, ever.
           (null (nelisp-eln-registration--dynamic-values
                  entries nelisp-eln-registration--dt-rpath))
           (null (nelisp-eln-registration--dynamic-values
                  entries nelisp-eln-registration--dt-runpath))
           (null (nelisp-eln-registration--dynamic-values
                  entries nelisp-eln-registration--dt-preinit-array))
           (null (nelisp-eln-registration--dynamic-values
                  entries nelisp-eln-registration--dt-textrel))
           (null (delq nil (mapcar (lambda (v)
                                     (/= 0 (logand v
                                            nelisp-eln-registration--df-textrel)))
                                   dt-flags)))
           ;; Every declared external library is on the fixed allowlist.
           (or (null needed)
               (and strtab-section
                    (let ((ok t))
                      (dolist (off needed)
                        (unless (member
                                 (nelisp-eln-registration--c-string-at
                                  bytes (+ (nth 1 strtab-section) off))
                                 nelisp-eln-registration--allowed-needed-libraries)
                          (setq ok nil)))
                      ok)))
           ;; DT_INIT/DT_FINI, when present, point exactly at .init/.fini.
           (or (null dt-init)
               (and init-section (equal dt-init (list (nth 0 init-section)))))
           (or (null dt-fini)
               (and fini-section (equal dt-fini (list (nth 0 fini-section)))))
           (if (null crt-text-addr)
               ;; No CRT stub block: none of this dynamic-loader-executed
               ;; array surface is admitted either.
               (and (null dt-init) (null dt-fini)
                    (null dt-init-array) (null dt-fini-array))
             (and dt-init dt-fini
                  (= (length dt-init-array) 1)
                  (equal dt-init-arraysz '(8))
                  (= (length dt-fini-array) 1)
                  (equal dt-fini-arraysz '(8))
                  (let* ((init-array-addr (car dt-init-array))
                         (fini-array-addr (car dt-fini-array))
                         (frame-dummy-addr (+ crt-text-addr 176))
                         (dtors-aux-addr (+ crt-text-addr 112))
                         (init-target
                          (or (nelisp-eln-registration--rela-addend-at
                               relocations init-array-addr)
                              (nelisp-eln-registration--read-u64-le
                               bytes
                               (+ (nth 1 (nelisp-eln-registration--elf-section-in-bytes
                                          bytes ".init_array"))
                                  0))))
                         (fini-target
                          (or (nelisp-eln-registration--rela-addend-at
                               relocations fini-array-addr)
                              (nelisp-eln-registration--read-u64-le
                               bytes
                               (+ (nth 1 (nelisp-eln-registration--elf-section-in-bytes
                                          bytes ".fini_array"))
                                  0)))))
                    (and (= init-target frame-dummy-addr)
                         (= fini-target dtors-aux-addr))))))))))

(defun nelisp-eln-registration--validate-preopen (bytes)
  "Authenticate BYTES before `nelisp-eln-system-loader-open' ever calls
`nl-ffi--dlopen' on them: `.init'/`.plt'/`.plt.got'/`.fini', the CRT stub
block, and `.dynamic' (DT_INIT/DT_FINI/INIT_ARRAY/FINI_ARRAY/RPATH/
RUNPATH/PREINIT_ARRAY/TEXTREL/NEEDED). GOT-relative displacements in the
fixed templates are checked against the file's own authenticated GOT page
shift (`nelisp-eln-registration--layout-shift-in-bytes').  Installed as
`nelisp-eln-system-loader-preopen-validator' below."
  (let* ((init (nelisp-eln-registration--elf-section-in-bytes bytes ".init"))
         (text (nelisp-eln-registration--elf-section-in-bytes bytes ".text"))
         (shift (if (nelisp-eln-registration--elf-section-in-bytes
                     bytes ".got.plt")
                    (nelisp-eln-registration--layout-shift-in-bytes bytes)
                  0))
         (templates (and shift
                         (nelisp-eln-registration--fixed-templates shift))))
    (unless
        (and shift
             (nelisp-eln-registration--validate-fixed-section-in-bytes
              bytes ".init" (nth 0 templates))
             (nelisp-eln-registration--validate-fixed-section-in-bytes
              bytes ".plt" (nth 1 templates))
             (nelisp-eln-registration--validate-fixed-section-in-bytes
              bytes ".plt.got" (nth 2 templates))
             (nelisp-eln-registration--validate-fixed-section-in-bytes
              bytes ".fini" nelisp-eln-registration--fini-template)
             (or (null init)
                 (let ((actual (substring bytes (nth 1 text)
                                          (+ (nth 1 text)
                                             (length (nth 3 templates))))))
                   (and (nelisp-eln-registration--match-holed-template
                         actual (nth 3 templates)
                         nelisp-eln-registration--crt-stub-holes)
                        (nelisp-eln-registration--validate-direct-rip-relocs
                         (nth 0 text) actual
                         nelisp-eln-registration--crt-stub-reloc-groups))))
             (nelisp-eln-registration--validate-dynamic-section
              bytes (and init text (nth 0 text))))
      (nelisp-eln-registration--fail 'preopen-executable-region-not-admitted))))

(defun nelisp-eln-registration--validate-fixed-section-in-bytes
    (bytes name template)
  "Like `nelisp-eln-registration--validate-fixed-section', but reads
NAME's bytes directly out of the on-file BYTES string -- no handle, no
live memory, no bias; safe to call before `nl-ffi--dlopen' has ever run."
  (let ((section (nelisp-eln-registration--elf-section-in-bytes bytes name)))
    (or (null section)
        (and (= (nth 2 section) (length template))
             (equal (substring bytes (nth 1 section)
                               (+ (nth 1 section) (nth 2 section)))
                    template)))))

(defun nelisp-eln-registration--raw-read-bytes (handle address length)
  "Read LENGTH bytes at runtime ADDRESS in HANDLE, verified against disk.
Finds the PT_LOAD segment containing ADDRESS itself and requires the live
memory there to equal the on-file bytes at the corresponding offset --
the same invariant `nelisp-eln-system-loader-read-root-function-bytes'
already enforces for symbols it can name; this exists only because CRT
stub symbols have no size that function would accept."
  (let* ((state (nelisp-eln-system-loader--state handle))
         (bias (plist-get state :bias))
         (relative (- address bias))
         (loads (plist-get (plist-get state :elf) :loads))
         (row (catch 'found
                (dolist (r loads)
                  (when (and (/= 0 (logand (nth 4 r)
                                           nelisp-eln-system-loader--pf-x))
                             (>= relative (nth 0 r))
                             (<= (+ (- relative (nth 0 r)) length) (nth 2 r)))
                    (throw 'found r)))
                nil)))
    (unless row
      (nelisp-eln-registration--fail 'raw-read-outside-executable-load
                                     (list address length)))
    (let* ((file-offset (+ (nth 1 row) (- relative (nth 0 row))))
           (memory (ptr-read-bytes address length))
           (disk (substring (plist-get state :file-bytes)
                            file-offset (+ file-offset length))))
      (unless (equal memory disk)
        (nelisp-eln-registration--fail 'loaded-code-does-not-match-file
                                       (list address length)))
      memory)))

(defun nelisp-eln-registration--validate-direct-rip-relocs
    (base actual groups)
  "Validate CRT-stub holes reached directly by `lea'/`cmpb'/`movb' -- no
GOT-style indirection cell in between, unlike top_level_run's own
d_reloc/eph/freloc_link_table loads.  `__TMC_END__' and `completed.0'
are LOCAL linker-internal symbols with no `.dynsym' entry at all (only
`.symtab', which `nelisp-eln-system-loader' never parses -- adding that
would be new, non-trivial ELF parsing this file does not otherwise need),
so unlike every other hole in this file, these cannot be checked against
an authenticated address. What GROUPS validates instead: each of
GROUPS is a list of hole offsets that reference the exact same symbol
\(all four `__TMC_END__' references, or both `completed.0' references);
every computed target address within one group must be identical, and
the groups' targets must differ from each other. A single flipped byte
inside a hole -- or in any of the surrounding fixed bytes, which
`nelisp-eln-registration--match-holed-template' already covers byte-exact
-- breaks that agreement."
  (let ((group-targets nil) (ok t))
    (dolist (group groups)
      (let ((first nil) (agree t))
        (dolist (offset group)
          (let* ((disp (nelisp-eln-registration--read-u32-le actual offset))
                 (signed (if (>= disp #x80000000) (- disp #x100000000) disp))
                 (target (+ base offset 4 signed)))
            (if first
                (unless (= target first) (setq agree nil))
              (setq first target))))
        (unless agree (setq ok nil))
        (push first group-targets)))
    (setq group-targets (nreverse group-targets))
    (and ok
         (let ((rest group-targets) (distinct t))
           (while rest
             (when (member (car rest) (cdr rest)) (setq distinct nil))
             (setq rest (cdr rest)))
           distinct))))

(defun nelisp-eln-registration--validate-fixed-section (handle name template)
  "Validate ELF section NAME against the byte-exact TEMPLATE, if present.
Absent (e.g. a self-emitted fixture's minimal `.text'-only layout) is
admitted: this authenticates real CRT sections, never requires them."
  (let ((section (nelisp-eln-registration--elf-section handle name)))
    (or (null section)
        (let* ((bias (plist-get (nelisp-eln-system-loader--state handle) :bias))
               (address (+ bias (car section)))
               (size (cdr section)))
          (and (= size (length template))
               (equal (nelisp-eln-registration--raw-read-bytes
                       handle address size)
                      template))))))

(defun nelisp-eln-registration--padding-p (bytes)
  "Return non-nil if BYTES is exactly alignment filler: all `int3'
\(0xCC), all single-byte `nop' (0x90), or a concatenation of one or more
canonical multi-byte `nop' encodings covering BYTES exactly (see
`nelisp-eln-registration--padding-decomposes-p') -- GAS emits a single
gap as more than one `nop' instruction whenever no one canonical form
is exactly the gap's own length."
  (let ((n (length bytes)))
    (or (= n 0)
        (let ((all-cc t) (all-90 t) (i 0))
          (while (< i n)
            (let ((b (aref bytes i)))
              (unless (= b #xcc) (setq all-cc nil))
              (unless (= b #x90) (setq all-90 nil)))
            (setq i (1+ i)))
          (or all-cc all-90
              (nelisp-eln-registration--padding-decomposes-p bytes))))))

(defun nelisp-eln-registration--validate-text-padding (handle ranges)
  "Validate every gap between RANGES, and around them within `.text',
is exact alignment filler.  Each of RANGES is (ADDRESS . SIZE) for a
region this preflight (top_level_run, an admitted leaf, or the CRT stub
block) already separately authenticates; anything else in `.text' must
be nothing but padding."
  (let* ((section (nelisp-eln-registration--elf-section handle ".text"))
         (bias (plist-get (nelisp-eln-system-loader--state handle) :bias)))
    (or (null section)
        (let* ((text-start (+ bias (car section)))
               (text-end (+ text-start (cdr section)))
               (sorted (sort (copy-sequence (delq nil ranges))
                             (lambda (a b) (< (car a) (car b)))))
               (cursor text-start) (ok t))
          (dolist (range sorted)
            (when ok
              (let* ((start (car range)) (end (+ start (cdr range))))
                (cond
                 ((< start cursor) (setq ok nil))
                 ((> start cursor)
                  (setq ok (nelisp-eln-registration--padding-p
                            (nelisp-eln-registration--raw-read-bytes
                             handle cursor (- start cursor))))))
                (when ok (setq cursor (max cursor end))))))
          (when (and ok (< cursor text-end))
            (setq ok (nelisp-eln-registration--padding-p
                      (nelisp-eln-registration--raw-read-bytes
                       handle cursor (- text-end cursor)))))
          ok))))

(defun nelisp-eln-registration--validate-executable-regions (handle ranges)
  "Authenticate every executable byte in HANDLE's file: `.init', `.plt',
`.plt.got', `.fini', the CRT stub block, and `.text' padding around
RANGES (the already-otherwise-authenticated admitted functions -- see
the file-level commentary above for what this can and cannot prevent)."
  (unless (and (let* ((file-bytes (plist-get
                                   (nelisp-eln-system-loader--state handle)
                                   :file-bytes))
                      (shift (if (nelisp-eln-registration--elf-section-in-bytes
                                  file-bytes ".got.plt")
                                 (nelisp-eln-registration--layout-shift-in-bytes
                                  file-bytes)
                               0))
                      (templates (and shift
                                      (nelisp-eln-registration--fixed-templates
                                       shift))))
                 (and shift
                      (nelisp-eln-registration--validate-fixed-section
                       handle ".init" (nth 0 templates))
                      (nelisp-eln-registration--validate-fixed-section
                       handle ".plt" (nth 1 templates))
                      (nelisp-eln-registration--validate-fixed-section
                       handle ".plt.got" (nth 2 templates))
                      (nelisp-eln-registration--validate-fixed-section
                       handle ".fini" nelisp-eln-registration--fini-template)))
               (nelisp-eln-registration--validate-crt-stubs handle)
               (nelisp-eln-registration--validate-text-padding
                handle
                (cons (and (nelisp-eln-registration--elf-section
                            handle ".init")
                           (let* ((state (nelisp-eln-system-loader--state
                                          handle))
                                  (text (nelisp-eln-registration--elf-section
                                         handle ".text")))
                             (cons (+ (plist-get state :bias) (car text))
                                   (length
                                    nelisp-eln-registration--crt-stub-template))))
                      ranges)))
    (nelisp-eln-registration--fail 'executable-region-not-admitted)))

;; Install this file's pre-dlopen validator into system-loader.el's own
;; hook (see `nelisp-eln-system-loader-preopen-validator''s docstring for
;; why the call runs there and why that module never `require's this one
;; back to reach it).
(setq nelisp-eln-system-loader-preopen-validator
      #'nelisp-eln-registration--validate-preopen)

(provide 'nelisp-eln-registration)

;;; nelisp-eln-registration.el ends here
