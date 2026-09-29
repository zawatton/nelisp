;;; nelisp-eln-objects.el --- owned GNU object graph views -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This experimental codec owns GNU-layout cons, string, symbol, bignum, and
;; float views across unit and activation leases. Numeric views retain their
;; canonical immutable source identity. Interned symbols are admitted only by
;; an explicit registration-only path; their views expire with that activation.
;; Inside one authenticated S6 native call an interned symbol may also cross
;; as an empty view (its global state is entirely empty) or, when the exact
;; admitted body never reads symbol cells inline, as an opaque view: an
;; identity-only view whose value, function and plist cells hold a poison word
;; that no decoder accepts (see `nelisp-eln-objects--opaque-symbol-cell-word').
;; A vector may likewise cross, inside one authenticated S6 native call whose
;; exact admitted body only passes vectors through (or reads them with
;; authenticated ports), as an opaque view: an identity-preserving 16-byte
;; block whose header and content words hold a poison word no decoder
;; accepts (see `nelisp-eln-objects--admit-opaque-vectors').
;; This is not general symbol-cell synchronization or an .eln loader.

;;; Code:

(require 'nelisp-eln-abi)
(require 'nelisp-eln-string)
(require 'nl-ffi-memory)
(require 'cl-lib)

(declare-function ptr-read-u32 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-read-u8 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u32 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-write-u8 "ext:nelisp-runtime" (ptr offset value))
(declare-function nelisp-eln-bignum-allocate "nelisp-eln-bignum" (source))
(declare-function nelisp-eln-bignum-source "nelisp-eln-bignum" (owner))
(declare-function nelisp-eln-bignum-address "nelisp-eln-bignum" (owner))
(declare-function nelisp-eln-bignum-word "nelisp-eln-bignum" (owner))
(declare-function nelisp-eln-bignum-release "nelisp-eln-bignum" (owner))
(declare-function nelisp-eln-float-allocate "nelisp-eln-float" (source))
(declare-function nelisp-eln-float-source "nelisp-eln-float" (owner))
(declare-function nelisp-eln-float-address "nelisp-eln-float" (owner))
(declare-function nelisp-eln-float-word "nelisp-eln-float" (owner))
(declare-function nelisp-eln-float-release "nelisp-eln-float" (owner))

(define-error 'nelisp-eln-objects-error "Unsupported GNU cons graph")
(define-error 'nelisp-eln-objects-unsupported
  "Unsupported object in GNU cons graph" 'nelisp-eln-objects-error)

(defconst nelisp-eln-objects--marker 'nelisp-eln-objects-unit)
(defconst nelisp-eln-objects--handle-marker 'nelisp-eln-objects-handle)
(defconst nelisp-eln-objects--activation-marker 'nelisp-eln-objects-activation)
(defconst nelisp-eln-objects--symbol-base-lease-marker
  'nelisp-eln-objects-symbol-base-lease)
(defconst nelisp-eln-objects--symbol-view-bytes 48)
(defconst nelisp-eln-objects--symbol-qunbound-offset 48)
(defconst nelisp-eln-objects--symbol-t-word
  (* 2 nelisp-eln-objects--symbol-view-bytes)
  "Wire GNU word this codec uses for Lisp `t'.
Real GNU Emacs 31.1 x86_64 tags a builtin symbol as a signed byte offset
from `lispsym', tag 0 (see `nelisp-eln-abi-gnu-31-1-x86_64' plist key
`:builtin-symbol-word' and its `:tag-map' entry `(symbol . 0)').  A host
GNU Emacs 31.1 build, probed with gdb at a live `Fkill_emacs' breakpoint
so its symbol table is fully initialized, gives `iQnil'=0, `iQt'=1,
`iQunbound'=2, and `sizeof (struct Lisp_Symbol)'=48 (matching
`nelisp-eln-objects--symbol-view-bytes'), so genuine GNU `Qt' is byte
offset 1*48=48 from `lispsym'.  This codec's `--ensure-symbol-base'
already committed that exact word (48) to its own private Qunbound
sentinel before `t' support existed here, so reusing 48 for `t' would make
`t' decoding shadow Qunbound decoding.  `t' therefore takes the next
unused slot in this codec's own tag-0/48-byte-stride private wire space
(96) rather than GNU's literal `t' offset, and — like nil — needs no
`nelisp-eln-objects--symbol-base' lease or heap allocation to encode or
decode: both are pure immediates handled before any base-relative lookup.")
(defconst nelisp-eln-objects--symbol-interned-in-initial-obarray 2)
(defconst nelisp-eln-objects--symbol-nowrite-trapped-write 1
  "GNU 31.1 lisp.h `SYMBOL_NOWRITE', the 2-bit `trapped_write' field at
bits 3-4 of a symbol's first byte (after `gcmarkbit' and the 2-bit
`redirect').  Opaque views carry it so GNU's own `set_internal' would
refuse to write through them.")
(defconst nelisp-eln-objects--opaque-symbol-cell-word 1
  "Poison word stored in an opaque symbol view's value, function and plist
cells.  Its tag 1 is GNU 31.1's `Lisp_Type_Unused0', never a valid Lisp
object, and every decoder here classifies it `unused' and signals
`nelisp-eln-objects-unsupported'.  A native body that read one of these
cells and passed or returned the result therefore fails closed instead
of observing a fabricated value or function.")
;; Opaque vector views are 16 bytes: a header word and one content word, both
;; the poison word.  Unit slot 8 holds their local records
;; [VECTOR ADDRESS GLOBAL LEASED]; the shared record is
;; [VECTOR `vector' ADDRESS OWNER nil UNIT-LEASES ACTIVATION-LEASES STATE nil].
(defconst nelisp-eln-objects--opaque-vector-view-bytes 16)
(defconst nelisp-eln-objects--vector-tag 5)
(defun nelisp-eln-objects--opaque-symbol-header-byte ()
  "Return the first byte of an opaque interned symbol view."
  (logior (lsh nelisp-eln-objects--symbol-interned-in-initial-obarray 5)
          (lsh nelisp-eln-objects--symbol-nowrite-trapped-write 3)))
(defvar nelisp-eln-objects--live-units nil
  "Handle-to-owner entries; this registry is the strong root for unit data.")
(defvar nelisp-eln-objects--identity-records nil
  "Strong identity map of objects with unit or activation leases.")
(defvar nelisp-eln-objects--arenas nil
  "Nonmoving cons arenas shared by all leased object records.")
(defvar nelisp-eln-objects--activations nil
  "Open activation tokens and their record leases.")
(defvar nelisp-eln-objects--registry-state 'open
  "Shared registry state; post-write failures poison all consumers.")
(defvar nelisp-eln-objects--symbol-base nil
  "(MAPPING ADDRESS UNBOUND-NAME-OWNER STATE) for GNU symbol words.")
(defvar nelisp-eln-objects--symbol-base-leases nil
  "Open tokens pinning the shared GNU symbol base independently of units.")
(defvar nelisp-eln-objects--pending-cleanups nil
  "Owners whose failed release must be retried before the registry reopens.")

(defun nelisp-eln-objects--global-record (object)
  (let ((entry (assq object nelisp-eln-objects--identity-records)))
    (cdr entry)))

(defun nelisp-eln-objects--check-registry ()
  (unless (eq nelisp-eln-objects--registry-state 'open)
    (signal 'nelisp-eln-objects-error
            (list "shared GNU object registry is quarantined")))
  t)

(defun nelisp-eln-objects--poison-registry ()
  "Fail closed for all views after a partial shared-view mutation."
  (setq nelisp-eln-objects--registry-state 'poisoned))

(defun nelisp-eln-objects--arena-record (owner)
  (let ((arenas nelisp-eln-objects--arenas) found)
    (while (and arenas (not found))
      (if (eq owner (aref (car arenas) 0))
          (setq found (car arenas))
        (setq arenas (cdr arenas))))
    found))

(defun nelisp-eln-objects--register-global (object kind address owner arena)
  (let ((record (vector object kind address owner arena 0 0 'open)))
    (push (cons object record) nelisp-eln-objects--identity-records)
    record))

(defun nelisp-eln-objects--record-leased-p (record)
  (or (> (aref record 5) 0) (> (aref record 6) 0)))

(defun nelisp-eln-objects--remove-global-record (record)
  (setq nelisp-eln-objects--identity-records
        (assq-delete-all (aref record 0) nelisp-eln-objects--identity-records)))

(defun nelisp-eln-objects--reset-poison-if-empty ()
  (when (and (eq nelisp-eln-objects--registry-state 'poisoned)
             (null nelisp-eln-objects--identity-records)
             (null nelisp-eln-objects--arenas)
             (null nelisp-eln-objects--pending-cleanups)
             (null nelisp-eln-objects--symbol-base)
             (null nelisp-eln-objects--symbol-base-leases)
             (null nelisp-eln-objects--activations))
    (setq nelisp-eln-objects--registry-state 'open)))

(defun nelisp-eln-objects--numeric-module (kind)
  "Load the numeric projection module for KIND and return its prefix."
  (let ((feature (if (eq kind 'bignum)
                     'nelisp-eln-bignum 'nelisp-eln-float)))
    (require feature)
    (if (eq kind 'bignum) "nelisp-eln-bignum" "nelisp-eln-float")))

(defun nelisp-eln-objects--numeric-call (kind operation owner)
  "Call OPERATION in the numeric module associated with KIND on OWNER."
  (let* ((prefix (nelisp-eln-objects--numeric-module kind))
         (function (intern (concat prefix "-" operation)))
         (state-index (and (vectorp owner) 4)))
    (when (and (equal operation "release") state-index
               (< state-index (length owner))
               (eq (aref owner state-index) 'cleanup-pending))
      (let ((retry (intern (concat prefix "-retry-pending-cleanup"))))
        (when (fboundp retry) (funcall retry)))
      (when (eq (aref owner state-index) 'cleanup-pending)
        (signal 'nelisp-eln-objects-error
                (list 'numeric-cleanup-still-pending kind))))
    (funcall function owner)))

(defun nelisp-eln-objects--release-owner (kind owner)
  (if (eq kind 'string)
      (nelisp-eln-string-release owner)
    (if (memq kind '(bignum float))
        (nelisp-eln-objects--numeric-call kind "release" owner)
      (nl-ffi-memory-release owner))))

(defun nelisp-eln-objects--retain-cleanup (kind owner)
  (when owner
    (push (vector kind owner) nelisp-eln-objects--pending-cleanups))
  (nelisp-eln-objects--poison-registry))

(defun nelisp-eln-objects--release-symbol-base ()
  "Release the symbol base in retryable steps after its last symbol lease."
  (when nelisp-eln-objects--symbol-base-leases
    (signal 'nelisp-eln-objects-error
            (list "GNU symbol base is pinned by an explicit lease")))
  (when nelisp-eln-objects--symbol-base
    (let ((base nelisp-eln-objects--symbol-base))
      (aset base 3 'releasing)
      ;; No word may be decoded from this base while either mapping or name
      ;; cleanup is pending.  Clear each owner only after its release succeeds.
      (condition-case failure
          (progn
            (when (aref base 0)
              (nl-ffi-memory-release (aref base 0))
              (aset base 0 nil))
            (when (aref base 2)
              (nelisp-eln-string-release (aref base 2))
              (aset base 2 nil))
            (setq nelisp-eln-objects--symbol-base nil))
        (error
         (nelisp-eln-objects--poison-registry)
         (signal (car failure) (cdr failure))))))
  t)

(defun nelisp-eln-objects--release-unused-symbol-base ()
  "Release the base when no published symbol view can refer to it."
  (when (and nelisp-eln-objects--symbol-base
             (null nelisp-eln-objects--symbol-base-leases)
             (not (cl-some (lambda (entry)
                             (eq (aref (cdr entry) 1) 'symbol))
                           nelisp-eln-objects--identity-records)))
    (nelisp-eln-objects--release-symbol-base)))

(defun nelisp-eln-objects-symbol-base-acquire ()
  "Return a token pinning the shared GNU nil/Qunbound base independently of units."
  (nelisp-eln-objects--check-registry)
  (let* ((base (nelisp-eln-objects--ensure-symbol-base))
         (token (vector nelisp-eln-objects--symbol-base-lease-marker base 'open)))
    (push token nelisp-eln-objects--symbol-base-leases)
    token))

(defun nelisp-eln-objects-symbol-base-release (token)
  "Release TOKEN and drop the shared base if it has no other owners."
  (unless (and (vectorp token) (= (length token) 3)
               (eq (aref token 0) nelisp-eln-objects--symbol-base-lease-marker)
               (eq (aref token 2) 'open)
               (memq token nelisp-eln-objects--symbol-base-leases)
               (eq (aref token 1) nelisp-eln-objects--symbol-base))
    (signal 'nelisp-eln-objects-error
            (list "invalid or released GNU symbol-base lease")))
  (setq nelisp-eln-objects--symbol-base-leases
        (delq token nelisp-eln-objects--symbol-base-leases))
  (aset token 2 'closed)
  (nelisp-eln-objects--release-unused-symbol-base)
  (nelisp-eln-objects--reset-poison-if-empty)
  t)

(defun nelisp-eln-objects-retry-pending-cleanup ()
  "Retry release of owners retained after failed setup or final cleanup."
  (when (featurep 'nelisp-eln-bignum)
    (nelisp-eln-bignum-retry-pending-cleanup))
  (when (featurep 'nelisp-eln-float)
    (nelisp-eln-float-retry-pending-cleanup))
  (let ((remaining nil))
    (dolist (entry (copy-sequence nelisp-eln-objects--pending-cleanups))
      (condition-case nil
          (progn
            (nelisp-eln-objects--release-owner (aref entry 0) (aref entry 1))
            (setq nelisp-eln-objects--pending-cleanups
                  (delq entry nelisp-eln-objects--pending-cleanups)))
        (error (push entry remaining))))
    (setq nelisp-eln-objects--pending-cleanups (nreverse remaining)))
  (dolist (arena (copy-sequence nelisp-eln-objects--arenas))
    (when (and (eq (aref arena 2) 'releasing)
               (= (aref arena 1) 0))
      (nl-ffi-memory-release (aref arena 0))
      (setq nelisp-eln-objects--arenas
            (delq arena nelisp-eln-objects--arenas))))
  (when (and nelisp-eln-objects--symbol-base
             (eq (aref nelisp-eln-objects--symbol-base 3) 'releasing)
             (null nelisp-eln-objects--symbol-base-leases)
             (not (cl-some (lambda (entry)
                             (eq (aref (cdr entry) 1) 'symbol))
                           nelisp-eln-objects--identity-records)))
    (condition-case nil
        (nelisp-eln-objects--release-symbol-base)
      (error nil)))
  (nelisp-eln-objects--reset-poison-if-empty)
  t)

(defun nelisp-eln-objects--finish-global-release (record)
  "Release RECORD storage after its last lease, preserving retryability."
  (when (eq (aref record 7) 'closed)
    (signal 'nelisp-eln-objects-error (list "object view was already released")))
  (unless (nelisp-eln-objects--record-leased-p record)
    (aset record 7 'releasing)
    (cond
     ((eq (aref record 1) 'string)
      (when (aref record 3)
        (nelisp-eln-string-release (aref record 3))
        (aset record 3 nil)))
     ((eq (aref record 1) 'symbol)
      (when (aref record 3)
        (nl-ffi-memory-release (aref record 3))
        (aset record 3 nil)))
     ((eq (aref record 1) 'vector)
      (when (aref record 3)
        (nl-ffi-memory-release (aref record 3))
        (aset record 3 nil)))
     ((memq (aref record 1) '(bignum float))
      (when (aref record 3)
        (condition-case failure
            (nelisp-eln-objects--numeric-call
             (aref record 1) "release" (aref record 3))
          (error
           (nelisp-eln-objects--poison-registry)
           (signal (car failure) (cdr failure))))
        (aset record 3 nil)))
     (t
      (let* ((arena (aref record 4))
             (remaining (1- (aref arena 1))))
        (unless (>= remaining 0)
          (signal 'nelisp-eln-objects-error (list "cons arena lease underflow")))
        (if (> remaining 0)
            (aset arena 1 remaining)
          (nl-ffi-memory-release (aref arena 0))
          (setq nelisp-eln-objects--arenas
                (delq arena nelisp-eln-objects--arenas))))))
    (nelisp-eln-objects--remove-global-record record)
    (when (and (eq (aref record 1) 'symbol)
               (null nelisp-eln-objects--symbol-base-leases)
               (not (cl-some (lambda (entry)
                               (eq (aref (cdr entry) 1) 'symbol))
                             nelisp-eln-objects--identity-records))
               nelisp-eln-objects--symbol-base)
      (nelisp-eln-objects--release-symbol-base))
    (aset record 7 'closed)
    (nelisp-eln-objects--reset-poison-if-empty))
  t)

(defun nelisp-eln-objects--resolve (handle)
  (unless (and (vectorp handle) (= (length handle) 2)
               (eq (aref handle 0) nelisp-eln-objects--handle-marker))
    (signal 'nelisp-eln-objects-error (list "invalid unit handle")))
  (let ((entry (assq handle nelisp-eln-objects--live-units)))
    (unless entry
      (signal 'nelisp-eln-objects-error (list "stale or released unit handle")))
    (cdr entry)))

(defun nelisp-eln-objects-create ()
  "Create an empty owner for GNU-layout object views."
  (let ((handle (vector nelisp-eln-objects--handle-marker nil))
        (unit (vector nelisp-eln-objects--marker 'open nil nil nil nil nil nil nil nil)))
    (push (cons handle unit) nelisp-eln-objects--live-units)
    handle))

(defun nelisp-eln-objects--check-unit (handle)
  (let ((unit (nelisp-eln-objects--resolve handle)))
   (nelisp-eln-objects--check-registry)
   (unless (eq (aref unit 1) 'open)
     (signal 'nelisp-eln-objects-error (list "closed or invalid unit")))
   unit))

(defun nelisp-eln-objects--preflight (roots &optional unit)
  "Return a plan of supported reachable objects in ROOTS."
  (let ((pending roots) (seen-conses nil) (seen-strings nil) (seen-symbols nil)
        (seen-numerics nil) (conses nil) (strings nil) (string-plans nil)
        (symbols nil) (numerics nil) (seen-vectors nil) (vectors nil))
    (while pending
      (let ((value (pop pending)))
        (cond
         ((null value) nil)
         ((eq value t) nil)
         ((and (integerp value)
               (<= nelisp-eln-abi-fixnum-min value)
               (<= value nelisp-eln-abi-fixnum-max)) nil)
         ((integerp value)
          (unless (memq value seen-numerics)
            (push value seen-numerics)
            (push value numerics)
            (let ((record (and unit
                               (nelisp-eln-objects--numeric-record unit value))))
              (when record
                (unless (and (eq (nelisp-eln-objects--numeric-call
                                  'bignum "source"
                                  (aref (aref record 1) 3)) value)
                             (= (nelisp-eln-objects--numeric-call
                                 'bignum "address"
                                 (aref (aref record 1) 3))
                                (aref (aref record 1) 2)))
                  (signal 'nelisp-eln-objects-error
                          (list "bignum owner changed" value)))))))
         ((floatp value)
          (unless (memq value seen-numerics)
            (push value seen-numerics)
            (push value numerics)
            (let ((record (and unit
                               (nelisp-eln-objects--numeric-record unit value))))
              (when record
                (unless (and (eq (nelisp-eln-objects--numeric-call
                                  'float "source"
                                  (aref (aref record 1) 3)) value)
                             (= (nelisp-eln-objects--numeric-call
                                 'float "address"
                                 (aref (aref record 1) 3))
                                (aref (aref record 1) 2)))
                  (signal 'nelisp-eln-objects-error
                          (list "float owner changed" value)))))))
         ((stringp value)
          (unless (memq value seen-strings)
            (push value seen-strings)
            (push value strings)
            (push (nelisp-eln-string-prepare value) string-plans))
          (let ((record (and unit
                             (nelisp-eln-objects--string-record unit value))))
            (when record
              (unless (= (nelisp-eln-string-address (aref record 1))
                         (aref record 2))
                (signal 'nelisp-eln-objects-error
                        (list "string owner address changed" value))))))
         ((and (symbolp value)
               (assq value nelisp-eln-objects--constant-symbol-words))
          nil)
         ((and (symbolp value)
               (not (nelisp-eln-objects--unbound-sentinel-p value)))
          (unless (memq value seen-symbols)
            (when (and unit (nelisp-eln-objects--registration-only-symbol-p
                             unit value))
              (signal 'nelisp-eln-objects-unsupported
                      (list 'registration-symbol-outside-activation value)))
            (unless (or (nelisp-eln-objects--fresh-symbol-p value unit)
                        (nelisp-eln-objects--empty-interned-symbol-p value)
                        (nelisp-eln-objects--opaque-interned-symbol-p value))
              (signal 'nelisp-eln-objects-unsupported
                      (list 'unsupported-symbol-state value)))
            (push value seen-symbols)
            (push value symbols)
            (push (symbol-name value) pending)))
         ((consp value)
          (unless (memq value seen-conses)
            (push value seen-conses)
            (push value conses)
            (push (car value) pending)
            (push (cdr value) pending)))
         ;; An opaque vector is never descended into: its contents stay
         ;; invisible to native code (see `--admit-opaque-vectors').
         ((and (vectorp value) nelisp-eln-objects--admit-opaque-vectors)
          (unless (memq value seen-vectors)
            (push value seen-vectors)
            (push value vectors)))
         (t (signal 'nelisp-eln-objects-unsupported (list value))))))
    (vector (nreverse conses) (nreverse strings) (nreverse string-plans)
            (nreverse symbols) (nreverse numerics) (nreverse vectors))))

(defun nelisp-eln-objects--global-cell-free-p (symbol)
  "Return non-nil only when SYMBOL has no global mirror variable cell.
The query is required because `defvaralias' is represented in the runtime's
name-keyed mirror, outside the symbol Sexp fields."
  (unless (fboundp 'nelisp--symbol-global-cell-p)
    (signal 'nelisp-eln-objects-unsupported
            (list 'symbol-global-cell-query-unavailable symbol)))
  (let ((result (nelisp--symbol-global-cell-p symbol)))
    (cond ((null result) t)
          ((eq result t) nil)
          (t (signal 'nelisp-eln-objects-unsupported
                     (list 'invalid-symbol-global-cell-query-result
                           symbol result))))))

(defun nelisp-eln-objects--fresh-symbol-p (symbol &optional unit)
  "Return non-nil for the constructor-default uninterned subset only.
The runtime tracks special declarations separately from Sexp symbol fields,
so reject those. Its alias path is name-keyed mirror state rather than a field
on this object; this view layer does not claim to serialize alias, localized
value, or watcher state. Bound/function/plist mutations are rejected rather
than guessed into GNU's fields."
  (and (symbolp symbol)
       (not (nelisp-eln-objects--unbound-sentinel-p symbol))
       (or (let ((record (nelisp-eln-objects--global-record symbol)))
           (and record (eq (aref record 1) 'symbol)
                (eq (aref record 7) 'open)
                (not (aref record 8))))
           (and unit (memq symbol (aref unit 7))))
       (not (eq (intern-soft (symbol-name symbol)) symbol))
       (nelisp-eln-objects--global-cell-free-p symbol)
       (fboundp 'special-variable-p)
       (not (special-variable-p symbol))
       (not (boundp symbol))
       (not (fboundp symbol))
       (null (symbol-plist symbol))))

(defvar nelisp-eln-objects--constant-symbol-words nil
  "Alist of (SYMBOL . GNU-WORD) for authenticated artifact symbol constants.
Set only around one S6 multi-import native call (see
`nelisp-eln-native-subr-create-multi'): each GNU-WORD is the live
`d_reloc' word the artifact's own metadata capability wrote for SYMBOL,
whose identity was authenticated against the artifact's data
relocations.  While set, encoding SYMBOL yields exactly that word, so the
native body's word comparisons against its constant see the same object
GNU would; no view is created for SYMBOL.")

(defvar nelisp-eln-objects--admit-empty-interned-symbols nil
  "Non-nil admits interned symbols with entirely empty global state.
Set only around one S6 multi-import native call.  Such a symbol gets an
ordinary view carrying GNU's interned-in-initial-obarray bit and empty
cells, which is faithful precisely because
`nelisp-eln-objects--registration-symbol-p' holds (no value, function,
plist, alias/mirror or special state); decoding its word returns the
same interned symbol.  Any other interned symbol still fails closed.")

(defvar nelisp-eln-objects--admit-opaque-interned-symbols nil
  "Non-nil admits any other interned symbol as an opaque view.
Set only around one S6 multi-import native call whose exact admitted
body is declared never to read symbol cells inline (spec key
:OPAQUE-ARGUMENT-SYMBOLS in `nelisp-eln-native-subr--multi-import-specs').
An opaque view preserves identity -- its word decodes back to the same
interned symbol, so authenticated ports receive the genuine NeLisp
symbol and act on its genuine state -- but it never mirrors that state:
its value, function and plist cells hold
`nelisp-eln-objects--opaque-symbol-cell-word', which no decoder accepts,
and its `trapped_write' is `SYMBOL_NOWRITE'.  The cells are checked
unchanged at every native boundary.  Symbols with entirely empty state
still get ordinary empty views.")

(defvar nelisp-eln-objects--admit-opaque-vectors nil
  "Non-nil admits vectors as opaque identity-only views.
Set only around one S6 multi-import native call whose exact admitted body is
declared (spec key :OPAQUE-VECTORS in
`nelisp-eln-native-subr--multi-import-specs') to only pass vectors on to
authenticated ports.  A vector's word decodes back to the same vector, but
the 16-byte view holds only `nelisp-eln-objects--opaque-symbol-cell-word' in
its header and content words; contents are never mirrored and the words are
checked unchanged at every native boundary.  Without this a vector fails
closed as unsupported.")

(defun nelisp-eln-objects-call-with-artifact-symbols
    (symbol-words function &optional opaque-symbols opaque-vectors)
  "Call FUNCTION with artifact symbol constants and empty interned views.
SYMBOL-WORDS is an alist (SYMBOL . GNU-WORD) of authenticated artifact
constants (see `nelisp-eln-objects--constant-symbol-words'); while
FUNCTION runs, encoding each SYMBOL yields its word and interned symbols
with entirely empty global state may get views (see
`nelisp-eln-objects--admit-empty-interned-symbols').  OPAQUE-SYMBOLS
non-nil also admits any other interned symbol as an opaque view (see
`nelisp-eln-objects--admit-opaque-interned-symbols').  OPAQUE-VECTORS
non-nil admits vectors as opaque views (see
`nelisp-eln-objects--admit-opaque-vectors').  All are restored
on any exit."
  (unless (and (listp symbol-words)
               (cl-every (lambda (entry)
                           (and (consp entry) (symbolp (car entry))
                                (car entry) (not (eq (car entry) t))
                                (integerp (cdr entry))))
                         symbol-words))
    (signal 'nelisp-eln-objects-unsupported
            (list 'invalid-artifact-symbol-words symbol-words)))
  (let ((nelisp-eln-objects--constant-symbol-words symbol-words)
        (nelisp-eln-objects--admit-empty-interned-symbols t)
        (nelisp-eln-objects--admit-opaque-interned-symbols
         (and opaque-symbols t))
        (nelisp-eln-objects--admit-opaque-vectors (and opaque-vectors t)))
    (funcall function)))

(defvar nelisp-eln-objects--isolated-registration-symbols nil
  "Registration symbols whose function cell and plist live in an isolated
namespace rather than in the global symbol, while
`nelisp-eln-objects-admit-registration-symbols' runs with ISOLATED.
Never set this directly; it is bound only by that function.")

(defun nelisp-eln-objects--isolated-registration-symbol-p (symbol)
  "Return non-nil for an isolated-namespace registration SYMBOL.
The GNU view written for a registration symbol always carries empty value,
function and plist cells (see `nelisp-eln-objects--add-symbols').  For an
isolated registration the function cell and plist that view stands for are
the isolated namespace's own -- verified empty by the registration loader
before admission -- so the global function cell, plist and the name-keyed
mirror entry (which every prelude-defined function owns) are deliberately
not consulted here.  The variable cell is still global, so it must still be
empty: unbound and not special.  Registration views never pass through
ordinary `encode' or graph synchronization, so the snapshot is never
written back through the global symbol."
  (and (symbolp symbol)
       (memq symbol nelisp-eln-objects--isolated-registration-symbols)
       (not (nelisp-eln-objects--unbound-sentinel-p symbol))
       (eq (intern-soft (symbol-name symbol)) symbol)
       (fboundp 'special-variable-p)
       (not (special-variable-p symbol))
       (not (boundp symbol))))

(defun nelisp-eln-objects--registration-symbol-p (symbol)
  "Return non-nil only for a default-obarray symbol with empty global cells.
While an isolated admission is in progress, SYMBOLs it names are judged by
`nelisp-eln-objects--isolated-registration-symbol-p' instead."
  (if (memq symbol nelisp-eln-objects--isolated-registration-symbols)
      (nelisp-eln-objects--isolated-registration-symbol-p symbol)
    (and (symbolp symbol)
         (not (nelisp-eln-objects--unbound-sentinel-p symbol))
         (eq (intern-soft (symbol-name symbol)) symbol)
         (nelisp-eln-objects--global-cell-free-p symbol)
         (not (and (fboundp 'special-variable-p) (special-variable-p symbol)))
         (not (boundp symbol))
         (not (fboundp symbol))
         (null (symbol-plist symbol)))))

(defun nelisp-eln-objects--empty-interned-symbol-p (symbol)
  "Non-nil when SYMBOL may get an ordinary empty interned view now.
See `nelisp-eln-objects--admit-empty-interned-symbols'."
  (and nelisp-eln-objects--admit-empty-interned-symbols
       (symbolp symbol)
       (not (memq symbol nelisp-eln-objects--isolated-registration-symbols))
       (nelisp-eln-objects--registration-symbol-p symbol)))

(defun nelisp-eln-objects--opaque-interned-symbol-p (symbol)
  "Non-nil when SYMBOL may get an opaque interned view now.
See `nelisp-eln-objects--admit-opaque-interned-symbols'."
  (and nelisp-eln-objects--admit-opaque-interned-symbols
       (symbolp symbol)
       symbol (not (eq symbol t))
       (not (nelisp-eln-objects--unbound-sentinel-p symbol))
       (not (memq symbol nelisp-eln-objects--isolated-registration-symbols))
       (eq (intern-soft (symbol-name symbol)) symbol)))

(defun nelisp-eln-objects--opaque-view-p (local)
  "Non-nil when symbol unit record LOCAL leases an opaque view."
  (let ((global (aref local 2)))
    (and (vectorp global) (eq (aref global 4) 'opaque))))

(defun nelisp-eln-objects--registration-only-symbol-p (unit symbol)
  (let ((local (nelisp-eln-objects--symbol-record unit symbol)))
    (and local (aref local 4))))

(defun nelisp-eln-objects--unbound-sentinel-p (value)
  (and (boundp 'nelisp--unbound-marker)
       (eq value nelisp--unbound-marker)))

(defun nelisp-eln-objects-make-uninterned-symbol (handle name)
  "Create a fresh symbol NAME and return (SYMBOL . GNU-WORD) for HANDLE.
This explicit constructor is the only admission path for previously unseen
symbols. The supported view is uninterned, unbound, fmakunbound, and has nil
plist; later cell changes make synchronization fail closed."
  (let* ((unit (nelisp-eln-objects--check-unit handle))
         (symbol (make-symbol name)))
    (aset unit 7 (cons symbol (aref unit 7)))
    (unwind-protect
        (cons symbol (nelisp-eln-objects-encode handle symbol))
      (aset unit 7 (delq symbol (aref unit 7))))))

(defun nelisp-eln-objects--record (unit object)
  (let ((records (aref unit 3)))
    ;; `car' of `car', not `caar': the codec must not call a function an
    ;; S6 artifact it is marshalling for may itself register.
    (while (and records (not (eq object (car (car records)))))
      (setq records (cdr records)))
    (car records)))

(defun nelisp-eln-objects--string-record (unit string &optional records)
  (let ((records (or records (aref unit 5))))
    (while (and records (not (eq string (aref (car records) 0))))
      (setq records (cdr records)))
    (car records)))

(defun nelisp-eln-objects--symbol-record (unit symbol &optional records)
  (let ((records (or records (aref unit 6))))
    (while (and records (not (eq symbol (aref (car records) 0))))
      (setq records (cdr records)))
    (car records)))

(defun nelisp-eln-objects--numeric-record (unit number &optional records)
  (let ((records (or records (aref unit 9))))
    (while (and records (not (eq number (aref (car records) 0))))
      (setq records (cdr records)))
    (car records)))

(defun nelisp-eln-objects--vector-record (unit vector-object)
  (let ((records (aref unit 8)))
    (while (and records (not (eq vector-object (aref (car records) 0))))
      (setq records (cdr records)))
    (car records)))

(defun nelisp-eln-objects--remove-unit-string-record (unit record)
  (aset unit 5 (delq record (aref unit 5))))

(defun nelisp-eln-objects--remove-unit-cons-record (unit record)
  (aset unit 3 (delq record (aref unit 3))))

(defun nelisp-eln-objects--remove-unit-symbol-record (unit record)
  (aset unit 6 (delq record (aref unit 6))))

(defun nelisp-eln-objects--remove-unit-numeric-record (unit record)
  (aset unit 9 (delq record (aref unit 9))))

(defun nelisp-eln-objects--numeric-kind (number)
  (cond ((and (integerp number)
              (or (< number nelisp-eln-abi-fixnum-min)
                  (> number nelisp-eln-abi-fixnum-max)))
         'bignum)
        ((floatp number) 'float)
        (t nil)))

(defun nelisp-eln-objects--numeric-word (unit number kind)
  (let* ((local (nelisp-eln-objects--numeric-record unit number))
         (shared (and local (aref local 1))))
    (unless (and shared (eq (aref shared 1) kind))
      (signal 'nelisp-eln-objects-error
              (list 'numeric-object-missing-from-preflight kind number)))
    (nelisp-eln-objects--numeric-call kind "word" (aref shared 3))))

(defun nelisp-eln-objects--decode-numeric (unit word kind)
  "Decode registered numeric WORD of KIND through UNIT's immutable view."
  (let* ((bits (nelisp-eln-abi-normalize-word word))
         (tag (if (eq kind 'bignum) 5 7))
         (address (- bits tag))
         (records (aref unit 9)) found)
    (while (and records (not found))
      (let* ((local (car records))
             (shared (aref local 1)))
        (when (and (eq (aref shared 1) kind)
                   (= (aref shared 2) address))
          (unless (and (eq (nelisp-eln-objects--numeric-call
                            kind "source" (aref shared 3))
                           (aref shared 0))
                       (= (nelisp-eln-objects--numeric-call
                           kind "address" (aref shared 3)) address))
            (signal 'nelisp-eln-objects-error
                    (list 'numeric-owner-changed kind address)))
          (setq found shared)))
      (setq records (cdr records)))
    (unless found
      (signal 'nelisp-eln-objects-error
              (list 'foreign-or-unknown-numeric-word kind address)))
    (aref found 0)))

(defun nelisp-eln-objects--add-numerics (unit numerics)
  "Lease immutable NUMERICS in UNIT, sharing identity records across units."
  (let ((new-records nil) (complete nil))
    (unwind-protect
        (progn
          (dolist (number numerics)
            (unless (nelisp-eln-objects--numeric-record unit number)
              (let* ((kind (nelisp-eln-objects--numeric-kind number))
                     (shared (nelisp-eln-objects--global-record number))
                     (owner nil) (created nil))
                (condition-case failure
                    (progn
                      (unless kind
                        (signal 'nelisp-eln-objects-error
                                (list 'unsupported-numeric-object number)))
                      (if shared
                          (progn
                            (unless (and (eq (aref shared 1) kind)
                                         (eq (aref shared 7) 'open))
                              (signal 'nelisp-eln-objects-error
                                      (list 'numeric-view-releasing number)))
                            (unless (and
                                     (eq (nelisp-eln-objects--numeric-call
                                          kind "source" (aref shared 3)) number)
                                     (= (nelisp-eln-objects--numeric-call
                                         kind "address" (aref shared 3))
                                        (aref shared 2)))
                              (signal 'nelisp-eln-objects-error
                                      (list 'numeric-owner-changed number))))
                        (setq owner (nelisp-eln-objects--numeric-call
                                     kind "allocate" number))
                        (let ((address (nelisp-eln-objects--numeric-call
                                        kind "address" owner)))
                          (setq shared
                                (nelisp-eln-objects--register-global
                                 number kind address owner nil)
                                created t)))
                      (let ((local (vector number shared t)))
                      (aset shared 5 (1+ (aref shared 5)))
                        (push local new-records)))
                  (error
                   (when (and owner created)
                     (nelisp-eln-objects--remove-global-record shared))
                   (when owner
                     (condition-case nil
                         (nelisp-eln-objects--numeric-call kind "release" owner)
                       (error (nelisp-eln-objects--poison-registry))))
                   (signal (car failure) (cdr failure)))))))
          (setq new-records (nreverse new-records))
          (aset unit 9 (append new-records (aref unit 9)))
          (setq complete t)
          new-records)
      (unless complete
        (let ((remaining nil))
          (dolist (record new-records)
            (condition-case nil
                (nelisp-eln-objects--drop-unit-lease
                 unit (aref (aref record 1) 1) record)
              (error (push record remaining))))
          (when remaining
            (aset unit 9 (append (nreverse remaining) (aref unit 9)))
            (aset unit 1 'releasing)))))))

(defun nelisp-eln-objects--drop-unit-lease (unit kind local)
  "Drop UNIT's lease for LOCAL and remove its local whitelist entry."
  (let ((shared (cond ((eq kind 'string) (aref local 3))
                      ((memq kind '(symbol vector)) (aref local 2))
                      ((memq kind '(bignum float)) (aref local 1))
                      (t (nth 2 local))))
        (leased (cond ((memq kind '(string symbol vector))
                       (aref local (if (eq kind 'string) 4 3)))
                      ((memq kind '(bignum float)) (aref local 2))
                      (t (nth 3 local)))))
    (unless shared
      (signal 'nelisp-eln-objects-error (list "unit record lacks shared identity")))
    (when leased
      (aset shared 5 (1- (aref shared 5)))
      (if (memq kind '(string symbol vector bignum float))
          (aset local (cond ((eq kind 'string) 4)
                            ((memq kind '(symbol vector)) 3)
                            (t 2)) nil)
        (setcar (nthcdr 3 local) nil)))
    (nelisp-eln-objects--finish-global-release shared)
    (cond ((eq kind 'string)
           (nelisp-eln-objects--remove-unit-string-record unit local))
          ((eq kind 'symbol)
           (nelisp-eln-objects--remove-unit-symbol-record unit local))
          ((memq kind '(bignum float))
           (nelisp-eln-objects--remove-unit-numeric-record unit local))
          ((eq kind 'vector)
           (aset unit 8 (delq local (aref unit 8))))
          (t (nelisp-eln-objects--remove-unit-cons-record unit local)))))

(defun nelisp-eln-objects--encode-word (unit value)
  (cond
   ((null value) (nelisp-eln-abi-encode-nil))
   ((eq value t) nelisp-eln-objects--symbol-t-word)
   ((and (symbolp value)
         (assq value nelisp-eln-objects--constant-symbol-words))
    (cdr (assq value nelisp-eln-objects--constant-symbol-words)))
   ((nelisp-eln-objects--unbound-sentinel-p value)
    (unless (and nelisp-eln-objects--symbol-base (aref unit 6))
      (signal 'nelisp-eln-objects-error (list "Qunbound view is not leased")))
    nelisp-eln-objects--symbol-qunbound-offset)
   ((and (integerp value)
         (<= nelisp-eln-abi-fixnum-min value nelisp-eln-abi-fixnum-max))
    (nelisp-eln-abi-encode-fixnum value))
   ((integerp value)
    (nelisp-eln-objects--numeric-word unit value 'bignum))
   ((floatp value)
    (nelisp-eln-objects--numeric-word unit value 'float))
   ((stringp value)
    (let ((record (nelisp-eln-objects--string-record unit value)))
      (unless record
        (signal 'nelisp-eln-objects-error (list "string missing from preflight")))
      (+ (aref record 2) 4)))
   ((consp value)
    (let ((record (nelisp-eln-objects--record unit value)))
      (unless record
        (signal 'nelisp-eln-objects-error (list "cons missing from preflight")))
      (+ (nth 1 record) 3)))
   ((symbolp value)
    (let ((record (nelisp-eln-objects--symbol-record unit value)))
      (unless record
        (signal 'nelisp-eln-objects-error (list "symbol missing from preflight")))
      (nelisp-eln-objects--symbol-word (aref record 1))))
   ((vectorp value)
    (let ((record (nelisp-eln-objects--vector-record unit value)))
      (unless record
        (signal 'nelisp-eln-objects-error (list "vector missing from preflight")))
      (+ (aref record 1) nelisp-eln-objects--vector-tag)))
   (t (signal 'nelisp-eln-objects-unsupported (list value)))))

(defun nelisp-eln-objects--add-strings (unit strings plans)
  "Stage identity records for STRINGS using prevalidated PLANS."
  (let ((new-records nil) (complete nil))
    (unwind-protect
        (progn
          (while strings
            (unless (nelisp-eln-objects--string-record unit (car strings))
              (let* ((object (car strings))
                     (shared (nelisp-eln-objects--global-record object))
                     (local (vector object nil nil shared t)))
                (if shared
                    (progn
                      (unless (eq (aref shared 7) 'open)
                        (signal 'nelisp-eln-objects-error
                                (list "string view is releasing" object)))
                      (aset local 1 (aref shared 3))
                      (aset local 2 (aref shared 2))
                      (aset shared 5 (1+ (aref shared 5))))
                  (let ((owner (nelisp-eln-string-allocate-prepared (car plans))))
                    (aset local 1 owner)
                    (aset local 2 (nelisp-eln-objects--register-global
                                   object 'string
                                   (nelisp-eln-string-address owner) owner nil))
                    (let ((global (aref local 2)))
                      (aset global 5 1)
                      (aset local 2 (aref global 2))
                      (aset local 3 global))))
                (push local new-records)))
            (setq strings (cdr strings) plans (cdr plans)))
          (setq new-records (nreverse new-records))
          (aset unit 5 (append new-records (aref unit 5)))
          (setq complete t)
          new-records)
      (unless complete
        (let ((retained nil))
          (dolist (record new-records)
            (condition-case nil
                (nelisp-eln-objects--drop-unit-lease unit 'string record)
              (error (push record retained))))
          (when retained
            (aset unit 5 (append (nreverse retained) (aref unit 5)))
            (aset unit 1 'releasing)))))))

(defun nelisp-eln-objects--add-symbols (unit symbols &optional registration-only)
  "Lease SYMBOLS after validating constructor-default or registration state."
  (let ((new-records nil) (complete nil))
    (when symbols (nelisp-eln-objects--ensure-symbol-base))
    (unwind-protect
        (progn
          (dolist (symbol symbols)
            (let ((local (nelisp-eln-objects--symbol-record unit symbol)))
              (when (and local
                         (not (eq (aref local 4) (and registration-only t))))
                (signal 'nelisp-eln-objects-unsupported
                        (list 'symbol-view-mode-conflict symbol)))
              (unless local
               (let* ((shared (nelisp-eln-objects--global-record symbol))
                      ;; An existing view keeps its mode; a new one is
                      ;; opaque only for a symbol no faithful view admits.
                      (opaque
                       (and (not registration-only)
                            (if shared
                                (eq (aref shared 4) 'opaque)
                              (not (or (nelisp-eln-objects--fresh-symbol-p
                                        symbol unit)
                                       (nelisp-eln-objects--empty-interned-symbol-p
                                        symbol)))))))
                (unless (cond
                         (registration-only
                          (nelisp-eln-objects--registration-symbol-p symbol))
                         (opaque
                          (nelisp-eln-objects--opaque-interned-symbol-p symbol))
                         (t (or (nelisp-eln-objects--fresh-symbol-p symbol unit)
                                (nelisp-eln-objects--empty-interned-symbol-p
                                 symbol))))
                (signal 'nelisp-eln-objects-unsupported
                        (list 'symbol-state-changed symbol)))
                (if shared
                    (progn
                      (unless (and (eq (aref shared 1) 'symbol)
                                   (eq (aref shared 7) 'open)
                                   (eq (aref shared 4) (and opaque 'opaque))
                                   (eq (aref shared 8)
                                       (and registration-only t)))
                        (signal 'nelisp-eln-objects-error
                                (list "symbol view is releasing" symbol)))
                      (aset shared 5 (1+ (aref shared 5)))
                      (push (vector symbol (aref shared 2) shared t
                                    (and registration-only t))
                            new-records))
                  (let ((owner (nelisp-eln-objects--allocate-view-memory 48))
                        (published nil))
                    (unwind-protect
                        (let* ((address (nl-ffi-memory-address owner))
                               (name (symbol-name symbol))
                               (name-record
                                (nelisp-eln-objects--string-record unit name))
                               (global (vector symbol 'symbol address owner
                                               (and opaque 'opaque)
                                               1 0 'open
                                               (and registration-only t)))
                               (local (vector symbol address global t
                                              (and registration-only t))))
                          (unless name-record
                            (signal 'nelisp-eln-objects-error
                                    (list "symbol name string missing from preflight")))
                          ;; GNU 31.1 lread.c sets enum value 2 for symbols
                          ;; interned in the initial obarray. Other cells stay
                          ;; restricted to the validated empty snapshot.
                          ;; An opaque view mirrors only the name: its value,
                          ;; function and plist cells hold the poison word and
                          ;; it is `SYMBOL_NOWRITE'.
                          (ptr-write-u8
                           address 0
                           (cond
                            (opaque
                             (nelisp-eln-objects--opaque-symbol-header-byte))
                            ((or registration-only
                                 (eq (intern-soft name) symbol))
                             (lsh nelisp-eln-objects--symbol-interned-in-initial-obarray 5))
                            (t 0)))
                          (nelisp-eln-objects--write-word
                           address 8 (+ (aref name-record 2) 4))
                          (let ((poison nelisp-eln-objects--opaque-symbol-cell-word))
                            (nelisp-eln-objects--write-word
                             address 16 (if opaque poison 48))
                            (nelisp-eln-objects--write-word
                             address 24 (if opaque poison 0))
                            (nelisp-eln-objects--write-word
                             address 32 (if opaque poison 0)))
                          (nelisp-eln-objects--write-word address 40 0)
                          (push (cons symbol global)
                                nelisp-eln-objects--identity-records)
                          (setq published t)
                          (push local new-records))
                      (unless published
                        (condition-case nil
                            (nl-ffi-memory-release owner)
                          (error
                           (nelisp-eln-objects--retain-cleanup 'memory owner)))))))))))
          (setq new-records (nreverse new-records))
          (aset unit 6 (append new-records (aref unit 6)))
          (setq complete t)
          new-records)
      (unless complete
        (dolist (local new-records)
          (condition-case nil
              (nelisp-eln-objects--drop-unit-lease unit 'symbol local)
            (error (aset unit 1 'releasing))))
        (condition-case nil
            (nelisp-eln-objects--release-unused-symbol-base)
          (error (nelisp-eln-objects--poison-registry)))))))

;; The views themselves are only ever the poison block; see
;; `nelisp-eln-objects--admit-opaque-vectors'.
(defun nelisp-eln-objects--add-vectors (unit vectors)
  "Lease opaque views for VECTORS in UNIT and return the new local records."
  (let ((new-records nil) (complete nil))
    (unwind-protect
        (progn
          (dolist (object vectors)
            (unless (nelisp-eln-objects--vector-record unit object)
              (let ((shared (nelisp-eln-objects--global-record object)))
                (if shared
                    (progn
                      (unless (and (eq (aref shared 1) 'vector)
                                   (eq (aref shared 7) 'open))
                        (signal 'nelisp-eln-objects-error
                                (list "vector view is releasing" object)))
                      (aset shared 5 (1+ (aref shared 5)))
                      (push (vector object (aref shared 2) shared t)
                            new-records))
                  (let ((owner (nelisp-eln-objects--allocate-view-memory
                                nelisp-eln-objects--opaque-vector-view-bytes))
                        (published nil))
                    (unwind-protect
                        (let* ((address (nl-ffi-memory-address owner))
                               (global (vector object 'vector address owner nil
                                               1 0 'open nil))
                               (local (vector object address global t))
                               (poison nelisp-eln-objects--opaque-symbol-cell-word))
                          (nelisp-eln-objects--write-word address 0 poison)
                          (nelisp-eln-objects--write-word address 8 poison)
                          (push (cons object global)
                                nelisp-eln-objects--identity-records)
                          (setq published t)
                          (push local new-records))
                      (unless published
                        (condition-case nil
                            (nl-ffi-memory-release owner)
                          (error
                           (nelisp-eln-objects--retain-cleanup
                            'memory owner))))))))))
          (setq new-records (nreverse new-records))
          (aset unit 8 (append new-records (aref unit 8)))
          (setq complete t)
          new-records)
      (unless complete
        (dolist (local new-records)
          (condition-case nil
              (nelisp-eln-objects--drop-unit-lease unit 'vector local)
            (error (aset unit 1 'releasing))))))))

(defun nelisp-eln-objects--rollback-vectors (unit records)
  "Undo vector leases in RECORDS after a pre-write failure."
  (let ((remaining nil))
    (dolist (record records)
      (condition-case nil
          (nelisp-eln-objects--drop-unit-lease unit 'vector record)
        (error (push record remaining))))
    (when remaining
      (aset unit 8 (append (nreverse remaining) (aref unit 8)))
      (aset unit 1 'releasing))))

(defun nelisp-eln-objects--check-vector-view (_unit local)
  "Reject native edits to an opaque vector view's poison words."
  (let ((address (aref local 1))
        (poison nelisp-eln-objects--opaque-symbol-cell-word))
    (unless (and (= (nelisp-eln-objects--read-word address 0) poison)
                 (= (nelisp-eln-objects--read-word address 8) poison))
      (signal 'nelisp-eln-objects-unsupported
              (list 'native-vector-cell-mutation (aref local 0))))
    t))

(defun nelisp-eln-objects-admit-registration-symbols (handle symbols
                                                            &optional isolated)
  "Return (SYMBOL . GNU-WORD) entries for restricted registration SYMBOLS.
SYMBOLS must already be interned in the default obarray and have no variable,
function, plist, alias, or special-variable state. These views are registration
snapshots; they cannot pass through ordinary `encode' or graph synchronization
and are retired when the first activation lease that contains them is released.

With ISOLATED non-nil the caller publishes SYMBOLS' registrations into an
isolated namespace (see `nelisp-eln-registration-isolated-namespace') whose
function cells and plists it has already verified empty; only the global
variable state is then checked here (see
`nelisp-eln-objects--isolated-registration-symbol-p')."
  (let ((nelisp-eln-objects--isolated-registration-symbols
         (and isolated (listp symbols) (copy-sequence symbols))))
    (nelisp-eln-objects--admit-registration-symbols-1 handle symbols)))

(defun nelisp-eln-objects--admit-registration-symbols-1 (handle symbols)
  "Body of `nelisp-eln-objects-admit-registration-symbols'."
  (let* ((unit (nelisp-eln-objects--check-unit handle))
         (unique nil)
         (names nil)
         (plans nil)
         (new-strings nil)
         (new-symbols nil))
    (unless (listp symbols)
      (signal 'nelisp-eln-objects-unsupported
              (list 'registration-symbol-list-required symbols)))
    ;; Validate the complete request before publishing any string or symbol
    ;; owner. This path never invents a fresh surrogate for an interned name.
    (dolist (symbol symbols)
      (unless (memq symbol unique)
        (unless (nelisp-eln-objects--registration-symbol-p symbol)
          (signal 'nelisp-eln-objects-unsupported
                  (list 'unsupported-registration-symbol symbol)))
        (push symbol unique)))
    (setq unique (nreverse unique))
    (dolist (symbol unique)
      (let ((name (symbol-name symbol)))
        (unless (memq name names)
          (push name names)
          (push (nelisp-eln-string-prepare name) plans))))
    (setq names (nreverse names) plans (nreverse plans))
    (setq new-strings
          (nelisp-eln-objects--add-strings unit names plans))
    (condition-case failure
        (setq new-symbols (nelisp-eln-objects--add-symbols unit unique t))
      (error
       (nelisp-eln-objects--rollback-strings unit new-strings)
       (signal (car failure) (cdr failure))))
    (mapcar (lambda (symbol)
              (let ((record (nelisp-eln-objects--symbol-record unit symbol)))
                (cons symbol
                      (nelisp-eln-objects--symbol-word (aref record 1)))))
            unique)))

(defun nelisp-eln-objects--rollback-symbols (unit records)
  "Undo symbol leases in RECORDS after a pre-write failure."
  (let ((remaining nil))
    (dolist (record records)
      (condition-case nil
          (nelisp-eln-objects--drop-unit-lease unit 'symbol record)
        (error (push record remaining))))
    (when remaining
      (aset unit 6 (append (nreverse remaining) (aref unit 6)))
      (aset unit 1 'releasing))))

(defun nelisp-eln-objects--rollback-strings (unit records)
  (let ((remaining nil) (failed nil) (kept nil))
    (dolist (record records)
      (condition-case nil
          (nelisp-eln-objects--drop-unit-lease unit 'string record)
        (error (setq failed t) (push record remaining))))
    (dolist (record (aref unit 5))
      (unless (memq record records) (push record kept)))
    (aset unit 5 (append remaining (nreverse kept)))
    (when failed (aset unit 1 'releasing))))

(defun nelisp-eln-objects--rollback-numerics (unit records)
  (let ((remaining nil))
    (dolist (record records)
      (condition-case nil
          (nelisp-eln-objects--drop-unit-lease
           unit (aref (aref record 1) 1) record)
        (error (push record remaining))))
    (when remaining
      (aset unit 9 (append (nreverse remaining) (aref unit 9)))
      (aset unit 1 'releasing)
      (nelisp-eln-objects--poison-registry))))

(defun nelisp-eln-objects--allocate-view-memory (bytes)
  (nl-ffi-memory-allocate bytes))

(defun nelisp-eln-objects--ensure-symbol-base ()
  "Create the shared nil anchor and private constructor-default Qunbound view."
  (when (and nelisp-eln-objects--symbol-base
             (not (eq (aref nelisp-eln-objects--symbol-base 3) 'open)))
    (signal 'nelisp-eln-objects-error (list "symbol base is closing")))
  (unless nelisp-eln-objects--symbol-base
    (let* ((name (copy-sequence "unbound"))
           (name-owner (nelisp-eln-string-allocate-prepared
                        (nelisp-eln-string-prepare name)))
           (mapping nil)
           (complete nil))
      (unwind-protect
          (progn
            (setq mapping (nelisp-eln-objects--allocate-view-memory 96))
            (let* ((base (nl-ffi-memory-address mapping))
                   (unbound (+ base nelisp-eln-objects--symbol-qunbound-offset))
                   (name-word (+ (nelisp-eln-string-address name-owner) 4)))
              ;; Offset zero anchors Lisp nil's word. Its fields are not
              ;; exposed; only Qunbound at offset 48 is a serialized symbol.
              (nelisp-eln-objects--write-word unbound 8 name-word)
              (nelisp-eln-objects--write-word unbound 16 48)
              (nelisp-eln-objects--write-word unbound 24 0)
              (nelisp-eln-objects--write-word unbound 32 0)
              (nelisp-eln-objects--write-word unbound 40 0)
              (setq nelisp-eln-objects--symbol-base
                    (vector mapping base name-owner 'open))
              (setq complete t)))
      (unless complete
          (when mapping
            (condition-case nil
                (nl-ffi-memory-release mapping)
              (error (nelisp-eln-objects--retain-cleanup 'memory mapping))))
          (when name-owner
            (condition-case nil
                (nelisp-eln-string-release name-owner)
              (error (nelisp-eln-objects--retain-cleanup 'string name-owner))))))))
  nelisp-eln-objects--symbol-base)

(defun nelisp-eln-objects--symbol-word (address)
  "Encode ADDRESS as a signed byte displacement from the nil anchor."
  (unless (and nelisp-eln-objects--symbol-base
               (eq (aref nelisp-eln-objects--symbol-base 3) 'open))
    (signal 'nelisp-eln-objects-error (list "symbol base is not leased")))
  (nelisp-eln-abi-normalize-word
   (- address (aref nelisp-eln-objects--symbol-base 1))))

(defun nelisp-eln-objects--write-word (address offset word)
  "Store WORD through the shared GNU raw-word codec."
  (nelisp-eln-abi-write-word address offset word))

(defun nelisp-eln-objects--read-word (address offset)
  "Read WORD through the shared GNU raw-word codec."
  (nelisp-eln-abi-read-word address offset))

(defun nelisp-eln-objects--add-conses (unit conses)
  "Lease every CONSES for UNIT and return writes plus rollback records."
  (let ((new-objects nil) (shared-objects nil))
    (dolist (object conses)
      (unless (nelisp-eln-objects--record unit object)
        (let ((shared (nelisp-eln-objects--global-record object)))
          (if shared
              (progn
                (unless (eq (aref shared 7) 'open)
                  (signal 'nelisp-eln-objects-error
                          (list "cons view is releasing" object)))
                (push (cons object shared) shared-objects))
            (push object new-objects)))))
    (setq new-objects (nreverse new-objects)
          shared-objects (nreverse shared-objects))
    (let* ((arena (and new-objects (vector nil (length new-objects) 'open)))
           (new-shared
            (mapcar (lambda (object)
                      (vector object 'cons nil nil arena 1 0 'open))
                    new-objects))
           (new-links (cl-mapcar #'cons new-objects new-shared))
           (new-local
            (mapcar (lambda (shared)
                      (list (aref shared 0) nil shared t)) new-shared))
           (old-local
            (mapcar (lambda (entry)
                      (list (car entry) (aref (cdr entry) 2) (cdr entry) t))
                    shared-objects))
           (local-records (append new-local old-local))
           (write-records new-local)
           (transaction (vector write-records local-records))
           (all-unit-records (append local-records (aref unit 3)))
           (all-global-records (append new-links
                                       nelisp-eln-objects--identity-records))
           (all-arenas (and arena (cons arena nelisp-eln-objects--arenas)))
           (new-pairs (cl-mapcar #'cons new-shared new-local))
           (complete nil))
      (unwind-protect
          (progn
            (when new-objects
              (let ((mapping (nelisp-eln-objects--allocate-view-memory
                              (* (length new-objects) 16)))
                    (address nil))
                (aset arena 0 mapping)
                (setq address (nl-ffi-memory-address mapping))
                (dolist (pair new-pairs)
                  (aset (car pair) 2 address)
                  (aset (car pair) 3 mapping)
                  (setcar (cdr (cdr pair)) address)
                  (setq address (+ address 16)))))
            (dolist (entry shared-objects)
              (aset (cdr entry) 5 (1+ (aref (cdr entry) 5))))
            (setq nelisp-eln-objects--identity-records all-global-records)
            (when arena (setq nelisp-eln-objects--arenas all-arenas))
            (aset unit 3 all-unit-records)
            (setq complete t)
            transaction)
        (unless complete
          (when (and arena (aref arena 0))
            (aset arena 1 0)
            (condition-case nil
                (nl-ffi-memory-release (aref arena 0))
              (error
               (aset arena 2 'releasing)
               (setq nelisp-eln-objects--arenas (cons arena
                                                    nelisp-eln-objects--arenas))
               (setq nelisp-eln-objects--registry-state 'poisoned)))))))))

(defun nelisp-eln-objects--rollback-add (unit transaction)
  "Undo unit leases for records in TRANSACTION after safe pre-write failure."
  (dolist (record (aref transaction 1))
    (condition-case nil
        (nelisp-eln-objects--drop-unit-lease unit 'cons record)
      (error
       (aset unit 1 'releasing)
       (setq nelisp-eln-objects--registry-state 'poisoned)))))

(defun nelisp-eln-objects--stage-records (unit records)
  "Validate RECORDS' complete canonical edges before producing write data."
  (let ((staged nil))
    (dolist (record records)
      (let ((object (car record)))
        (setq staged
              (cons (list (nth 1 record)
                          (nelisp-eln-objects--encode-word unit (car object))
                          (nelisp-eln-objects--encode-word unit (cdr object)))
                    staged))))
    (nreverse staged)))

(defun nelisp-eln-objects--check-symbol-view (unit local)
  "Reject native edits to unsupported symbol cells before any sync commits."
  ;; An interned view (see `nelisp-eln-objects--admit-empty-interned-symbols')
  ;; stays valid only while its symbol's global state is still entirely
  ;; empty, and carries the interned-in-initial-obarray bit.  An opaque
  ;; view (see `nelisp-eln-objects--admit-opaque-interned-symbols') never
  ;; mirrors that state, so only its identity and poison cells are checked.
  (let* ((opaque (nelisp-eln-objects--opaque-view-p local))
         (interned (eq (intern-soft (symbol-name (aref local 0)))
                       (aref local 0)))
         (cell-word (if opaque nelisp-eln-objects--opaque-symbol-cell-word)))
    (unless (cond (opaque interned)
                  (interned
                   (nelisp-eln-objects--registration-symbol-p (aref local 0)))
                  (t (nelisp-eln-objects--fresh-symbol-p (aref local 0) unit)))
      (signal 'nelisp-eln-objects-unsupported
              (list 'symbol-state-changed (aref local 0))))
    (let* ((address (aref local 1))
           (name-record (nelisp-eln-objects--string-record
                         unit (symbol-name (aref local 0))))
           (name-word (and name-record (+ (aref name-record 2) 4))))
      (unless (and name-word
                   (= (ptr-read-u8 address 0)
                      (cond (opaque
                             (nelisp-eln-objects--opaque-symbol-header-byte))
                            (interned
                             (lsh nelisp-eln-objects--symbol-interned-in-initial-obarray 5))
                            (t 0)))
                   (= (nelisp-eln-objects--read-word address 8) name-word)
                   (= (nelisp-eln-objects--read-word address 16)
                      (or cell-word 48))
                   (= (nelisp-eln-objects--read-word address 24)
                      (or cell-word 0))
                   (= (nelisp-eln-objects--read-word address 32)
                      (or cell-word 0))
                   (= (nelisp-eln-objects--read-word address 40) 0))
        (signal 'nelisp-eln-objects-unsupported
                (list 'native-symbol-cell-mutation (aref local 0))))
      t)))

(defun nelisp-eln-objects--write-staged (staged)
  (dolist (entry staged)
    (nelisp-eln-objects--write-word (nth 0 entry) 0 (nth 1 entry))
    (nelisp-eln-objects--write-word (nth 0 entry) 8 (nth 2 entry)))
  t)

(defun nelisp-eln-objects-encode (unit value)
  "Return VALUE as a GNU word, adding stable views for its object graph.
Repeated calls do not overwrite native mutations; call `sync-to-native'
explicitly to publish changed canonical Lisp edges."
  (setq unit (nelisp-eln-objects--check-unit unit))
  (let* ((roots (if (memq value (aref unit 4))
                    (aref unit 4)
                  (cons value (aref unit 4))))
         (plan (nelisp-eln-objects--preflight roots unit))
         (conses (aref plan 0)) (strings (aref plan 1))
         (string-plans (aref plan 2))
         (symbols (aref plan 3))
         (numerics (aref plan 4))
         (vectors (aref plan 5))
         (new-strings (nelisp-eln-objects--add-strings
                       unit strings string-plans))
         (new-numerics (condition-case failure
                           (nelisp-eln-objects--add-numerics unit numerics)
                         (error
                          (nelisp-eln-objects--rollback-strings unit new-strings)
                          (signal (car failure) (cdr failure)))))
         (new-symbols (condition-case failure
                          (nelisp-eln-objects--add-symbols unit symbols)
                        (error
                         (nelisp-eln-objects--rollback-numerics
                          unit new-numerics)
                         (nelisp-eln-objects--rollback-strings unit new-strings)
                         (condition-case nil
                             (nelisp-eln-objects--release-unused-symbol-base)
                           (error (nelisp-eln-objects--poison-registry)))
                         (signal (car failure) (cdr failure)))))
         (new-vectors (condition-case failure
                          (nelisp-eln-objects--add-vectors unit vectors)
                        (error
                         (nelisp-eln-objects--rollback-symbols unit new-symbols)
                         (nelisp-eln-objects--rollback-numerics
                          unit new-numerics)
                         (nelisp-eln-objects--rollback-strings unit new-strings)
                         (signal (car failure) (cdr failure)))))
         (transaction (condition-case failure
                          (nelisp-eln-objects--add-conses unit conses)
                        (error
                         (nelisp-eln-objects--rollback-vectors unit new-vectors)
                         (nelisp-eln-objects--rollback-symbols unit new-symbols)
                         (nelisp-eln-objects--rollback-numerics
                          unit new-numerics)
                         (nelisp-eln-objects--rollback-strings unit new-strings)
                         (signal (car failure) (cdr failure)))))
         (new-records (aref transaction 0))
         (staged (condition-case failure
                     (nelisp-eln-objects--stage-records unit new-records)
                    (error
                    (nelisp-eln-objects--rollback-add unit transaction)
                    (nelisp-eln-objects--rollback-vectors unit new-vectors)
                    (nelisp-eln-objects--rollback-symbols unit new-symbols)
                    (nelisp-eln-objects--rollback-numerics
                     unit new-numerics)
                    (nelisp-eln-objects--rollback-strings unit new-strings)
                    (signal (car failure) (cdr failure))))))
    (condition-case failure
        (nelisp-eln-objects--write-staged staged)
      (error
       (nelisp-eln-objects--poison-registry)
       (aset unit 1 'releasing)
       (nelisp-eln-objects--rollback-add unit transaction)
       (nelisp-eln-objects--rollback-vectors unit new-vectors)
       (nelisp-eln-objects--rollback-symbols unit new-symbols)
       (nelisp-eln-objects--rollback-numerics unit new-numerics)
       (nelisp-eln-objects--rollback-strings unit new-strings)
       (signal (car failure) (cdr failure))))
    (aset unit 4 roots))
  (nelisp-eln-objects--encode-word unit value))

(defun nelisp-eln-objects--decode-word (unit word)
  (let ((kind (nelisp-eln-abi-classify-word word)))
    (cond
     ((eq kind 'nil) nil)
     ((eq kind 'fixnum) (nelisp-eln-abi-decode-fixnum word))
     ((eq kind 'vectorlike)
      (let* ((address (- (nelisp-eln-abi-normalize-word word)
                         nelisp-eln-objects--vector-tag))
             (records (aref unit 8)) found)
        (while (and records (not found))
          (if (= address (aref (car records) 1))
              (setq found (car records))
            (setq records (cdr records))))
        (if found
            (aref found 0)
          (nelisp-eln-objects--decode-numeric unit word 'bignum))))
     ((eq kind 'float)
      (nelisp-eln-objects--decode-numeric unit word 'float))
     ((eq kind 'string)
      (let ((address (- (nelisp-eln-abi-normalize-word word) 4))
            (records (aref unit 5)) found)
        (while (and records (not found))
          (if (= address (aref (car records) 2))
              (setq found (car records))
            (setq records (cdr records))))
        (unless found
          (signal 'nelisp-eln-objects-error
                  (list "foreign or unknown GNU string address" address)))
        (unless (= (nelisp-eln-string-address (aref found 1)) address)
          (signal 'nelisp-eln-objects-error (list "string owner address changed")))
        (aref found 0)))
     ((eq kind 'cons)
      (let ((address (- (nelisp-eln-abi-normalize-word word) 3))
            (records (aref unit 3)) found)
        (while (and records (not found))
          (if (= address (nth 1 (car records)))
              (setq found (car records))
            (setq records (cdr records))))
        (unless found
          (signal 'nelisp-eln-objects-error
                  (list "foreign or unknown GNU cons address" address)))
        (nth 0 found)))
     ((eq kind 'symbol)
      (let ((bits (nelisp-eln-abi-normalize-word word)))
        (if (= bits nelisp-eln-objects--symbol-t-word)
            t
          (progn
            (unless nelisp-eln-objects--symbol-base
              (signal 'nelisp-eln-objects-error
                      (list "symbol word has no leased base")))
            (let* ((signed (if (> bits nelisp-eln-abi-signed-word-max)
                                (- bits (1+ nelisp-eln-abi-word-mask))
                              bits))
                   (address (+ (aref nelisp-eln-objects--symbol-base 1) signed)))
              (cond
               ((= address (+ (aref nelisp-eln-objects--symbol-base 1) 48))
                (if (aref unit 6)
                    (if (boundp 'nelisp--unbound-marker)
                        nelisp--unbound-marker nil)
                  (signal 'nelisp-eln-objects-error
                          (list "Qunbound is outside this unit's symbol lease"))))
               (t (let ((record (aref unit 6)) found)
                    (while (and record (not found))
                      (if (= address (aref (car record) 1))
                          (setq found (car record))
                        (setq record (cdr record)))
                      (when found (setq record nil)))
                    (unless found
                      (signal 'nelisp-eln-objects-error
                              (list "foreign or unknown GNU symbol address"
                                    address)))
                    (aref found 0)))))))))
     (t (signal 'nelisp-eln-objects-unsupported (list kind word))))))

(defun nelisp-eln-objects-decode (handle word)
  "Decode GNU WORD using open unit HANDLE.
Pointer-tagged cons and string addresses must belong to HANDLE.  This does not
synchronize changed native fields; call `nelisp-eln-objects-sync-from-native'
at the boundary."
  (nelisp-eln-objects--decode-word
   (nelisp-eln-objects--check-unit handle) word))

(defun nelisp-eln-objects--resolve-activation (token)
  (unless (and (vectorp token) (= (length token) 2)
               (eq (aref token 0) nelisp-eln-objects--activation-marker))
    (signal 'nelisp-eln-objects-error (list "invalid activation token")))
  (let ((entry (assq token nelisp-eln-objects--activations)))
    (unless entry
      (signal 'nelisp-eln-objects-error (list "stale activation token")))
    (cdr entry)))

(defun nelisp-eln-objects-activation-acquire (handle)
  "Lease all views in open unit HANDLE for one native activation."
  (let* ((unit (nelisp-eln-objects--check-unit handle))
         (token (vector nelisp-eln-objects--activation-marker nil))
         (records (append (mapcar (lambda (local) (nth 2 local))
                                  (aref unit 3))
                          (mapcar (lambda (local) (aref local 3))
                                  (aref unit 5))
                          (mapcar (lambda (local) (aref local 2))
                                  (aref unit 6))
                          (mapcar (lambda (local) (aref local 2))
                                  (aref unit 8))
                          (mapcar (lambda (local) (aref local 1))
                                  (aref unit 9))))
         (members (mapcar (lambda (record) (cons record t)) records))
         (registration-locals
          (cl-remove-if-not (lambda (local) (aref local 4))
                            (aref unit 6)))
         (activation (vector 'open members unit registration-locals)))
    (dolist (record records)
      (unless (eq (aref record 7) 'open)
        (signal 'nelisp-eln-objects-error
                (list "activation references releasing object"))))
    (push (cons token activation) nelisp-eln-objects--activations)
    (dolist (record records)
      (aset record 6 (1+ (aref record 6))))
    token))

(defun nelisp-eln-objects--word-address (kind word)
  "Return the view address pointer WORD of KIND names."
  (let* ((tag (cond ((eq kind 'cons) 3) ((eq kind 'string) 4)
                    ((memq kind '(bignum vector)) 5)
                    ((eq kind 'float) 7) (t 0)))
         (bits (nelisp-eln-abi-normalize-word word))
         (signed (if (and (eq kind 'symbol)
                          (> bits nelisp-eln-abi-signed-word-max))
                     (- bits (1+ nelisp-eln-abi-word-mask))
                   bits)))
    (if (eq kind 'symbol)
        (progn
          (unless nelisp-eln-objects--symbol-base
            (signal 'nelisp-eln-objects-error
                    (list "symbol activation has no base")))
          (+ (aref nelisp-eln-objects--symbol-base 1) signed))
      (- bits tag))))

(defun nelisp-eln-objects--activation-find (activation kind word)
  "Return ACTIVATION's leased record for pointer WORD of KIND, or nil.
Return the symbol `qunbound' for the leased Qunbound view.  Signals only
for a leased record whose numeric owner changed, or a symbol word with
no symbol base; an unleased pointer is plain nil so callers choose
between rejecting it and trying another activation without a handler."
  (let* ((address (nelisp-eln-objects--word-address kind word))
         (members (aref activation 1)) found)
    (when (and (eq kind 'symbol) nelisp-eln-objects--symbol-base
               (cl-some (lambda (member)
                          (eq (aref (car member) 1) 'symbol))
                        (aref activation 1))
               (= address (+ (aref nelisp-eln-objects--symbol-base 1) 48)))
      (setq found 'qunbound))
    (while (and members (not found))
      (let ((record (car (car members))))
        (when (and (eq (aref record 1) kind)
                   (= (aref record 2) address))
          (when (memq kind '(bignum float))
            (unless (and (eq (nelisp-eln-objects--numeric-call
                              kind "source" (aref record 3))
                             (aref record 0))
                         (= (nelisp-eln-objects--numeric-call
                             kind "address" (aref record 3)) address))
              (signal 'nelisp-eln-objects-error
                      (list 'numeric-owner-changed kind address))))
          (setq found record)))
      (setq members (cdr members)))
    found))

(defun nelisp-eln-objects--activation-find-word (activation kind word)
  "Return ACTIVATION's leased record for WORD of decoder KIND.
A vectorlike word (decoder kind `bignum') may also name an opaque vector."
  (or (nelisp-eln-objects--activation-find activation kind word)
      (and (eq kind 'bignum)
           (nelisp-eln-objects--activation-find activation 'vector word))))

(defun nelisp-eln-objects--word-kind (word)
  "Return the decoder kind of GNU WORD (bignum for any vectorlike)."
  (let ((raw-kind (nelisp-eln-abi-classify-word word)))
    (if (eq raw-kind 'vectorlike) 'bignum raw-kind)))

(defun nelisp-eln-objects-activation-leases-word-p (token word)
  "Return non-nil when TOKEN's open activation can decode WORD.
Immediates (nil, fixnums, t) are decodable by every activation; a pointer
word only when TOKEN leases its record.  Never decodes."
  (nelisp-eln-objects--check-registry)
  (let ((activation (nelisp-eln-objects--resolve-activation token))
        (kind (nelisp-eln-objects--word-kind word)))
    (unless (eq (aref activation 0) 'open)
      (signal 'nelisp-eln-objects-error (list "activation token is closing")))
    (cond
     ((memq kind '(nil fixnum)) t)
     ((and (eq kind 'symbol)
           (= (nelisp-eln-abi-normalize-word word)
              nelisp-eln-objects--symbol-t-word))
      t)
     ((memq kind '(cons string symbol bignum float))
      (and (nelisp-eln-objects--activation-find-word activation kind word) t))
     (t nil))))

(defun nelisp-eln-objects-activation-decode (token word)
  "Decode WORD only when its pointer record is leased by TOKEN."
  (nelisp-eln-objects--check-registry)
  (let* ((activation (nelisp-eln-objects--resolve-activation token))
         (kind (nelisp-eln-objects--word-kind word)))
    (unless (eq (aref activation 0) 'open)
      (signal 'nelisp-eln-objects-error (list "activation token is closing")))
    (cond
     ((eq kind 'nil) nil)
     ((eq kind 'fixnum) (nelisp-eln-abi-decode-fixnum word))
     ((memq kind '(cons string symbol bignum float))
      (if (and (eq kind 'symbol)
               (= (nelisp-eln-abi-normalize-word word)
                  nelisp-eln-objects--symbol-t-word))
          t
        (let ((found (nelisp-eln-objects--activation-find-word
                      activation kind word)))
          (unless found
            (signal 'nelisp-eln-objects-error
                    (list "pointer is outside activation lease"
                          (nelisp-eln-objects--word-address kind word))))
          (if (eq found 'qunbound)
              (if (boundp 'nelisp--unbound-marker) nelisp--unbound-marker nil)
            (aref found 0)))))
     (t (signal 'nelisp-eln-objects-unsupported (list kind word))))))

(defun nelisp-eln-objects-activation-release (token)
  "Release TOKEN leases; failed unmaps leave a retryable closing token."
  (let ((activation (nelisp-eln-objects--resolve-activation token)))
    (aset activation 0 'releasing)
    (while (aref activation 1)
      (let* ((members (aref activation 1))
             (member (car members))
             (record (car member)))
        (condition-case failure
            (progn
              (when (cdr member)
                (aset record 6 (1- (aref record 6)))
                (setcdr member nil))
              (nelisp-eln-objects--finish-global-release record)
              ;; Commit each completed lease immediately. If a later release
              ;; fails, retry starts at the first still-pending member.
              (aset activation 1 (cdr members)))
              (error (signal (car failure) (cdr failure))))))
    ;; Interned registration symbol views are snapshots, not synchronized
    ;; global objects. Drop their unit leases after activation decoding ends.
    (while (aref activation 3)
      (let* ((local (car (aref activation 3)))
             (unit (aref activation 2)))
        (if (memq local (aref unit 6))
            (nelisp-eln-objects--drop-unit-lease unit 'symbol local)
          (aset activation 3 (cdr (aref activation 3))))))
    (setq nelisp-eln-objects--activations
          (assq-delete-all token nelisp-eln-objects--activations))
    (aset token 1 'closed)
    (nelisp-eln-objects--reset-poison-if-empty)
    t))

(defun nelisp-eln-objects-sync-to-native (unit)
  "Publish all canonical graph edges in UNIT to its stable GNU views."
  (setq unit (nelisp-eln-objects--check-unit unit))
  (let* ((existing (mapcar (lambda (record) (car record)) (aref unit 3)))
         (owned-strings (mapcar (lambda (record) (aref record 0))
                                (aref unit 5)))
         (owned-symbols (mapcar (lambda (record) (aref record 0))
                               (aref unit 6)))
         (owned-vectors (mapcar (lambda (record) (aref record 0))
                                (aref unit 8)))
         (plan (nelisp-eln-objects--preflight
                (append (aref unit 4) existing owned-strings owned-symbols
                        owned-vectors)
                unit))
         (new-strings (nelisp-eln-objects--add-strings
                       unit (aref plan 1) (aref plan 2)))
         (new-numerics (condition-case failure
                           (nelisp-eln-objects--add-numerics unit (aref plan 4))
                         (error
                          (nelisp-eln-objects--rollback-strings unit new-strings)
                          (signal (car failure) (cdr failure)))))
         (new-symbols (condition-case failure
                          (nelisp-eln-objects--add-symbols unit (aref plan 3))
                        (error
                         (nelisp-eln-objects--rollback-numerics
                          unit new-numerics)
                         (nelisp-eln-objects--rollback-strings unit new-strings)
                         (signal (car failure) (cdr failure)))))
         (new-vectors (condition-case failure
                          (nelisp-eln-objects--add-vectors unit (aref plan 5))
                        (error
                         (nelisp-eln-objects--rollback-symbols unit new-symbols)
                         (nelisp-eln-objects--rollback-numerics
                          unit new-numerics)
                         (nelisp-eln-objects--rollback-strings unit new-strings)
                         (signal (car failure) (cdr failure)))))
         (transaction (condition-case failure
                          (nelisp-eln-objects--add-conses unit (aref plan 0))
                        (error
                         (nelisp-eln-objects--rollback-vectors unit new-vectors)
                         (nelisp-eln-objects--rollback-symbols unit new-symbols)
                         (nelisp-eln-objects--rollback-numerics
                          unit new-numerics)
                         (nelisp-eln-objects--rollback-strings unit new-strings)
                         (signal (car failure) (cdr failure)))))
         (staged-conses nil) (staged-strings nil) (validated nil))
    (condition-case failure
        (progn
          (setq staged-conses
                (nelisp-eln-objects--stage-records unit (aref unit 3)))
          (dolist (record (aref unit 5))
            (push (nelisp-eln-string-prepare-sync-to (aref record 1))
                  staged-strings))
          (dolist (record (aref unit 6))
            (nelisp-eln-objects--check-symbol-view unit record))
          (dolist (record (aref unit 8))
            (nelisp-eln-objects--check-vector-view unit record))
          (setq staged-strings (nreverse staged-strings))
          (dolist (string-plan staged-strings)
            (nelisp-eln-string-validate-sync-plan string-plan))
          (setq validated t))
      (error
       (nelisp-eln-objects--rollback-add unit transaction)
       (nelisp-eln-objects--rollback-vectors unit new-vectors)
       (nelisp-eln-objects--rollback-symbols unit new-symbols)
       (nelisp-eln-objects--rollback-numerics unit new-numerics)
       (nelisp-eln-objects--rollback-strings unit new-strings)
       (signal (car failure) (cdr failure))))
    (when validated
      ;; All graph and string plans validate before the first external write.
      ;; Any unexpected later error quarantines the whole unit for release.
      (condition-case failure
          (progn
            (dolist (string-plan staged-strings)
              (nelisp-eln-string-commit-sync-plan string-plan))
            (nelisp-eln-objects--write-staged staged-conses))
        (error
         (aset unit 1 'releasing)
         (nelisp-eln-objects--poison-registry)
         (signal (car failure) (cdr failure)))))))

(defun nelisp-eln-objects-sync-from-native (unit)
  "Validate and stage all owned GNU cons edges, then update canonical conses.
No native pointer is dereferenced unless it is an owned cons address."
  (setq unit (nelisp-eln-objects--check-unit unit))
  (let ((records (aref unit 3)) (staged nil) (string-plans nil))
    (dolist (record (aref unit 6))
      (nelisp-eln-objects--check-symbol-view unit record))
    (dolist (record (aref unit 8))
      (nelisp-eln-objects--check-vector-view unit record))
    (dolist (record (aref unit 5))
      (push (nelisp-eln-string-prepare-sync-from (aref record 1))
            string-plans))
    (setq string-plans (nreverse string-plans))
    (while records
      (let* ((record (car records))
             (address (nth 1 record))
             (car-value (nelisp-eln-objects--decode-word unit
                         (nelisp-eln-objects--read-word address 0)))
             (cdr-value (nelisp-eln-objects--decode-word unit
                         (nelisp-eln-objects--read-word address 8))))
        (push (list (nth 0 record) car-value cdr-value) staged))
      (setq records (cdr records)))
    (dolist (plan string-plans)
      (nelisp-eln-string-validate-sync-plan plan))
    (condition-case failure
        (progn
          (dolist (plan string-plans)
            (nelisp-eln-string-commit-sync-plan plan))
          (dolist (entry staged)
            (setcar (car entry) (nth 1 entry))
            (setcdr (car entry) (nth 2 entry))))
      (error
       (aset unit 1 'releasing)
       (nelisp-eln-objects--poison-registry)
       (signal (car failure) (cdr failure))))
    t))

(defun nelisp-eln-objects-release (unit)
  "Release UNIT leases; active calls keep shared views alive independently."
  (let* ((handle unit)
         (unit (nelisp-eln-objects--resolve handle)))
    (aset unit 1 'releasing)
    (while (aref unit 3)
      (nelisp-eln-objects--drop-unit-lease unit 'cons (car (aref unit 3))))
    (while (aref unit 5)
      (nelisp-eln-objects--drop-unit-lease unit 'string (car (aref unit 5))))
    (while (aref unit 6)
      (nelisp-eln-objects--drop-unit-lease unit 'symbol (car (aref unit 6))))
    (while (aref unit 8)
      (nelisp-eln-objects--drop-unit-lease unit 'vector (car (aref unit 8))))
    (while (aref unit 9)
      (nelisp-eln-objects--drop-unit-lease
       unit (aref (aref (car (aref unit 9)) 1) 1) (car (aref unit 9))))
    (setq nelisp-eln-objects--live-units
          (assq-delete-all handle nelisp-eln-objects--live-units))
    (aset handle 1 'closed)
    (aset unit 4 nil)
    (nelisp-eln-objects--reset-poison-if-empty)
    t))

(provide 'nelisp-eln-objects)

;;; nelisp-eln-objects.el ends here
