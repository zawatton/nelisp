;;; nelisp-eln-registration-objects.el --- temporary GNU registration views -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module builds narrowly scoped GNU 31.1 x86-64 pseudovector views for
;; registration callbacks. Unit views live with a loader owner. Subr views
;; exist only during an explicit activation and must be retired before that
;; activation returns to the caller. It does not provide a general Lisp heap.

;;; Code:

(require 'nelisp-eln-abi)
(require 'nelisp-eln-objects)
(require 'nelisp-eln-native-subr)
(require 'nl-ffi-memory)
(require 'cl-lib)

(declare-function ptr-write-u8 "ext:nelisp-runtime" (ptr offset value))
(declare-function ptr-read-u8 "ext:nelisp-runtime" (ptr offset))
(declare-function nelisp-eln-registration-metadata-type-word
  "nelisp-eln-registration-metadata" (token))
(declare-function nelisp-eln-registration-metadata-decode
  "nelisp-eln-registration-metadata" (token word))

(define-error 'nelisp-eln-registration-objects-error
  "Invalid temporary GNU registration view")

(defconst nelisp-eln-registration-objects--unit-marker
  'nelisp-eln-registration-unit)
(defconst nelisp-eln-registration-objects--activation-marker
  'nelisp-eln-registration-activation)
(defconst nelisp-eln-registration-objects--pseudovector-flag
  4611686018427387904)
(defconst nelisp-eln-registration-objects--vectorlike-tag 5)
(defconst nelisp-eln-registration-objects--pvec-subr 18)
(defconst nelisp-eln-registration-objects--pvec-native-comp-unit 26)
(defvar nelisp-eln-registration-objects--live-units nil)
(defvar nelisp-eln-registration-objects--pending-cleanups nil
  "Externally owned buffers retained after a failed constructor cleanup.")
(defconst nelisp-eln-registration-objects--layout
  '(:unit-size 80 :unit-memlen 9 :unit-lisplen 6 :unit-restsize 3
    :unit-file 8 :unit-qualities 16 :unit-lambda-guard 24
    :unit-lambda-index 32 :unit-doc-vector 40 :unit-data-vector 48
    :unit-data-relocs 56 :unit-loaded-once 64 :unit-load-ongoing 65
    :unit-handle 72 :subr-size 88 :subr-memlen 10 :subr-lisplen 0
    :subr-restsize 10 :subr-function 8 :subr-min-args 16
    :subr-max-args 18 :subr-symbol-name 24 :subr-intspec 32
    :subr-command-modes 40 :subr-doc 48 :subr-native-unit 56
    :subr-native-c-name 64 :subr-lambda-list 72 :subr-type 80))

(defun nelisp-eln-registration-objects--header (tag memlen lisplen)
  "Encode a GNU pseudovector header for TAG and measured payload sizes."
  (+ nelisp-eln-registration-objects--pseudovector-flag
     (ash tag 24)
     (ash (- memlen lisplen) 12)
     lisplen))

(defun nelisp-eln-registration-objects--pointer-word (address)
  (unless (and (integerp address) (> address 0) (= (logand address 7) 0))
    (signal 'nelisp-eln-registration-objects-error
            (list 'invalid-aligned-address address)))
  (+ address nelisp-eln-registration-objects--vectorlike-tag))

(defun nelisp-eln-registration-objects--live-unit (unit)
  (unless (and (vectorp unit) (= (length unit) 8)
               (eq (aref unit 0)
                   nelisp-eln-registration-objects--unit-marker)
               (eq (aref unit 5) 'open)
               (memq unit nelisp-eln-registration-objects--live-units))
    (signal 'nelisp-eln-registration-objects-error
            (list 'stale-or-invalid-unit unit)))
  (condition-case nil
      (nelisp-eln-system-loader--state (aref unit 1))
    (error
     (signal 'nelisp-eln-registration-objects-error
             (list 'closed-loader-handle))))
  unit)

(defun nelisp-eln-registration-objects-create-unit (handle &optional fields)
  "Create a unit view for live loader HANDLE with six Lisp FIELD values.
FIELD order is file, optimize qualities, lambda guard, lambda-name index,
documentation vector, and data vector. The supported scalar registration
slice should pass nil for fields it does not consume."
  (setq fields (or fields '(nil nil nil nil nil nil)))
  (unless (= (length fields) 6)
    (signal 'nelisp-eln-registration-objects-error
            (list 'unit-requires-six-fields (length fields))))
  (let ((state (nelisp-eln-system-loader--state handle))
        (objects nil) (memory nil) (unit nil) (address nil)
        (result nil) (failure nil))
    (condition-case err
        (setq objects (nelisp-eln-objects-create)
              memory (nl-ffi-memory-allocate 80)
              address (nl-ffi-memory-address memory))
      (error (setq failure err)))
    (unless failure
      (condition-case err
          (progn
            (nelisp-eln-abi-write-word
             address 0 (nelisp-eln-registration-objects--header 26 9 6))
            (let ((i 0))
              (while (< i 6)
                (nelisp-eln-abi-write-word
                 address (+ 8 (* i 8))
                 (nelisp-eln-objects-encode objects (nth i fields)))
                (setq i (1+ i))))
            (nelisp-eln-abi-write-word address 56 0)
            (ptr-write-u8 address 64 0)
            (ptr-write-u8 address 65 1)
            (nelisp-eln-abi-write-word address 72 (plist-get state :dl-handle))
            (setq unit (vector nelisp-eln-registration-objects--unit-marker
                               handle objects memory address 'open nil nil)
                  nelisp-eln-registration-objects--live-units
                  (cons unit nelisp-eln-registration-objects--live-units)
                  result unit))
        (error (setq failure err))))
    (when failure
      (when memory
        (condition-case nil
            (nl-ffi-memory-release memory)
          (error
           (push (cons 'memory memory)
                 nelisp-eln-registration-objects--pending-cleanups))))
      (when objects
        (condition-case nil
            (nelisp-eln-objects-release objects)
          (error
           (push (cons 'objects objects)
                 nelisp-eln-registration-objects--pending-cleanups))))
      (signal (car failure) (cdr failure)))
    result))

(defun nelisp-eln-registration-objects-retry-pending-cleanup ()
  "Retry external releases retained by a failed unit constructor."
  (let ((remaining nil))
    (dolist (entry (copy-sequence
                    nelisp-eln-registration-objects--pending-cleanups))
      (condition-case nil
          (progn
            (if (eq (car entry) 'memory)
                (nl-ffi-memory-release (cdr entry))
              (nelisp-eln-objects-release (cdr entry)))
            (setq nelisp-eln-registration-objects--pending-cleanups
                  (delq entry
                        nelisp-eln-registration-objects--pending-cleanups)))
        (error (push entry remaining))))
    (setq nelisp-eln-registration-objects--pending-cleanups
          (nreverse remaining)))
  t)

(defun nelisp-eln-registration-objects-crash-boundary-snapshot ()
  "Return a plist describing this module's live units and pending cleanups.

Read by `nelisp-eln-registration-crash-boundary-report' (see
nelisp-eln-registration.el) to fold this module's lower-level unit/memory
pending-cleanup list into the whole-process S7.7 consistency snapshot.
This function only reads existing state; it never mutates it.

  :live-units        count of `nelisp-eln-registration-objects--live-units'
  :pending-cleanups   count of `nelisp-eln-registration-objects--pending-cleanups'
  :inconsistencies    list of plists describing any structural problem found"
  (let (problems)
    (dolist (unit nelisp-eln-registration-objects--live-units)
      (unless (and (vectorp unit) (= (length unit) 8)
                   (eq (aref unit 0)
                       nelisp-eln-registration-objects--unit-marker))
        (push (list :kind 'malformed-live-unit :unit unit) problems)))
    (dolist (entry nelisp-eln-registration-objects--pending-cleanups)
      (unless (memq (car entry) '(memory objects))
        (push (list :kind 'unrecognized-pending-cleanup-kind :entry entry)
              problems)))
    (list :live-units (length nelisp-eln-registration-objects--live-units)
          :pending-cleanups
          (length nelisp-eln-registration-objects--pending-cleanups)
          :inconsistencies (nreverse problems))))

(defun nelisp-eln-registration-objects--release-memory-or-retain (memory)
  "Release MEMORY, retaining it for retry when the external release fails."
  (when memory
    (condition-case nil
        (nl-ffi-memory-release memory)
      (error
       (push (cons 'memory memory)
             nelisp-eln-registration-objects--pending-cleanups))))
  t)

(defun nelisp-eln-registration-objects-unit-word (unit)
  "Return UNIT as a GNU PVEC_NATIVE_COMP_UNIT Lisp word."
  (setq unit (nelisp-eln-registration-objects--live-unit unit))
  (nelisp-eln-registration-objects--pointer-word (aref unit 4)))

(defun nelisp-eln-registration-objects-encode-word (unit value)
  "Encode supported Lisp VALUE as a GNU word owned by UNIT."
  (setq unit (nelisp-eln-registration-objects--live-unit unit))
  (nelisp-eln-objects-encode (aref unit 2) value))

(defun nelisp-eln-registration-objects-decode-word (unit word)
  "Decode supported GNU WORD through UNIT's existing object codec."
  (setq unit (nelisp-eln-registration-objects--live-unit unit))
  (nelisp-eln-objects-decode (aref unit 2) word))

(defun nelisp-eln-registration-objects-begin-activation (unit)
  "Begin a bounded registration activation for UNIT."
  (setq unit (nelisp-eln-registration-objects--live-unit unit))
  (let ((activation (vector nelisp-eln-registration-objects--activation-marker
                            unit 'open nil nil)))
    (aset unit 6 (cons activation (aref unit 6)))
    activation))

(defun nelisp-eln-registration-objects--live-activation (activation)
  (unless (and (vectorp activation) (= (length activation) 5)
               (eq (aref activation 0)
                   nelisp-eln-registration-objects--activation-marker)
               (eq (aref activation 2) 'open))
    (signal 'nelisp-eln-registration-objects-error
            (list 'stale-or-invalid-activation activation)))
  (nelisp-eln-registration-objects--live-unit (aref activation 1))
  activation)

(defun nelisp-eln-registration-objects--write-short (address offset value)
  (unless (and (integerp value) (<= 0 value) (<= value 32767))
    (signal 'nelisp-eln-registration-objects-error
            (list 'unsupported-argument-count value)))
  (ptr-write-u8 address offset (logand value 255))
  (ptr-write-u8 address (1+ offset) (ash value -8)))

(defun nelisp-eln-registration-objects-subr-view
    (activation name c-name intspec command-modes doc-index type
                &optional arity metadata-token native-constructor
                type-index)
  "Create a temporary scalar PVEC_SUBR view for registration.
NAME and C-NAME are strings; DOC-INDEX is a nonnegative integer.
ARITY is the exact admitted fixed arity: zero, one, or two (only a
NATIVE-CONSTRUCTOR-built binary body, e.g. a pair's compiler macro).
METADATA-TOKEN, when non-nil, authenticates TYPE and supplies its GNU word;
TYPE-INDEX is TYPE's data-relocation index there (default 0).
NATIVE-CONSTRUCTOR, when non-nil, is called as (NATIVE-CONSTRUCTOR HANDLE
C-NAME NAME) in place of the default `nelisp-eln-native-subr-create' --
S6.25's hook for a leaf body shape (e.g. caar/cadr's
`nelisp-eln-native-subr-create-cxr', which needs its own extra
FIRST-DISP/D-RELOC-SLOT arguments and so cannot be that default) that a
preflight-admitted `nelisp-eln-registration--leaf-shape' already decided
was safe to build differently.
The returned plist contains its GNU :word and the canonical NeLisp
:callable. The raw GNU view is valid only until activation retirement."
  (setq arity (or arity 0))
  (setq activation
        (nelisp-eln-registration-objects--live-activation activation))
  ;; Arity 2 exists only for the Doc 207 chain leaf, whose admitted shape
  ;; always supplies its own NATIVE-CONSTRUCTOR.
  (unless (and (stringp name) (stringp c-name)
               (or (memq arity '(0 1)) (and (eql arity 2) native-constructor))
               (integerp doc-index)
               (<= 0 doc-index) (<= doc-index (1- (ash 1 63)))
               (= (length name) (string-bytes name))
               (= (length c-name) (string-bytes c-name)))
    (signal 'nelisp-eln-registration-objects-error
            (list 'invalid-registration-metadata name c-name doc-index)))
  (let ((metadata-type-word nil))
    (when metadata-token
      (require 'nelisp-eln-registration-metadata)
      (setq metadata-type-word
            (nelisp-eln-registration-metadata-type-word
             metadata-token type-index))
      (unless (and (integerp metadata-type-word)
                   (eq type
                       (nelisp-eln-registration-metadata-decode
                        metadata-token metadata-type-word)))
        (signal 'nelisp-eln-registration-objects-error
                (list 'metadata-type-provenance-mismatch))))
    (let* ((unit (aref activation 1))
         (handle (aref unit 1))
         (objects (aref unit 2))
         (capability (nelisp-eln-system-loader-function-capability handle c-name))
         (_key (list capability name c-name intspec command-modes doc-index
                     (unless metadata-token type) arity))
         (cached (cl-find-if
                  (lambda (entry)
                    (and (equal _key (aref entry 5))
                         (eq metadata-token
                             (and (> (length entry) 6) (aref entry 6)))))
                  (aref activation 4)))
         (native (or (and cached (aref cached 1))
                     (if native-constructor
                         (funcall native-constructor handle c-name name)
                       (nelisp-eln-native-subr-create
                        handle c-name name))))
         (memory nil) (symbol-owner nil) (c-name-owner nil)
         (address nil) (word nil) (entry nil) (new-entries nil)
         (new-cache nil) (ok nil))
    (unless (equal (func-arity native) (cons arity arity))
      (signal 'nelisp-eln-registration-objects-error
              (list 'native-callable-arity-mismatch
                    (func-arity native) arity)))
    (let ((failure nil) (result nil))
      (unwind-protect
          (condition-case err
              (progn
                (if cached
                    (setq entry cached word (aref cached 0)
                          result (list :word word :callable native :owner unit)
                          ok t)
                  (setq symbol-owner (nl-ffi-memory-cstring name)
                        c-name-owner (nl-ffi-memory-cstring c-name)
                        memory (nl-ffi-memory-allocate 88)
                        address (nl-ffi-memory-address memory))
                  (nelisp-eln-abi-write-word
                   address 0 (nelisp-eln-registration-objects--header 18 10 0))
                  (nelisp-eln-abi-write-word address 8 (nth 3 capability))
                  (nelisp-eln-registration-objects--write-short address 16 arity)
                  (nelisp-eln-registration-objects--write-short address 18 arity)
                  (nelisp-eln-abi-write-word
                   address 24 (nl-ffi-memory-address symbol-owner))
                  (nelisp-eln-abi-write-word
                   address 32 (nelisp-eln-objects-encode objects intspec))
                  (nelisp-eln-abi-write-word
                   address 40 (nelisp-eln-objects-encode objects command-modes))
                  (nelisp-eln-abi-write-word address 48 (- (- doc-index) 1))
                  (nelisp-eln-abi-write-word
                   address 56 (nelisp-eln-registration-objects-unit-word unit))
                  (nelisp-eln-abi-write-word
                   address 64 (nl-ffi-memory-address c-name-owner))
                  (nelisp-eln-abi-write-word address 72 0)
                  (nelisp-eln-abi-write-word
                   address 80
                   (if metadata-token
                       metadata-type-word
                     (nelisp-eln-objects-encode objects type)))
                  (setq word
                        (nelisp-eln-registration-objects--pointer-word address)
                        entry (vector word native memory symbol-owner
                                      c-name-owner _key metadata-token)
                        new-entries (cons entry (aref activation 3))
                        new-cache (cons entry (aref activation 4))
                        result (list :word word :callable native :owner unit))
                  ;; Allocate every reachable list before publication.  Once
                  ;; either activation slot changes, no allocating operation
                  ;; remains that could run failure cleanup on a live entry.
                  (aset activation 3 new-entries)
                  (aset activation 4 new-cache)
                  (setq ok t)))
            (error (setq failure err)))
        (unless ok
          ;; Each owner is attempted independently; failed releases remain
          ;; reachable for retry and cannot replace the constructor condition.
          (nelisp-eln-registration-objects--release-memory-or-retain memory)
          (nelisp-eln-registration-objects--release-memory-or-retain symbol-owner)
          (nelisp-eln-registration-objects--release-memory-or-retain c-name-owner)
          ;; Do not root a NativeSubr created by a failed constructor.
          (setq native nil)))
      (when failure
        (signal (car failure) (cdr failure)))
      result))))

(defun nelisp-eln-registration-objects-decode (activation word)
  "Decode temporary GNU WORD to its original NeLisp callable."
  (setq activation
        (nelisp-eln-registration-objects--live-activation activation))
  (let ((entries (aref activation 3)) found)
    (while (and entries (not found))
      (if (= word (aref (car entries) 0))
          (setq found (aref (car entries) 1))
        (setq entries (cdr entries))))
    (or found
        (signal 'nelisp-eln-registration-objects-error
                (list 'unknown-temporary-subr-word word)))))

(defun nelisp-eln-registration-objects-retire-activation (activation)
  "Retire all transient subr views and reverse mappings in ACTIVATION."
  (unless (and (vectorp activation) (= (length activation) 5)
               (eq (aref activation 0)
                   nelisp-eln-registration-objects--activation-marker)
               (memq (aref activation 2) '(open retiring)))
    (signal 'nelisp-eln-registration-objects-error
            (list 'stale-or-invalid-activation activation)))
  (nelisp-eln-registration-objects--live-unit (aref activation 1))
  (when (eq (aref activation 2) 'open)
    (aset activation 2 'retiring))
  (let ((entries (aref activation 3)) (unit (aref activation 1)))
    (while entries
      (let ((i 2))
        (while (< i 5)
          (when (aref (car entries) i)
            (nl-ffi-memory-release (aref (car entries) i))
            (aset (car entries) i nil))
          (setq i (1+ i))))
      (setq entries (cdr entries)))
    (aset activation 2 'closed)
    (aset activation 3 nil)
    (aset activation 4 nil)
    (aset unit 6 (delq activation (aref unit 6)))
    t))

(defun nelisp-eln-registration-objects-release-unit (unit)
  "Release UNIT after all registration activations have retired."
  (unless (and (vectorp unit) (= (length unit) 8)
               (eq (aref unit 0)
                   nelisp-eln-registration-objects--unit-marker)
               (memq unit nelisp-eln-registration-objects--live-units)
               (memq (aref unit 5) '(open closing)))
    (signal 'nelisp-eln-registration-objects-error
            (list 'stale-or-invalid-unit unit)))
  (when (aref unit 6)
    (signal 'nelisp-eln-registration-objects-error
            (list 'unit-has-active-registrations)))
  (when (and (not (aref unit 7))
             (fboundp 'nelisp--native-subr-live-count)
             (> (nelisp--native-subr-live-count
                 (plist-get
                  (nelisp-eln-system-loader--state (aref unit 1) t)
                  :module-id)) 0))
    (signal 'nelisp-eln-registration-objects-error
            (list 'unit-has-live-callables)))
  (aset unit 5 'closing)
  ;; The GNU comp_unit field points into the loaded module.  dlclose must run
  ;; before releasing the persistent unit view that contains that relocation.
  (unless (aref unit 7)
    (nelisp-eln-system-loader-close (aref unit 1))
    (aset unit 7 t))
  (when (aref unit 3)
    (nl-ffi-memory-release (aref unit 3))
    (aset unit 3 nil))
  (when (aref unit 2)
    (nelisp-eln-objects-release (aref unit 2))
    (aset unit 2 nil))
  (setq nelisp-eln-registration-objects--live-units
        (delq unit nelisp-eln-registration-objects--live-units))
  (aset unit 5 'closed)
  t)

(provide 'nelisp-eln-registration-objects)

;;; nelisp-eln-registration-objects.el ends here
