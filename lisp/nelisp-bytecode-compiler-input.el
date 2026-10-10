;;; nelisp-bytecode-compiler-input.el --- Materialized byte-code input adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Inspect already materialized GNU byte-code functions.  This module never
;; loads a compiled Lisp file or calls the function it inspects.

;;; Code:

(require 'cl-lib)
(require 'bytecomp)
(require 'json)
(require 'nelisp-bytecode-ir)
(require 'nelisp-bytecode-frame-ir)
(require 'nelisp-bytecode-handlers-u8)

(require 'nelisp-bytecode-compiler-input-dialect)

(defun nelisp-bytecode-compiler-input--argument-count (argument-list)
  "Return named byte-code arguments described by ARGUMENT-LIST."
  (let ((count 0) (tail argument-list) (done nil))
    (while (and (consp tail) (not done))
      (let ((item (car tail)))
        (cond ((eq item '&aux) (setq done t))
              ((memq item '(&optional &rest &key &allow-other-keys)) nil)
              ((symbolp item) (setq count (1+ count)))
              (t (setq done t))))
      (setq tail (cdr tail)))
    count))

(defun nelisp-bytecode-compiler-input--closure-template-descriptor-p (value)
  "Return non-nil for legacy ambiguous metadata shape VALUE."
  (and (consp value)
       (or (null (car value)) (stringp (car value)))
       (integerp (cdr value))
       (<= 0 (cdr value) most-positive-fixnum)))

(defun nelisp-bytecode-compiler-input-documentation-reference-p (value)
  "Return non-nil for the GNU 31.1 lazy documentation reference VALUE."
  (nelisp-bytecode-compiler-input--closure-template-descriptor-p value))

(defun nelisp-bytecode-compiler-input-potential-capture-placeholder-p (function)
  "Return non-nil when FUNCTION constants start with GNU's V0 placeholder.

This conservative guard can reject a genuine user literal named V0."
  (and (byte-code-function-p function)
       (let ((constants (aref function 2)))
         (and (vectorp constants) (> (length constants) 0)
              (eq (aref constants 0) 'V0)))))

(defun nelisp-bytecode-compiler-input--argument-descriptor-p (descriptor)
  "Return non-nil when DESCRIPTOR is a supported byte-code arg descriptor."
  (if (integerp descriptor)
      (and (<= 0 descriptor 65535)
           (let ((minimum (logand descriptor 127))
                 (maximum (ash descriptor -8))
                 (restp (/= 0 (logand descriptor 128))))
             (or restp (<= minimum maximum))))
    (let ((tail descriptor) (seen nil) (valid t))
      (while (and valid (consp tail))
        (if (memq tail seen)
            (setq valid nil)
          (push tail seen)
          (unless (symbolp (car tail))
            (setq valid nil))
          (setq tail (cdr tail))))
      (and valid (null tail)))))

(defun nelisp-bytecode-compiler-input--argument-bounds (descriptor)
  "Return the minimum and maximum arity encoded by DESCRIPTOR."
  (if (integerp descriptor)
      (list :min (logand descriptor 127)
            :max (unless (/= 0 (logand descriptor 128))
                   (ash descriptor -8)))
    (let ((tail descriptor) (minimum 0) (maximum 0)
          (optional nil) (rest nil) (done nil))
      (while (and (consp tail) (not done))
        (let ((item (car tail)))
          (cond
           ((eq item '&aux) (setq done t))
           ((eq item '&optional) (setq optional t))
           ((memq item '(&rest &key &allow-other-keys))
            (setq optional t rest t))
           ((symbolp item)
            (unless optional (setq minimum (1+ minimum)))
            (unless rest (setq maximum (1+ maximum)))))
          (setq tail (cdr tail))))
      (list :min minimum :max (unless rest maximum)))))

(defun nelisp-bytecode-compiler-input--argument-stack-depth (descriptor)
  "Return entry stack depth for DESCRIPTOR, or nil when its frame is unknown."
  (if (integerp descriptor)
      (if (/= 0 (logand descriptor 128))
          (let ((minimum (logand descriptor 127))
                (maximum (ash descriptor -8)))
            (and (<= minimum maximum) (1+ maximum)))
        (ash descriptor -8))
    (unless (memq '&rest descriptor)
      (unless (eq (plist-get
                   (nelisp-bytecode-compiler-input--argument-bounds descriptor)
                   :max)
                  nil)
        0))))

(defun nelisp-bytecode-compiler-input--rest-argument-p (descriptor)
  "Return non-nil when DESCRIPTOR carries a terminal `&rest' argument."
  (if (integerp descriptor)
      (/= 0 (logand descriptor 128))
    (memq '&rest descriptor)))

(defun nelisp-bytecode-compiler-input--rest-slot-return-p
    (descriptor code constants declared-depth)
  "Recognize only GNU's required-plus-REST slot-return byte-code shape."
  (let* ((bounds (nelisp-bytecode-compiler-input--argument-bounds descriptor))
         (required (plist-get bounds :min)))
    (and (consp descriptor) (memq '&rest descriptor)
         (= (length descriptor) (+ required 2))
         (null (plist-get bounds :max))
         (= declared-depth 1)
         (stringp code) (= (length code) 2)
         (= (aref code 0) 8) (= (aref code 1) 135)
         (= (length constants) 1)
         (eq (aref constants 0) (car (last descriptor))))))

(defun nelisp-bytecode-compiler-input--rest-layout-valid-p
    (descriptor initial-depth layout)
  "Check that LAYOUT puts DESCRIPTOR's rest list after required arguments."
  (let* ((bounds (nelisp-bytecode-compiler-input--argument-bounds descriptor))
         (required (plist-get bounds :min))
         (maximum (if (integerp descriptor)
                      (ash descriptor -8)
                    (plist-get bounds :max)))
         (layout-required (plist-get layout :required-argument-count))
         (layout-index (plist-get layout :rest-binding-stack-index)))
    (and (nelisp-bytecode-compiler-input--rest-argument-p descriptor)
         (integerp required)
         (or (and (integerp maximum) (>= maximum required))
             (and (null maximum) (consp descriptor)))
         (integerp initial-depth)
         (integerp layout-required)
         (integerp layout-index)
         (= initial-depth (1+ (or maximum required)))
         (= layout-required required)
         (= layout-index (or maximum required)))))

(defun nelisp-bytecode-compiler-input--switch-ir-supported-p (ir frame constants)
  "Whether IR's unsupported markers are exactly frame-verified Bswitch data.

The structural IR deliberately does not lower table-driven control flow or
boxed constants.  The frame IR separately verifies Bswitch targets and table
state; accept these markers for the runtime lookup lane. Dynamic tables
are validated at execution, and boxed constants remain in protected roots."
  (let ((switch-pcs nil) (table-indices nil)
        (unsupported (plist-get ir :unsupported))
        (instructions (plist-get ir :instructions)))
    (dolist (block (append (plist-get frame :blocks) nil))
      (dolist (instruction (append (plist-get block :instructions) nil))
        (when (eq (plist-get instruction :kind) 'switch)
          (push (plist-get instruction :pc) switch-pcs)
          (push (plist-get instruction :table-constant-index) table-indices))))
    (and switch-pcs
         (consp unsupported)
         (eq (plist-get frame :status) 'complete)
         (eq (plist-get ir :status) 'unsupported)
         (vectorp instructions)
         (cl-every (lambda (pc)
                     (member (cons pc 'unsupported-semantics) unsupported))
                   switch-pcs)
         (cl-every
          (lambda (marker)
            (let* ((pc (car marker)) (reason (cdr marker))
                   (row (cl-find pc instructions :key (lambda (item) (aref item 0))))
                   (opcode (and row (aref row 1)))
                   (metadata (and row (aref row 4)))
                   (constant-index (plist-get metadata :constant-index)))
              (cond
               ((eq reason 'unsupported-semantics)
                (and (integerp opcode) (= opcode 183) (memq pc switch-pcs)))
               ((eq reason 'non-fixnum-constant)
                (and (integerp constant-index) (<= 0 constant-index)
                     (< constant-index (length constants))))
               (t nil))))
          unsupported))))

(defun nelisp-bytecode-compiler-input--call1-template-p
    (function frame initial-depth)
  "Return non-nil only for the fixed GNU 31.1 symbol-call template."
  (let* ((constants (and (byte-code-function-p function) (aref function 2)))
         (callee (and (vectorp constants) (= (length constants) 1)
                      (aref constants 0))))
    (and (byte-code-function-p function)
         (integerp (aref function 0))
         (= (aref function 0) 257)
         (equal (aref function 1) (unibyte-string 192 1 33 135))
         (symbolp callee) callee
         (eq callee (intern-soft (symbol-name callee)))
         (= initial-depth 1) (= (aref function 3) 3)
         (eq (plist-get frame :status) 'complete)
         (= (or (plist-get frame :max-stack-depth) -1) 3))))

(defun nelisp-bytecode-compiler-input--discard-ir-supported-p (ir frame)
  "Whether IR's unsupported markers are only frame-verified discard ops.

The structural IR keeps opcode 136 outside its general lowering claim.  This
adapter admits it only for this frame-verified CFG path, whose backend models
discard by removing the top slot from the live stack map."
  (let ((unsupported (plist-get ir :unsupported))
        (instructions (plist-get ir :instructions))
        (discard-pcs nil))
    (dolist (block (append (plist-get frame :blocks) nil))
      (dolist (instruction (append (plist-get block :instructions) nil))
        (when (eq (plist-get instruction :kind) 'discard)
          (push (plist-get instruction :pc) discard-pcs))))
    (and discard-pcs
         (consp unsupported)
         (eq (plist-get frame :status) 'complete)
         (eq (plist-get ir :status) 'unsupported)
         (vectorp instructions)
         (cl-every (lambda (pc)
                     (member (cons pc 'unsupported-semantics) unsupported))
                   discard-pcs)
         (cl-every
          (lambda (marker)
            (let* ((pc (car marker))
                   (row (cl-find pc instructions :key (lambda (item) (aref item 0)))))
              (and (eq (cdr marker) 'unsupported-semantics)
                   (member pc discard-pcs)
                   row (= (aref row 1) 136))))
          unsupported))))

(defun nelisp-bytecode-compiler-input--cons-ir-supported-p (ir frame)
  "Whether IR's unsupported markers are frame-verified CONS and discard.

This admits opcode 66 to the verified input contract. Native lowering still
recognizes only the pinned two-argument CONS template; other uses remain
unsupported by the compiler."
  (let ((unsupported (plist-get ir :unsupported))
        (instructions (plist-get ir :instructions))
        (cons-pcs nil)
        (admitted nil))
    (dolist (block (append (plist-get frame :blocks) nil))
      (dolist (instruction (append (plist-get block :instructions) nil))
        (when (= (plist-get instruction :opcode) 66)
          (push (plist-get instruction :pc) cons-pcs))
        (when (or (= (plist-get instruction :opcode) 66)
                  (and (= (plist-get instruction :opcode) 136)
                       (eq (plist-get instruction :kind) 'discard)))
          (push (cons (plist-get instruction :pc)
                      (plist-get instruction :opcode)) admitted))))
    (and cons-pcs
         (consp unsupported)
         (eq (plist-get frame :status) 'complete)
         (eq (plist-get ir :status) 'unsupported)
         (vectorp instructions)
         (cl-every (lambda (entry)
                     (member (cons (car entry) 'unsupported-semantics) unsupported))
                   admitted)
         (cl-every
          (lambda (marker)
            (let* ((pc (car marker))
                   (entry (assq pc admitted))
                   (row (cl-find pc instructions
                                 :key (lambda (item) (aref item 0)))))
              (and (eq (cdr marker) 'unsupported-semantics)
                   entry row (= (aref row 1) (cdr entry)))))
          unsupported))))

(defun nelisp-bytecode-compiler-input--call-ir-supported-p (ir frame)
  "Admit frame-verified CALL-family instructions mixed with CONS/discard.
This is an input verification claim only; it grants no native lowering or
runtime capability. Every unsupported marker must match both verifier views."
  (let ((unsupported (plist-get ir :unsupported))
        (rows (plist-get ir :instructions)) (admitted nil) (calls nil))
    (dolist (block (append (plist-get frame :blocks) nil))
      (dolist (instruction (append (plist-get block :instructions) nil))
        (let ((opcode (plist-get instruction :opcode)))
          (when (or (and (<= 32 opcode) (<= opcode 39))
                    (memq opcode '(66 136)))
            (push instruction admitted)
            (when (<= opcode 39) (push instruction calls))))))
    (and calls (consp unsupported) (vectorp rows)
         (eq (plist-get ir :status) 'unsupported)
         (eq (plist-get frame :status) 'complete)
         (cl-every
          (lambda (instruction)
            (let* ((pc (plist-get instruction :pc))
                   (opcode (plist-get instruction :opcode))
                   (row (cl-find pc rows :key (lambda (item) (aref item 0))))
                   (kind (plist-get instruction :kind))
                   (nargs (plist-get instruction :operand)))
              (and (member (cons pc 'unsupported-semantics) unsupported)
                   row (= (aref row 1) opcode)
                   (cond
                    ((<= opcode 39)
                     (and (eq kind 'call)
                          (eq (plist-get (aref row 4) :kind) 'call)
                          (integerp nargs) (>= nargs 0)
                          (equal (aref row 3) nargs)
                          (or (>= opcode 38) (= nargs (- opcode 32)))
                          (equal (plist-get (aref row 4) :stack-delta) (- nargs))
                          (= (length (plist-get instruction :inputs)) (1+ nargs))
                          (= (length (plist-get instruction :outputs)) 1)))
                    ((= opcode 66)
                     (and (eq kind 'primitive)
                          (eq (plist-get (aref row 4) :kind) 'cons)
                          (= (length (plist-get instruction :inputs)) 2)
                          (= (length (plist-get instruction :outputs)) 1)))
                    (t (and (eq kind 'discard)
                            (eq (plist-get (aref row 4) :kind) 'discard)
                            (= (length (plist-get instruction :inputs)) 1)
                            (null (plist-get instruction :outputs))))))))
          admitted)
         (cl-every (lambda (marker)
                     (and (eq (cdr marker) 'unsupported-semantics)
                          (cl-find (car marker) admitted
                                   :key (lambda (instruction)
                                          (plist-get instruction :pc)))))
                   unsupported))))

(defun nelisp-bytecode-compiler-input-build (function)
  "Inspect materialized byte-code FUNCTION without executing it.

Return a plist with :status (`complete', `malformed', or `unsupported'),
identity-preserving :function, original metadata fields, and the results of
the byte-code IR and frame verifier. Complete means verified input, not native
lowering or runtime admission. The bounded packed rest shape records
its required count and rest-list stack slot in :argument-layout; variable
arity remains distinct from :argument-count.  The legacy
:closure-template-descriptor field preserves its old shape; a GNU 31.1
(nil|string . offset) value is lazy documentation metadata, not proof of
runtime captures.  Source forms and .elc loading are not part of this API."
  (let ((dialect (nelisp-bytecode-compiler-input--dialect)))
    (cond
     ((not (byte-code-function-p function))
      (list :status 'malformed :reason "input is not a byte-code function"
            :function function :dialect-evidence dialect))
     ((not (eq (plist-get dialect :status) 'pinned))
      (list :status 'unsupported :reason (plist-get dialect :reason)
            :function function :dialect-evidence dialect))
     ((or (< (length function) 4) (> (length function) 6)
          (not (nelisp-bytecode-compiler-input--argument-descriptor-p
                (aref function 0)))
          (not (stringp (aref function 1)))
          (not (vectorp (aref function 2)))
          (not (integerp (aref function 3)))
          (< (aref function 3) 0)
          (and (> (length function) 4)
               (let ((metadata (aref function 4)))
                 (and metadata (not (stringp metadata))
                      (not (nelisp-bytecode-compiler-input-documentation-reference-p
                            metadata))))))
      (list :status 'malformed :reason "invalid byte-code function fields"
            :function function :dialect-evidence dialect))
     (t
      (let* ((code (aref function 1)) (constants (aref function 2))
             (declared-depth (aref function 3))
             (argument-descriptor (aref function 0))
             (argument-list (and (listp argument-descriptor)
                                 argument-descriptor))
             (argument-bounds
              (nelisp-bytecode-compiler-input--argument-bounds
               argument-descriptor))
             (rest-argument-p
              (nelisp-bytecode-compiler-input--rest-argument-p
               argument-descriptor))
             (required-argument-count (plist-get argument-bounds :min))
             (rest-slot-return-p
              (nelisp-bytecode-compiler-input--rest-slot-return-p
               argument-descriptor code constants declared-depth))
             (rest-layout-supported-p
              (or (and rest-argument-p (integerp argument-descriptor)
                       (<= required-argument-count
                           (ash argument-descriptor -8)))
                  rest-slot-return-p))
             (rest-binding-stack-index
              (and rest-layout-supported-p
                   (if (integerp argument-descriptor)
                       (ash argument-descriptor -8) required-argument-count)))
             (argument-layout
              (and rest-layout-supported-p
                   (list :required-argument-count required-argument-count
                         :rest-binding-stack-index rest-binding-stack-index)))
             (argument-count
              (if argument-list
                  (nelisp-bytecode-compiler-input--argument-count argument-list)
                (and (equal (plist-get argument-bounds :min)
                            (plist-get argument-bounds :max))
                     (plist-get argument-bounds :min))))
             (initial-depth
              (or (and rest-slot-return-p (1+ required-argument-count))
                  (nelisp-bytecode-compiler-input--argument-stack-depth
                   argument-descriptor)))
             (rest-native-code
              (and rest-slot-return-p (unibyte-string 135)))
             (ir (nelisp-bytecode-ir-validate code constants initial-depth))
             (frame
              (if (integerp initial-depth)
                  (funcall (if (cl-some (lambda (row) (memq (aref row 1) '(48 49 50)))
                                        (append (plist-get ir :instructions) nil))
                               #'nelisp-bytecode-handlers-u8-build
                             #'nelisp-bytecode-frame-ir-build)
                   (or rest-native-code code)
                   (if rest-native-code [] constants) initial-depth)
                (list :status 'unsupported
                      :reason "unbounded argument descriptor has no fixed stack depth"
                      :blocks [])))
             (frame
              (if (and argument-layout
                       (nelisp-bytecode-compiler-input--rest-layout-valid-p
                        argument-descriptor initial-depth argument-layout))
                  (plist-put frame :argument-layout argument-layout)
                frame))
             (frame-status (plist-get frame :status))
             (switch-ir-supported
              (nelisp-bytecode-compiler-input--switch-ir-supported-p
               ir frame constants))
             (discard-ir-supported
              (nelisp-bytecode-compiler-input--discard-ir-supported-p ir frame))
             (cons-ir-supported
              (nelisp-bytecode-compiler-input--cons-ir-supported-p ir frame))
             (call-ir-supported
              (nelisp-bytecode-compiler-input--call-ir-supported-p ir frame))
             (call1-template
              (nelisp-bytecode-compiler-input--call1-template-p
               function frame initial-depth))
             (status (cond
                      ((and argument-layout
                            (not (nelisp-bytecode-compiler-input--rest-layout-valid-p
                                  argument-descriptor initial-depth
                                  argument-layout)))
                       'malformed)
                      ((null initial-depth) 'unsupported)
                      ((or (eq (plist-get ir :status) 'malformed)
                           (eq frame-status 'malformed)) 'malformed)
                      ((or (and (eq (plist-get ir :status) 'unsupported)
                                (not (or switch-ir-supported
                                         discard-ir-supported
                                         cons-ir-supported
                                         call-ir-supported
                                         call1-template)))
                           (eq frame-status 'unsupported)) 'unsupported)
                      ((and (eq frame-status 'complete)
                            (not rest-slot-return-p)
                            (> (or (plist-get frame :max-stack-depth) 0)
                               declared-depth))
                       'malformed)
                      (t 'complete)))
             (reason (or (and argument-layout
                              (not (nelisp-bytecode-compiler-input--rest-layout-valid-p
                                    argument-descriptor initial-depth
                                    argument-layout))
                              "invalid rest argument frame layout")
                         (and (null initial-depth)
                              "unbounded argument descriptor has no fixed stack depth")
                         (and (eq (plist-get ir :status) 'malformed)
                              (plist-get ir :reason))
                         (and (memq frame-status '(malformed unsupported))
                              (plist-get frame :reason))
                         (and (eq status 'malformed)
                              "computed stack depth exceeds declared depth"))))
        (list :status status :reason reason :function function
              :call1-symbol-template-p call1-template
              :argument-descriptor argument-descriptor
              :argument-list argument-list :argument-count argument-count
              :argument-min (plist-get argument-bounds :min)
              :argument-max (plist-get argument-bounds :max)
              :rest-argument-p rest-argument-p
              :rest-slot-return-template-p rest-slot-return-p
              :rest-native-code rest-native-code
              :required-argument-count required-argument-count
              :rest-binding-stack-index rest-binding-stack-index
              :argument-layout argument-layout
              :initial-stack-depth initial-depth
              :code code :constants constants
              :declared-stack-depth declared-depth
              :computed-temporary-stack-depth
              (and (integerp initial-depth)
                   (max 0 (or (plist-get frame :max-stack-depth)
                              initial-depth)))
              :doc (and (> (length function) 4)
                        (stringp (aref function 4)) (aref function 4))
              :documentation-reference
              (and (> (length function) 4)
                   (nelisp-bytecode-compiler-input-documentation-reference-p
                    (aref function 4))
                   (aref function 4))
              :metadata-role
              (cond ((and (> (length function) 4)
                          (nelisp-bytecode-compiler-input-documentation-reference-p
                           (aref function 4)))
                     'lazy-documentation-reference)
                    ((and (> (length function) 4) (stringp (aref function 4)))
                     'docstring)
                    (t 'none))
              :potential-capture-placeholder-p
              (nelisp-bytecode-compiler-input-potential-capture-placeholder-p
               function)
              :closure-template-descriptor
              (and (> (length function) 4)
                   (nelisp-bytecode-compiler-input--closure-template-descriptor-p
                    (aref function 4))
                   (aref function 4))
              :capture-values-available nil
              :interactive (and (> (length function) 5) (aref function 5))
              :dialect-evidence dialect :ir-result ir :frame-result frame))))))

(defun nelisp-bytecode-compiler-input-native-package-dependencies ()
  "Return compiler-input function identities guarded by native packages."
  (append
   (when nelisp-bytecode-compiler-input--runtime-source-evaluator
     '(nelisp--eval-source-string))
   '(nelisp-bytecode-compiler-input-inventory-sha256
    nelisp-bytecode-compiler-input-root
    nelisp-bytecode-compiler-input--sha256-file
    nelisp-bytecode-compiler-input--standalone-runtime-p
    subrp funcall eq equal secure-hash copy-sequence string-bytes length
    nelisp-bytecode-compiler-input-native-package-runtime-context
    nelisp-bytecode-compiler-input--dialect
    nelisp-bytecode-compiler-input-dialect
    nelisp-bytecode-compiler-input--argument-count
    nelisp-bytecode-compiler-input--closure-template-descriptor-p
    nelisp-bytecode-compiler-input-documentation-reference-p
    nelisp-bytecode-compiler-input-potential-capture-placeholder-p
    nelisp-bytecode-compiler-input--argument-descriptor-p
    nelisp-bytecode-compiler-input--argument-bounds
    nelisp-bytecode-compiler-input--argument-stack-depth
    nelisp-bytecode-compiler-input--rest-argument-p
    nelisp-bytecode-compiler-input--rest-slot-return-p
    nelisp-bytecode-compiler-input--rest-layout-valid-p
    nelisp-bytecode-compiler-input--switch-ir-supported-p
    nelisp-bytecode-compiler-input--call1-template-p
    nelisp-bytecode-compiler-input--discard-ir-supported-p
    nelisp-bytecode-compiler-input--cons-ir-supported-p
    nelisp-bytecode-compiler-input--call-ir-supported-p
    nelisp-bytecode-compiler-input-build)))

(provide 'nelisp-bytecode-compiler-input)
;;; nelisp-bytecode-compiler-input.el ends here
