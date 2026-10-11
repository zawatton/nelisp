;;; nelisp-gnu-bytecode-vm.el --- bounded GNU ELC to NeLisp VM bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Experimental, source-free GNU Emacs 31.1 `.elc' loader.  It lowers a
;; deliberately small byte-code subset into the existing NeLisp BCL VM so
;; global reads/writes and function calls use NeLisp's public runtime
;; stores. Unsupported forms/opcodes fail before any action
;; from the file is evaluated.  This is not a general GNU `.elc' loader.

;;; Code:

(require 'nelisp-load)
(require 'nelisp-bytecode)
(require 'cl-lib)

(define-error 'nelisp-gnu-bytecode-vm-error
  "Unsupported GNU byte-code for NeLisp VM")

(defconst nelisp-gnu-bytecode-vm--unbound
  (make-symbol "nelisp-gnu-bytecode-vm-unbound")
  "Private sentinel used to distinguish absent NeLisp variable values.")

(defun nelisp-gnu-bytecode-vm--symbol-value-intrinsic (symbol)
  "Read SYMBOL through the NeLisp variable store, independent of its function cell."
  (unless (symbolp symbol)
    (signal 'wrong-type-argument (list 'symbolp symbol)))
  (let ((value (nelisp-variable-get symbol nelisp-gnu-bytecode-vm--unbound)))
    (if (eq value nelisp-gnu-bytecode-vm--unbound)
        (signal 'void-variable (list symbol))
      value)))

(defconst nelisp-gnu-bytecode-vm--symbol-value-intrinsic-callable
  (symbol-function 'nelisp-gnu-bytecode-vm--symbol-value-intrinsic)
  "Captured runtime provider used by GNU's intrinsic SYMBOL_VALUE opcode.")

(defconst nelisp-gnu-bytecode-vm--eq-callable (symbol-function 'eq)
  "Captured EQ primitive used by the intrinsic NCONC implementation.")
(defconst nelisp-gnu-bytecode-vm--consp-callable (symbol-function 'consp)
  "Captured CONSP primitive used by the intrinsic NCONC implementation.")
(defconst nelisp-gnu-bytecode-vm--cdr-callable (symbol-function 'cdr)
  "Captured CDR primitive used by the intrinsic NCONC implementation.")
(defconst nelisp-gnu-bytecode-vm--setcdr-callable (symbol-function 'setcdr)
  "Captured SETCDR primitive used by the intrinsic NCONC implementation.")

(defun nelisp-gnu-bytecode-vm--nconc-intrinsic (first second)
  "Apply GNU's destructive two-argument `nconc' bytecode operation.

This operation deliberately bypasses the public `nconc' function cell, as
GNU's Bnconc instruction does.  A non-nil FIRST must be a cons.  Its final
cons cdr is replaced by SECOND, including when FIRST originally had a dotted
tail."
  (cond
   ((funcall nelisp-gnu-bytecode-vm--eq-callable first nil) second)
   ((not (funcall nelisp-gnu-bytecode-vm--consp-callable first))
    (signal 'wrong-type-argument (list 'consp first)))
   (t
    (let ((tail first))
      (while (funcall nelisp-gnu-bytecode-vm--consp-callable
                      (funcall nelisp-gnu-bytecode-vm--cdr-callable tail))
        (setq tail (funcall nelisp-gnu-bytecode-vm--cdr-callable tail)))
      (funcall nelisp-gnu-bytecode-vm--setcdr-callable tail second)
      first))))

(defconst nelisp-gnu-bytecode-vm--nconc-intrinsic-callable
  (symbol-function 'nelisp-gnu-bytecode-vm--nconc-intrinsic)
  "Captured runtime provider for GNU's intrinsic NCONC opcode.")

(defvar nelisp-gnu-bytecode-vm--callable-signatures (make-hash-table :test 'eq)
  "Trusted callable signature records keyed by function symbol.
Each value is (CALLABLE . ARITIES); the VM checks CALLABLE against the
current function-store cell before admitting any recorded arity.")

(defvar nelisp-gnu-bytecode-vm--host-read-from-string
  (and (fboundp 'read-from-string) (symbol-function 'read-from-string)))
(defvar nelisp-gnu-bytecode-vm--host-check-parens
  (and (fboundp 'check-parens) (symbol-function 'check-parens)))

(defun nelisp-gnu-bytecode-vm--signal (reason &rest details)
  "Signal a bounded bridge error with REASON and DETAILS."
  (signal 'nelisp-gnu-bytecode-vm-error
          (list :reason reason :details details)))

(defun nelisp-gnu-bytecode-vm--skip-trivia (text pos &optional check-holder)
  "Return the first non-whitespace, non-comment position at or after POS."
  (let ((len (length text)) done)
    (while (not done)
      (while (and (< pos len)
                  (memq (aref text pos) '(?\s ?\t ?\n ?\r ?\f 31)))
        (setq pos (1+ pos)))
      (if (and (< pos len) (= (aref text pos) ?\;))
          (progn
            (while (and (< pos len) (/= (aref text pos) ?\n))
              (setq pos (1+ pos)))
            (when (< pos len) (setq pos (1+ pos))))
        (if (and (< (1+ pos) len)
                 (= (aref text pos) ?#)
                 (= (aref text (1+ pos)) ?@))
            (let ((start pos))
              (setq pos (nelisp-gnu-bytecode-vm--skip-docstring text pos))
              (when check-holder
                (setcar check-holder
                        (concat (substring (car check-holder) 0 start)
                                (nelisp-gnu-bytecode-vm--mask-docstring
                                 text start pos)
                                (substring (car check-holder) pos)))))
          (setq done t))))
    pos))

(defun nelisp-gnu-bytecode-vm--mask-docstring (text start end)
  "Return whitespace preserving newlines for TEXT's range START to END."
  (let (chars)
    (while (< start end)
      (push (if (memq (aref text start) '(?\n ?\r))
                (aref text start)
              ?\s)
            chars)
      (setq start (1+ start)))
    (apply #'string (nreverse chars))))

(defun nelisp-gnu-bytecode-vm--skip-docstring (text start)
  "Skip one GNU ELC #@ byte-count marker at START in TEXT.
The count covers bytes immediately after the digits.  The docstring must
end at a unit separator (byte 31), either inside the counted range or
after whitespace following that range."
  (let* ((len (length text))
         (digits-start (+ start 2))
         (digits-end digits-start))
    (while (and (< digits-end len)
                (<= ?0 (aref text digits-end))
                (<= (aref text digits-end) ?9))
      (setq digits-end (1+ digits-end)))
    (unless (> digits-end digits-start)
      (nelisp-gnu-bytecode-vm--signal 'malformed-docstring-marker start))
    (let ((remaining (string-to-number
                     (substring text digits-start digits-end)))
          (pos digits-end))
      (while (> remaining 0)
        (when (>= pos len)
          (nelisp-gnu-bytecode-vm--signal 'truncated-docstring-marker start))
        (let ((bytes (string-bytes (substring text pos (1+ pos)))))
          (when (> bytes remaining)
            (nelisp-gnu-bytecode-vm--signal 'misaligned-docstring-marker
                                            start remaining))
          (setq remaining (- remaining bytes)
                pos (1+ pos))))
      (let ((terminator pos))
        (while (and (< terminator len)
                    (memq (aref text terminator) '(?\s ?\t ?\n ?\r ?\f)))
          (setq terminator (1+ terminator)))
        (unless (or (and (> pos digits-end)
                         (= (aref text (1- pos)) 31))
                    (and (< terminator len)
                         (= (aref text terminator) 31)))
          (nelisp-gnu-bytecode-vm--signal 'malformed-docstring-marker start)))
      pos)))

(defun nelisp-gnu-bytecode-vm--read-all (text)
  "Read every form from TEXT before the caller may execute any of them."
  (unless (functionp nelisp-gnu-bytecode-vm--host-read-from-string)
    (nelisp-gnu-bytecode-vm--signal 'reader-unavailable))
  (unless (functionp nelisp-gnu-bytecode-vm--host-check-parens)
    (nelisp-gnu-bytecode-vm--signal 'strict-completeness-check-unavailable))
  (let* ((check-holder (list text))
         (pos 0) (len (length text)) forms)
    (while (< (setq pos (nelisp-gnu-bytecode-vm--skip-trivia
                         text pos check-holder)) len)
      (let ((start pos) parsed)
        (condition-case err
            (setq parsed (funcall nelisp-gnu-bytecode-vm--host-read-from-string
                                  text pos))
          (error
           (nelisp-gnu-bytecode-vm--signal 'malformed-form start err)))
        (unless (and (consp parsed) (> (cdr parsed) start))
          (nelisp-gnu-bytecode-vm--signal 'reader-made-no-progress start))
        (push (car parsed) forms)
        (setq pos (cdr parsed))))
    (let ((original (current-buffer))
          (buffer (generate-new-buffer " *nelisp-gnu-elc-check*")))
      (unwind-protect
          (progn
            (set-buffer buffer)
            (insert (car check-holder))
            (condition-case err
                (funcall nelisp-gnu-bytecode-vm--host-check-parens)
              (error
               (nelisp-gnu-bytecode-vm--signal 'malformed-file err))))
        (set-buffer original)
        (kill-buffer buffer)))
    (nreverse forms)))

(defun nelisp-gnu-bytecode-vm--symbol-constant (constants index pc)
  "Return a valid variable symbol from CONSTANTS at INDEX for PC."
  (unless (and (< index (length constants))
               (symbolp (aref constants index))
               (not (memq (aref constants index) '(nil t)))
               (not (keywordp (aref constants index))))
    (nelisp-gnu-bytecode-vm--signal 'bad-variable-constant pc index))
  index)

(defun nelisp-gnu-bytecode-vm--fixed-bcl-arity (function)
  "Return FUNCTION's fixed positional arity, or nil if it is not fixed BCL."
  (when (nelisp-bcl-p function)
    (let ((params (nelisp-bc-params function)))
      (when (and (proper-list-p params)
                 (cl-every #'symbolp params)
                 (= (length params)
                    (length (delete-dups (copy-sequence params))))
                 (not (cl-intersection params
                                       '(nil t &optional &rest &key &aux
                                         &whole &body &environment))))
        (length params)))))

(defun nelisp-gnu-bytecode-vm--bcl-supported-arities (function)
  "Return safe CALL arities for FUNCTION's simple BCL parameter list.
Support required parameters, optional parameters, and a final `&rest'
parameter.  Other lambda-list markers and malformed parameter lists fail."
  (when (nelisp-bcl-p function)
    (let ((params (nelisp-bc-params function))
          (mode 'required) (required 0) (optional 0) (rest nil) (names nil)
          valid)
      (setq valid (proper-list-p params))
      (while (and valid params)
        (let ((parameter (pop params)))
          (cond
           ((eq parameter '&optional)
            (if (eq mode 'required) (setq mode 'optional) (setq valid nil)))
           ((eq parameter '&rest)
            (if (and (not rest) params)
                (let ((name (pop params)))
                  (if (and (symbolp name) (not (memq name '(nil t)))
                           (not (keywordp name))
                           (not (string-prefix-p "&" (symbol-name name)))
                           (not (memq name names))
                           (null params))
                      (setq rest t mode 'rest names (cons name names))
                    (setq valid nil)))
              (setq valid nil)))
           ((and (symbolp parameter)
                 (string-prefix-p "&" (symbol-name parameter)))
            (setq valid nil))
           ((and (symbolp parameter) (not (memq parameter '(nil t)))
                 (not (keywordp parameter)) (not (memq parameter names))
                 (not (eq mode 'rest)))
            (push parameter names)
            (if (eq mode 'required)
                (setq required (1+ required))
              (setq optional (1+ optional))))
           (t (setq valid nil)))))
      (when (and valid (or rest (<= (+ required optional) 15)))
        (let ((maximum (if rest 15 (+ required optional))) arities)
          (while (<= required maximum)
            (push required arities)
            (setq required (1+ required)))
          (nreverse arities))))))

(defun nelisp-gnu-bytecode-vm-register-callable-signatures
    (symbol callable arities)
  "Register CALLABLE's explicitly supported GNU CALL ARITIES for SYMBOL.
CALLABLE must be the currently installed BCL function object.  This is for
trusted bootstrap providers whose BCL lambda list has optional arguments;
ordinary unregistered functions retain the fixed-arity-only rule."
  (let ((absent (make-symbol "nelisp-gnu-bytecode-vm-absent")))
    (unless (and (symbolp symbol)
               (nelisp-gnu-bytecode-vm--bcl-supported-arities callable)
               (proper-list-p arities)
               arities
               (cl-every (lambda (arity)
                           (and (integerp arity) (<= 0 arity 15)
                                (memq arity
                                      (nelisp-gnu-bytecode-vm--bcl-supported-arities
                                       callable))))
                         arities)
               (= (length arities)
                  (length (delete-dups (copy-sequence arities))))
               (nelisp-eval-function-store-ready-p)
               (eq (nelisp-eval-function-cell-ref symbol absent) callable))
      (nelisp-gnu-bytecode-vm--signal
       'invalid-callable-signature-registration symbol arities))
    (puthash symbol (cons callable (copy-sequence arities))
             nelisp-gnu-bytecode-vm--callable-signatures)
    symbol))

(defun nelisp-gnu-bytecode-vm-install-callable-bundle (entries)
  "Atomically install callable BCL ENTRIES, each (SYMBOL BCL ARITIES).
Prevalidate every exact BCL signature before changing the function store;
restore both function cells and registry records if installation errors."
  (unless (and (nelisp-eval-function-store-ready-p)
               (hash-table-p nelisp-gnu-bytecode-vm--callable-signatures)
               (proper-list-p entries))
    (nelisp-gnu-bytecode-vm--signal 'invalid-callable-bundle))
  (let ((seen (make-hash-table :test 'eq)) (absent (make-symbol "absent"))
        prepared old-functions old-signatures committed)
    (dolist (entry entries)
      (unless (and (proper-list-p entry) (= (length entry) 3)
                   (symbolp (nth 0 entry))
                   (nelisp-bcl-p (nth 1 entry))
                   (proper-list-p (nth 2 entry)) (nth 2 entry)
                   (cl-every (lambda (arity)
                               (and (integerp arity) (<= 0 arity 15)
                                    (memq arity
                                          (nelisp-gnu-bytecode-vm--bcl-supported-arities
                                           (nth 1 entry)))))
                             (nth 2 entry))
                   (= (length (nth 2 entry))
                      (length (delete-dups (copy-sequence (nth 2 entry)))))
                   (not (gethash (nth 0 entry) seen)))
        (nelisp-gnu-bytecode-vm--signal 'invalid-callable-bundle-entry entry))
      (puthash (nth 0 entry) t seen)
      (push entry prepared))
    (setq prepared (nreverse prepared))
    (setq old-functions
          (nelisp-eval-function-cell-snapshot (mapcar #'car prepared)))
    (unwind-protect
        (progn
          (dolist (entry prepared)
            (push (cons (car entry)
                        (gethash (car entry)
                                 nelisp-gnu-bytecode-vm--callable-signatures
                                 absent))
                  old-signatures))
          (dolist (entry prepared)
            (nelisp-eval-function-cell-put (nth 0 entry) (nth 1 entry))
            (nelisp-gnu-bytecode-vm-register-callable-signatures
             (nth 0 entry) (nth 1 entry) (nth 2 entry)))
          (setq committed t)
          t)
      (unless committed
        (nelisp-eval-function-cell-restore old-functions)
        (dolist (old old-signatures)
          (if (eq (cdr old) absent)
              (remhash (car old) nelisp-gnu-bytecode-vm--callable-signatures)
            (puthash (car old) (cdr old)
                     nelisp-gnu-bytecode-vm--callable-signatures)))))))

(defun nelisp-gnu-bytecode-vm--live-call-signatures ()
  "Snapshot fixed-arity BCL signatures currently in NeLisp's function table."
  (unless (nelisp-eval-function-store-ready-p)
    (nelisp-gnu-bytecode-vm--signal 'function-store-unavailable))
  (let ((signatures (make-hash-table :test 'eq))
        (absent (make-symbol "nelisp-gnu-bytecode-vm-absent")))
    (dolist (entry (nelisp-eval-function-cell-snapshot-all))
       (let ((symbol (car entry))
            (function (cdr entry)))
       (let ((arity (nelisp-gnu-bytecode-vm--fixed-bcl-arity function)))
         (when arity (puthash symbol arity signatures)))))
    (maphash
     (lambda (symbol registration)
       (let ((callable (car registration)))
         (when (and (nelisp-bcl-p callable)
                    (eq (nelisp-eval-function-cell-ref symbol absent) callable))
           (puthash symbol (copy-sequence (cdr registration)) signatures))))
     nelisp-gnu-bytecode-vm--callable-signatures)
    signatures))

(defun nelisp-gnu-bytecode-vm--known-call-target
    (symbol nargs pc signatures forward-definitions)
  "Validate SYMBOL's earlier fixed-arity BCL signature during preflight."
  (when (gethash symbol forward-definitions)
    (nelisp-gnu-bytecode-vm--signal 'forward-call-target pc symbol))
  (let ((arity (gethash symbol signatures :nelisp-no-known-call-signature)))
    (when (eq arity :nelisp-no-known-call-signature)
      (nelisp-gnu-bytecode-vm--signal 'unsupported-call-target pc symbol))
    (unless (if (integerp arity)
                (= arity nargs)
              (and (proper-list-p arity) (memq nargs arity)))
      (nelisp-gnu-bytecode-vm--signal
       'unsupported-call-arity pc symbol nargs arity))
    symbol))

(defun nelisp-gnu-bytecode-vm--lower-code
    (code constants initial-stack declared-depth signatures forward-definitions)
  "Lower the supported GNU instructions into NeLisp BCL bytecode.
CODE and CONSTANTS are GNU byte-code fields.  INITIAL-STACK is the
number of positional arguments already in the VM stack."
  (unless (and (stringp code) (vectorp constants)
               (integerp declared-depth) (>= declared-depth 0))
    (nelisp-gnu-bytecode-vm--signal 'malformed-function-fields))
  (let ((pc 0) (sp initial-stack) (peak initial-stack) (out nil)
        (binding-depth 0)
        (abstract-stack (make-list initial-stack :argument))
        (len (length code)) (returned nil))
    (while (< pc len)
      (let ((op (aref code pc)) (bcl-op nil) (arg nil) (has-arg nil)
            (emitted-sequence nil))
        (cond
         ;; GNU 31.1 compact variable-reference and variable-set opcodes.
         ((and (<= 8 op) (<= op 13))
          (setq bcl-op 16
                arg (nelisp-gnu-bytecode-vm--symbol-constant
                     constants (- op 8) pc)
                has-arg t
                sp (1+ sp)
                abstract-stack (append abstract-stack (list nil))))
         ((and (<= 16 op) (<= op 21))
          (when (<= sp 0)
            (nelisp-gnu-bytecode-vm--signal 'stack-underflow pc op))
          (setq bcl-op 17
                arg (nelisp-gnu-bytecode-vm--symbol-constant
                     constants (- op 16) pc)
                has-arg t
                sp (1- sp)
                abstract-stack (butlast abstract-stack)))
         ;; GNU compact constants 0..15.
         ((and (<= 192 op) (<= op 207))
          (setq arg (- op 192))
          (unless (< arg (length constants))
            (nelisp-gnu-bytecode-vm--signal 'constant-index-out-of-range pc arg))
          (setq bcl-op 1 has-arg t sp (1+ sp)
                abstract-stack
                (append abstract-stack (list (list :constant arg)))))
         ;; GNU 31.1 DUP copies the value and its abstract provenance.
         ((= op 137)
          (when (<= sp 0)
            (nelisp-gnu-bytecode-vm--signal 'stack-underflow pc op))
          (let ((marker (car (last abstract-stack))))
            (push 4 out)
            (setq sp (1+ sp)
                  abstract-stack (append abstract-stack (list marker))
                  emitted-sequence t)))
         ;; Keep SYMBOL_VALUE's operand dynamic and route it through the
         ;; runtime's registered public BCL CALL target.
         ((= op 74)
          (when (<= sp 0)
            (nelisp-gnu-bytecode-vm--signal 'stack-underflow pc op))
          (let ((index (cl-position
                        nelisp-gnu-bytecode-vm--symbol-value-intrinsic-callable
                        constants :test #'eq)))
            (unless index
              (nelisp-gnu-bytecode-vm--signal
               'missing-symbol-value-intrinsic pc))
            (unless (functionp (aref constants index))
              (nelisp-gnu-bytecode-vm--signal
               'invalid-symbol-value-intrinsic pc))
            ;; Original top is the argument.  DUP it, push function, then
            ;; STACK-SET offset 2 arranges (function argument) for CALL1.
            (push 4 out)
            (push 1 out) (push index out)
            (push 20 out) (push 2 out)
            (push 30 out) (push 1 out)
            (setq abstract-stack
                  (append (butlast abstract-stack) (list nil))
                  emitted-sequence t)))
         ;; GNU BNCONC destructively joins the final cons of its first
         ;; operand to the second operand.  Rebuild a BCL CALL2 frame using
         ;; duplicates so the source stack operands remain rooted until the
         ;; call starts; the captured callable bypasses public redefinition.
         ((= op 164)
          (when (< sp 2)
            (nelisp-gnu-bytecode-vm--signal 'stack-underflow pc op))
          (let ((index (cl-position
                        nelisp-gnu-bytecode-vm--nconc-intrinsic-callable
                        constants :test #'eq)))
            (unless index
              (nelisp-gnu-bytecode-vm--signal 'missing-nconc-intrinsic pc))
            (unless (functionp (aref constants index))
              (nelisp-gnu-bytecode-vm--signal 'invalid-nconc-intrinsic pc))
            ;; Input [... FIRST SECOND] becomes
            ;; [... FN FIRST FIRST SECOND], then STACK-SET/DISCARDN
            ;; yields [... FN FIRST SECOND] for CALL2.
            (push 2 out) (push 1 out)
            (push 2 out) (push 1 out)
            (push 1 out) (push index out)
            (push 20 out) (push 4 out)
            (push 2 out) (push 1 out)
            (push 20 out) (push 3 out)
            (push 21 out) (push 1 out)
            (push 30 out) (push 2 out)
            (setq sp (1- sp)
                  abstract-stack
                  (append (cl-subseq abstract-stack 0 (1- sp)) (list nil))
                  emitted-sequence t)))
         ;; GNU compact VARBIND for constant zero; the observed GNU 31.1
         ;; special-let shape uses this after pushing its initializer.
         ((= op 24)
          (when (<= sp 0)
            (nelisp-gnu-bytecode-vm--signal 'stack-underflow pc op))
          (setq bcl-op 18
                arg (nelisp-gnu-bytecode-vm--symbol-constant constants 0 pc)
                has-arg t
                sp (1- sp)
                abstract-stack (butlast abstract-stack)
                binding-depth (1+ binding-depth)))
         ;; GNU compact UNBIND1 restores exactly one dynamic binding.
         ((= op 41)
          (when (<= binding-depth 0)
            (nelisp-gnu-bytecode-vm--signal 'unbind-underflow pc op))
          (setq bcl-op 19 arg 1 has-arg t
                binding-depth (1- binding-depth)))
         ;; GNU compact STACK-REF 1, 2 and 3 are emitted by the observed
         ;; fixed-arity named-call shapes.  Restrict references to original
         ;; argument slots; arbitrary stack aliases remain refused.
         ((memq op '(1 2 3))
          (let ((source (- sp op 1)))
            (unless (and (>= source 0)
                         (eq (nth source abstract-stack) :argument))
              (nelisp-gnu-bytecode-vm--signal
               'bad-stack-reference pc op source))
            (setq bcl-op 2 arg op has-arg t sp (1+ sp)
                  abstract-stack (append abstract-stack (list nil)))))
         ;; Opcode 66 consumes two values and returns their CONS. Keep this
         ;; VM admission independent from native lowering's exact-template
         ;; gate; the BCL already has a direct CONS instruction.
         ((= op 66)
          (when (< sp 2)
            (nelisp-gnu-bytecode-vm--signal 'stack-underflow pc op))
          (setq bcl-op 24 sp (1- sp)
                abstract-stack
                (append (cl-subseq abstract-stack 0 (- sp 1)) (list nil))))
         ;; GNU add1 has a fixed opcode; BCL's corresponding instruction is 8.
         ((= op 84)
          (when (<= sp 0)
            (nelisp-gnu-bytecode-vm--signal 'stack-underflow pc op))
          (setq bcl-op 8
                abstract-stack
                (append (butlast abstract-stack) (list nil))))
         ;; GNU compact CALL0..CALL15 opcodes encode nargs in the opcode.
         ;; The function must be a known symbol constant and a same-arity
         ;; BCL in NeLisp's function table; runtime CALL then resolves its
         ;; current cell again, preserving redefinition semantics.
         ((and (<= 32 op) (<= op 47))
          (let* ((nargs (- op 32))
                 (function-position (- sp nargs 1))
                 (marker (and (>= function-position 0)
                              (nth function-position abstract-stack)))
                 (constant-index
                  (and (consp marker) (eq (car marker) :constant)
                       (cadr marker))))
            (unless (and (integerp constant-index)
                         (<= 0 constant-index)
                         (< constant-index (length constants)))
              (nelisp-gnu-bytecode-vm--signal
               'unsupported-call-operand pc op marker))
            (unless (symbolp (aref constants constant-index))
              (nelisp-gnu-bytecode-vm--signal
               'unsupported-call-operand pc op marker))
            (nelisp-gnu-bytecode-vm--known-call-target
             (aref constants constant-index) nargs pc signatures
             forward-definitions)
            (setq bcl-op 30 arg nargs has-arg t
                  sp (- sp nargs)
                  abstract-stack
                  (append (cl-subseq abstract-stack 0 function-position)
                          (list nil)))))
         ;; GNU RETURN; it must end the stream with one result on stack.
         ((= op 135)
          (unless (and (= binding-depth 0)
                       (= pc (1- len))
                       (or (= sp 1) (= sp (1+ initial-stack))))
            (nelisp-gnu-bytecode-vm--signal
             'invalid-return-shape pc sp len binding-depth))
          (setq bcl-op 0 sp 0 returned t))
         (t
          (nelisp-gnu-bytecode-vm--signal 'unsupported-opcode pc op)))
        (unless emitted-sequence
          (push bcl-op out)
          (when has-arg (push arg out)))
        (setq peak (max peak sp)
              pc (1+ pc))))
    (unless returned
      (nelisp-gnu-bytecode-vm--signal 'missing-return len))
    (when (> peak declared-depth)
      (nelisp-gnu-bytecode-vm--signal 'declared-stack-too-small peak declared-depth))
    (apply #'vector (nreverse out))))

(defun nelisp-gnu-bytecode-vm--fixed-params (descriptor)
  "Return fixed positional parameter symbols for DESCRIPTOR, or signal."
  (cond
   ((and (listp descriptor)
         (proper-list-p descriptor)
         (cl-every #'symbolp descriptor)
         (= (length descriptor)
            (length (delete-dups (copy-sequence descriptor))))
         (cl-every (lambda (symbol)
                     (and (not (memq symbol '(nil t &optional &rest &key &aux
                                                   &whole &body &environment)))
                          (not (keywordp symbol))))
                   descriptor))
    descriptor)
   ((and (integerp descriptor) (>= descriptor 0)
         (= (logand descriptor 255) (ash descriptor -8)))
    (let ((count (logand descriptor 255)) (i 0) params)
      (while (< i count)
        (push (intern (format "nelisp-gnu-bytecode-arg-%d" i)) params)
        (setq i (1+ i)))
      (nreverse params)))
   (t (nelisp-gnu-bytecode-vm--signal 'unsupported-argument-descriptor descriptor))))

(defun nelisp-gnu-bytecode-vm--with-intrinsic-constants (code constants)
  "Append captured runtime callables required by CODE to CONSTANTS."
  (let ((result constants))
    (dolist (entry `((74 . ,nelisp-gnu-bytecode-vm--symbol-value-intrinsic-callable)
                     (164 . ,nelisp-gnu-bytecode-vm--nconc-intrinsic-callable)))
      (when (and (cl-position (car entry) code)
                 (not (cl-position (cdr entry) result :test #'eq)))
        (when (>= (length result) 256)
          (nelisp-gnu-bytecode-vm--signal 'constant-index-out-of-range
                                           (length result)))
        (setq result (vconcat result (vector (cdr entry))))))
    result))

(defun nelisp-gnu-bytecode-vm--extra-stack-depth (code)
  "Return the temporary BCL stack growth needed to lower CODE."
  (cond ((cl-position 164 code) 3)
        ((cl-position 74 code) 2)
        (t 0)))

(defun nelisp-gnu-bytecode-vm--lower-function
    (function signatures forward-definitions)
  "Translate materialized GNU byte-code FUNCTION into a NeLisp BCL."
  (unless (and (byte-code-function-p function) (>= (length function) 4))
    (nelisp-gnu-bytecode-vm--signal 'not-byte-code-function function))
  (let* ((params (nelisp-gnu-bytecode-vm--fixed-params (aref function 0)))
         (code (aref function 1))
         (constants (nelisp-gnu-bytecode-vm--with-intrinsic-constants
                     (aref function 1) (aref function 2)))
         (declared-depth (aref function 3))
         (lowered (nelisp-gnu-bytecode-vm--lower-code
                   code constants (length params) declared-depth signatures
                   forward-definitions)))
    (nelisp-bc-make nil params constants lowered
                    (max 1 (+ declared-depth
                              (nelisp-gnu-bytecode-vm--extra-stack-depth code))
                         (length params)) 0)))

(defun nelisp-gnu-bytecode-vm--literal-p (form)
  "Return non-nil for an inert initializer admitted by this bridge."
  (or (numberp form) (stringp form) (null form) (eq form t)
      (keywordp form)
      (and (consp form) (eq (car form) 'quote)
           (consp (cdr form)) (null (cddr form)))))

(defun nelisp-gnu-bytecode-vm--quoted-symbol-p (form)
  "Return non-nil when FORM is exactly a quote of a usable symbol."
  (and (consp form) (eq (car form) 'quote)
       (consp (cdr form)) (null (cddr form))
       (symbolp (cadr form))
       (not (memq (cadr form) '(nil t)))
       (not (keywordp (cadr form)))))

(defun nelisp-gnu-bytecode-vm--action (form signatures forward-definitions)
  "Validate FORM and return an inert action record, without executing it."
  (cond
   ((and (consp form) (eq (car form) 'defvar)
         (<= 2 (length form)) (<= (length form) 4)
         (symbolp (cadr form))
         (not (memq (cadr form) '(nil t)))
         (not (keywordp (cadr form)))
         (or (= (length form) 2)
             (nelisp-gnu-bytecode-vm--literal-p (nth 2 form)))
         (or (< (length form) 4) (stringp (nth 3 form))))
    (list :eval form))
   ((and (consp form) (eq (car form) 'provide)
         (= (length form) 2)
         (nelisp-gnu-bytecode-vm--quoted-symbol-p (cadr form)))
    (list :eval form))
   ((and (consp form) (eq (car form) 'defalias)
         (<= 3 (length form)) (<= (length form) 4)
         (nelisp-gnu-bytecode-vm--quoted-symbol-p (cadr form))
         (byte-code-function-p (nth 2 form))
         (or (< (length form) 4)
             (null (nth 3 form)) (stringp (nth 3 form))))
    (let* ((name (cadr (cadr form)))
           (function (nelisp-gnu-bytecode-vm--lower-function
                      (nth 2 form) signatures forward-definitions))
           (arity (nelisp-gnu-bytecode-vm--fixed-bcl-arity function)))
      (unless arity
        (nelisp-gnu-bytecode-vm--signal 'unsupported-function-signature name))
      (puthash name arity signatures)
      (list :function name function)))
   ((and (consp form) (eq (car form) 'byte-code)
         (= (length form) 4)
         (stringp (nth 1 form)) (vectorp (nth 2 form))
         (integerp (nth 3 form)))
    (let ((constants (nelisp-gnu-bytecode-vm--with-intrinsic-constants
                      (nth 1 form) (nth 2 form))))
      (list :run
          (nelisp-bc-make nil nil constants
                          (nelisp-gnu-bytecode-vm--lower-code
                           (nth 1 form) constants 0 (nth 3 form)
                           signatures forward-definitions)
                          (max 1 (+ (nth 3 form)
                                    (nelisp-gnu-bytecode-vm--extra-stack-depth
                                     (nth 1 form))))
                          0))))
   (t (nelisp-gnu-bytecode-vm--signal 'unsupported-top-level-form form))))

(defun nelisp-gnu-bytecode-vm-load-file (path)
  "Experimentally load a narrow GNU 31.1 ELC PATH into NeLisp stores.
The whole file is parsed and every form/opcode is validated before any
form runs.  Supported top-level forms are literal `defvar', `provide',
the `byte-code' instructions listed in `nelisp-gnu-bytecode-vm--lower-code',
and fixed-arity materialized functions using that same instruction subset.
All other inputs signal `nelisp-gnu-bytecode-vm-error'."
  (unless (nelisp-core-file-readable-p path)
    (signal 'file-error (list "Cannot read GNU ELC file" path)))
  (let ((text (nelisp-core-read-file-as-string path)))
    (unless (nelisp-load-gnu-elc-p text)
      (nelisp-gnu-bytecode-vm--signal 'not-gnu-elc path))
    (let* ((forms (nelisp-gnu-bytecode-vm--read-all text))
           ;; Signature tracking is a private preflight snapshot.  Each
           ;; defalias updates it only after its body has passed lowering;
           ;; no file action runs until every later form has passed too.
           (signatures (nelisp-gnu-bytecode-vm--live-call-signatures))
           (forward-definitions (make-hash-table :test 'eq))
           (actions nil)
           (last nil))
      (dolist (form forms)
        (when (and (consp form) (eq (car form) 'defalias)
                   (nelisp-gnu-bytecode-vm--quoted-symbol-p (cadr form)))
          (puthash (cadr (cadr form)) t forward-definitions)))
      (dolist (form forms)
        (let* ((is-defalias (and (consp form) (eq (car form) 'defalias)
                                 (nelisp-gnu-bytecode-vm--quoted-symbol-p
                                  (cadr form))))
               (name (and is-defalias (cadr (cadr form))))
               (action (nelisp-gnu-bytecode-vm--action
                        form signatures forward-definitions)))
          (when is-defalias (remhash name forward-definitions))
          (push action actions)))
      (setq actions (nreverse actions))
      (dolist (action actions)
        (setq last
              (pcase (car action)
                (:eval (nelisp-eval (cadr action)))
                (:function
                 (nelisp-eval-function-defalias (nth 1 action) (nth 2 action)))
                (:run (nelisp-bc-run (cadr action) nil)))))
      last)))

(provide 'nelisp-gnu-bytecode-vm)
;;; nelisp-gnu-bytecode-vm.el ends here
