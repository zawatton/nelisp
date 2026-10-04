;;; nelisp-bytecode-native-constant.el --- constant-return CFG native slice -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Source-free native lowering for verified straight-line stack operations.
;; Constants travel as hidden boxed arguments, so machine code contains no
;; movable object address.

;;; Code:

(require 'nelisp-bytecode-frame-ir)
(require 'nelisp-asm-x86_64)
(require 'nelisp-elf-write)
(require 'nelisp-artifact)
(require 'cl-lib)

(defun nelisp-bytecode-native-constant--argument-symbols-p (symbols arity)
  "Return non-nil when SYMBOLS is a proper, unique positional arg list."
  (let ((rest symbols) (count 0) (seen-cells nil) (seen-names nil)
        (valid t))
    (while (and valid (consp rest))
      (if (memq rest seen-cells)
          (setq valid nil)
        (push rest seen-cells)
        (let ((name (car rest)))
          (if (or (not (symbolp name))
                  (memq name '(&optional &rest &key &allow-other-keys &aux))
                  (memq name seen-names)
                  (not (fboundp 'special-variable-p))
                  (special-variable-p name))
              (setq valid nil)
            (push name seen-names)
            (setq count (1+ count))))
        (setq rest (cdr rest))))
    (and valid (null rest) (= count arity))))

(defun nelisp-bytecode-native-constant-return-build
    (code constants artifact-path entry-name &optional user-arity
          user-argument-symbols initial-stack-depth rest-required-count)
  "Build a boxed `.neln' from straight-line verified byte-code CODE.

The complete frame CFG may use constant, argument variable-ref, stack-ref,
dup, discard, and return. CONSTANTS are hidden boxed arguments, followed by
USER-ARITY ordinary arguments. USER-ARGUMENT-SYMBOLS are the byte-code
function's argument names and bind variable-ref constants to those arguments.
INITIAL-STACK-DEPTH seeds entry stack slots from corresponding user arguments.
REST-REQUIRED-COUNT marks the final user argument as an ABI-packed rest list;
it is admitted only for the exact terminal rest-slot return template.
The result is returned from its originating argument slot, preserving object
identity and leaving roots in the checked call boundary."
  (unless (and (stringp artifact-path)
               (stringp entry-name)
               (vectorp constants)
               (integerp user-arity) (>= user-arity 0)
               (integerp (or initial-stack-depth 0))
               (<= 0 (or initial-stack-depth 0) user-arity)
               (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" entry-name))
    (error "bytecode-native-constant: invalid artifact path or entry name"))
  (when (and user-argument-symbols
             (not (nelisp-bytecode-native-constant--argument-symbols-p
                   user-argument-symbols user-arity)))
    (error "bytecode-native-constant: unsupported positional argument descriptor"))
  (when rest-required-count
    (unless (and (integerp rest-required-count) (>= rest-required-count 0)
                 (= user-arity (1+ rest-required-count))
                 (= (or initial-stack-depth 0) user-arity)
                 (= (length constants) 0)
                 (null user-argument-symbols))
      (error "bytecode-native-constant: invalid REST return template")))
  (when (> (+ (length constants) user-arity) 6)
    (error "bytecode-native-constant: hidden plus user arity exceeds six"))
  (when (= (+ (length constants) user-arity) 0)
    (error "bytecode-native-constant: zero-argument result has no rooted value source"))
  (when (file-exists-p artifact-path)
    (error "bytecode-native-constant: artifact path already exists"))
  (let* ((initial-stack-depth (or initial-stack-depth 0))
         (verified (nelisp-bytecode-frame-ir-build
                    code constants initial-stack-depth))
         (blocks (plist-get verified :blocks)))
    (unless (eq (plist-get verified :status) 'complete)
      (error "bytecode-native-constant: byte-code rejected: %s"
             (plist-get verified :reason)))
    (unless (= (length blocks) 1)
      (error "bytecode-native-constant: unsupported control-flow blocks"))
    (let* ((block (aref blocks 0))
           (instructions (append (plist-get block :instructions) nil))
           (successors (plist-get block :successors))
           (origins
            (cl-loop for index below initial-stack-depth
                     collect (cons (list :entry 0 index)
                                   (+ (length constants) index))))
           (return-index nil) (return-count 0))
      (when (or (> (length successors) 0)
                (/= (plist-get block :entry-stack-depth)
                    initial-stack-depth))
        (error "bytecode-native-constant: unsupported control flow or entry stack"))
      (dolist (instruction instructions)
        (let* ((kind (plist-get instruction :kind))
               (inputs (plist-get instruction :inputs))
               (outputs (plist-get instruction :outputs))
               (source (and (= (length inputs) 1)
                            (assoc (car inputs) origins))))
          (pcase kind
            ('constant
             (let ((index (plist-get instruction :constant-index)))
               (unless (and (= (length outputs) 1)
                            (integerp index) (<= 0 index)
                            (< index (length constants)))
                 (error "bytecode-native-constant: invalid constant slot at %d"
                        (plist-get instruction :pc)))
               (push (cons (car outputs) index) origins)))
            ('variable-ref
             (let* ((index (plist-get instruction :constant-index))
                    (name (and (integerp index) (<= 0 index)
                               (< index (length constants))
                               (aref constants index)))
                    (argument-index
                     (and (symbolp name)
                          user-argument-symbols
                          (cl-position name user-argument-symbols :test #'eq))))
               (unless (and (= (length outputs) 1)
                            (integerp argument-index))
                 (error "bytecode-native-constant: variable-ref is not a declared user argument at %d"
                        (plist-get instruction :pc)))
               (push (cons (car outputs)
                           (+ (length constants) argument-index))
                     origins)))
            ((or 'stack-ref 'dup)
             (unless (and source (= (length outputs) 1))
               (error "bytecode-native-constant: unsupported %s slot at %d"
                      kind (plist-get instruction :pc)))
             (push (cons (car outputs) (cdr source)) origins))
            ('discard
             (unless (and source (null outputs))
               (error "bytecode-native-constant: unsupported discard slot at %d"
                      (plist-get instruction :pc))))
            ('return
             (unless (and source (null outputs)
                          (eq instruction (car (last instructions))))
               (error "bytecode-native-constant: unsupported return slot at %d"
                      (plist-get instruction :pc)))
             (setq return-index (cdr source)
                   return-count (1+ return-count)))
            (_
             (error "bytecode-native-constant: unsupported %s effect at %d"
                    (or kind 'unknown) (plist-get instruction :pc))))))
      (unless (= return-count 1)
        (error "bytecode-native-constant: expected one terminal return"))
      (when (and rest-required-count
                 (/= return-index rest-required-count))
        (error "bytecode-native-constant: REST template does not return the rest slot"))
      (let* ((arg-registers [rdi rsi rdx rcx r8 r9])
             (asm (nelisp-asm-x86_64-make-buffer 'sysv))
             (object-path (make-temp-file "nelisp-bytecode-native-" nil ".o"))
             (written nil))
        (unwind-protect
            (progn
              ;; The IR slot map resolves the returned value to its rooted
              ;; hidden constant argument. User arguments follow constants.
              ;; The loader trampoline owns an rbp frame that this body exits.
              (nelisp-asm-x86_64-define-label asm entry-name)
              (nelisp-asm-x86_64-mov-reg-reg
               asm 'rax (aref arg-registers return-index))
              (nelisp-asm-x86_64-mov-reg-reg asm 'rsp 'rbp)
              (nelisp-asm-x86_64-pop asm 'rbp)
              (nelisp-asm-x86_64-ret asm)
              (let* ((asm-unit (nelisp-asm-x86_64-buffer-to-unit
                                asm entry-name))
                     (text (cdr (assq 'text (plist-get asm-unit :sections))))
                     (symbols
                      (mapcar (lambda (symbol)
                                (let ((copy (copy-sequence symbol)))
                                  (plist-put copy :size (length text))))
                              (plist-get asm-unit :symbols)))
                     (defun-entry
                      (append
                       (list :name entry-name :offset 0 :size (length text)
                             :arity (+ (length constants) user-arity)
                             :param-class 'gp
                             :param-repr 'sexp-ptr :return-repr 'sexp-ptr
                             :rt-slot-count 0 :body-offset 0)
                       (when rest-required-count
                         (list :rest-required-count rest-required-count))))
                     (unit
                      (list :text text :rodata (unibyte-string)
                            :data (unibyte-string) :bss-size 0
                            :symbols symbols :relocs nil
                            :extern-symbols nil :machine 'x86_64
                            :defuns (list defun-entry))))
                (nelisp-elf-write-binary
                 object-path
                 (list :e-type 'rel :text text :symbols symbols
                       :relocs nil :machine 'x86_64))
                (unless (fboundp 'nelisp-artifact-write-native-link-unit)
                  (error "bytecode-native-constant: source-free artifact writer unavailable"))
                (nelisp-artifact-write-native-link-unit
                 artifact-path (concat "bytecode-cfg:" entry-name)
                 object-path unit)
                (setq written t)
                (list :status 'complete :artifact artifact-path
                      :entry entry-name :return-argument-index return-index
                      :rest-required-count rest-required-count
                      :constants constants :user-arity user-arity
                      :verified-cfg verified :machine-code text)))
          (when (file-exists-p object-path)
            (delete-file object-path))
          (unless written
            (when (file-exists-p artifact-path)
              (delete-file artifact-path))))))))

(provide 'nelisp-bytecode-native-constant)
;;; nelisp-bytecode-native-constant.el ends here
