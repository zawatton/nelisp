;;; nelisp-bytecode-native-boxed-branch.el --- boxed branch byte-code slice -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Source-free native lowering for one truthiness branch and a boxed-value join.

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-frame-ir)
(require 'nelisp-asm-x86_64)
(require 'nelisp-elf-write)
(require 'nelisp-artifact)
(require 'nelisp-native-load)

(defun nelisp-bytecode-native-boxed-branch--label (pc)
  (intern (format "nl_bc_boxed_%d" pc)))

(defun nelisp-bytecode-native-boxed-branch--optional-default-shape-p
    (code constants descriptor blocks &optional compiler-input)
  "Whether verified BLOCKS prove a bounded optional short-circuit."
  (let* ((entry (car blocks)) (fallback (cadr blocks)) (join (caddr blocks))
         (entry-ins (append (plist-get entry :instructions) nil))
         (fallback-ins (append (plist-get fallback :instructions) nil))
         (join-ins (append (plist-get join :instructions) nil))
         (or-code (unibyte-string 137 134 5 0 1 135))
         (and-code (unibyte-string 137 133 5 0 1 135))
         (short-circuit-op (and (= (length entry-ins) 2)
                                (plist-get (cadr entry-ins) :opcode)))
         (entry-edges (append (plist-get entry :successors) nil))
         (fallback-edges (append (plist-get fallback :successors) nil))
         (taken (cl-find-if (lambda (edge) (eq (plist-get edge :kind) 'taken)) entry-edges))
         (fall (cl-find-if (lambda (edge) (eq (plist-get edge :kind) 'fallthrough)) entry-edges)))
    (or
     (and (eql descriptor 513)
         (or (and (equal code or-code) (eql short-circuit-op 134))
             (and (equal code and-code) (eql short-circuit-op 133)))
         (vectorp constants) (= (length constants) 0)
         (= (length blocks) 3)
         (equal (mapcar (lambda (block) (plist-get block :start)) blocks) '(0 4 5))
         (= (length entry-ins) 2)
         (= (plist-get (car entry-ins) :opcode) 137)
         (eq (plist-get (car entry-ins) :kind) 'dup)
         (equal (plist-get (car entry-ins) :inputs) '((:entry 0 1)))
         (memq (plist-get (cadr entry-ins) :opcode) '(133 134))
         (eq (plist-get (cadr entry-ins) :kind) 'branch)
         (= (plist-get (cadr entry-ins) :operand) 5)
         (equal (plist-get (cadr entry-ins) :inputs)
                (plist-get (car entry-ins) :outputs))
         (= (length entry-edges) 2)
         taken fall (= (plist-get taken :target) 5) (= (plist-get fall :target) 4)
         (= (length fallback-ins) 1)
         (= (plist-get (car fallback-ins) :opcode) 1)
         (eq (plist-get (car fallback-ins) :kind) 'stack-ref)
         (= (plist-get (car fallback-ins) :operand) 1)
         (equal (plist-get (car fallback-ins) :inputs) '((:entry 4 0)))
         (= (length fallback-edges) 1)
         (eq (plist-get (car fallback-edges) :kind) 'fallthrough)
         (= (plist-get (car fallback-edges) :target) 5)
         (= (length join-ins) 1)
         (= (plist-get (car join-ins) :opcode) 135)
         (eq (plist-get (car join-ins) :kind) 'return)
         (equal (plist-get (car join-ins) :inputs) '((:entry 5 2)))
          (= (length (plist-get join :successors)) 0))
     (nelisp-bytecode-native-boxed-branch--optional-variable-shape-p
      compiler-input blocks))))

(defun nelisp-bytecode-native-boxed-branch--optional-variable-shape-p
    (input blocks)
  "Prove the named GNU optional OR/AND shape from INPUT's verified CFG."
  (when (and (listp input) (= (length blocks) 3))
    (let* ((args (plist-get input :argument-list))
           (descriptor (plist-get input :argument-descriptor))
           (constants (plist-get input :constants))
           (entry (nth 0 blocks)) (fallback (nth 1 blocks)) (join (nth 2 blocks))
           (entry-ins (append (plist-get entry :instructions) nil))
           (fallback-ins (append (plist-get fallback :instructions) nil))
           (join-ins (append (plist-get join :instructions) nil))
           (optional-var (car entry-ins)) (branch-ins (cadr entry-ins))
           (fallback-var (car fallback-ins))
           (optional-index (plist-get optional-var :constant-index))
           (fallback-index (plist-get fallback-var :constant-index))
           (optional-name (and (integerp optional-index)
                               (<= 0 optional-index) (< optional-index (length constants))
                               (aref constants optional-index)))
           (fallback-name (and (integerp fallback-index)
                               (<= 0 fallback-index) (< fallback-index (length constants))
                               (aref constants fallback-index)))
           (edges (append (plist-get entry :successors) nil))
           (taken (cl-find-if (lambda (edge) (eq (plist-get edge :kind) 'taken)) edges))
           (fall (cl-find-if (lambda (edge) (eq (plist-get edge :kind) 'fallthrough)) edges))
           (fallback-edges (append (plist-get fallback :successors) nil))
           (fallback-edge (car fallback-edges)))
      (and (equal descriptor args)
           (= (length args) 3) (symbolp (car args))
           (eq (cadr args) '&optional) (symbolp (caddr args))
           (not (eq (car args) (caddr args)))
           (fboundp 'special-variable-p)
           (not (special-variable-p (car args)))
           (not (special-variable-p (caddr args)))
           (= (plist-get input :argument-min) 1)
           (= (plist-get input :argument-max) 2)
           (= (plist-get input :argument-count) 2)
           (= (plist-get input :initial-stack-depth) 0)
           (eq (plist-get (plist-get input :frame-result) :status) 'complete)
           (= (plist-get entry :start) 0)
           (= (length entry-ins) 2)
           (eq (plist-get optional-var :kind) 'variable-ref)
           (eq optional-name (caddr args))
           (memq (plist-get branch-ins :opcode) '(133 134))
           (eq (plist-get branch-ins :kind) 'branch)
           (equal (plist-get branch-ins :inputs) (plist-get optional-var :outputs))
           (= (length edges) 2) taken fall
           (/= (plist-get taken :target) (plist-get fall :target))
           (= (length fallback-ins) 1)
           (eq (plist-get fallback-var :kind) 'variable-ref)
           (eq fallback-name (car args))
           (= (length fallback-edges) 1)
           (eq (plist-get fallback-edge :kind) 'fallthrough)
           (= (plist-get fallback-edge :target) (plist-get join :start))
           (= (plist-get taken :target) (plist-get join :start))
           (= (plist-get fall :target) (plist-get fallback :start))
           (= (length join-ins) 1)
           (eq (plist-get (car join-ins) :kind) 'return)
           (= (length (plist-get join :successors)) 0)
           (equal (append (plist-get (car join-ins) :inputs) nil)
                  (append (plist-get taken :target-slots) nil))
           (equal (append (plist-get (car join-ins) :inputs) nil)
                  (append (plist-get fallback-edge :target-slots) nil))))))

(defun nelisp-bytecode-native-boxed-branch-optional-variable-shape-p
    (input blocks)
  "Return non-nil when INPUT has the verified optional-variable shape."
  (nelisp-bytecode-native-boxed-branch--optional-variable-shape-p input blocks))

(defun nelisp-bytecode-native-boxed-branch--optional-formal-index
    (name input instruction)
  "Return NAME's positional index only for a proven optional variable PC."
  (let* ((blocks (append (plist-get (plist-get input :frame-result) :blocks) nil))
         (entry-instructions (append (plist-get (car blocks) :instructions) nil))
         (fallback-instructions (append (plist-get (cadr blocks) :instructions) nil))
         (pc (plist-get instruction :pc))
         (expected-pcs (list (plist-get (car entry-instructions) :pc)
                             (plist-get (car fallback-instructions) :pc))))
    (when (member pc expected-pcs)
      (let ((args (plist-get input :argument-list)) (index 0) found)
        (while (and args (not found))
          (let ((item (pop args)))
            (cond ((memq item '(&optional &rest &key &allow-other-keys &aux)) nil)
                  ((eq item name) (setq found index))
                  ((symbolp item) (setq index (1+ index))))))
        found))))

(defun nelisp-bytecode-native-boxed-branch-build
    (code constants artifact-path entry-name argument-descriptor &optional compiler-input)
  "Build boxed native CODE selecting a rooted constant or positional argument.

The verified byte-code must have one conditional truth test and no effects.
It supports either two terminal return arms or two arms that store one rooted
boxed value in a stack slot before jumping to one return join.
ARGUMENT-DESCRIPTOR may be packed descriptor 257 or 514 for one or two required
positional arguments, packed 513 for the pinned legacy optional branch, or the
named GNU shape =(required &optional optional)= when its variable-reference,
branch, fallback, and return dataflow is verified. Other list descriptors are
refused. CONSTANTS are hidden leading arguments except variable-name constants
in the admitted named-optional form.
Truthiness reads the tag from each rooted Sexp argument cell."
  (let* ((optional-variable-input
          (and compiler-input
               (nelisp-bytecode-native-boxed-branch--optional-variable-shape-p
                compiler-input
                (append (plist-get (plist-get compiler-input :frame-result) :blocks)
                        nil))))
         (argument-count
          (or (pcase argument-descriptor (257 1) ((or 513 514) 2) (_ nil))
              (and optional-variable-input 2)))
         (hidden-constant-count (if optional-variable-input 0 (length constants))))
  (unless (and (stringp artifact-path) (stringp entry-name)
               (vectorp constants) argument-count
               (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" entry-name)
               (<= (+ (length constants) argument-count) 6))
    (error "bytecode-native-boxed-branch: invalid entry contract"))
  (when (file-exists-p artifact-path)
    (error "bytecode-native-boxed-branch: artifact path already exists"))
  (let* ((verified (or (and compiler-input (plist-get compiler-input :frame-result))
                       (nelisp-bytecode-frame-ir-build code constants argument-count)))
         (blocks (append (plist-get verified :blocks) nil))
         (arg-regs [rdi rsi rdx rcx r8 r9])
         (origins nil) (branches nil) (returns nil) (asm nil) (object nil)
         (written nil))
    (unless (eq (plist-get verified :status) 'complete)
      (error "bytecode-native-boxed-branch: byte-code rejected: %s"
             (plist-get verified :reason)))
    (unless (memq (length blocks) '(3 4))
      (error "bytecode-native-boxed-branch: requires three or four blocks"))
    (dotimes (i argument-count)
      (push (cons (list :entry 0 i) (+ hidden-constant-count i)) origins))
    ;; Resolve every produced frame value to one stable hidden or user arg.
    (dolist (block blocks)
      (dolist (ins (append (plist-get block :instructions) nil))
        (let* ((kind (plist-get ins :kind))
               (inputs (plist-get ins :inputs))
               (outputs (plist-get ins :outputs))
               (source (and inputs (cdr (assoc (car inputs) origins))))
               (opcode (plist-get ins :opcode)))
          (pcase kind
            ('constant
             (let ((i (plist-get ins :constant-index)))
               (unless (and (= (length outputs) 1) (integerp i)
                            (<= 0 i) (< i (length constants)))
                 (error "bytecode-native-boxed-branch: invalid constant at %d" (plist-get ins :pc)))
               (push (cons (car outputs) i) origins)))
            ('variable-ref
             (let* ((index (nelisp-bytecode-native-boxed-branch--optional-formal-index
                            (and (integerp (plist-get ins :constant-index))
                                 (<= 0 (plist-get ins :constant-index))
                                 (< (plist-get ins :constant-index) (length constants))
                                 (aref constants (plist-get ins :constant-index)))
                            compiler-input ins)))
               (unless (and optional-variable-input index (= (length outputs) 1))
                 (error "bytecode-native-boxed-branch: unresolved variable reference at %d"
                        (plist-get ins :pc)))
               (push (cons (car outputs) index) origins)))
            ((or 'stack-ref 'dup)
             (unless (and source (= (length outputs) 1))
               (error "bytecode-native-boxed-branch: unresolved stack value at %d" (plist-get ins :pc)))
             (push (cons (car outputs) source) origins))
            ((or 'discard 'goto) nil)
            ('branch
             (if (= opcode 130)
                 nil
               (unless (and (memq opcode '(131 132 133 134)) source)
                 (error "bytecode-native-boxed-branch: unsupported branch at %d" (plist-get ins :pc)))
               (push (list ins source) branches)))
            ('return
             (unless source
               (error "bytecode-native-boxed-branch: unresolved return at %d" (plist-get ins :pc)))
             (push (cons block source) returns))
            (_ (error "bytecode-native-boxed-branch: unsupported %s at %d"
                      (or kind 'unknown) (plist-get ins :pc))))))
      ;; Carry verified predecessor slot provenance to each successor's
      ;; synthetic entry values. The accepted CFG has no joins or backedges.
      (dolist (edge (append (plist-get block :successors) nil))
        (let ((from (append (plist-get edge :slots) nil))
              (to (append (plist-get edge :target-slots) nil)))
          (while (and from to)
            (let ((origin (cdr (assoc (car from) origins))))
              (when (integerp origin)
                (push (cons (car to) origin) origins)))
            (setq from (cdr from) to (cdr to))))))
    (unless (and (= (length branches) 1)
                 (or (= (length returns) 2) (= (length returns) 1)))
      (error "bytecode-native-boxed-branch: requires one branch and terminal returns"))
    (let* ((branch-block (car (cl-remove-if-not
                               (lambda (b) (cl-some (lambda (i) (eq (plist-get i :kind) 'branch))
                                                   (append (plist-get b :instructions) nil))) blocks)))
           (branch-ins (caar branches))
           (condition-index (cadar branches))
           (branch-op (plist-get branch-ins :opcode))
           (successors (append (plist-get branch-block :successors) nil))
           (taken (cl-find-if (lambda (edge) (eq (plist-get edge :kind) 'taken)) successors))
           (fall (cl-find-if (lambda (edge) (eq (plist-get edge :kind) 'fallthrough)) successors))
           (optional-default-shape
            (nelisp-bytecode-native-boxed-branch--optional-default-shape-p
             code constants argument-descriptor blocks compiler-input))
           (join-block (and (or (= (length blocks) 4) optional-default-shape)
                            (cl-find-if (lambda (b)
                                          (and (not (eq b branch-block))
                                               (= (length (plist-get b :successors)) 0)
                                               (cl-some (lambda (i) (eq (plist-get i :kind) 'return))
                                                        (append (plist-get b :instructions) nil))))
                                        blocks)))
           (join-start (and join-block (plist-get join-block :start)))
           (join-ins (and join-block (append (plist-get join-block :instructions) nil)))
           (join-input (and join-ins (car (plist-get (car join-ins) :inputs))))
           (arms (and join-block (delq join-block (delq branch-block (copy-sequence blocks)))))
           (legacy-shape
            (and (= (plist-get branch-block :start) 0)
                 taken fall (= (length returns) 2)
                 (cl-every (lambda (b)
                             (or (eq b branch-block)
                                 (= (length (plist-get b :successors)) 0))) blocks)))
           (join-shape
            (and join-block (= (plist-get branch-block :start) 0)
                 taken fall (= (length successors) 2) (= (length arms) 2)
                 (/= (plist-get taken :target) (plist-get fall :target))
                 (equal (sort (mapcar (lambda (edge) (plist-get edge :target))
                                      (list taken fall)) #'<)
                        (sort (mapcar (lambda (block) (plist-get block :start)) arms) #'<))
                 (= (length returns) 1)
                 (= (length join-ins) 1)
                 (eq (plist-get (car join-ins) :kind) 'return)
                 (consp join-input) (eq (car join-input) :entry)
                 (= (nth 1 join-input) join-start)
                 (cl-every (lambda (b)
                             (let ((ins (append (plist-get b :instructions) nil))
                                   (edges (append (plist-get b :successors) nil)))
                               (and (= (length edges) 1)
                                    (= (plist-get (car edges) :target) join-start)
                                    (= (plist-get (car (last ins)) :opcode) 130)
                                    (cl-every (lambda (i)
                                                (and (memq (plist-get i :kind)
                                                           '(constant stack-ref dup discard branch))
                                                     (or (not (eq (plist-get i :kind) 'branch))
                                                         (= (plist-get i :opcode) 130)))) ins)))) arms)))
           (optional-shape
            (and optional-default-shape join-block taken fall
                 (= (length returns) 1) (= (length arms) 1)
                 (= (plist-get branch-block :start) 0)
                 (= (plist-get join-block :start) 5)
                 (= (plist-get (car arms) :start) 4)
                 (= (plist-get taken :target) join-start)
                 (= (plist-get fall :target) (plist-get (car arms) :start))
                 (= condition-index 1)))
           (join-sources nil))
      (unless (or join-shape optional-shape (and (not join-block) legacy-shape))
        (error "bytecode-native-boxed-branch: unsupported CFG shape legacy=%S join=%S arms=%S returns=%d"
               legacy-shape join-block arms (length returns)))
      (when join-block
        (dolist (arm arms)
          (let* ((edge (car (append (plist-get arm :successors) nil)))
                 (slots (append (plist-get edge :slots) nil))
                 (targets (append (plist-get edge :target-slots) nil))
                 (index (cl-position join-input targets :test #'equal))
                 (origin (and index (cdr (assoc (nth index slots) origins)))))
            (unless (integerp origin)
              (error "bytecode-native-boxed-branch: join source is not rooted"))
            (push (cons arm origin) join-sources))))
      (setq asm (nelisp-asm-x86_64-make-buffer 'sysv)
            object (make-temp-file "nelisp-bytecode-boxed-branch-" nil ".o"))
      (unwind-protect
          (progn
            (dolist (block blocks)
              (nelisp-asm-x86_64-define-label
               asm (if (= (plist-get block :start) 0)
                       entry-name
                     (nelisp-bytecode-native-boxed-branch--label
                      (plist-get block :start))))
              (when (and join-block (= (plist-get block :start) 0))
                ;; A real stack cell receives the selected rooted cell pointer.
                (nelisp-asm-x86_64-push asm 'rax))
              (if (eq block branch-block)
                  (let ((condition-reg (aref arg-regs condition-index)))
                    ;; Boxed ABI arguments are pointers to rooted Sexp cells,
                    ;; not direct object pointers. The tag word at offset zero
                    ;; is 0 for nil, 1 for t, and 7 for cons.
                    (nelisp-asm-x86_64-mov-reg-mem-disp8
                     asm 'rax condition-reg 0)
                    (when optional-shape
                      ;; The taken edge reaches the return join directly.
                      ;; Seed its rooted cell with the supplied optional arg;
                      ;; the fallback block overwrites it when arg is nil.
                      (nelisp-asm-x86_64-mov-mem-rsp-disp-reg
                       asm 0 condition-reg))
                    (nelisp-asm-x86_64-cmp-imm32
                     asm 'rax nelisp-native-load-tag-nil)
                    (if (memq branch-op '(131 133))
                        (nelisp-asm-x86_64-jz-rel32
                         asm (nelisp-bytecode-native-boxed-branch--label (plist-get taken :target)))
                      (nelisp-asm-x86_64-jnz-rel32
                       asm (nelisp-bytecode-native-boxed-branch--label (plist-get taken :target))))
                    (nelisp-asm-x86_64-jmp-rel32
                     asm (nelisp-bytecode-native-boxed-branch--label (plist-get fall :target))))
                (if join-block
                    (if (eq block join-block)
                        (progn
                          (nelisp-asm-x86_64-mov-reg-mem-rsp-disp asm 'rax 0)
                          (nelisp-asm-x86_64-mov-reg-reg asm 'rsp 'rbp)
                          (nelisp-asm-x86_64-pop asm 'rbp)
                          (nelisp-asm-x86_64-ret asm))
                      (let ((origin (cdr (assq block join-sources))))
                        (unless origin (error "bytecode-native-boxed-branch: join arm missing"))
                        (nelisp-asm-x86_64-mov-mem-rsp-disp-reg asm 0 (aref arg-regs origin))
                        (nelisp-asm-x86_64-jmp-rel32
                         asm (nelisp-bytecode-native-boxed-branch--label join-start))))
                  (let ((returned (cdr (assq block returns))))
                    (unless returned (error "bytecode-native-boxed-branch: return arm missing"))
                    (nelisp-asm-x86_64-mov-reg-reg asm 'rax (aref arg-regs returned))
                    (nelisp-asm-x86_64-mov-reg-reg asm 'rsp 'rbp)
                    (nelisp-asm-x86_64-pop asm 'rbp)
                    (nelisp-asm-x86_64-ret asm)))))
            (nelisp-asm-x86_64-resolve-fixups asm)
            (let* ((unit (nelisp-asm-x86_64-buffer-to-unit asm entry-name))
                   (text (cdr (assq 'text (plist-get unit :sections))))
                   (symbols (mapcar (lambda (s) (let ((c (copy-sequence s)))
                                                  (plist-put c :size (length text))))
                                    (plist-get unit :symbols)))
                   (entry (list :name entry-name :offset 0 :size (length text)
                                :arity (+ hidden-constant-count argument-count)
                                :param-class 'gp :param-repr 'sexp-ptr
                                :return-repr 'sexp-ptr :rt-slot-count 0 :body-offset 0)))
              (nelisp-elf-write-binary object (list :e-type 'rel :text text
                                                    :symbols symbols :relocs nil
                                                    :machine 'x86_64))
              (nelisp-artifact-write-native-link-unit
               artifact-path (concat "bytecode-boxed-branch:" entry-name) object
               (list :text text :rodata (unibyte-string) :data (unibyte-string)
                     :bss-size 0 :symbols symbols :relocs nil :extern-symbols nil
                     :machine 'x86_64 :defuns (list entry)))
              (setq written t)
              (list :status 'complete :artifact artifact-path :entry entry-name
                    :verified-cfg verified :machine-code text)))
        (when (and object (file-exists-p object)) (delete-file object))
        (unless written (when (file-exists-p artifact-path) (delete-file artifact-path))))))))

(provide 'nelisp-bytecode-native-boxed-branch)
;;; nelisp-bytecode-native-boxed-branch.el ends here
