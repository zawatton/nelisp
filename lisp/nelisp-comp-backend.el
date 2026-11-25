;;; nelisp-comp-backend.el --- Lower finalized GNU LIMPLE leaves -*- lexical-binding: t; -*-

(require 'comp)
(require 'nelisp-eln-emitter)

(defun nelisp-comp-backend--instructions (block)
  "Return instructions in BLOCK, omitting analysis annotations."
  (cl-remove-if (lambda (insn) (memq (car insn) '(comment assume)))
                (comp-block-insns block)))

(defun nelisp-comp-backend--slot-p (operand slot)
  (and (comp-mvar-p operand) (eql (comp-mvar-slot operand) slot)))

(defun nelisp-comp-backend--immediate-p (operand value)
  (and (comp-mvar-p operand)
       (comp-cstr-imm-vld-p operand)
       (equal (comp-cstr-imm operand) value)))

(defun nelisp-comp-backend--reject (function block instructions)
  (signal 'native-ice
          (list "Unsupported LIMPLE for the ELN leaf emitter"
                function block instructions)))

(defun nelisp-comp-backend--leaf-ir (function arity)
  "Lower FUNCTION's fixed-arity constant LIMPLE to emitter IR."
  (let* ((name (comp-func-name function))
         (blocks (comp-func-blocks function))
         (entry (gethash 'entry blocks))
         (prologue (and entry (nelisp-comp-backend--instructions entry)))
         (jump (car (last prologue)))
         (label (cadr jump))
         (block (gethash label blocks))
         (body (and block (nelisp-comp-backend--instructions block))))
    (unless (and (= (hash-table-count blocks) 2)
                 (equal jump (list 'jump label))
                 (not (eq label 'entry)) block)
      (nelisp-comp-backend--reject name 'entry prologue))
    (pcase (list arity (butlast prologue) body)
      (`(0 nil ((setimm ,dst ,value) (return ,result)))
       (unless (and (integerp value) (<= 0 value #x1fffffff)
                    (nelisp-comp-backend--slot-p dst 0)
                    (nelisp-comp-backend--slot-p result 0))
         (nelisp-comp-backend--reject name label body))
       (vector 'defun :name name :params nil :body (vector 'imm :value value)))
      (_ (nelisp-comp-backend--reject name label body)))))

(defun nelisp-comp-backend--unit-ir (context)
  "Validate CONTEXT against the emitter's one-definition registration unit."
  (unless (and (comp-ctxt-p context)
               (= (hash-table-count (comp-ctxt-funcs-h context)) 2)
               (let ((docs (comp-ctxt-function-docs context)))
                 (and (vectorp docs) (= (length docs) 1)
                      (or (null (aref docs 0)) (stringp (aref docs 0))))))
    (nelisp-comp-backend--reject 'comp-ctxt nil nil))
  (let* ((functions (comp-ctxt-funcs-h context))
         (top (gethash "top_level_run" functions))
         (blocks (and top (comp-func-blocks top)))
         (entry (and blocks (gethash 'entry blocks)))
         (instructions (and entry (nelisp-comp-backend--instructions entry))))
    (pcase instructions
      (`((set-par-to-local ,incoming 0)
         (set ,dst (call comp--register-subr
                         ,name ,export ,min ,max ,_type ,rest ,unit))
         (return ,result))
       (let* ((symbol (and (comp-cstr-imm-vld-p name) (comp-cstr-imm name)))
              (c-name (and (comp-cstr-imm-vld-p export) (comp-cstr-imm export)))
              (arity (and (comp-cstr-imm-vld-p min) (comp-cstr-imm min)))
              (function (gethash c-name functions))
              (args (and (comp-func-l-p function) (comp-func-l-args function))))
         (unless (and (= (hash-table-count blocks) 1)
                      args (symbolp symbol) symbol
                      (eq symbol (comp-func-name function))
                      (equal c-name
                             (condition-case nil
                                 (nelisp-eln-emitter--symbol-name symbol)
                               (error nil)))
                      (memq arity '(0 1)) (comp-args-p args)
                      (eql arity (comp-args-min args)) (eql arity (comp-args-max args))
                      (not (comp-func-has-non-local function))
                      (nelisp-comp-backend--immediate-p max arity)
                      (nelisp-comp-backend--immediate-p rest '(0 nil nil))
                      (nelisp-comp-backend--slot-p incoming 0)
                      (nelisp-comp-backend--slot-p unit 0)
                      (nelisp-comp-backend--slot-p dst 1)
                      (nelisp-comp-backend--slot-p result 1))
           (nelisp-comp-backend--reject 'top-level-run 'entry instructions))
         (nelisp-comp-backend--leaf-ir function arity)))
      (_ (nelisp-comp-backend--reject 'top-level-run 'entry instructions)))))

(defun nelisp-comp-backend-write (context filename)
  "Emit CONTEXT's leaf unit and atomically replace FILENAME."
  (let* ((ir (nelisp-comp-backend--unit-ir context))
         (directory (make-temp-file
                     (expand-file-name ".nelisp-comp-" (file-name-directory filename))
                     t))
         (temporary (expand-file-name "unit.eln" directory)))
    (unwind-protect
        (progn
          (nelisp-eln-emitter-write-ir ir temporary nil
                                       (comp-ctxt-function-docs context))
          (comp-delete-or-replace-file filename temporary)
          filename)
      (ignore-errors (delete-directory directory t)))))

(provide 'nelisp-comp-backend)
