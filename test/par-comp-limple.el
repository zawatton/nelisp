;;; par-comp-limple.el --- GNU native compiler front-end parity -*- lexical-binding: t; -*-

(when (fboundp 'nelisp--native-subr-arity)
  (require 'nelisp-comp nil t))
(require 'comp)

(defun par-comp-limple--capture (filename)
  (let* ((top (gethash "top_level_run" (comp-ctxt-funcs-h comp-ctxt)))
         (function (comp--symbol-func-to-fun '--anonymous-lambda))
         (args (comp-func-l-args function))
         (body (gethash 'bb_0 (comp-func-blocks function)))
         (call (nth 2 (assq 'set (comp-block-insns body)))))
    (throw 'par-comp-limple
           (list (file-name-extension filename)
                 (comp-args-min args) (comp-args-max args)
                 (car call) (cadr call)
                 (mapcar #'car (comp-block-insns body))
                 (mapcar #'car (comp-block-insns
                                (gethash 'entry (comp-func-blocks top))))
                 (vectorp (comp-ctxt-function-docs comp-ctxt))
                 (not (null (memq 'listp (comp-data-container-l
                                         (comp-ctxt-d-default comp-ctxt)))))
                 (gethash '1+ comp-subr-arities-h)))))

;; The reference can run these Lisp passes even when built without libgccjit.
;; Supply the comp.c interface only on such a GNU build.  The NeLisp run
;; must obtain the arity table and other compiler state from its bridge.
(when (and (not (fboundp 'nelisp--native-subr-arity))
           (not (native-comp-available-p)))
  (defvar comp-subr-arities-h (make-hash-table :test 'equal))
  (mapatoms (lambda (name)
              (let ((function (symbol-function name)))
                (when (subr-primitive-p function)
                  (puthash name (subr-arity function) comp-subr-arities-h)))))
  (defalias 'comp--init-ctxt #'ignore)
  (defalias 'comp--release-ctxt #'ignore))

(prin1
 (condition-case err
     (let ((comp-running-batch-compilation t)
           (output (expand-file-name "par-comp-limple.eln"
                                     temporary-file-directory)))
       (cl-letf (((symbol-function 'comp-ensure-native-compiler) #'ignore)
                 ((symbol-function 'comp--compile-ctxt-to-file0)
                  #'par-comp-limple--capture))
         (catch 'par-comp-limple
           (native-compile '(lambda (x) (1+ x)) output))))
   (error (list (car err) (cadr err)))))
(terpri)

;; The later passes read an instruction through a symbol macro in a
;; destructuring-bind initializer.  This also isolates that prerequisite
;; from native compiler availability.
(prin1 (condition-case err
           (eval '(let ((cell '(17)))
                    (cl-symbol-macrolet ((instruction cell))
                      (cl-destructuring-bind (opcode) instruction opcode)))
                 t)
         (error (list (car err) (cadr err)))))
(terpri)
