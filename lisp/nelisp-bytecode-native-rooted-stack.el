;;; nelisp-bytecode-native-rooted-stack.el --- rooted straight-line stack plans -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-frame-ir)
(require 'nelisp-bytecode-compiler-input)

(defvar nelisp-bytecode-native-rooted-stack--authenticated-results nil)

(defun nelisp-bytecode-native-rooted-stack-plan (input)
  "Plan rooted operations for a complete verified compiler INPUT.

INPUT is the complete plist returned by `nelisp-bytecode-compiler-input-build'.
The plan is data only; it allocates no artifact and emits no machine code."
  (let* ((code (plist-get input :code))
         (constants (plist-get input :constants))
         (frame (and (eq (plist-get input :status) 'complete)
                     (nelisp-bytecode-frame-ir-build
                      code constants (plist-get input :initial-stack-depth))))
         (blocks (and frame (plist-get frame :blocks)))
         (arity (plist-get input :argument-count))
         (args (plist-get input :argument-list))
         (descriptor (plist-get input :argument-descriptor))
         (initial-depth (plist-get input :initial-stack-depth))
         (stack nil)
         (ops nil) (const-roots (make-hash-table :test 'eql))
         (next-root 1) (failed t) final plan)
    (when (and (eq (plist-get frame :status) 'complete)
               (= (length blocks) 1) (integerp arity) (>= arity 0)
               (integerp initial-depth)
               (= (or (plist-get input :argument-min) -1) arity)
               (= (or (plist-get input :argument-max) -1) arity)
               (= (or (plist-get input :required-argument-count) -1) arity)
               (not (plist-get input :rest-argument-p))
               (not (plist-get input :capture-values-available))
               (not (plist-get input :potential-capture-placeholder-p))
               (or (null (plist-get input :closure-template-descriptor))
                   (and (eq (plist-get input :metadata-role)
                            'lazy-documentation-reference)
                        (eq (plist-get input :documentation-reference)
                            (plist-get input :closure-template-descriptor))
                        (nelisp-bytecode-compiler-input-documentation-reference-p
                         (plist-get input :documentation-reference))))
               (or (and (integerp descriptor) (= initial-depth arity) (null args))
                   (and (proper-list-p descriptor) (= initial-depth 0)
                        (proper-list-p args) (= (length args) arity)
                        (cl-every (lambda (arg)
                                    (and (symbolp arg)
                                         (not (and (fboundp 'special-variable-p)
                                                   (special-variable-p arg)))))
                                  args))))
      (setq failed nil)
      (setq next-root (1+ arity)
            stack (reverse (number-sequence 1 initial-depth)))
      (let ((instructions (append (plist-get (aref blocks 0) :instructions) nil)))
        (dolist (ins instructions)
          (let* ((kind (plist-get ins :kind)) (pc (plist-get ins :pc))
                 (operand (plist-get ins :operand)) (inputs nil) (outputs nil)
                 (operation nil))
            (unless failed
              (pcase kind
                ('constant
                 (let* ((idx (plist-get ins :constant-index))
                        (root (and (integerp idx) (<= 0 idx) (< idx (length constants))
                                   (or (gethash idx const-roots)
                                       (let ((r next-root))
                                         (setq next-root (1+ next-root))
                                         (puthash idx r const-roots) r)))))
                   (if root (push root stack) (setq failed t))))
                ((or 'stack-ref 'dup)
                 (let ((idx (if (eq kind 'dup) 0 operand)))
                   (if (and (integerp idx) (<= 0 idx) (< idx (length stack)))
                       (push (nth idx stack) stack) (setq failed t))))
                ('variable-ref
                 (let* ((ci (plist-get ins :constant-index))
                        (sym (and (integerp ci) (< -1 ci (length constants))
                                  (aref constants ci)))
                        (ai (cl-position sym args :test #'eq)))
                   (if ai (push (1+ ai) stack) (setq failed t))))
                ('discard (if stack (pop stack) (setq failed t)))
                ('primitive
                 ;; GNU 31.1 byte-defop identities; frame IR has already
                 ;; verified stack effects and exposes no primitive name yet.
                 (setq operation (cdr (assq (plist-get ins :opcode)
                                            '((64 . car) (65 . cdr) (66 . cons)))))
                 (unless (and operation
                              (>= (length stack) (if (eq operation 'cons) 2 1)))
                   (setq failed t))
                 (unless failed
                   (setq inputs (if (eq operation 'cons)
                                    (let ((right (pop stack))
                                          (left (pop stack)))
                                      (list left right))
                                  (list (pop stack))))
                   (setq outputs (list next-root))
                   (push next-root stack) (setq next-root (1+ next-root))
                   (push (list :pc pc :operation operation :inputs inputs
                               :outputs outputs) ops)))
                ('return (if stack (setq final (pop stack)) (setq failed t)))
                (_ (setq failed t))))))
      (unless failed
        (setq plan
              (list :status 'complete :initial-roots (number-sequence 1 arity)
              :constant-roots (let (pairs)
                                (maphash (lambda (k v) (push (cons k v) pairs)) const-roots)
                                (sort pairs (lambda (a b) (< (car a) (car b)))))
              :operations (nreverse ops) :final-root final
              :required-root-count next-root)))))
    (or plan
        (list :status 'unsupported :reason "input is outside rooted stack plan scope"
              :frame-status (plist-get frame :status)
              :block-count (and blocks (length blocks))))))

(defun nelisp-bytecode-native-rooted-stack--body (operations)
  "Emit nested authenticated gateway calls for planned OPERATIONS."
  (let ((body 0))
    (dolist (op (reverse operations))
      (let* ((kind (plist-get op :operation))
             (inputs (plist-get op :inputs))
             (output (car (plist-get op :outputs)))
             (args (pcase kind
                     ((or 'car 'cdr)
                      (list 'env 'ticket (car inputs) output 0 0))
                     ('cons
                      (list 'env 'ticket (car inputs) (cadr inputs) output 0))
                     (_ (error "rooted-stack: unsupported planned operation"))))
             (entry (intern (format "nl_native_%s_v2" kind)))
             (status (make-symbol "gateway-status")))
        (setq body `(let ((,status (extern-call ,entry ,@args)))
                      (cond ((= ,status 0) ,body)
                            ((= ,status 1) ,(+ 256 (car inputs)))
                            (t ,status))))))
    body))

(defun nelisp-bytecode-native-rooted-stack-body (operations)
  "Return generated AST for verified rooted-stack OPERATIONS."
  (nelisp-bytecode-native-rooted-stack--body operations))

(defun nelisp-bytecode-native-rooted-stack-build (input artifact-path)
  "Compile verified INPUT to a bounded raw-v2 rooted-stack .nelr artifact."
  (let ((plan (nelisp-bytecode-native-rooted-stack-plan input)))
    (if (not (eq (plist-get plan :status) 'complete))
        (list :status 'unsupported :reason "input is outside rooted stack compiler scope")
      (if (not (and (stringp artifact-path)
                    (string-suffix-p ".nelr" artifact-path)
                    (not (file-exists-p artifact-path))))
          (list :status 'unsupported :reason "artifact path must be absent .nelr")
        (if (null (plist-get plan :operations))
            (list :status 'unsupported
                  :reason "raw rooted stack requires a gateway operation")
        (require 'nelisp-runtime-reload-abi)
        (require 'nelisp-native-load)
        (require 'nelisp-bytecode-native-package)
        (let* ((binary-sha256 (nelisp-native-load-running-binary-sha256))
               (entry "nl_native_stack_probe_v1")
               (source-path nil) (forms nil) result)
          (unless (and (stringp binary-sha256)
                       (nelisp-runtime-reload-contract-matches-p))
            (error "rooted-stack: running v2 runtime identity unavailable"))
          (dolist (contract nelisp-runtime-reload-gc-contract)
            (let ((function (intern (car contract))) (arity (cdr contract)) args)
              (dotimes (i arity) (setq args (append args (list (intern (format "arg%d" i))))))
              (push (list 'defun function args 0) forms)))
          (setq source-path (make-temp-file "nelisp-rooted-stack-" nil ".el"))
          (unwind-protect
              (progn
                (with-temp-file source-path
                  (let ((print-length nil) (print-level nil))
                    (dolist (form (append (nreverse forms)
                                          (list (list 'defun (intern entry)
                                                      '(env ticket argument-count root-count)
                                                      (nelisp-bytecode-native-rooted-stack--body
                                                       (plist-get plan :operations))))))
                      (prin1 form (current-buffer)) (insert "\n"))))
                (setq result
                      (nelisp-native-load-raw-v2-compile-file
                       source-path artifact-path "gnu31-rooted-stack-v1" binary-sha256 nil
                       (list :input input :plan plan)))
                (let* ((artifact-path (expand-file-name artifact-path))
                       (artifact-sha256
                        (nelisp-bytecode-native-package-raw-file-sha256 artifact-path))
                       (result-plist
                        (list :status 'complete :artifact-kind 'raw-runtime-v2
                              :artifact-path artifact-path :manifest result
                              :entry-name entry :arity 4 :plan plan :input input
                              :constants (plist-get input :constants)
                              :argument-count (plist-get input :argument-count)
                              :source-tag "gnu31-rooted-stack-v1"
                              :runtime-abi (nelisp-native-load--runtime-abi-v2)
                              :runtime-binary-sha256 binary-sha256
                              :artifact-sha256 artifact-sha256
                              :gateway-imports
                              (sort
                               (delete-dups
                                (mapcar (lambda (op)
                                          (format "nl_native_%s_v2" (plist-get op :operation)))
                                        (plist-get plan :operations)))
                               #'string<))))
                  (push (list result-plist
                              (secure-hash 'sha256 (prin1-to-string result-plist))
                              artifact-path artifact-sha256)
                        nelisp-bytecode-native-rooted-stack--authenticated-results)
                  result-plist))
            (when (and source-path (file-exists-p source-path))
              (delete-file source-path)))))))))

(defun nelisp-bytecode-native-rooted-stack-authenticated-result-p (result)
  "Return non-nil only for an unchanged producer RESULT with intact artifact."
  (require 'nelisp-bytecode-native-package)
  (let ((registered (assq result nelisp-bytecode-native-rooted-stack--authenticated-results)))
    (and registered
         (equal (nth 1 registered) (secure-hash 'sha256 (prin1-to-string result)))
         (file-readable-p (nth 2 registered))
         (condition-case nil
             (equal (nth 3 registered)
                    (nelisp-bytecode-native-package-raw-file-sha256 (nth 2 registered)))
           (error nil)))))

(provide 'nelisp-bytecode-native-rooted-stack)
;;; nelisp-bytecode-native-rooted-stack.el ends here
