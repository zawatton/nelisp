;;; nelisp-bytecode-native-unary-chain.el --- bounded CAR/CDR chains -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'nelisp-bytecode-compiler-input)

(defun nelisp-bytecode-native-unary-chain-operations (input)
  "Return the verified GNU 31.1 CAR/CDR sequence in INPUT, or nil.

Only a source-free, single-block CAR/CDR body followed by RETURN is admitted.
The exact GNU argument-preserving DUP preamble is permitted and removed."
  (let* ((code (plist-get input :code))
         (bytes (and (stringp code) (append code nil)))
         (body (and (>= (length bytes) 2) (butlast bytes)))
         (argument-dup (and body (= (car body) 137)))
         (ops (if argument-dup (cdr body) body)))
    (and (eq (plist-get input :status) 'complete)
         (equal (plist-get input :argument-descriptor) 257)
         (= (or (plist-get input :argument-min) -1) 1)
         (= (or (plist-get input :argument-max) -1) 1)
         (= (or (plist-get input :argument-count) -1) 1)
         (= (or (plist-get input :initial-stack-depth) -1) 1)
         (= (or (plist-get input :declared-stack-depth) -1) 2)
         (= (or (plist-get input :computed-temporary-stack-depth) -1)
            (if argument-dup 2 1))
         (equal (plist-get input :constants) [])
         (= (car (last bytes)) 135)
         (cl-every (lambda (op) (memq op '(64 65))) ops)
         (let* ((frame (plist-get input :frame-result))
                (blocks (append (plist-get frame :blocks) nil)))
           (and (eq (plist-get frame :status) 'complete)
                (= (length blocks) 1)
                (equal (mapcar (lambda (instruction)
                                 (plist-get instruction :opcode))
                               (append (plist-get (car blocks) :instructions) nil))
                       bytes)))
         (mapcar (lambda (op) (if (= op 64) 'car 'cdr)) ops))))

(defun nelisp-bytecode-native-unary-chain--body (operations)
  (let ((destination-index
         (nelisp-bytecode-native-unary-chain-result-root-index operations))
        (body 0))
    (dolist (operation (reverse operations))
      (let* ((output-index destination-index)
             (call-input-index (if (= output-index 1) 2 1))
             (status (make-symbol "gateway-status"))
             (call (list 'extern-call
                         (intern (format "nl_native_%s_v2" operation))
                         'env 'ticket call-input-index output-index 0 0)))
        (setq body `(let ((,status ,call))
                      (cond ((= ,status 0) ,body)
                            ((= ,status 1)
                             ,(if (= call-input-index 1) 17 18))
                            (t ,status))))
        (setq destination-index call-input-index)))
    body))

(defun nelisp-bytecode-native-unary-chain-result-root-index (operations)
  "Return the authenticated root slot containing the final result."
  (if (cl-oddp (length operations)) 2 1))

(defun nelisp-bytecode-native-unary-chain-build (input artifact-path)
  "Compile verified unary INPUT to a raw-v2 chain artifact.

The production caller is
`nelisp-native-load-raw-v2-unary-chain-call'; ordinary boxed packages still
refuse this bounded single-block scope. The caller roots the input, invokes
the authenticated handle, and decodes the declared result slot. Return an
unsupported plist for nonmatching code. Successful results include
`:result-root-index', the rooted output slot after gateway calls."
  (let ((operations (nelisp-bytecode-native-unary-chain-operations input)))
    (if (not operations)
        (list :status 'unsupported :reason "input is not a verified CAR/CDR chain"
              :input input)
      (if (not (and (stringp artifact-path)
                    (string-suffix-p ".nelr" artifact-path)))
          (list :status 'unsupported :reason "unary chain requires a raw-v2 .nelr artifact"
                :input input)
        (let* ((entry "nl_native_chain_probe_v2")
               (source-path nil)
               (binary-sha256 (progn
                                (require 'nelisp-runtime-reload-abi)
                                (require 'nelisp-native-load)
                                (nelisp-native-load-running-binary-sha256)))
               forms result)
          (unless (and (stringp binary-sha256)
                       (nelisp-runtime-reload-contract-matches-p))
            (error "bytecode-native-unary-chain: running v2 runtime identity unavailable"))
          (dolist (contract nelisp-runtime-reload-gc-contract)
            (let ((function (intern (car contract))) (arity (cdr contract)) args)
              (dotimes (index arity)
                (setq args (append args (list (intern (format "arg%d" index))))))
              (push (list 'defun function args 0) forms)))
          (setq source-path (make-temp-file "nelisp-bytecode-chain-" nil ".el"))
          (unwind-protect
              (progn
                (with-temp-file source-path
                  (let ((print-length nil) (print-level nil))
                    (dolist (form
                             (append (nreverse forms)
                                     (list (list 'defun (intern entry)
                                                 '(env ticket input-index output-index)
                                                 (nelisp-bytecode-native-unary-chain--body
                                                  operations)))))
                      (prin1 form (current-buffer))
                      (insert "\n"))))
                (setq result
                      (nelisp-native-load-raw-v2-compile-file
                       source-path artifact-path "gnu31-bytecode-unary-chain-v2"
                       binary-sha256))
                (list :status 'complete :artifact-kind 'raw-runtime-v2
                      :artifact-path (expand-file-name artifact-path)
                      :manifest result :entry-name entry :arity 4
                      :operations operations
                      :chain-status-contract 'v2
                      :result-root-index
                      (nelisp-bytecode-native-unary-chain-result-root-index operations)
                      :runtime-abi (nelisp-native-load--runtime-abi-v2)
                      :gateway-imports
                      (cl-loop for operation in '(car cdr)
                               when (memq operation operations)
                               collect (format "nl_native_%s_v2" operation))))
            (when (and source-path (file-exists-p source-path))
              (delete-file source-path))))))))

(provide 'nelisp-bytecode-native-unary-chain)
;;; nelisp-bytecode-native-unary-chain.el ends here
