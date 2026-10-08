;;; nelisp-bytecode-native-rooted-branch-join.el --- joined branch gateways -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'nelisp-bytecode-frame-ir)
(require 'nelisp-bytecode-compiler-input)

(defvar nelisp-bytecode-native-rooted-branch-join--results nil)
(defconst nelisp-bytecode-native-rooted-branch-join-entry
  "nl_native_rooted_branch_join_probe_v1")

(defun nelisp-bytecode-native-rooted-branch-join-plan (input operation)
  "Verify GNU31 INPUT's exact joined conditional frame for OPERATION.
OPERATION is `car' or `cdr'; every other verified frame is unsupported."
  (let* ((code (plist-get input :code))
         (frame (and (eq (plist-get input :status) 'complete)
                     (nelisp-bytecode-frame-ir-build
                      code (plist-get input :constants)
                      (plist-get input :initial-stack-depth))))
         (blocks (and frame (plist-get frame :blocks)))
         (opcode (if (eq operation 'car) 64 (if (eq operation 'cdr) 65 -1)))
         (entry (and (vectorp blocks) (= (length blocks) 4) (aref blocks 0))))
    (if (and (eq (plist-get frame :status) 'complete)
             (memq operation '(car cdr))
             (vectorp blocks) (= (length blocks) 4) entry
             (equal code (if (= opcode 64)
                             (unibyte-string 2 131 8 0 1 130 9 0 137 64 135)
                           (unibyte-string 2 131 8 0 1 130 9 0 137 65 135)))
             (equal (mapcar (lambda (b) (plist-get b :start)) (append blocks nil))
                    '(0 4 8 9))
             (equal (mapcar (lambda (e) (plist-get e :target))
                            (append (plist-get entry :successors) nil)) '(4 8))
             (equal (mapcar (lambda (e) (plist-get e :target))
                            (append (plist-get (aref blocks 1) :successors) nil)) '(9))
             (equal (mapcar (lambda (e) (plist-get e :target))
                            (append (plist-get (aref blocks 2) :successors) nil)) '(9))
             (nelisp-bytecode-native-rooted-branch-join--empty-successors-p
              (plist-get (aref blocks 3) :successors))
             (= (or (plist-get input :argument-count) -1) 3)
             (= (or (plist-get input :initial-stack-depth) -1) 3)
             (= (or (plist-get input :required-argument-count) -1) 3)
             (= (or (plist-get input :argument-min) -1) 3)
             (= (or (plist-get input :argument-max) -1) 3)
             (= (or (plist-get input :argument-descriptor) -1) 771)
             (not (plist-get input :rest-argument-p))
             (null (plist-get input :capture-values-available))
             (null (plist-get input :potential-capture-placeholder-p))
             (or (null (plist-get input :closure-template-descriptor))
                 (and (eq (plist-get input :metadata-role) 'lazy-documentation-reference)
                      (eq (plist-get input :documentation-reference)
                          (plist-get input :closure-template-descriptor))
                      (nelisp-bytecode-compiler-input-documentation-reference-p
                       (plist-get input :documentation-reference))))
             (vectorp (plist-get input :constants))
             (= (length (plist-get input :constants)) 0)
             (= (plist-get (aref (plist-get (aref blocks 3) :instructions) 0) :opcode)
                opcode))
        (list :status 'complete :operation operation :argument-count 3
              :required-root-count 5 :condition-root 1 :left-root 2 :right-root 3
              :output-root 4 :gateway-imports
              (list (format "nl_native_%s_v2" operation) "nl_root_pin_slot_v2")
              :frame frame)
      (list :status 'unsupported :reason "outside verified rooted branch-join scope"))))

(defun nelisp-bytecode-native-rooted-branch-join--empty-successors-p (value)
  (or (null value) (and (vectorp value) (= (length value) 0))))

(defun nelisp-bytecode-native-rooted-branch-join--body (operation)
  "Generate the fixed joined entry AST for gateway OPERATION."
  (let ((gateway (intern (format "nl_native_%s_v2" operation))))
    `(defun nl_native_rooted_branch_join_probe_v1 (env ticket argument-count root-count)
       (if (/= argument-count 3) 3
         (if (/= root-count 5) 3
       (let ((condition-slot (extern-call nl_root_pin_slot_v2 env ticket 1 0 0 0)))
         (if (= condition-slot 0) 2
           (if (= (ptr-read-u64 condition-slot 0) 0)
               (let ((gateway-status
                      (extern-call ,gateway env ticket 3 4 0 0)))
                 (if (= gateway-status 0) 0
                   (if (= gateway-status 1) 259 gateway-status)))
             (let ((gateway-status
                    (extern-call ,gateway env ticket 2 4 0 0)))
               (if (= gateway-status 0) 0
                 (if (= gateway-status 1) 258 gateway-status)))))))))))

(cl-defun nelisp-bytecode-native-rooted-branch-join-build (input operation artifact-path)
  "Compile verified joined branch INPUT using OPERATION into ARTIFACT-PATH."
  (let ((plan (nelisp-bytecode-native-rooted-branch-join-plan input operation)))
    (unless (eq (plist-get plan :status) 'complete)
      (cl-return-from nelisp-bytecode-native-rooted-branch-join-build
        (list :status 'unsupported :reason (plist-get plan :reason))))
    (unless (and (stringp artifact-path) (string-suffix-p ".nelr" artifact-path)
                 (not (file-exists-p artifact-path)))
      (cl-return-from nelisp-bytecode-native-rooted-branch-join-build
        (list :status 'unsupported :reason "artifact path must be absent .nelr")))
    (require 'nelisp-runtime-reload-abi)
    (require 'nelisp-native-load)
    (require 'nelisp-bytecode-native-package)
    (let* ((binary (nelisp-native-load-running-binary-sha256))
           (source (make-temp-file "nelisp-rooted-branch-join-" nil ".el"))
           (entry-ast (nelisp-bytecode-native-rooted-branch-join--body operation))
           forms manifest result)
      (unless (and (stringp binary) (nelisp-runtime-reload-contract-matches-p))
        (delete-file source)
        (error "rooted-branch-join: runtime identity unavailable"))
      (dolist (contract nelisp-runtime-reload-gc-contract)
        (let ((fn (intern (car contract))) args)
          (dotimes (i (cdr contract))
            (setq args (append args (list (intern (format "arg%d" i))))))
          (push (list 'defun fn args 0) forms)))
      (setq forms (append (nreverse forms) (list entry-ast)))
      (unwind-protect
          (progn
            (with-temp-file source
              (let ((print-length nil) (print-level nil))
                (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))))
            (setq manifest
                  (nelisp-native-load-raw-v2-compile-file
                   source artifact-path "gnu31-rooted-branch-join-v1" binary
                   nil nil nil nil
                   (list :input input :operation operation :plan plan
                         :entry-ast entry-ast)))
            (setq result
                  (list :status 'complete :artifact-kind 'raw-runtime-v2
                        :artifact-path (expand-file-name artifact-path)
                        :manifest manifest :entry-name nelisp-bytecode-native-rooted-branch-join-entry
                        :gateway-operation operation :arity 4 :input input :plan plan
                        :argument-count 3 :runtime-binary-sha256 binary
                        :artifact-sha256
                        (nelisp-bytecode-native-package-raw-file-sha256 artifact-path)
                        :gateway-imports (plist-get plan :gateway-imports)
                        :source-tag "gnu31-rooted-branch-join-v1"))
            (push (list result (secure-hash 'sha256 (prin1-to-string result)))
                  nelisp-bytecode-native-rooted-branch-join--results)
            result)
        (when (file-exists-p source) (delete-file source))))))

(defun nelisp-bytecode-native-rooted-branch-join-authenticated-result-p (result)
  "Return non-nil for an unchanged result and currently intact artifact."
  (let ((record (assq result nelisp-bytecode-native-rooted-branch-join--results)))
    (and record
         (equal (cadr record) (secure-hash 'sha256 (prin1-to-string result)))
         (file-readable-p (plist-get result :artifact-path))
         (equal (plist-get result :artifact-sha256)
                (nelisp-bytecode-native-package-raw-file-sha256
                 (plist-get result :artifact-path))))))

(provide 'nelisp-bytecode-native-rooted-branch-join)
;;; nelisp-bytecode-native-rooted-branch-join.el ends here
