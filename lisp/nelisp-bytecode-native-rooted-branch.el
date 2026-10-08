;;; nelisp-bytecode-native-rooted-branch.el --- rooted conditional CAR/CDR -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'nelisp-bytecode-frame-ir)
(require 'nelisp-bytecode-compiler-input)

(defvar nelisp-bytecode-native-rooted-branch--results nil)

(defconst nelisp-bytecode-native-rooted-branch-entry
  "nl_native_rooted_branch_probe_v1")

(defun nelisp-bytecode-native-rooted-branch--empty-successors-p (value)
  (or (null value) (and (vectorp value) (= (length value) 0))))

(defun nelisp-bytecode-native-rooted-branch-plan (input)
  "Admit only GNU 31.1's complete three-block conditional CAR/CDR frame."
  (let* ((code (plist-get input :code))
         (frame (and (eq (plist-get input :status) 'complete)
                     (nelisp-bytecode-frame-ir-build
                      code (plist-get input :constants)
                      (plist-get input :initial-stack-depth))))
         (blocks (and frame (plist-get frame :blocks)))
         (descriptor (plist-get input :argument-descriptor))
         (entry-block (and (vectorp blocks) (> (length blocks) 0) (aref blocks 0))))
    (if (and (eq (plist-get frame :status) 'complete)
             (equal code (unibyte-string 2 131 7 0 1 64 135 65 135))
             (vectorp blocks) (= (length blocks) 3)
             (equal (mapcar (lambda (block) (plist-get block :start))
                            (append blocks nil))
                    '(0 4 7))
             (equal (mapcar (lambda (edge) (plist-get edge :target))
                            (plist-get entry-block :successors))
                    '(4 7))
             (= (plist-get (aref (plist-get entry-block :instructions) 1) :opcode) 131)
             (= (plist-get (aref (plist-get entry-block :instructions) 1) :operand) 7)
             (nelisp-bytecode-native-rooted-branch--empty-successors-p
              (plist-get (aref blocks 1) :successors))
             (nelisp-bytecode-native-rooted-branch--empty-successors-p
              (plist-get (aref blocks 2) :successors))
             (= (or (plist-get input :argument-count) -1) 3)
             (= (or (plist-get input :initial-stack-depth) -1) 3)
             (= (or (plist-get input :required-argument-count) -1) 3)
             (= (or (plist-get input :argument-min) -1) 3)
             (= (or (plist-get input :argument-max) -1) 3)
             (= (or descriptor -1) 771)
             (not (plist-get input :rest-argument-p))
             (null (plist-get input :capture-values-available))
             (null (plist-get input :potential-capture-placeholder-p))
             (or (null (plist-get input :closure-template-descriptor))
                 (and (eq (plist-get input :metadata-role)
                          'lazy-documentation-reference)
                      (eq (plist-get input :documentation-reference)
                          (plist-get input :closure-template-descriptor))
                      (nelisp-bytecode-compiler-input-documentation-reference-p
                       (plist-get input :documentation-reference))))
             (vectorp (plist-get input :constants))
             (= (length (plist-get input :constants)) 0))
        (list :status 'complete :argument-count 3 :required-root-count 5
              :condition-root 1 :car-root 2 :cdr-root 3 :output-root 4
              :imports '("nl_native_car_v2" "nl_native_cdr_v2"
                         "nl_root_pin_slot_v2")
              :frame frame)
      (list :status 'unsupported :reason "outside three-block rooted branch scope"))))

(defun nelisp-bytecode-native-rooted-branch--body ()
  "Generate the source body for the fixed branch gateway entry."
  '(defun nl_native_rooted_branch_probe_v1 (env ticket argument-count root-count)
     (let ((condition-slot (extern-call nl_root_pin_slot_v2 env ticket 1 0 0 0)))
       (if (= condition-slot 0) 2
         (if (= (ptr-read-u64 condition-slot 0) 0)
             (let ((gateway-status
                    (extern-call nl_native_cdr_v2 env ticket 3 4 0 0)))
               (if (= gateway-status 0) 0
                 (if (= gateway-status 1) 259 gateway-status)))
           (let ((gateway-status
                  (extern-call nl_native_car_v2 env ticket 2 4 0 0)))
             (if (= gateway-status 0) 0
               (if (= gateway-status 1) 258 gateway-status))))))))

(cl-defun nelisp-bytecode-native-rooted-branch-build (input artifact-path)
  "Compile one verified branch INPUT into an authenticated v2 artifact."
  (let ((plan (nelisp-bytecode-native-rooted-branch-plan input)))
    (unless (eq (plist-get plan :status) 'complete)
      (cl-return-from nelisp-bytecode-native-rooted-branch-build
        (list :status 'unsupported :reason (plist-get plan :reason))))
    (unless (and (stringp artifact-path) (string-suffix-p ".nelr" artifact-path)
                 (not (file-exists-p artifact-path)))
      (cl-return-from nelisp-bytecode-native-rooted-branch-build
        (list :status 'unsupported :reason "artifact path must be absent .nelr")))
    (require 'nelisp-runtime-reload-abi)
    (require 'nelisp-native-load)
    (require 'nelisp-bytecode-native-package)
    (let* ((binary (nelisp-native-load-running-binary-sha256))
           (source (make-temp-file "nelisp-rooted-branch-" nil ".el"))
           forms manifest result)
      (unless (and (stringp binary) (nelisp-runtime-reload-contract-matches-p))
        (delete-file source)
        (error "rooted-branch: runtime identity unavailable"))
      (dolist (contract nelisp-runtime-reload-gc-contract)
        (let ((fn (intern (car contract))) args)
          (dotimes (i (cdr contract))
            (setq args (append args (list (intern (format "arg%d" i))))))
          (push (list 'defun fn args 0) forms)))
      (setq forms (append (nreverse forms)
                          (list (nelisp-bytecode-native-rooted-branch--body))))
      (unwind-protect
          (progn
            (with-temp-file source
              (let ((print-length nil) (print-level nil))
                (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))))
            (setq manifest
                  (nelisp-native-load-raw-v2-compile-file
                   source artifact-path "gnu31-rooted-branch-v1" binary nil nil nil
                   (list :input input :plan plan
                         :entry-ast (nelisp-bytecode-native-rooted-branch--body))))
            (setq result
                  (list :status 'complete :artifact-kind 'raw-runtime-v2
                        :artifact-path (expand-file-name artifact-path)
                        :manifest manifest :entry-name nelisp-bytecode-native-rooted-branch-entry
                        :arity 4 :input input :plan plan :argument-count 3
                        :runtime-binary-sha256 binary
                        :artifact-sha256
                        (nelisp-bytecode-native-package-raw-file-sha256 artifact-path)
                        :gateway-imports (plist-get plan :imports)
                        :source-tag "gnu31-rooted-branch-v1"))
            (push (list result (secure-hash 'sha256 (prin1-to-string result)))
                  nelisp-bytecode-native-rooted-branch--results)
            result)
        (when (file-exists-p source) (delete-file source))))))

(defun nelisp-bytecode-native-rooted-branch-authenticated-result-p (result)
  "Return non-nil for an unchanged producer result and artifact."
  (let ((record (assq result nelisp-bytecode-native-rooted-branch--results)))
    (and record
         (equal (cadr record) (secure-hash 'sha256 (prin1-to-string result)))
         (file-readable-p (plist-get result :artifact-path))
         (equal (plist-get result :artifact-sha256)
                (nelisp-bytecode-native-package-raw-file-sha256
                 (plist-get result :artifact-path))))))

(provide 'nelisp-bytecode-native-rooted-branch)
;;; nelisp-bytecode-native-rooted-branch.el ends here
