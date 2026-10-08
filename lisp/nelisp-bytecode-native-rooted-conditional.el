;;; nelisp-bytecode-native-rooted-conditional.el --- rooted conditional probe -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'nelisp-bytecode-frame-ir)

(defvar nelisp-bytecode-native-rooted-conditional--results nil)

(defun nelisp-bytecode-native-rooted-conditional-plan (input)
  "Recognize only GNU 31.1's fixed `(if x y z)' argument-selection frame."
  (let* ((code (plist-get input :code))
         (frame (plist-get input :frame-result))
         (blocks (and (vectorp (plist-get frame :blocks))
                      (append (plist-get frame :blocks) nil)))
         (descriptor (plist-get input :argument-descriptor)))
    (if (and (eq (plist-get input :status) 'complete)
             (equal code (unibyte-string 2 131 6 0 1 135 135))
             (eq (plist-get frame :status) 'complete)
             (= (length blocks) 3)
             (= (or (plist-get input :argument-count) -1) 3)
             (= (or (plist-get input :initial-stack-depth) -1) 3)
             (= (or (plist-get input :required-argument-count) -1) 3)
             (= (or (plist-get input :argument-min) -1) 3)
             (= (or (plist-get input :argument-max) -1) 3)
             (= (or descriptor -1) 771)
             (not (plist-get input :rest-argument-p))
             (not (plist-get input :capture-values-available))
             (not (plist-get input :potential-capture-placeholder-p)))
        (list :status 'complete :argument-count 3 :required-root-count 4
              :imports '("nl_root_pin_slot_v2")
              :frame frame)
      (list :status 'unsupported :reason "outside fixed conditional probe"))))

(defun nelisp-bytecode-native-rooted-conditional-input-p (input)
  "Return non-nil only for the exact capture-free GNU fixed conditional input."
  (let ((plan (nelisp-bytecode-native-rooted-conditional-plan input)))
    (and (eq (plist-get plan :status) 'complete)
         (vectorp (plist-get input :constants))
         (= (length (plist-get input :constants)) 0)
         (null (plist-get input :closure-template-descriptor))
         (null (plist-get input :capture-values-available))
         (null (plist-get input :potential-capture-placeholder-p))
         t)))

(defun nelisp-bytecode-native-rooted-conditional-body ()
  "Return the fixed source AST for the authenticated conditional probe."
  (let ((entry 'nl_native_rooted_conditional_probe_v1))
    `(defun ,entry (env ticket argument-count root-count)
       (let ((condition-slot
              (extern-call nl_root_pin_slot_v2 env ticket 1 0 0 0))
             (truth-slot
              (extern-call nl_root_pin_slot_v2 env ticket 2 0 0 0))
             (false-slot
              (extern-call nl_root_pin_slot_v2 env ticket 3 0 0 0)))
         (if (= condition-slot 0) 2
           (if (= truth-slot 0) 2
             (if (= false-slot 0) 2
               (if (= (ptr-read-u64 condition-slot 0) 0) 259 258))))))))

(defun nelisp-bytecode-native-rooted-conditional-build (input artifact-path)
  "Build the source-verified fixed `(if x y z)' slot-tag selection probe."
  (let ((plan (nelisp-bytecode-native-rooted-conditional-plan input)))
    (if (not (eq (plist-get plan :status) 'complete))
        (list :status 'unsupported :reason (plist-get plan :reason))
      (if (not (and (stringp artifact-path)
                    (string-suffix-p ".nelr" artifact-path)
                    (not (file-exists-p artifact-path))))
          (list :status 'unsupported :reason "artifact path must be absent .nelr")
        (require 'nelisp-runtime-reload-abi)
        (require 'nelisp-native-load)
        (require 'nelisp-bytecode-native-package)
        (let* ((binary (nelisp-native-load-running-binary-sha256))
               (entry "nl_native_rooted_conditional_probe_v1")
               (source (make-temp-file "nelisp-rooted-conditional-" nil ".el"))
               (forms nil) manifest result)
          (unless (and (stringp binary) (nelisp-runtime-reload-contract-matches-p))
            (delete-file source)
            (error "rooted-conditional: runtime identity unavailable"))
          (dolist (contract nelisp-runtime-reload-gc-contract)
            (let ((fn (intern (car contract))) (args nil))
              (dotimes (i (cdr contract))
                (setq args (append args (list (intern (format "arg%d" i))))))
              (push (list 'defun fn args 0) forms)))
          (setq forms (append (nreverse forms)
                              (list (nelisp-bytecode-native-rooted-conditional-body))))
          (unwind-protect
              (progn
                (with-temp-file source
                  (let ((print-length nil) (print-level nil))
                    (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))))
                (setq manifest
                      (nelisp-native-load-raw-v2-compile-file
                       source artifact-path "gnu31-rooted-conditional-v1" binary nil nil
                       (list :entry-name entry :entry-ast
                             (nelisp-bytecode-native-rooted-conditional-body))))
                (setq artifact-path (expand-file-name artifact-path)
                      result (list :status 'complete :artifact-kind 'raw-runtime-v2
                                   :artifact-path artifact-path :manifest manifest
                                   :entry-name entry :arity 4 :plan plan :input input
                                   :argument-count 3 :required-root-count 4
                                   :runtime-binary-sha256 binary
                                   :artifact-sha256
                                   (nelisp-bytecode-native-package-raw-file-sha256
                                    artifact-path)
                                   :source-tag "gnu31-rooted-conditional-v1"))
                (push (list result (secure-hash 'sha256 (prin1-to-string result)))
                      nelisp-bytecode-native-rooted-conditional--results)
                result)
            (when (file-exists-p source) (delete-file source))))))))

(defun nelisp-bytecode-native-rooted-conditional-authenticated-result-p (result)
  "Return non-nil for unchanged results created by the conditional producer."
  (let ((record (assq result nelisp-bytecode-native-rooted-conditional--results)))
    (and record
         (equal (cadr record) (secure-hash 'sha256 (prin1-to-string result)))
         (file-readable-p (plist-get result :artifact-path))
         (equal (plist-get result :artifact-sha256)
                (nelisp-bytecode-native-package-raw-file-sha256
                 (plist-get result :artifact-path))))))

(provide 'nelisp-bytecode-native-rooted-conditional)
;;; nelisp-bytecode-native-rooted-conditional.el ends here
