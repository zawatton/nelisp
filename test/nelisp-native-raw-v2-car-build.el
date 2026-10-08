;;; nelisp-native-raw-v2-car-build.el --- Build the CAR raw-v2 smoke unit -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'nelisp-runtime-reload-abi)
(require 'nelisp-native-load)
(require 'nelisp-aot-compiler)
(require 'nelisp-standalone-build)

(let* ((source-path (getenv "NELISP_CAR_SOURCE"))
       (artifact-path (getenv "NELISP_CAR_ARTIFACT"))
       (binary-sha256 (getenv "NELISP_CAR_BINARY_SHA"))
       (forms nil))
  (unless (and source-path artifact-path
               (string-match-p "\\`[0-9a-f]\\{64\\}\\'" binary-sha256))
    (error "CAR smoke build requires source, artifact, and binary SHA"))
  (dolist (entry nelisp-runtime-reload-gc-contract)
    (let ((name (intern (car entry)))
          (arity (cdr entry))
          (args nil))
      (dotimes (index arity)
        (setq args (append args (list (intern (format "arg%d" index))))))
      (push (list 'defun name args 0) forms)))
  ;; These inert contract fillers are never installed as runtime functions;
  ;; this smoke maps only the separate CAR probe export.
  (setq forms
        (append (nreverse forms)
                '((defun nl_native_car_probe
                    (env ticket input-index output-index)
                    (extern-call nl_native_car_v2
                                 env ticket input-index output-index 0 0)))))
  (with-temp-file source-path
    (let ((print-length nil) (print-level nil))
      (dolist (form forms)
        (prin1 form (current-buffer))
        (insert "\n"))))
  (let ((nelisp-standalone--target 'linux-x86_64))
    (nelisp-native-load-raw-v2-compile-file
     source-path artifact-path "car-import-smoke" binary-sha256))
  (let* ((bad-source (concat source-path ".bad"))
         (bad-artifact (concat artifact-path ".bad"))
         (bad-forms (copy-sequence forms)))
    (setcar (last bad-forms)
            '(defun nl_native_pin_probe (env ticket index)
               (extern-call nl_root_pin_begin_v2 env ticket index 0 0 0)))
    (with-temp-file bad-source
      (let ((print-length nil) (print-level nil))
        (dolist (form bad-forms)
          (prin1 form (current-buffer))
          (insert "\n"))))
    (unwind-protect
        (progn
          (should-error
           (nelisp-native-load-raw-v2-compile-file
            bad-source bad-artifact "car-import-negative" binary-sha256)
           :type 'error)
          (when (file-exists-p bad-artifact)
            (error "Unsupported v2 pin import published an artifact")))
      (when (file-exists-p bad-source) (delete-file bad-source))
      (when (file-exists-p bad-artifact) (delete-file bad-artifact))))
  (unless (null (nelisp-native-load-raw-v2-check
                 (nelisp-native-load-manifest artifact-path)
                 "nl_native_car_probe"))
    (error "CAR smoke artifact failed its v2 manifest preflight")))

;;; nelisp-native-raw-v2-car-build.el ends here
