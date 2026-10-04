;;; nelisp-native-raw-v2-cons-build.el --- Build the rooted CONS smoke unit -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'nelisp-runtime-reload-abi)
(require 'nelisp-native-load)
(require 'nelisp-aot-compiler)
(require 'nelisp-standalone-build)

(let* ((source-path (getenv "NELISP_CONS_SOURCE"))
       (artifact-path (getenv "NELISP_CONS_ARTIFACT"))
       (binary-sha256 (getenv "NELISP_CONS_BINARY_SHA"))
       (forms nil))
  (unless (and source-path artifact-path
               (string-match-p "\\`[0-9a-f]\\{64\\}\\'" binary-sha256))
    (error "CONS smoke build requires source, artifact, and binary SHA"))
  (dolist (entry nelisp-runtime-reload-gc-contract)
    (let ((name (intern (car entry)))
          (arity (cdr entry))
          (args nil))
      (dotimes (index arity)
        (setq args (append args (list (intern (format "arg%d" index))))))
      (push (list 'defun name args 0) forms)))
  (setq forms
        (append (nreverse forms)
                '((defun nl_native_cons_probe
                    (env ticket left-index right-index output-index)
                    (extern-call nl_native_cons_v2
                                 env ticket left-index right-index output-index 0)))))
  (with-temp-file source-path
    (let ((print-length nil) (print-level nil))
      (dolist (form forms)
        (prin1 form (current-buffer))
        (insert "\n"))))
  (let ((nelisp-standalone--target 'linux-x86_64))
    (nelisp-native-load-raw-v2-compile-file
     source-path artifact-path "cons-rooted-v2-smoke" binary-sha256))
  (let* ((bad-source (concat source-path ".bad"))
         (bad-artifact (concat artifact-path ".bad"))
         (bad-forms (copy-sequence forms)))
    (setcar (last bad-forms)
            '(defun nl_native_cons_probe
               (env ticket left-index right-index output-index)
               (extern-call nl_root_pin_slot_v2
                            env ticket left-index right-index output-index 0)))
    (with-temp-file bad-source
      (let ((print-length nil) (print-level nil))
        (dolist (form bad-forms)
          (prin1 form (current-buffer))
          (insert "\n"))))
    (unwind-protect
        (progn
          (should-error
           (nelisp-native-load-raw-v2-compile-file
            bad-source bad-artifact "cons-rooted-v2-negative" binary-sha256)
           :type 'error)
          (when (file-exists-p bad-artifact)
            (error "Unsupported v2 root-pin import published an artifact")))
      (when (file-exists-p bad-source) (delete-file bad-source))
      (when (file-exists-p bad-artifact) (delete-file bad-artifact))))
  (unless (null (nelisp-native-load-raw-v2-check
                 (nelisp-native-load-manifest artifact-path)
                 "nl_native_cons_probe"))
    (error "CONS smoke artifact failed its v2 manifest preflight")))

;;; nelisp-native-raw-v2-cons-build.el ends here
