;;; nelisp-native-raw-v2-object-op-build.el --- Build object-op ELF probe -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'nelisp-runtime-reload-abi)
(require 'nelisp-native-load)
(require 'nelisp-aot-compiler)
(require 'nelisp-standalone-build)

(let* ((source-path (getenv "NELISP_OBJECT_OP_SOURCE"))
       (artifact-path (getenv "NELISP_OBJECT_OP_ARTIFACT"))
       (binary-sha256 (getenv "NELISP_OBJECT_OP_BINARY_SHA"))
       (forms nil))
  (unless (and source-path artifact-path
               (string-match-p "\\`[0-9a-f]\\{64\\}\\'" binary-sha256))
    (error "object-op build requires source, artifact, and binary SHA"))
  (dolist (entry nelisp-runtime-reload-gc-contract)
    (let ((name (intern (car entry))) (arity (cdr entry)) (args nil))
      (dotimes (index arity)
        (setq args (append args (list (intern (format "arg%d" index))))))
      (push (list 'defun name args 0) forms)))
  (setq forms
        (append (nreverse forms)
                '((defun nl_native_object_probe
                    (env ticket opcode input-index output-index)
                    (if (= opcode 1)
                        (extern-call nl_native_car_v2
                                     env ticket input-index output-index 0 0)
                      (if (= opcode 2)
                          (extern-call nl_native_cdr_v2
                                       env ticket input-index output-index 0 0)
                        3))))))
  (with-temp-file source-path
    (let ((print-length nil) (print-level nil))
      (dolist (form forms)
        (prin1 form (current-buffer))
        (insert "\n"))))
  (let ((nelisp-standalone--target 'linux-x86_64))
    (nelisp-native-load-raw-v2-compile-file
     source-path artifact-path "object-op-dispatch-v1" binary-sha256))
  (let* ((manifest (nelisp-native-load-manifest artifact-path))
         (native (nelisp-native-load--raw-native manifest))
         (relocs (plist-get native :relocs)))
    (unless (null (nelisp-native-load-raw-v2-check
                   manifest "nl_native_object_probe"))
      (error "object-op ELF manifest failed preflight"))
    (unless (and (member "nl_native_car_v2"
                         (mapcar (lambda (reloc) (plist-get reloc :symbol)) relocs))
                 (member "nl_native_cdr_v2"
                         (mapcar (lambda (reloc) (plist-get reloc :symbol)) relocs)))
      (error "object-op ELF lacks one of the authenticated fixed-gateway calls"))
    (unless (and (equal (plist-get manifest :native-object-opcodes)
                        '((1 . car) (2 . cdr)))
                 (equal (plist-get manifest :native-object-op-contract-version)
                        "nelisp-native-object-op-v1"))
      (error "object-op manifest did not publish the exact versioned allowlist"))
    ;; Red mutation: adding an unreviewed opcode to the versioned manifest
    ;; must fail preflight even when the candidate otherwise looks loadable.
    (let ((mutated (copy-sequence manifest)))
      (setq mutated
            (plist-put mutated :native-object-opcodes
                       '((1 . car) (2 . cdr) (3 . setcar))))
      (setq mutated
            (plist-put mutated :artifact-sha256
                       (nelisp-native-load--sha256
                        (prin1-to-string
                         (nelisp-native-load--raw-plist-without
                          mutated :artifact-sha256)))))
      (unless (nelisp-native-load-raw-v2-check mutated "nl_native_object_probe")
        (error "object-op manifest red mutation was accepted")))))

;;; nelisp-native-raw-v2-object-op-build.el ends here
