;;; nelisp-eln-raw-call-standalone-smoke.el --- full-width call probe -*- lexical-binding: t; -*-

(let* ((root (getenv "NELISP_ROOT"))
       (module-root (getenv "NELISP_ELN_MODULE_ROOT"))
       (artifact (expand-file-name "scalar-boundary.eln" (getenv "OUTDIR"))))
  (add-to-list 'load-path (expand-file-name "lisp" module-root))
  (add-to-list 'load-path (expand-file-name "packages/nl-ffi/src" root))
  (require 'nl-ffi)
  (require 'nelisp-eln-raw-call)
  (ffi:library artifact)
  (let* ((handle (nl-ffi-library-handle artifact))
         (min-address
          (nl-ffi-loader-symbol
           handle "F6e656c6973702d7261772d70726f6f662d6d696e_nelisp_raw_proof_min_0"))
         (max-address
          (nl-ffi-loader-symbol
           handle "F6e656c6973702d7261772d70726f6f662d6d6178_nelisp_raw_proof_max_0"))
         (identity-address
          (nl-ffi-loader-symbol
           handle "F6e656c6973702d7261772d70726f6f662d6964656e74697479_nelisp_raw_proof_identity_0"))
         (context (nelisp-eln-raw-call-context-create)))
    (unwind-protect
        (progn
          (unless (and (= (nelisp-eln-raw-call-word context min-address nil)
                          9223372036854775810)
                       (= (nelisp-eln-raw-call-word context max-address nil)
                          9223372036854775806)
                       (= (nelisp-eln-raw-call-word
                           context identity-address '(9223372036854775810))
                          9223372036854775810)
                       (= (nelisp-eln-raw-call-word context identity-address '(-1))
                          18446744073709551615))
            (error "GNU .eln full-width call/argument capture mismatch"))
          ;; This native x86_64 fixture stores all six incoming GP words to
          ;; the output buffer supplied as RDI, then returns small status 0.
          (let* ((capture (nl-ffi-memory-allocate 48))
                 (code (nl-ffi-memory-allocate 64))
                 (capture-address (nl-ffi-memory-address capture))
                 (code-address (nl-ffi-memory-address code))
                 (fixture [#x48 #x89 #x3f #x48 #x89 #x77 #x08
                           #x48 #x89 #x57 #x10 #x48 #x89 #x4f #x18
                           #x4c #x89 #x47 #x20 #x4c #x89 #x4f #x28
                           #x31 #xc0 #xc3])
                 (inputs (list capture-address 1311768467463790320
                               9223372036854775810 9223372036854775806
                               -1 8195))
                 (i 0))
            (unwind-protect
                (progn
                  (while (< i (length fixture))
                    (ptr-write-u8 code-address i (aref fixture i))
                    (setq i (1+ i)))
                  (unless (= (syscall-direct 10 code-address (aref code 2)
                                             5 0 0 0) 0)
                    (error "Could not mark argument-capture fixture RX"))
                  (unless (= (nelisp-eln-raw-call-word
                              context code-address inputs) 0)
                    (error "Argument-capture fixture status was not zero"))
                  (let ((offset 0) (rest inputs))
                    (while rest
                      (let* ((word (nelisp-eln-abi-normalize-word (car rest)))
                             (low (ptr-read-u32 capture-address offset))
                             (high (ptr-read-u32 capture-address (+ offset 4)))
                             (actual (+ low (ash high 32))))
                        (unless (= actual word)
                          (error "GP argument %d mismatch expected %S got %S"
                                 (/ offset 8) word actual)))
                      (setq offset (+ offset 8) rest (cdr rest)))))
              (nl-ffi-memory-release code)
              (nl-ffi-memory-release capture)))
          (aset context 3 t)
          (unless (condition-case nil
                      (progn
                        (nelisp-eln-raw-call-word context min-address nil)
                        nil)
                    (nelisp-eln-raw-call-error t))
            (error "Busy context was accepted"))
          (aset context 3 nil)
          (nelisp-eln-raw-call-context-release context)
          (unless (condition-case nil
                      (progn
                        (nelisp-eln-raw-call-word context min-address nil)
                        nil)
                    (nelisp-eln-raw-call-error t))
            (error "Released context was accepted")))
      (unless (aref context 4)
        (nelisp-eln-raw-call-context-release context)))))

(princ "NELISP-ELN-RAW-CALL-SMOKE-PASS\n")
