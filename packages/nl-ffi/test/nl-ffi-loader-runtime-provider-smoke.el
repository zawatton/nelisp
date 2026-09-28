;;; nl-ffi-loader-runtime-provider-smoke.el --- runtime symbol provider smoke -*- lexical-binding: t; -*-

(load "packages/nl-ffi/src/nl-ffi-loader.el")
(load "packages/nl-ffi/src/nl-ffi.el")

(defun nl-ffi-runtime-provider-smoke-check (condition)
  (unless condition (error "runtime provider smoke assertion failed: %S" condition)))

(defun nl-ffi-runtime-provider-smoke-error (thunk expected-reason)
  (let ((caught
         (condition-case data
             (progn (funcall thunk) nil)
           (nl-ffi-loader-unsupported data))))
    (nl-ffi-runtime-provider-smoke-check caught)
    (unless (memq expected-reason caught)
      (error "expected reason %S in %S" expected-reason caught))
    caught))

(defconst nl-ffi-runtime-provider-smoke--dir
  (or (getenv "NL_FFI_RUNTIME_PROVIDER_DIR") "target"))
(defconst nl-ffi-runtime-provider-smoke--consumer
  (concat nl-ffi-runtime-provider-smoke--dir "/nl-ffi-loader-runtime-consumer.so"))
(defconst nl-ffi-runtime-provider-smoke--consumer-notype
  (concat nl-ffi-runtime-provider-smoke--dir "/nl-ffi-loader-runtime-consumer-notype.so"))
(defconst nl-ffi-runtime-provider-smoke--consumer-noplt
  (concat nl-ffi-runtime-provider-smoke--dir "/nl-ffi-loader-runtime-consumer-noplt.so"))
(defconst nl-ffi-runtime-provider-smoke--dependency-root
  (concat nl-ffi-runtime-provider-smoke--dir "/nl-ffi-loader-runtime-root.so"))
(defconst nl-ffi-runtime-provider-smoke--provider-a
  (concat nl-ffi-runtime-provider-smoke--dir "/nl-ffi-loader-runtime-provider-a.so"))
(defconst nl-ffi-runtime-provider-smoke--provider-b
  (concat nl-ffi-runtime-provider-smoke--dir "/nl-ffi-loader-runtime-provider-b.so"))
(defconst nl-ffi-runtime-provider-smoke--local-consumer
  (concat nl-ffi-runtime-provider-smoke--dir "/nl-ffi-loader-runtime-local.so"))

(let* ((no-provider-error
        (nl-ffi-runtime-provider-smoke-error
         (lambda () (nl-ffi-loader-open nl-ffi-runtime-provider-smoke--consumer))
         :undefined-symbol))
       (handle-a (nl-ffi-loader-open nl-ffi-runtime-provider-smoke--provider-a))
       (handle-b (nl-ffi-loader-open nl-ffi-runtime-provider-smoke--provider-b))
       (addr-a (nl-ffi-loader-symbol handle-a "nl_ffi_runtime_provider"))
       (addr-b (nl-ffi-loader-symbol handle-b "nl_ffi_runtime_provider"))
       (consumer-a
        (nl-ffi-loader-open nl-ffi-runtime-provider-smoke--consumer
                            (list (cons "nl_ffi_runtime_provider" addr-a))))
       (consumer-b
        (nl-ffi-loader-open nl-ffi-runtime-provider-smoke--consumer
                            (list (cons "nl_ffi_runtime_provider" addr-b))))
       (consumer-notype-a
        (nl-ffi-loader-open nl-ffi-runtime-provider-smoke--consumer-notype
                            (list (cons "nl_ffi_runtime_provider" addr-a))))
       (call-a (nl-ffi-loader-symbol consumer-a "nl_ffi_runtime_consumer"))
       (call-b (nl-ffi-loader-symbol consumer-b "nl_ffi_runtime_consumer"))
       (call-notype-a
        (nl-ffi-loader-symbol consumer-notype-a "nl_ffi_runtime_consumer_notype"))
       (local (nl-ffi-loader-open nl-ffi-runtime-provider-smoke--local-consumer
                                  (list (cons "nl_ffi_runtime_provider" addr-a))))
       (local-call (nl-ffi-loader-symbol local "nl_ffi_runtime_local_consumer")))
  (nl-ffi-runtime-provider-smoke-check (memq :undefined-symbol no-provider-error))
  (nl-ffi-runtime-provider-smoke-check
   (= (ptr-call call-a 1 0 0 0 0 0) 102))
  (nl-ffi-runtime-provider-smoke-check
   (= (ptr-call call-b 1 0 0 0 0 0) 202))
  (nl-ffi-runtime-provider-smoke-check
   (= (ptr-call call-notype-a 1 0 0 0 0 0) 102))
  (nl-ffi-runtime-provider-smoke-check
   (= (ptr-call local-call 1 0 0 0 0 0) 301))
  (nl-ffi-runtime-provider-smoke-error
   (lambda () (nl-ffi-loader-open nl-ffi-runtime-provider-smoke--consumer
                                  (list (cons "nl_ffi_runtime_provider" 0))))
   :runtime-symbol-provider-invalid)
  (nl-ffi-runtime-provider-smoke-error
   (lambda () (nl-ffi-loader-open nl-ffi-runtime-provider-smoke--consumer
                                  (list (cons "nl_ffi_runtime_provider" addr-a)
                                        (cons "nl_ffi_runtime_provider" addr-b))))
   :runtime-symbol-provider-invalid)
  (nl-ffi-runtime-provider-smoke-error
   (lambda () (nl-ffi-loader-open nl-ffi-runtime-provider-smoke--consumer-noplt
                                  (list (cons "nl_ffi_runtime_provider" addr-a))))
   :runtime-symbol-type)
  (let ((dependency-error
         (nl-ffi-runtime-provider-smoke-error
          (lambda () (nl-ffi-loader-open nl-ffi-runtime-provider-smoke--dependency-root
                                         (list (cons "nl_ffi_runtime_provider" addr-a))))
          :undefined-symbol)))
    (nl-ffi-runtime-provider-smoke-check
     (string-match "nl-ffi-loader-runtime-dependency[.]so" (nth 2 dependency-error))))
  (princ "RUNTIME-SYMBOL-PROVIDER-SMOKE-PASS\n"))
