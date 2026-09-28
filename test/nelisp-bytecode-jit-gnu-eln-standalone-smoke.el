;;; nelisp-bytecode-jit-gnu-eln-standalone-smoke.el -*- lexical-binding: t; -*-

;; Setup only.  The runner uses separate --eval forms for user calls so each
;; call reaches the standalone driver's outer evaluation boundary.
(let ((jit-source (getenv "NELISP_JIT_ADAPTER_SOURCE")))
  (unless (and jit-source (file-readable-p jit-source))
    (error "Missing JIT adapter source"))
  (load jit-source nil t t))

(setq nelisp-bytecode-jit-threshold 1
      nelisp-bytecode-jit--deferred-preparation-enabled t)

(setq nelisp-test-f
      (make-byte-code 0 (unibyte-string 192 135) [17] 1))

;;; nelisp-bytecode-jit-gnu-eln-standalone-smoke.el ends here
