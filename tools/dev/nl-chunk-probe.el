;;; nl-chunk-probe.el --- compile ONE applyfn chunk alone (no full build)  -*- lexical-binding: t; -*-
;; env: CHUNK=i (0..K-1), NELISP_STANDALONE_APPLYFN_CHUNKS=K, CHUNK_OUT=file for the result line.
;; Inline imports are resolved by cached-unit as in the build; the cumulative xinline
;; table of earlier units is absent, so the object differs from the build's (timing probe).
(require 'nelisp-standalone-build)
(let* ((k (nelisp-standalone-chunk-count))
       (i (string-to-number (or (getenv "CHUNK") "0")))
       (split (nelisp-standalone--applyfn-split-units
               (nelisp-standalone--patch-autoload-lookups (nelisp-standalone--applyfn-source))))
       (chunks (nelisp-standalone-chunk-split (car split) k (nelisp-standalone-chunk-load-table k)))
       (src (nth i chunks)) (t0 (float-time)) unit)
  (message "chunk %d/%d: %d forms" i k (length (cdr src)))
  (setq unit (let ((nelisp-aot-compiler--x86-direct-only-defuns nelisp-standalone--direct-only-reader-defuns))
               (nelisp-standalone--cached-unit (nelisp-standalone-chunk-unit-name i) src)))
  (let ((line (format "chunk=%d K=%d forms=%d compile_s=%.1f sha=%s\n" i k (length (cdr src)) (- (float-time) t0)
                      (secure-hash 'sha1 (let ((print-length nil) (print-level nil) (print-circle t))
                                           (prin1-to-string (nelisp-standalone--unit-cache-encode unit)))))))
    (message "%s" line)
    (when (getenv "CHUNK_OUT") (write-region line nil (getenv "CHUNK_OUT") t 'silent))))
