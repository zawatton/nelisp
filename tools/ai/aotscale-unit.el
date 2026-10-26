;;; aotscale-unit.el --- offline compile of one heavy build unit, written as the cache stores it  -*- lexical-binding: t; -*-
;; NELISP_READER_DYNAMIC=1 NELISP_STANDALONE_TARGET=linux-x86_64 AOTSCALE_UNIT=applyfn-reader|fxir-native \
;; AOTSCALE_OUT=FILE emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/aotscale-unit.el
;; Prints  UNITTIME NAME SECS  and writes FILE in the unit-cache encoding, so it
;; can be compared byte for byte with the same unit in a build's cache.
(setq load-prefer-newer t max-lisp-eval-depth 200000)
(require 'nelisp-standalone-build)
(load (expand-file-name "aotscale-prof" (file-name-directory load-file-name)) nil t)
(aotscale-prof-start)
(let* ((which (getenv "AOTSCALE_UNIT"))
       (t0 (float-time))
       (unit
        (pcase which
          ("applyfn-reader"
           (let ((nelisp-aot-compiler--x86-direct-only-defuns
                  nelisp-standalone--direct-only-reader-defuns)
                 (nelisp-standalone--inline-import-sources
                  (nelisp-standalone--unit-inline-imports "applyfn-reader.o")))
             (nelisp-standalone--compile-to-unit
              "applyfn-reader.o" (nelisp-standalone--patch-autoload-lookups
                                  (nelisp-standalone--applyfn-source)))))
          ("fxir-native"
           (let ((nelisp-standalone--inline-import-sources
                  (nelisp-standalone--unit-inline-imports "fxir-native.o")))
             (nelisp-standalone--compile-to-unit "fxir-native.o" nelisp-cc-fxir-native--source)))))
       (secs (- (float-time) t0)))
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file (getenv "AOTSCALE_OUT")
      (let ((print-escape-nonascii t) (print-length nil) (print-level nil))
        (prin1 (nelisp-standalone--unit-cache-encode unit) (current-buffer)))))
  (message "UNITTIME %s %.1f" which secs))
(aotscale-prof-report)
