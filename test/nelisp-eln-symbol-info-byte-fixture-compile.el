;;; nelisp-eln-symbol-info-byte-fixture-compile.el --- build raw octet fixture -*- lexical-binding: t; -*-

(require 'comp)

(let* ((outdir (getenv "OUTDIR"))
       (source (expand-file-name "test/nelisp-eln-symbol-info-byte-fixture.el"
                                 (getenv "NELISP_SOURCE_ROOT")))
       (output (expand-file-name "nelisp-eln-symbol-info-byte-fixture.eln"
                                 outdir)))
  (unless (and (string= comp-abi-hash "ba35c031")
               (stringp (native-compile source output)))
    (error "Expected GNU Emacs 31.1 ABI ba35c031 native compilation"))
  (load output nil nil t)
  (unless (and (native-comp-function-p
                (symbol-function 'nelisp-eln-symbol-info-byte-fixture))
               (= (nelisp-eln-symbol-info-byte-fixture) 17))
    (error "Host did not execute native byte fixture"))
  (princ "HOST_BYTE_FIXTURE_NATIVE=17 ABI=ba35c031\n"))

;;; nelisp-eln-symbol-info-byte-fixture-compile.el ends here
