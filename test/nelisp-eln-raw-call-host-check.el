;;; nelisp-eln-raw-call-host-check.el --- compile and verify raw-call fixture -*- lexical-binding: t; -*-

(require 'comp)

(defun nelisp-eln-raw-call-host-check--sha256 (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(let* ((test-dir (file-name-directory load-file-name))
       (source (expand-file-name
                "fixtures/nelisp-eln-raw-call-scalar.el" test-dir))
       (outdir (getenv "OUTDIR"))
       (output (expand-file-name "scalar-boundary.eln" outdir)))
  (unless (and outdir (file-directory-p outdir))
    (error "OUTDIR must name an existing temporary directory"))
  (unless (stringp (native-compile source output))
    (error "native-compile did not produce the raw-call fixture"))
  (let ((before (nelisp-eln-raw-call-host-check--sha256 output)))
    (load output nil nil t)
    (unless (and
             (native-comp-function-p (symbol-function 'nelisp-raw-proof-min))
             (native-comp-function-p (symbol-function 'nelisp-raw-proof-max))
             (native-comp-function-p
              (symbol-function 'nelisp-raw-proof-identity))
             (= (nelisp-raw-proof-min) most-negative-fixnum)
             (= (nelisp-raw-proof-max) most-positive-fixnum)
             (= (nelisp-raw-proof-identity most-negative-fixnum)
                most-negative-fixnum)
             (= (nelisp-raw-proof-identity most-positive-fixnum)
                most-positive-fixnum)
             (string= before
                      (nelisp-eln-raw-call-host-check--sha256 output)))
      (error "Host did not execute the exact generated native fixture"))
    (princ (format "HOST_RAW_CALL_FIXTURE_PASS emacs=%s abi=%s sha256=%s\n"
                   emacs-version comp-abi-hash before))))

;;; nelisp-eln-raw-call-host-check.el ends here
