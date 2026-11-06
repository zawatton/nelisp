;;; probe-require.el --- Load each dependency of the standalone build on NeLisp  -*- lexical-binding: t; -*-

;; Usage (from the repository root):
;;   $SELFHOST_BIN --load tools/ai/selfhost-build/probe-require.el -- [FEATURE...]
;; Loads the top-level requires of scripts/nelisp-standalone-build.el one by
;; one (or only FEATURE...) and prints OK/FAIL per dependency, then
;; PROBE-REQUIRE-PASS when every one loaded.  Never starts a build.

;;; Code:

(setq load-path (append (list (expand-file-name "lisp") (expand-file-name "src")
                              (expand-file-name "scripts"))
                        load-path))
(let* ((all '(nelisp-heap-accessors nelisp-cc-string-blocks cl-lib
              "nelisp-standalone-argframe.el" nelisp-native-arithmetic-v2
              nelisp-standalone-arena-rewrite bytecomp nelisp-cc-variable-watchers json
              nelisp-cc-fx-call-primitives nelisp-cc-string-compare nelisp-cc-fxir-native
              nelisp-cc-sort nelisp-cc-number-order nelisp-cc-variable-alias-public
              nelisp-aot-compiler nelisp-static-linker nelisp-elf-write nelisp-mach-o-write
              nelisp-cc-nlboolvector-alloc nelisp-runtime-reload-abi
              nelisp-runtime-reload-telemetry nelisp-vendor-source nelisp-boot-image
              nelisp-version nelisp-prelude-bytecode nelisp-cc-eln-callback
              nelisp-cc-eln-callback7))
       ;; NeLisp leaves `command-line-args-left' nil and keeps the script
       ;; arguments after "--" in `command-line-args'.
       (wanted (or (cdr (member "--" command-line-args))
                   (delete "--" (copy-sequence command-line-args-left))))
       (features (if wanted
                     (mapcar (lambda (name) (if (string-suffix-p ".el" name) name (intern name)))
                             wanted)
                   all))
       (failures 0))
  (dolist (feature features)
    (condition-case err
        ;; Evaluate the literal form the build script uses: on NeLisp a
        ;; literal (require 'FEATURE) can take a different path than
        ;; (require VARIABLE) (2026-11-05: only the literal form failed).
        (progn (eval (if (stringp feature) (list 'load feature nil t t)
                       (list 'require (list 'quote feature)))
                     t)
               (princ (format "OK %s\n" feature)))
      (error (setq failures (1+ failures))
             (princ (format "FAIL %s: %S\n" feature err)))))
  (princ (if (= failures 0) "PROBE-REQUIRE-PASS\n"
           (format "PROBE-REQUIRE-FAIL %d\n" failures))))

;;; probe-require.el ends here
