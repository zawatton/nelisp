;;; probe-load.el --- Load scripts/nelisp-standalone-build.el on NeLisp  -*- lexical-binding: t; -*-

;; Usage (from the repository root):
;;   $SELFHOST_BIN --load tools/ai/selfhost-build/probe-load.el --
;; Prints PROBE-LOAD-PASS when the build script loads and defines its entry
;; point; does not call it.

;;; Code:

(setq load-path (append (list (expand-file-name "lisp") (expand-file-name "src")
                              (expand-file-name "scripts"))
                        load-path))
(condition-case err
    (progn (require 'nelisp-standalone-build)
           (princ (if (fboundp 'nelisp-standalone-build-reader)
                      "PROBE-LOAD-PASS\n"
                    "PROBE-LOAD-FAIL entry point undefined\n")))
  (error (princ (format "PROBE-LOAD-FAIL %S\n" err))))

;;; probe-load.el ends here
