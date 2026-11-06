;;; probe-arith.el --- Literal require of nelisp-native-arithmetic-v2 on NeLisp  -*- lexical-binding: t; -*-

;; Usage: SELFHOST_BIN=... bash tools/ai/selfhost-build/run.sh probe-arith
;; The require is written literally, as in scripts/nelisp-standalone-build.el:
;; on build115 only a literal top-level (require 'nelisp-native-arithmetic-v2)
;; in a loaded file failed ("Root evidence node/depth bound exceeded"); a
;; require built at run time loaded.

;;; Code:

(setq load-path (append (list (expand-file-name "lisp") (expand-file-name "src")
                              (expand-file-name "scripts"))
                        load-path))
(condition-case err
    (progn (require 'nelisp-native-arithmetic-v2)
           (princ "PROBE-ARITH-PASS\n"))
  (error (princ (format "PROBE-ARITH-FAIL %S\n" err))))

;;; probe-arith.el ends here
