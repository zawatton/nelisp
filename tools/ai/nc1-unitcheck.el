;;; nc1-unitcheck.el --- offline compile of the fxir-native unit  -*- lexical-binding: t; -*-
;; emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/nc1-unitcheck.el
;; Compiles `nelisp-cc-fxir-native-source' as its own link unit, as the build
;; does, and prints: seconds, fx words harvested, seconds per fx word, and per
;; generated function its text bytes and stack-map reason (nil = precise map).
(setq load-prefer-newer t max-lisp-eval-depth 200000)
(require 'nelisp-standalone-build)
(require 'nelisp-cc-fxir-native)
(defvar nc1--maps nil)
(advice-add 'nelisp-aot-compiler--stackmap-finish :before
            (lambda (records &rest _) (setq nc1--maps (append records nc1--maps))))
(let* ((rows (nelisp-cc-fxir-native-all-rows))
       (words (apply #'+ (mapcar (lambda (r) (length (prin1-to-string (nth 4 r)))) rows)))
       (t0 (float-time)) (src (nelisp-cc-fxir-native-source))
       (r (condition-case e (let ((nelisp-standalone--inline-import-sources (nelisp-standalone--unit-inline-imports "fxir-native.o"))) (nelisp-standalone--compile-to-unit "fxir-native.o" src))
            (error (message "ERR %S" e) nil)))
       (secs (- (float-time) t0)) (bad 0) (text 0))
  (dolist (m nc1--maps)
    (let ((nm (plist-get m :name)) (size (- (plist-get m :end) (plist-get m :start))))
      (when (string-prefix-p "nc_" nm)
        (setq text (+ text size))
        (when (plist-get m :reason) (setq bad (1+ bad)))
        (message "FN %s bytes=%d reason=%S" nm size (plist-get m :reason)))))
  (message "UNIT ok=%S secs=%.1f rows=%d fxir-chars=%d nc-text=%d unmapped=%d"
           (and r t) secs (length rows) words text bad))
