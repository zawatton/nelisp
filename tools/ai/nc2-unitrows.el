;;; nc2-unitrows.el --- compile time of each fxir-native row alone  -*- lexical-binding: t; -*-
;; emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/nc2-unitrows.el
;; NC2_ROWS: space-separated row names (default: every hot-set row).  Each row's
;; nc_ID defun (lowered with the full row index, so its direct calls are the
;; build's) is compiled as its own link unit; prints one ROWTIME line per row:
;;   ROWTIME NAME ID SECS FXIR-CHARS TEXT-BYTES
(setq load-prefer-newer t max-lisp-eval-depth 200000)
(require 'nelisp-standalone-build)
(require 'nelisp-cc-fxir-native)
(defvar nc2--maps nil)
(advice-add 'nelisp-aot-compiler--stackmap-finish :before
            (lambda (records &rest _) (setq nc2--maps (append records nc2--maps))))
(let* ((rows (cl-remove-if (lambda (r) (or (nth 6 r) (>= (nth 1 r) 1000)))
                           (nelisp-cc-fxir-native-all-rows)))
       (want (and (getenv "NC2_ROWS") (mapcar #'intern (split-string (getenv "NC2_ROWS") " " t))))
       (nelisp-cc-fxir-native--row-index (make-hash-table :test 'eq))
       (nelisp-cc-fxir-native--direct-ids nil))
  (dolist (r rows) (puthash (car r) r nelisp-cc-fxir-native--row-index))
  (dolist (row rows)
    (when (or (null want) (memq (car row) want))
      (let* ((form (nelisp-cc-fxir-native--defun row))
             (unit (format "fxir-row-%d.o" (nth 1 row)))
             (t0 (float-time)) (nc2--maps nil)
             (r (condition-case e
                    (let ((nelisp-standalone--inline-import-sources
                           (nelisp-standalone--unit-inline-imports "fxir-native.o")))
                      (nelisp-standalone--compile-to-unit unit `(seq ,form)))
                  (error (message "ERR %s %S" (car row) e) nil)))
             (secs (- (float-time) t0))
             (text (apply #'+ 0 (mapcar (lambda (m) (- (plist-get m :end) (plist-get m :start))) nc2--maps))))
        (message "ROWTIME %s %d %.1f %d %d %S" (car row) (nth 1 row) secs
                 (length (prin1-to-string (nth 4 row))) text (and r t))))))
