;;; aotscale-rows.el --- compile fxir-native rows one unit each; time + digest  -*- lexical-binding: t; -*-
;; emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/aotscale-rows.el
;; Env: AOTSCALE_DATA  optional row data file used instead of the hot set
;;                     (nc2's 143-row harvest: target/aotscale/hot143-data.el);
;;      NC2_ROWS       space-separated row names (default: every row);
;;      AOTSCALE_PROF  non-empty: CPU-profile the run, print SELF/INCL tables.
;; Prints per row:  ROWTIME NAME ID SECS FXIR-CHARS UNIT-SHA1
;; UNIT-SHA1 hashes the printed unit exactly as the build's unit cache stores it,
;; so equal digests mean byte-identical object output (code, relocations,
;; symbols, stack maps).
(setq load-prefer-newer t max-lisp-eval-depth 200000)
(require 'nelisp-standalone-build)
(require 'nelisp-cc-fxir-native)
(when (getenv "AOTSCALE_DATA")
  (setq nelisp-cc-fxir-native--data-files
        (list (cons (expand-file-name (getenv "AOTSCALE_DATA")) 'nelisp-fxir-native-data))))
(load (expand-file-name "aotscale-prof" (file-name-directory load-file-name)) nil t)
(aotscale-prof-start)
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
             (t0 (float-time))
             (r (condition-case e
                    (let ((nelisp-standalone--inline-import-sources
                           (nelisp-standalone--unit-inline-imports "fxir-native.o")))
                      (nelisp-standalone--compile-to-unit unit `(seq ,form)))
                  (error (message "ERR %s %S" (car row) e) nil)))
             (secs (- (float-time) t0)))
        (message "ROWTIME %s %d %.1f %d %s" (car row) (nth 1 row) secs
                 (length (prin1-to-string (nth 4 row)))
                 (if r (let ((print-length nil) (print-level nil) (print-circle t)
                             (print-escape-nonascii t))
                         (secure-hash 'sha1 (prin1-to-string (nelisp-standalone--unit-cache-encode r))))
                   "ERR"))))))
(aotscale-prof-report)
