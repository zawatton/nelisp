;;; prim-strcmp-library-parity.el --- Check the retained library source -*- lexical-binding: t -*-

;; Run on stock GNU, retaining its primitive objects before installing only
;; the four library definitions under test. Standalone parity alone cannot
;; detect a regression in a reference definition which boot does not load.
(require 'cl-lib)
(defvar pc-library-root
  (expand-file-name "../.." (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "scripts" pc-library-root))
(require 'nelisp-prelude-bytecode)
(defvar pc-library-values
  (list "é" (unibyte-string 233) "" "ab" "abc" "ABC" "É" "あ"
        (unibyte-string 128)
        (string-as-multibyte (unibyte-string 233)) 'alpha 'beta nil t 7))
(defun pc-library-observe (fn args)
  (condition-case e (list 'value (apply fn args)) (error (list 'condition e))))
(defvar pc-library-rows nil)
(dolist (a pc-library-values)
  (dolist (b pc-library-values)
    (push (list 'string-lessp (list a b)
                (pc-library-observe (symbol-function 'string-lessp) (list a b)))
          pc-library-rows)
    (dolist (fold '(nil t))
      (let ((args (list a nil nil b nil nil fold)))
        (push (list 'compare-strings args
                    (pc-library-observe (symbol-function 'compare-strings) args))
              pc-library-rows)))))
(setq pc-library-rows (nreverse pc-library-rows))
;; Preserve the complete GNU oracle in captured stdout.
(dolist (row pc-library-rows) (princ (format "GNU-LIBRARY %S\n" row)))
(defun nelisp--check-string (s)
  (unless (stringp s) (signal 'wrong-type-argument (list 'stringp s))) s)
(let ((forms (with-temp-buffer
               (insert-file-contents
                (expand-file-name (or (getenv "PC_LIBRARY_SOURCE")
                                      "lisp/nelisp-stdlib-plist-str.el") pc-library-root))
               (nelisp-prelude-bytecode-source-defuns (buffer-string)))))
  (dolist (name '(nelisp--compare-strings-subarray compare-strings string-lessp))
    (let ((form (cl-find name forms :key #'cadr)))
      (unless form (error "Missing library definition: %s" name))
      (eval form t))))
(let ((count 0))
  (dolist (row pc-library-rows)
    (setq count (1+ count))
    (unless (equal (nth 2 row) (pc-library-observe (symbol-function (car row)) (cadr row)))
      (error "Library parity case %d: %S" count row)))
  (unless (= count 675) (error "Incomplete library corpus: %s" count))
  (princ (format "STRING-LIBRARY PASS: %d comparisons\n" count)))
(kill-emacs 0)
