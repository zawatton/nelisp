;;; probe-bytecomp.el --- GNU bytecomp on NeLisp compiles one lambda as GNU does  -*- lexical-binding: t; -*-

;; Expected parts measured on GNU Emacs 31.1 (`emacs --batch -Q', lexical).
;; Prints PROBE-BYTECOMP-PASS when NeLisp's compiled function has the same
;; arglist descriptor, byte string, constants and max depth.  Default
;; warnings stay on: their checks use `rx', which GNU autoloads.

;;; Code:

(setq load-path (append (list (expand-file-name "lisp") (expand-file-name "src")
                              (expand-file-name "scripts"))
                        load-path))
(defvar probe-bytecomp--loaded (condition-case e (progn (require (quote bytecomp)) t) (error e)))
(let* ((lexical-binding t)
       (f (condition-case e
              (progn
                     (byte-compile '(lambda (x l) (let ((s 0)) (dolist (e l s) (setq s (+ s (* x e))))))))
            (error e)))
       (got (if (byte-code-function-p f)
                (list (aref f 0) (append (aref f 1) nil) (aref f 2) (aref f 3))
              f))
       (want '(514 (192 1 137 131 20 0 137 64 2 5 2 95 92 178 3 136 65 130 2 0 136 135) [0] 8)))
  (princ (if (equal got want) "PROBE-BYTECOMP-PASS\n"
           (format "PROBE-BYTECOMP-FAIL %.200S\n" got))))

;;; probe-bytecomp.el ends here
