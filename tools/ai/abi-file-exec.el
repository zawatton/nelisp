;;; abi-file-exec.el --- Doc 215 source execution workload -*- lexical-binding: t; -*-

;; Load source explicitly, even when the feature is already present at boot.
(let ((root (getenv "ABI_SOURCE_ROOT")))
  (dolist (name '("emacs-lisp/subr-x.el" "emacs-lisp/ring.el"
                  "emacs-lisp/easy-mmode.el" "emacs-lisp/derived.el"
                  "emacs-lisp/benchmark.el"))
    (load (concat root "/vendor/emacs-lisp/" name) nil t t)))

(defvar abi-file-exec-value 17)
(let ((i 0) (sum 0) (table (make-hash-table :test 'equal))
      (vec (make-vector 3 nil)) (last nil))
  (while (< i 1000)
    (let* ((pair (cons i (cons 3 nil)))
           (name (concat "abi-file-" (substring "execution" 0 4)))
           (sym (intern "abi-file-exec-value")))
      (aset vec 0 (car pair))
      (aset vec 1 (car (cdr pair)))
      (puthash name (aref vec 0) table)
      (setq sum (+ sum (gethash name table) (symbol-value sym)
                   (funcall '+ (aref vec 1) 2) (apply '+ '(1 2 3))))
      (setq last (list (string-match "exec" name) (type-of pair)
                       (format "%s:%d" name (aref vec 1)))))
    (setq i (1+ i)))
  (princ (format "ABI-FILES %S\n" (list i sum last))))

;;; abi-file-exec.el ends here
