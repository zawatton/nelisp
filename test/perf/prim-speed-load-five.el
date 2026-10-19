;;; prim-speed-load-five.el --- fixed vendored-library workload -*- lexical-binding: t -*-

(let ((root (expand-file-name "../.." (file-name-directory load-file-name))))
  (dolist (file '("vendor/emacs-lisp/emacs-lisp/cl-seq.el"
                  "vendor/emacs-lisp/emacs-lisp/easy-mmode.el"
                  "vendor/emacs-lisp/emacs-lisp/gv.el"
                  "vendor/emacs-lisp/emacs-lisp/regexp-opt.el"
                  "vendor/emacs-lisp/emacs-lisp/ring.el"))
    (load (expand-file-name file root) nil t)))
(princ "FIVE-LIBRARIES PASS: 5 loaded\n")
(kill-emacs 0)
