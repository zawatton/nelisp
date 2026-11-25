;;; map.el --- Load the GNU map library  -*- lexical-binding: t; -*-

(let* ((file (expand-file-name
              "emacs-lisp/emacs-lisp/map.el"
              (or (and (boundp 'nelisp-emacs-vendor-root) nelisp-emacs-vendor-root)
                  (expand-file-name "../../../vendor"
                                    (file-name-directory load-file-name)))))
       (compiled (concat file "c")))
  (cond
   ((not (file-readable-p compiled)) (load file nil 'no-message t t))
   ((fboundp 'nelisp--load-vendor-bytecode) (nelisp--load-vendor-bytecode compiled))
   (t (load compiled nil 'no-message t t))))

;;; map.el ends here
