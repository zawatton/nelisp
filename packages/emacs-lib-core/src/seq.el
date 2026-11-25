;;; seq.el --- Load the GNU sequence library  -*- lexical-binding: t; -*-

(let* ((source (expand-file-name
                "emacs-lisp/emacs-lisp/seq.el"
                (or (and (boundp 'nelisp-emacs-vendor-root) nelisp-emacs-vendor-root)
                    (expand-file-name "../../../vendor"
                                      (file-name-directory load-file-name)))))
       (compiled (concat source "c"))
       saved)
  ;; These entry points have rows in nelisp-fxir-native-data.el. Keep the
  ;; registered functions when the GNU file installs its generic dispatchers.
  (when (fboundp 'nelisp--write-stdout-bytes)
    (dolist (name '(seq-elt seq-group-by seq-length seq-map seq-reduce seq-uniq))
      (when (fboundp name)
        (push (cons name (symbol-function name)) saved))))
  (unwind-protect
      (cond
       ((not (file-readable-p compiled)) (load source nil 'no-message t t))
       ((fboundp 'nelisp--load-vendor-bytecode) (nelisp--load-vendor-bytecode compiled))
       (t (load compiled nil 'no-message t t)))
    (dolist (entry saved)
      (fset (car entry) (cdr entry)))))

;;; seq.el ends here
