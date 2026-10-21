;;; gnu-semantics-load.el --- GNU eager source expansion fixture -*- lexical-binding: t; -*-
(setq gs-load-outer-dynvars macroexp--dynvars)
(condition-case e
    (load (expand-file-name "test/fixtures/gnu-semantics-load-library.el") nil t)
  (error (prin1 (list 'load-error e)) (terpri)))
(dolist (entry '((gs-load-before 5) (gs-load-after 6) (gs-load-progn-before 7)
                 (gs-load-deferred)))
  (prin1 (list (car entry) (condition-case e (eval entry) (error e)))) (terpri))
(prin1 (list 'fallback-count gs-load-fallback-count)) (terpri)
(prin1 (list 'abort (boundp 'gs-load-after-failure-reached)
             'expansion-environment-restored (eq gs-load-outer-dynvars macroexp--dynvars)))
(terpri)
(defvar gs-load-hook-replacement nil)
(defvar gs-load-hook-after nil)
(defvar gs-load-hook-calls 0)
(let ((saved (symbol-function 'internal-macroexpand-for-load)))
  (dolist (mode '(remove define))
    (let ((gs-load-hook-replacement
           (and (eq mode 'define)
                (lambda (form _full-p)
                  (setq gs-load-hook-calls (1+ gs-load-hook-calls))
                  form)))
          (gs-load-hook-after nil)
          (gs-load-hook-calls 0))
      (unwind-protect
          (progn
            (when (eq mode 'define) (fmakunbound 'internal-macroexpand-for-load))
            (let ((result
                   (condition-case err
                       (load (expand-file-name "test/fixtures/gnu-semantics-load-hook-library.el") nil t)
                     (error err))))
              (prin1 (list 'expander-selected-at-file-start mode result
                           gs-load-hook-after gs-load-hook-calls))
              (terpri)))
        (fset 'internal-macroexpand-for-load saved)))))
(kill-emacs 0)
