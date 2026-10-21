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
(kill-emacs 0)
