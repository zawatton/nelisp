(dump-emacs-portable
 (condition-case e (dump-emacs-portable nil) (error e))
 (condition-case e (dump-emacs-portable nil t) (error e))
 (condition-case e (dump-emacs-portable 1) (error e)))
(dump-emacs-portable--sort-predicate-copied
 (dump-emacs-portable--sort-predicate-copied nil nil)
 (dump-emacs-portable--sort-predicate-copied 1 2)
 (dump-emacs-portable--sort-predicate-copied 'alpha 'beta))
(pdumper-stats
 (and (pdumper-stats) t)
 (condition-case e (pdumper-stats 1) (wrong-number-of-arguments (car e))))
