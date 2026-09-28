(autoload 'nelisp-autoload-nested-after-inner
          (expand-file-name "nested-inner-after.el"
                            (file-name-directory load-file-name)))
(autoload-do-load (symbol-function 'nelisp-autoload-nested-after-inner)
                  'nelisp-autoload-nested-after-inner)
(defun nelisp-autoload-nested-after-outer ()
  'changed)
(provide 'nelisp-autoload-nested-after-outer-feature)
(error "outer autoload failure")
