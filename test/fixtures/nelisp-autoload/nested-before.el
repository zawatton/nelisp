(provide 'nelisp-autoload-nested-before-outer-feature)
(autoload 'nelisp-autoload-nested-before-inner
          (expand-file-name "nested-inner-before.el"
                            (file-name-directory load-file-name)))
(autoload-do-load (symbol-function 'nelisp-autoload-nested-before-inner)
                  'nelisp-autoload-nested-before-inner)
(error "outer autoload failure")
