;;; universal-argument-map-diag.el --- diagnose universal-argument keymap -*- lexical-binding: t; -*-

;;; Code:

(defun universal-argument-map-diag-run ()
  "Build the vendor `universal-argument-map' shape with per-key output."
  (let ((map (make-sparse-keymap))
        (universal-argument-minus
         (list 'menu-item "" 'negative-argument
               :filter (lambda (_cmd) nil))))
    (dolist (pair
             (list
              (list [switch-frame] (lambda (_event) nil))
              (list [?\C-u] 'universal-argument-more)
              (list [?-] universal-argument-minus)
              (list [?0] 'digit-argument)
              (list [?1] 'digit-argument)
              (list [?2] 'digit-argument)
              (list [?3] 'digit-argument)
              (list [?4] 'digit-argument)
              (list [?5] 'digit-argument)
              (list [?6] 'digit-argument)
              (list [?7] 'digit-argument)
              (list [?8] 'digit-argument)
              (list [?9] 'digit-argument)
              (list [kp-0] 'digit-argument)
              (list [kp-1] 'digit-argument)
              (list [kp-2] 'digit-argument)
              (list [kp-3] 'digit-argument)
              (list [kp-4] 'digit-argument)
              (list [kp-5] 'digit-argument)
              (list [kp-6] 'digit-argument)
              (list [kp-7] 'digit-argument)
              (list [kp-8] 'digit-argument)
              (list [kp-9] 'digit-argument)
              (list [kp-subtract] universal-argument-minus)))
      (princ (format "define-key start key=%S\n" (car pair)))
      (define-key map (car pair) (cadr pair))
      (princ (format "define-key done key=%S\n" (car pair))))
    (princ (format "map-ok=%S\n" (keymapp map)))
    t))

(provide 'universal-argument-map-diag)

;;; universal-argument-map-diag.el ends here
