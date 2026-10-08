;;; gnu-31.1-closure-template.el --- Lexical closure byte-code fixture -*- lexical-binding: t; -*-

(let ((closure-capture (list 'identity-token)))
  (set 'closure-template-fixture-top-level-effect 'ran)
  (defalias 'closure-template-fixture-function
    (lambda (input) (cons input closure-capture))))

;;; gnu-31.1-closure-template.el ends here
