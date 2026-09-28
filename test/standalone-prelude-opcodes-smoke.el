;;; standalone-prelude-opcodes-smoke.el --- Prelude byte-opcode smoke -*- lexical-binding: nil; -*-

(let ((saved (symbol-function '1+)))
  (unwind-protect
      (progn
        (fset '1+ (lambda (&rest _) -99))
        (unless (equal (expand-file-name "a/b" "/x/") "/x/a/b")
          (error "compiled expand-file-name followed redefined 1+: %S"
                 (expand-file-name "a/b" "/x/")))
        (unless (= (funcall (lambda (x) (1+ x)) 1) -99)
          (error "interpreted sibling did not follow redefined 1+")))
    (fset '1+ saved)))

(provide 'standalone-prelude-opcodes-smoke)
