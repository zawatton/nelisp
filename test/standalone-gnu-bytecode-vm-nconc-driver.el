;;; standalone-gnu-bytecode-vm-nconc-driver.el --- NCONC source-VM check -*- lexical-binding: t; -*-

(load (expand-file-name "src/nelisp-gnu-bytecode-vm.el"
                        (getenv "NELISP_REPO_ROOT")) nil nil t)

(let ((elc (getenv "NELISP_GNU_ELC")))
  (nelisp-gnu-bytecode-vm-load-file elc)
  ;; These public logical function cells must not affect GNU intrinsic op 164.
  (let ((poison (nelisp-bc-make nil '(ignored) [] [255] 1 0)))
    (nelisp-eval-function-cell-put 'nconc poison)
    (nelisp-eval-function-cell-put 'cdr poison)
    (nelisp-eval-function-cell-put 'setcdr poison))
  (let* ((left (list 'left))
         (right (list 'right))
         (two (nelisp-eval (list 'nelisp-gnu-bytecode-vm-nconc-two
                                 (list 'quote left) (list 'quote right))))
         (left3 (list 'first))
         (middle3 (list 'middle))
         (last3 (list 'last))
         (three (nelisp-eval (list 'nelisp-gnu-bytecode-vm-nconc-three
                                   (list 'quote left3)
                                   (list 'quote middle3)
                                   (list 'quote last3)))))
    (unless (and (eq two left) (eq (cdr left) right)
                 (eq three left3) (eq (cdr left3) middle3)
                 (eq (cdr middle3) last3))
      (error "Source-VM NCONC identity/mutation mismatch"))
    (prin1 (list 'nconc-source-vm-pass t))))

(provide 'standalone-gnu-bytecode-vm-nconc-driver)
;;; standalone-gnu-bytecode-vm-nconc-driver.el ends here
