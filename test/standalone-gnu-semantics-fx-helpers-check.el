;;; standalone-gnu-semantics-fx-helpers-check.el --- Compiler helper adoption -*- lexical-binding: t; -*-

(load (expand-file-name "fixtures/gnu-semantics-fx-helpers.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(dolist (name '(nelisp--fx-env-ok-p nelisp--fx-special-p nelisp--fx-hopeless-p
                nelisp--fx-arglist-shape nelisp--fx-macros-used))
  (unless (byte-code-function-p (symbol-function name))
    (error "FX helper is not bytecode: %S" name)))
(princ "GNU-FX-HELPERS bytecode=5 PASS\n")

;;; standalone-gnu-semantics-fx-helpers-check.el ends here
