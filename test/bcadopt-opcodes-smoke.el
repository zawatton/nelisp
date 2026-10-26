;;; bcadopt-opcodes-smoke.el --- adopted prelude byte-code ignores builtin rebinding -*- lexical-binding: nil; -*-
;; `cl--loop-build-ands' compiles to byte-code containing opcode 164 (nconc).
;; Byte-code resolves that opcode natively, so a user `fset' of `nconc' must not
;; reach it; the interpreted baseline follows the rebinding.  Sentinel: -99.
(let ((old (symbol-function 'nconc)) (result nil))
  (unwind-protect
      (progn
        (fset 'nconc (lambda (&rest _) -99))
        (setq result (condition-case err
                         (cl--loop-build-ands '((progn a t) (progn b t) c))
                       (error (list :error err)))))
    (fset 'nconc old))
  (if (and (consp result) (not (memq :error result)) (not (member -99 (flatten-tree result))))
      (princ "bcadopt-opcodes-smoke: PASS\n")
    (error "bcadopt-opcodes-smoke: followed redefined nconc: %S" result)))
