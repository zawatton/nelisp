;;; nelisp-fxir-corpus-driver.el --- fx corpus through the fxir evaluator  -*- lexical-binding: t; -*-
;; Loaded before test/nelisp-fx-adopt-corpus.el (with FXC_FORCE=1): every corpus
;; function the corpus registers on the VM gets a cell that runs its fxir
;; (test/nelisp-fxir-eval.el).  FXIR_REPORT=1 prints one FXIR line per function.
(load (expand-file-name "test/nelisp-fxir-eval.el") nil t)
(setq nelisp--fx-want-ir t)
(let ((orig (symbol-function 'nelisp--fx-register)))
  (fset 'nelisp--fx-register
        (lambda (name)
          (prog1 (funcall orig name)
            (when (and (boundp 'fxc-fns) (memq name fxc-fns))
              (let ((ok (nelisp-fxir-install name)))
                (when (getenv "FXIR_REPORT")
                  (princ (format "FXIR %s %s\n" name (if ok 'ir 'none))))))))))
;; `nelisp--fx-check-macro' retires the callers of a name that became a macro by
;; reading their cells as closures: show it the closures, then wrap again.
(defvar nelisp-fxir--orig nil "(NAME . CLOSURE) of each wrapped corpus function.")
(let ((install (symbol-function 'nelisp-fxir-install)))
  (fset 'nelisp-fxir-install
        (lambda (name)
          (let ((fn (symbol-function name)))
            (when (funcall install name) (push (cons name fn) nelisp-fxir--orig) t)))))
(let ((check (symbol-function 'nelisp--fx-check-macro)))
  (fset 'nelisp--fx-check-macro
        (lambda (name)
          (let ((wrapped nil))
            (dolist (o nelisp-fxir--orig)
              (push (cons (car o) (symbol-function (car o))) wrapped)
              (fset (car o) (cdr o)))
            (prog1 (funcall check name)
              (dolist (w wrapped)
                (let ((rec (gethash (nthcdr 3 (cdr (assq (car w) nelisp-fxir--orig))) nelisp--fx-table)))
                  (when (vectorp rec) (fset (car w) (cdr w))))))))))
