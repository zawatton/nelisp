;;; par-fx-core-opcodes.el --- FX operands, branch targets and leaf bodies -*- lexical-binding: t; -*-

(defun par-fx-core-opcodes (a b flag)
  (list a b 'left a b 'right 'constant 'pair
        (if flag a b) a
        (if flag 'taken 'skipped) b))

(defun par-fx-core-opcodes-handler (a b)
  (catch 'par-fx-core-tag
    (throw 'par-fx-core-tag (list a b))))

(defun par-fx-core-project (_first second)
  second)

(defun par-fx-core-constant (_value)
  'constant)

(let (taken skipped caught projected constant (calls 0))
  (dotimes (_ 48)
    (setq taken (par-fx-core-opcodes 'a 'b t)
          skipped (apply #'par-fx-core-opcodes '(left right nil))
          caught (par-fx-core-opcodes-handler 'caught 'value)
          projected
          (list (par-fx-core-project (setq calls (1+ calls)) 'direct)
                (apply #'par-fx-core-project '(ignored applied)))
          constant (par-fx-core-constant (setq calls (1+ calls)))))
  (prin1 (list 'operands taken skipped caught projected constant calls))
  (terpri))
