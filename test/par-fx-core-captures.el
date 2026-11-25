;;; par-fx-core-captures.el --- Shared lexical cells and declaration markers -*- lexical-binding: t; -*-

(let* ((left (make-symbol "same-name"))
       (right (make-symbol "same-name"))
       (scope (list 'par-fx-core-local-special
                    (cons left 4) (cons right 19) t))
       (peek (make-interpreted-closure nil (list (list 'list left right)) scope))
       (bump (make-interpreted-closure nil (list (list 'setq left (list '1+ left))) scope))
       (dynamic (make-interpreted-closure
                 '(value)
                 '((let ((par-fx-core-local-special value))
                     (symbol-value 'par-fx-core-local-special)))
                 scope))
       (old (funcall peek)))
  (dotimes (_ 32)
    (funcall bump))
  (garbage-collect)
  (prin1 (list 'captures old (apply peek nil) (funcall dynamic 'dynamic-value)
               (boundp 'par-fx-core-local-special) (interpreted-function-p bump)
               (closurep peek)))
  (terpri))
