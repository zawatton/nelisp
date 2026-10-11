;;; nelisp-consumer-fixture.el --- Pinned GNU reader fixture -*- lexical-binding: t; -*-
(defun nelisp-consumer-fixture-add (x y) (+ x y))
(defun nelisp-consumer-fixture-constant () 'consumer-marker)
(provide 'nelisp-consumer-fixture)
