;;; s8-handler-probe.el --- condition-case probe for Doc 210 S8  -*- lexical-binding: t -*-
(defun s8-handler-probe (x)
  (condition-case nil
      (progn (set x (symbol-value x)) 'ok)
    (error 'caught)))
