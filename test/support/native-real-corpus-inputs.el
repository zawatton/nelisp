;;; native-real-corpus-inputs.el --- Portable F3 inputs and observations -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(defun f3-real-corpus-observe (function arguments)
  "Evaluate FUNCTION on recorded ARGUMENTS, retaining complete error data.
The reserved (:f3-subr NAME) input denotes the runtime's actual primitive;
it lets the fixture record function objects without unreadable #<...> text."
  (condition-case condition
      (list :value
            (apply function
                   (mapcar (lambda (argument)
                             (if (and (consp argument) (eq (car argument) :f3-subr))
                                 (symbol-function (cadr argument))
                               (copy-tree argument t))) arguments)))
    (error (list :error (car condition) :data (cdr condition)))))
(provide 'native-real-corpus-inputs)
