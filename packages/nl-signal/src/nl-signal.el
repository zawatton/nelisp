;;; nl-signal.el --- Lisp frontend for explicit signal calls -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Require this feature to enable GNU's signal hook and the existing .eln
;; debugger policy for explicit Lisp signal calls in the standalone reader.
;; Primitive errors raised directly by the runtime retain their own behavior.
(defvar signal-hook-function nil)
(declare-function nelisp-eln-handler-port--debugger-decision "nelisp-eln-handler-port"
                  (clause conditions failure))
(when (and (fboundp 'nelisp--eval-source-string) (not (featurep 'nl-signal)))
  (require 'nelisp-eln-handler-port)
  (let ((raise (symbol-function 'signal)))
    (fset 'signal
          (lambda (name data)
            (let ((hook signal-hook-function))
              (when hook
                (funcall hook name data)))
            (when debug-on-signal
              (nelisp-eln-handler-port--debugger-decision
               'error (get name 'error-conditions) (cons name data)))
            (funcall raise name data)))))

(provide 'nl-signal)
;;; nl-signal.el ends here
