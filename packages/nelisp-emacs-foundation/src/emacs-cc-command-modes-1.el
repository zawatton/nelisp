;;; emacs-cc-command-modes-1.el --- Command mode metadata -*- lexical-binding: t; -*-

(defun emacs-cc-command-modes-1--literal (function)
  "Return mode names from FUNCTION's literal interactive declaration."
  (let ((body (cond
               ((eq (car-safe function) 'lambda) (cddr function))
               ((eq (car-safe function) 'closure) (cdddr function)))))
    (when (stringp (car-safe body))
      (setq body (cdr body)))
    (let ((spec (car-safe body)))
      (and (consp spec) (eq (car spec) 'interactive)
           (cddr spec)))))

(unless (fboundp 'command-modes)
  (defun command-modes (&rest arguments)
    "Return the modes for which COMMAND is interactively defined."
    (unless (= (length arguments) 1)
      (signal 'wrong-number-of-arguments
              (list 'command-modes (length arguments))))
    (let ((command (car arguments))
          (seen nil)
          (modes nil))
      (when (commandp command)
        (while (and (symbolp command) (not (memq command seen)))
          (push command seen)
          (setq modes (or modes (get command 'command-modes)))
          (setq command (if (fboundp command)
                            (symbol-function command)
                          nil)))
        (or modes (emacs-cc-command-modes-1--literal command))))))

(provide 'emacs-cc-command-modes-1)
;;; emacs-cc-command-modes-1.el ends here
