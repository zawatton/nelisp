;;; emacs-cc-command-modes-1.el --- Command mode metadata -*- lexical-binding: t; -*-

(defun emacs-cc-command-modes-1--literal (function)
  "Return mode names from FUNCTION's literal interactive declaration."
  (let ((body (cond
               ((eq (car-safe function) 'lambda) (cddr function))
               ((eq (car-safe function) 'closure) (cdddr function))
               ((eq (type-of function) 'interpreted-function)
                (condition-case nil
                    (let ((entry (aref function 0)))
                      (if (and (consp entry) (eq (car entry) 'interactive))
                          (list entry)
                        (let ((metadata (aref function 5)))
                          (and (vectorp metadata)
                               (listp (aref metadata 1))
                               (list (append (list 'interactive nil)
                                             (aref metadata 1)))))))
                  (error nil))))))
    (when (stringp (car-safe body))
      (setq body (cdr body)))
    (let* ((entry (car-safe body))
           (spec (if (and (consp entry) (eq (car entry) 'interactive))
                     entry
                   (car-safe (cdr-safe entry)))))
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
      (while (and (symbolp command) (not (memq command seen)))
        (push command seen)
        (setq modes (or modes (get command 'command-modes)))
        (setq command (if (fboundp command)
                          (symbol-function command)
                        nil)))
      (or modes (emacs-cc-command-modes-1--literal command)))))

(provide 'emacs-cc-command-modes-1)
;;; emacs-cc-command-modes-1.el ends here
