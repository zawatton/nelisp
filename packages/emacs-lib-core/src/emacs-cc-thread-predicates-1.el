;;; emacs-cc-thread-predicates-1.el --- Thread object predicates -*- lexical-binding: t; -*-

(defun emacs-cc-thread-predicates-1--test (name type tag arguments)
  "Check NAME's arity and identify TYPE or legacy vector TAG in ARGUMENTS."
  (unless (= (length arguments) 1)
    (signal 'wrong-number-of-arguments (list name (length arguments))))
  (let ((object (car arguments)))
    ;; Fallback records carry TYPE; retain vectors from an existing bundle.
    (or (eq (type-of object) type)
        (and (vectorp object) (= (length object) 3)
             (eq (aref object 0) tag)))))

(unless (fboundp 'threadp)
  (defun threadp (&rest arguments)
    "Return non-nil if OBJECT is a thread."
    (emacs-cc-thread-predicates-1--test
     'threadp 'thread 'emacs-cc-thread-1-thread arguments)))

(unless (fboundp 'mutexp)
  (defun mutexp (&rest arguments)
    "Return non-nil if OBJECT is a mutex."
    (emacs-cc-thread-predicates-1--test
     'mutexp 'mutex 'emacs-cc-thread-1-mutex arguments)))

(unless (fboundp 'condition-variable-p)
  (defun condition-variable-p (&rest arguments)
    "Return non-nil if OBJECT is a condition variable."
    (emacs-cc-thread-predicates-1--test
     'condition-variable-p 'condition-variable 'emacs-cc-thread-1-condition
     arguments)))

(provide 'emacs-cc-thread-predicates-1)
;;; emacs-cc-thread-predicates-1.el ends here
