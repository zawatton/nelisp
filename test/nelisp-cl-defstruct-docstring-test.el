;;; nelisp-cl-defstruct-docstring-test.el --- struct docstring metadata -*- lexical-binding: t; -*-

(require 'ert)

;; `nelisp-cl-macros.el' redefines host names such as `setf' and
;; `cl-defstruct'.  Loading it bare leaves the subset installed for every
;; file loaded after this one in the shared ERT process (measured
;; 2026-10-11: `nelisp-daemon-test.el' then failed to load with "setf:
;; unsupported place").  Only `nelisp-cl-macros--' internals are needed
;; here, so restore every other name the file defines, as
;; `nelisp-cl-generic-test.el' does.
(let* ((file (expand-file-name "../lisp/nelisp-cl-macros.el"
                               (file-name-directory load-file-name)))
       (names nil))
  (with-temp-buffer
    (insert-file-contents file)
    (condition-case nil
        (while t
          (let* ((form (read (current-buffer)))
                 (name (nth 1 form)))
            (when (eq (car-safe name) 'quote) (setq name (nth 1 name)))
            (when (and (memq (car-safe form) '(defmacro defun defalias defsubst))
                       name (symbolp name)
                       (not (string-prefix-p "nelisp-" (symbol-name name))))
              (push name names))))
      (end-of-file nil)))
  (let ((snapshot (mapcar (lambda (sym) (cons sym (and (fboundp sym) (symbol-function sym))))
                          names)))
    (load-file file)
    (dolist (pair snapshot)
      (if (cdr pair) (fset (car pair) (cdr pair)) (fmakunbound (car pair))))))

(ert-deftest nelisp-cl-defstruct-retains-leading-docstring ()
  (should (equal
           (nelisp-cl-macros--struct-slots
            '("GNU cl-defstruct leading documentation" (slot nil :type symbol)))
           '((slot nil :type symbol))))
  (should-not
   (nelisp-cl-macros--struct-slots
    '("GNU cl-defstruct documentation with no declared slots"))))

(provide 'nelisp-cl-defstruct-docstring-test)

;;; nelisp-cl-defstruct-docstring-test.el ends here
