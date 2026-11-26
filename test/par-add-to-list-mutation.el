;;; par-add-to-list-mutation.el --- membership after in-place edits -*- lexical-binding: t; -*-

(defvar par-add-to-list-items nil)

(let ((par-add-to-list-items (list "packages/first" "packages/second"))
      (add (symbol-function 'add-to-list)))
  (funcall add 'par-add-to-list-items "packages/third")
  ;; The head stays identical while the list loses an existing member.
  (setcdr par-add-to-list-items (list "packages/second"))
  (prin1 (funcall add 'par-add-to-list-items "packages/first" t))
  (terpri)
  (prin1 (condition-case err
              (funcall add (make-symbol "unbound-list") 'value)
            (void-variable (car err))))
  (terpri))

(let* ((entry (copy-sequence "packages/a"))
       (par-add-to-list-items (list entry))
       (add (symbol-function 'add-to-list)))
  (funcall add 'par-add-to-list-items "packages/a")
  (aset entry (1- (length entry)) ?b)
  (prin1 (funcall add 'par-add-to-list-items "packages/a"))
  (terpri))
