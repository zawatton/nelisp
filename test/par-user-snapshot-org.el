;;; par-user-snapshot-org.el --- Org state retained by a user heap -*- lexical-binding: t; -*-

(when (getenv "NELISP_TEST_EXPECT_SNAPSHOT")
  (unless (and (featurep 'org) (symbol-file 'org-mode 'defun))
    (error "Expected Org to be present before require")))
(require 'org)
(let* ((origin (symbol-file 'org-mode 'defun))
       (history (assoc origin load-history)))
  (prin1
   (list 'user-snapshot-org
         (featurep 'org)
         (and (stringp origin) (string-match-p "/org\\.el" origin) t)
         (and (member '(provide . org) history) t)
         (with-temp-buffer
           (insert "* Heading")
           (org-mode)
           (goto-char (point-min))
           (and (org-at-heading-p) (equal (org-get-heading t t t t) "Heading")))))
  (terpri))
