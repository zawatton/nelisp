;;; -*- lexical-binding: t; -*-
(dolist (accessor '(file-attribute-type file-attribute-link-number
                    file-attribute-user-id file-attribute-group-id
                    file-attribute-access-time file-attribute-modification-time
                    file-attribute-status-change-time file-attribute-size
                    file-attribute-modes file-attribute-inode-number
                    file-attribute-device-number file-attribute-file-identifier))
  (prin1 (list accessor
               (mapcar (lambda (attrs)
                         (condition-case err (funcall accessor attrs)
                           (error err)))
                       '(("target" 1 1000 "staff" (0 1) (0 2) (0 3) 9
                          "lrwxrwxrwx" t 234 (8 . 1))
                         nil (t . broken)))))
  (terpri))
