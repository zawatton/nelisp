(recent-auto-save-p
 (recent-auto-save-p)
 (with-temp-buffer (set-buffer-auto-saved) (recent-auto-save-p))
 (with-temp-buffer (set-buffer-auto-saved) (insert "changed") (recent-auto-save-p)))
(set-binary-mode
 (set-binary-mode 'stdout t)
 (set-binary-mode 'stdin nil)
 (condition-case e (set-binary-mode 'bogus t) (error e)))
(set-buffer-auto-saved
 (with-temp-buffer (set-buffer-auto-saved))
 (with-temp-buffer (set-buffer-auto-saved) (insert "x") (recent-auto-save-p)))
(set-file-acl
 (set-file-acl "/tmp" "u::rwx")
 (condition-case e (set-file-acl nil "x") (error e))
 (condition-case e (set-file-acl "/tmp" nil) (error e)))
(set-file-selinux-context
 (set-file-selinux-context "/tmp" '("u" "r" "t" "s0"))
 (condition-case e (set-file-selinux-context nil nil) (error e))
 (set-file-selinux-context "/tmp" '(bad)))
(unix-sync
 (unix-sync)
 (with-temp-buffer (insert "pending") (unix-sync)))
