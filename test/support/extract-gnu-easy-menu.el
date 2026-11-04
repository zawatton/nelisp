;;; extract-gnu-easy-menu.el --- Pinned complete GNU31 menu source forms -*- lexical-binding: t; -*-
(let ((source (or (getenv "NELISP_GNU_EASY_MENU_SOURCE")
                  "target/progress/production-sol-source-cache-r12/cache/easymenu.el"))
      (output (or (getenv "NELISP_GNU_EASY_MENU_OUTPUT")
                  "target/progress/production-sol-struct-gv-r1/gnu-easy-menu-forms.el"))
      forms)
  (with-temp-buffer
    (insert-file-contents-literally source)
    (unless (equal (secure-hash 'sha256 (current-buffer))
                   "d5dd7ff2930eb3b873c59481dc8bd84f5dd403e835187d40e61f60cf8945a5e5")
      (error "GNU31 easymenu source pin differs"))
    (goto-char (point-min))
    (condition-case nil (while t (push (read (current-buffer)) forms)) (end-of-file nil)))
  (setq forms (nreverse forms))
  (unless (and (= (length forms) 29) (equal (car (last forms)) '(provide 'easymenu)))
    (error "GNU31 complete easymenu form inventory differs"))
  (with-temp-file output
    (insert ";;; gnu-easy-menu-forms.el --- Complete pinned source data -*- lexical-binding: t; -*-\n")
    (prin1 forms (current-buffer)) (insert "\n")))
