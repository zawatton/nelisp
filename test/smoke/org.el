;;; org.el --- Org workflow -*- lexical-binding: t; -*-

(defun smoke-org ()
  (when (smoke-step "org-load"
          (require 'org)
          (list (org-version) (and (symbol-file 'org-mode) t)))
    (let ((file (expand-file-name "tasks.org"))
          (capture (expand-file-name "capture.org")))
      (smoke-step "org-cycle"
        (find-file file)
        (goto-char (point-min))
        (let ((key (lookup-key (current-local-map) (kbd "TAB"))))
          (org-cycle)
          (forward-line 1)
          (list key (invisible-p (point)) (smoke-state))))
      (smoke-step "org-todo"
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (org-todo "DONE")
          (list (substring-no-properties (org-get-todo-state)) (smoke-state))))
      (smoke-step "org-agenda"
        (require 'org-agenda)
        (let ((org-agenda-files (list file))
              (org-agenda-show-all-dates nil)
              (org-agenda-window-setup 'current-window))
          (org-agenda-list nil "2026-01-05" 1)
          (list major-mode (smoke-matching-lines "Calendar task"))))
      (smoke-step "org-capture"
        (require 'org-capture)
        (let ((org-capture-templates
               `(("s" "Smoke" entry (file ,capture) "* TODO Captured %i"))))
          (org-capture-string "fixture" "s")
          (org-capture-finalize)
          (smoke-read capture)))
      (smoke-step "org-export-text"
        (require 'ox-ascii)
        (org-export-string-as "* Heading\n:PROPERTIES:\n:CUSTOM_ID: smoke-heading\n:END:\n\nHello *world*.\n" 'ascii t))
      (smoke-step "org-export-html"
        (require 'ox-html)
        (org-export-string-as "* Heading\n:PROPERTIES:\n:CUSTOM_ID: smoke-heading\n:END:\n\nHello *world*.\n" 'html t
                              '(:with-toc nil :section-numbers nil :with-date nil
                                :with-author nil :html-self-link-headlines nil))))))
