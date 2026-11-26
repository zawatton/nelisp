;;; par-user-snapshot-load-state.el --- ordinary loads after a user heap -*- lexical-binding: t; -*-

(defvar par-user-snapshot-events nil)
(defvar par-user-snapshot-test-hook nil)
(defvar par-user-snapshot-test-map nil)
(defvar par-user-snapshot-test-custom nil)

(let* ((directory (make-temp-file "par-user-snapshot-" t))
       (file (expand-file-name "par-user-snapshot-fixture.el" directory))
       (load-path (cons directory load-path)))
  (unwind-protect
      (progn
        (with-temp-file file
          (insert ";;; fixture -*- lexical-binding: t; -*-\n"
                  "(push 'load par-user-snapshot-events)\n"
                  "(defcustom par-user-snapshot-test-custom nil \"Fixture.\" :type 'boolean)\n"
                  "(setq par-user-snapshot-test-map (make-sparse-keymap))\n"
                  "(define-key par-user-snapshot-test-map (kbd \"C-c s\") 'par-user-snapshot-command)\n"
                  "(defun par-user-snapshot-command () (push 'hook par-user-snapshot-events))\n"
                  "(add-hook 'par-user-snapshot-test-hook #'par-user-snapshot-command)\n"
                  "(provide 'par-user-snapshot-fixture)\n"))
        (with-eval-after-load 'par-user-snapshot-fixture
          (push 'after-load par-user-snapshot-events))
        (require 'par-user-snapshot-fixture)
        (require 'par-user-snapshot-fixture)
        (run-hooks 'par-user-snapshot-test-hook)
        (prin1 (list 'user-snapshot-load-state
                     (reverse par-user-snapshot-events)
                     (featurep 'par-user-snapshot-fixture)
                     (and (member '(provide . par-user-snapshot-fixture)
                                  (assoc file load-history)) t)
                     (eq (lookup-key par-user-snapshot-test-map (kbd "C-c s"))
                         'par-user-snapshot-command)
                     (and (get 'par-user-snapshot-test-custom 'standard-value) t)))
        (terpri))
    (delete-directory directory t)))
