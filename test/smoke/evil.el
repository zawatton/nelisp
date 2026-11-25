;;; evil.el --- Modal edit commands, repeat and Ex -*- lexical-binding: t; -*-

(defun smoke-evil-key (label keys)
  (smoke-step label
    (execute-kbd-macro (kbd keys))
    (list evil-state (smoke-state))))

(defun smoke-evil ()
  (when (smoke-step "evil-mode"
          (setq evil-undo-system 'undo-redo)
          (require 'evil)
          (switch-to-buffer (generate-new-buffer " *smoke-evil*"))
          (fundamental-mode)
          (insert "one two three four\nred blue green\n")
          (goto-char (point-min))
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (evil-local-mode 1)
          (evil-normal-state)
          (list evil-state (key-binding (kbd "d")) (key-binding (kbd "p"))))
    (let ((kill-ring nil)
          (interprogram-cut-function nil)
          (interprogram-paste-function nil))
      (smoke-evil-key "evil-dw" "d w")
      (smoke-evil-key "evil-cw" "c w X <escape>")
      (smoke-evil-key "evil-p" "p")
      (smoke-evil-key "evil-u" "u")
      (smoke-evil-key "evil-repeat" "w .")
      (smoke-evil-key "evil-visual-line" "V y")
      (smoke-step "evil-yank" (substring-no-properties (current-kill 0)))
      ;; The Ex entry point takes the command directly, avoiding batch stdin.
      (smoke-step "evil-ex"
        (evil-ex-execute "%s/red/rouge/g")
        (smoke-state)))))
