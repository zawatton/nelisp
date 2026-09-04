;;; init.el --- nemacs-next theme smoke init fixture -*- lexical-binding: t; -*-

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(puthash 'nemacs-theme-smoke-face '(:foreground "#ff0000" :weight normal) emacs-redisplay--face-registry)

(load-theme 'nemacs-demo t)

(setq initial-scratch-message "* theme smoke\n")

;;; init.el ends here
