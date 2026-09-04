;;; nemacs-demo-theme.el --- Custom theme fixture for nemacs-next -*- lexical-binding: t; -*-

(deftheme nemacs-demo
  "Small real Custom theme fixture for nemacs-next theme smoke.")

(custom-theme-set-faces
 'nemacs-demo
 '(nemacs-theme-smoke-face ((t :foreground "#5fd7ff" :weight bold)))
 '(org-level-1 ((t :foreground "#5fd7ff" :weight bold))))

(provide-theme 'nemacs-demo)

;;; nemacs-demo-theme.el ends here
