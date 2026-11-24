;;; -*- lexical-binding: t; -*-
(defun par-preload-row (name thunk)
  (prin1 (list name (condition-case err (funcall thunk)
                      (error err))))
  (terpri))

(par-preload-row 'abbrev
 (lambda ()
   (with-temp-buffer
     (setq local-abbrev-table (make-abbrev-table))
     (define-abbrev local-abbrev-table "np" "NeLisp" nil :system t)
     (insert "np")
     (expand-abbrev)
     (list (buffer-string) (abbrev-expansion "np")))))

(par-preload-row 'tabulated-list
 (lambda ()
   (with-temp-buffer
     (tabulated-list-mode)
     (setq tabulated-list-format [("Name" 12 t)]
           tabulated-list-entries '((1 ["Alpha"]) (2 ["Beta"])))
     (tabulated-list-print)
     (goto-char (point-min))
     (list (tabulated-list-get-id) (tabulated-list-get-entry)))))

(par-preload-row 'faces
 (lambda ()
   (deftheme par-preload-theme)
   (custom-theme-set-faces
    'par-preload-theme
    '(par-preload-face ((t (:weight bold)))))
   (enable-theme 'par-preload-theme)
   (defface par-preload-face '((t (:weight normal))) "Preload probe face.")
   (face-spec-set 'par-preload-face '((t (:slant italic))))
   (list (face-attribute 'par-preload-face :weight)
         (face-attribute 'par-preload-face :slant))))
