(fontset-font
 (fontset-font t ?A)
 (condition-case e (fontset-font nil ?A t) (error e))
 (condition-case e (fontset-font t nil) (error e)))
(fontset-info
 (condition-case e (fontset-info t) (error e))
 (condition-case e (fontset-info nil (selected-frame)) (error e)))
(new-fontset
 (condition-case e (new-fontset "fontset-test" nil) (error e))
 (condition-case e (new-fontset nil nil) (error e)))
(query-fontset
 (condition-case e (query-fontset "*") (error e))
 (condition-case e (query-fontset "fontset-default" t) (error e)))
(set-fontset-font
 (condition-case e (set-fontset-font t ?A "Example") (error e))
 (condition-case e (set-fontset-font t nil "Example") (error e))
 (let ((b (generate-new-buffer " *fontset-probe*"))) (unwind-protect (with-current-buffer b (insert "x") (list (buffer-string) (condition-case e (set-fontset-font t ?A "Example" nil 'append) (error e)))) (kill-buffer b)))
 (condition-case e (set-fontset-font t 'not-a-script "Example") (error e)))
