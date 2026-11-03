;;; extract-gnu-text-mode-syntax.el --- Exact genuine text mode initializer -*- lexical-binding: t; -*-
(let ((source (or (getenv "NELISP_GNU_TEXT_MODE_SOURCE")
                  "target/progress/production-sol-source-cache-r3/cache/text-mode.el"))
      forms)
  (with-temp-buffer
    (insert-file-contents-literally source)
    (unless (equal (secure-hash 'sha256 (current-buffer))
                   "45aa47b76bd74581133a022f1737dfff6d374d040eaf7d65f660705fb7aa3160")
      (error "GNU31 text-mode source pin differs"))
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (and (eq (car-safe form) 'defvar) (eq (cadr form) 'text-mode-syntax-table))
              (push form forms))))
      (end-of-file nil)))
  (unless (= (length forms) 1) (error "Text mode initializer identity differs"))
  (with-temp-file (or (getenv "NELISP_GNU_TEXT_MODE_OUTPUT")
                     "target/progress/production-sol-struct-gv-r1/gnu-text-mode-syntax-forms.el")
    (insert ";;; gnu-text-mode-syntax-forms.el --- Pinned initializer data -*- lexical-binding: t; -*-\n")
    (prin1 forms (current-buffer)) (insert "\n")))
