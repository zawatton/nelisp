;;; gui-daily-expand-shorthands.el --- GNU reader adapter -*- lexical-binding: t; -*-

(require 'json)

(defun gui-daily-expand-shorthands (file)
  "Expand FILE's reader shorthands in its private fixture copy.
Use GNU's reader for every form, including quoted symbols and #_ escapes.
Return provenance when a file-local mapping is active; otherwise leave FILE
byte-for-byte intact.  Do not evaluate the package's forms or local eval."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (let ((enable-local-variables :all)
          (enable-local-eval nil)
          (enable-dir-local-variables nil))
      (hack-local-variables))
    (when read-symbol-shorthands
      (let ((mapping read-symbol-shorthands)
            (lexical lexical-binding)
            header forms)
        (goto-char (point-min))
        (forward-comment (buffer-size))
        ;; Retain the copyright/license header after the first line.  Emit a
        ;; fresh first line so a header shorthand declaration cannot run twice.
        (setq header (buffer-substring-no-properties
                      (save-excursion (goto-char (point-min)) (forward-line 1) (point))
                      (point)))
        (while (not (eobp))
          (push (read (current-buffer)) forms)
          (forward-comment (buffer-size)))
        (setq forms (nreverse forms))
        (let ((read-symbol-shorthands nil)
              (print-length nil) (print-level nil)
              (print-circle t) (print-gensym t))
          (with-temp-file file
            (insert (format ";;; GNU reader-expanded fixture. -*- lexical-binding: %s; -*-\n"
                            (if lexical "t" "nil")))
            (insert header)
            (dolist (form forms)
              (prin1 form (current-buffer))
              (terpri (current-buffer)))))
        `((file . ,file) (forms . ,(length forms))
          (mapping . ,mapping) (lexical_binding . ,(and lexical t)))))))

(provide 'gui-daily-expand-shorthands)
