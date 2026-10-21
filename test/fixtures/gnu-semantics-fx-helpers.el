;;; gnu-semantics-fx-helpers.el --- Pure compiler helper cases -*- lexical-binding: t; -*-

(defconst gs-fx-helper-cases
  (let ((path (expand-file-name
               "../nelisp-prelude-bytecode-parity.el"
               (file-name-directory (or load-file-name buffer-file-name))))
        found)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (while (not found)
        (let ((form (read (current-buffer))))
          (when (and (eq (car-safe form) 'defconst)
                     (eq (cadr form) 'nelisp-prelude-bytecode-parity--fx-helper-cases))
            (unless (eq (car-safe (nth 2 form)) 'quote)
              (error "FX helper cases must remain literal forms"))
            (setq found (cadr (nth 2 form)))))))
    found))

(dolist (case gs-fx-helper-cases)
  (prin1 (list (car case) (eval (cadr case))))
  (terpri))

;;; gnu-semantics-fx-helpers.el ends here
