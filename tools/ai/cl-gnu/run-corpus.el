;;; run-corpus.el --- evaluate a differential corpus form by form  -*- lexical-binding: t; -*-

;; Usage:
;;   GNU:    CLGNU_CORPUS=FILE emacs --batch -Q -l run-corpus.el
;;   NeLisp: BIN --load run-corpus.el -- FILE      (CLGNU_CORPUS also works)
;; Every top-level form of FILE is evaluated in order inside condition-case.
;; One output line per form: "NN <prin1 result>" or "NN (ERR <error-symbol>)".

(condition-case nil (require 'cl-lib) (error nil))

(let* ((file (or (let ((e (getenv "CLGNU_CORPUS")))
                   (and e (not (equal e "")) e))
                 (car (last command-line-args))))
       (text (with-temp-buffer
               (insert-file-contents file)
               (buffer-string)))
       (pos 0)
       (n 0)
       (done nil))
  (while (not done)
    (let ((read-result
           (condition-case nil
               (read-from-string text pos)
             (end-of-file nil))))
      (if (null read-result)
          (setq done t)
        (setq pos (cdr read-result))
        (setq n (1+ n))
        (let ((line
               (condition-case err
                   (prin1-to-string (eval (car read-result) t))
                 (error (format "(ERR %s)" (prin1-to-string (car err)))))))
          (princ (format "%02d %s\n" n line)))))))

;;; run-corpus.el ends here
