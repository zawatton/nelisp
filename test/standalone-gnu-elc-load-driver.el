;;; standalone-gnu-elc-load-driver.el --- GNU ELC smoke driver -*- lexical-binding: t; -*-

(defun nelisp-test-gnu-elc-load-cold-process ()
  "Load source-free GNU ELC and verify top-level and byte-code behavior."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (elc (getenv "NELISP_GNU_ELC"))
         (count 'nelisp-native-package-coldload-count)
         (identity 'nelisp-native-package-identity)
         (object (cons 'identity nil))
         (refused nil))
    (load (expand-file-name "src/nelisp-load.el" root) nil nil t)
    (condition-case nil
        (nelisp-load-file elc)
      (nelisp-load-error (setq refused t)))
    (unless (and refused (not (boundp count)))
      (error "ordinary NeLisp load did not refuse GNU ELC before effects"))
    (nelisp-load-gnu-elc-host elc)
    (unless (and (= (symbol-value count) 1)
                 (byte-code-function-p (symbol-function identity))
                 (eq (funcall identity object) object)
                 (condition-case nil
                     (progn (nelisp-eval count) nil)
                   (nelisp-unbound-variable t))
                 (condition-case nil
                     (progn (nelisp-eval (list identity object)) nil)
                   (nelisp-void-function t)))
      (error "GNU ELC cold-load checks failed"))
    t))

(defun nelisp-test-gnu-elc-truncated-control ()
  "Refuse truncated list and byte-code forms without partial effects."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (files (list (getenv "NELISP_GNU_ELC_BAD_PROVIDE")
                      (getenv "NELISP_GNU_ELC_BAD_BYTECODE")))
         (counter 'nelisp-gnu-elc-truncated-counter)
         (ok t))
    (load (expand-file-name "src/nelisp-load.el" root) nil nil t)
    (while (and files ok)
      (when (boundp counter) (makunbound counter))
      (let ((failed nil))
        (condition-case nil
            (nelisp-load-gnu-elc-host (car files))
          (nelisp-load-error (setq failed t)))
        (setq ok (and failed (not (boundp counter)))))
      (setq files (cdr files)))
    ok))

(provide 'standalone-gnu-elc-load-driver)
;;; standalone-gnu-elc-load-driver.el ends here
