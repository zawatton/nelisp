;;; nemacs-process-sync-smoke.el --- standalone sync process smoke -*- lexical-binding: nil; -*-

;; Run on the NeLisp standalone reader.  It loads the library process facade
;; from src/ and proves the Emacs-shaped synchronous process gateway used by
;; Magit-style callers: `call-process' output capture, non-zero exits,
;; explicit buffer destinations, and `git --version'.

(let ((src (or (getenv "NEMACS_PROCESS_SRC") "src")))
  (setq load-path (cons src load-path))
  (load (concat src "/emacs-vars.el") nil t)
  (load (concat src "/emacs-symbol.el") nil t)
  (load (concat src "/emacs-standalone.el") nil t)
  (load (concat src "/nelisp-text-buffer.el") nil t)
  (load (concat src "/nelisp-emacs-compat.el") nil t)
  (load (concat src "/emacs-buffer-builtins.el") nil t)
  (load (concat src "/emacs-process.el") nil t)
  (load (concat src "/emacs-process-builtins.el") nil t))

(fset 'nemacs-process-sync-smoke--print
      '(lambda (line)
         (if (fboundp 'nelisp--write-stdout-bytes)
             (nelisp--write-stdout-bytes (concat line "\n"))
           (princ (concat line "\n")))))

(let ((buf (get-buffer-create "*proc-smoke*")))
  (with-current-buffer buf
    (erase-buffer)
    (let ((rc (call-process "echo" nil t nil "hello")))
      (nemacs-process-sync-smoke--print
       (concat "CALL-PROCESS-ECHO rc=" (number-to-string rc)
               " output=" (prin1-to-string (buffer-string))))))
  (with-current-buffer buf
    (erase-buffer)
    (let ((rc (call-process "false" nil t nil)))
      (nemacs-process-sync-smoke--print
       (concat "CALL-PROCESS-FALSE rc=" (number-to-string rc)
               " output=" (prin1-to-string (buffer-string))))))
  (let ((dest-name "*proc-smoke-destination*")
        (dest (get-buffer-create "*proc-smoke-destination*")))
    (with-current-buffer dest
      (erase-buffer))
    (let ((rc (call-process "echo" nil dest-name nil "buffer-destination")))
      (nemacs-process-sync-smoke--print
       (concat "CALL-PROCESS-BUFFER rc=" (number-to-string rc)
               " output="
               (prin1-to-string
                (with-current-buffer dest (buffer-string)))))))
  (with-current-buffer buf
    (erase-buffer)
    (let ((rc (call-process "git" nil t nil "--version")))
      (nemacs-process-sync-smoke--print
       (concat "CALL-PROCESS-GIT rc=" (number-to-string rc)
               " output=" (prin1-to-string (buffer-string)))))))

;;; nemacs-process-sync-smoke.el ends here
