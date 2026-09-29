;;; nemacs-process-gnu-parity-smoke.el --- call-process GNU parity cases -*- lexical-binding: nil; -*-

;; Prints one line per case.  Run under host `emacs --batch' and under the
;; NeLisp standalone reader (`make proc-parity-smoke'); the two outputs must
;; be identical.  Cases avoid signals and binary coding, which the standalone
;; exit-code-only path cannot express.

(let ((src (getenv "NEMACS_PROCESS_SRC")))
  (when src
    (setq load-path (cons src load-path))
    (load (concat src "/emacs-vars.el") nil t)
    (load (concat src "/emacs-symbol.el") nil t)
    (load (concat src "/emacs-standalone.el") nil t)
    (load (concat src "/nelisp-text-buffer.el") nil t)
    (load (concat src "/nelisp-emacs-compat.el") nil t)
    (load (concat src "/emacs-stub.el") nil t)
    (load (concat src "/emacs-buffer-builtins.el") nil t)
    (load (concat src "/emacs-process.el") nil t)
    (load (concat src "/emacs-process-builtins.el") nil t)))

(defun gp--line (name form-result)
  (let ((text (prin1-to-string form-result)))
    (if (fboundp 'nelisp--write-stdout-bytes)
        (nelisp--write-stdout-bytes (concat name " " text "\n"))
      (princ (concat name " " text "\n")))))

(defmacro gp--case (name form)
  `(gp--line ,name (condition-case e ,form (error (list 'ERR (car e))))))

(gp--case "insert-at-point"
  (with-temp-buffer (insert "ab") (goto-char 2)
    (list (call-process "printf" nil t nil "XY") (buffer-string) (point))))
(gp--case "stderr-merged"
  (with-temp-buffer
    (list (call-process "sh" nil '(t t) nil "-c" "echo out; echo err >&2")
          (buffer-string))))
(gp--case "stderr-discarded"
  (with-temp-buffer
    (list (call-process "sh" nil '(t nil) nil "-c" "echo out; echo err >&2")
          (buffer-string))))
(gp--case "stderr-file"
  (let ((f (make-temp-file "gp-err")))
    (unwind-protect
        (with-temp-buffer
          (list (call-process "sh" nil (list t f) nil "-c"
                              "echo out; echo err >&2")
                (buffer-string)
                (with-temp-buffer (insert-file-contents f) (buffer-string))))
      (delete-file f))))
(gp--case "stdout-discard-list"
  (with-temp-buffer
    (list (call-process "sh" nil '(nil t) nil "-c" "echo out; echo err >&2")
          (buffer-string))))
(gp--case "dest-0"
  (with-temp-buffer (list (call-process "sh" nil 0 nil "-c" "echo out")
                          (buffer-string))))
(gp--case "dest-nil-status"
  (with-temp-buffer (list (call-process "sh" nil nil nil "-c" "echo out; exit 3")
                          (buffer-string))))
(gp--case "exit-130"
  (with-temp-buffer (list (call-process "sh" nil t nil "-c" "exit 130"))))
(gp--case "missing-program"
  (with-temp-buffer (call-process "no-such-prog-gnuparity" nil t nil)))
(gp--case "buffer-name-dest"
  (let ((b (generate-new-buffer "gp-named-buf")))
    (list (call-process "printf" nil "gp-named-buf" nil "hi")
          (with-current-buffer b (list (buffer-string) (point))))))
(gp--case "buffer-object-dest"
  (let ((b (generate-new-buffer "gp-obj")))
    (list (call-process "printf" nil b nil "obj")
          (with-current-buffer b (list (buffer-string) (point))))))
(gp--case "multibyte"
  (with-temp-buffer
    (list (call-process "printf" nil t nil "h\303\251llo")
          (buffer-string) (point))))
(gp--case "process-file"
  (let ((default-directory "/tmp/"))
    (with-temp-buffer (list (process-file "pwd" nil t nil) (buffer-string)))))
(gp--case "process-file-list-dest"
  (with-temp-buffer (list (process-file "printf" nil (list t nil) nil "pf")
                          (buffer-string))))
(gp--case "region-string-in"
  (with-temp-buffer (list (call-process-region "in" nil "cat" nil t nil)
                          (buffer-string))))
(gp--case "region-delete"
  (with-temp-buffer (insert "abc def")
    (list (call-process-region 1 4 "tr" t t nil "a-z" "A-Z")
          (buffer-string) (point))))
(gp--case "region-keep"
  (with-temp-buffer (insert "abc def")
    (list (call-process-region 1 4 "cat" nil nil nil) (buffer-string))))
(gp--case "process-lines"
  (process-lines "printf" "a\nb\n"))
(gp--case "process-lines-ignore-status"
  (process-lines-ignore-status "false"))
(gp--case "process-lines-error"
  (process-lines "false"))
(gp--case "shell-command-to-string"
  (shell-command-to-string "printf 'h\303\251llo'; echo e >&2"))

;;; nemacs-process-gnu-parity-smoke.el ends here
