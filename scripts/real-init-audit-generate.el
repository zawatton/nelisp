;;; real-init-audit-generate.el --- make one REPL wrapper per init form -*- lexical-binding: t; -*-

;;; Code:

(defun real-init-audit-generate--one-line (form)
  "Serialize FORM as one physical line for the standalone REPL."
  (let ((print-escape-newlines t)
        (print-escape-control-characters t))
    (replace-regexp-in-string
     "[\n\r]" " " (prin1-to-string form))))

(defun real-init-audit-generate--skip-space-comments ()
  "Move point over top-level whitespace and Emacs Lisp comments.
Return non-nil when point moved.  `forward-comment' does not recognize the
standalone reader's `#|...|#' block comments on every host syntax table, so
handle those explicitly (including nesting) while retaining semicolon lines."
  (let ((moved nil)
        (again t))
    (while again
      (setq again nil)
      (skip-chars-forward " \t\n\r\f")
      (cond
       ((looking-at ";")
        (forward-line 1)
        (setq moved t again t))
       ((looking-at "#|")
        (forward-char 2)
        (let ((depth 1))
          (while (> depth 0)
            (cond
             ((looking-at "#|")
              (forward-char 2)
              (setq depth (1+ depth)))
             ((looking-at "|#")
              (forward-char 2)
              (setq depth (1- depth)))
             ((eobp)
              (signal 'end-of-file '("unterminated block comment")))
             (t (forward-char 1)))))
        (setq moved t again t)))
    moved)))

(defun real-init-audit-generate--forms (path kind &optional limit)
  "Return independent wrapper forms for PATH, preserving source slices.
When LIMIT is a positive integer, emit at most that many forms.  The limit is
diagnostic-only and is useful for bounded memory measurements."
  (when (file-readable-p path)
    (with-temp-buffer
      ;; Decode the source using its normal coding cookie/default coding.  A
      ;; literal unibyte insert turns UTF-8 character literals such as ?─ into
      ;; invalid byte-oriented reader input before the host can extract them.
      (insert-file-contents path)
      (goto-char (point-min))
      (let ((index 0)
            wrappers)
        (catch 'real-init-audit-generate-done
          (while (progn
                   (real-init-audit-generate--skip-space-comments)
                   (not (eobp)))
            (let ((line (line-number-at-pos))
                  (start (point)))
              (read (current-buffer))
              (setq index (1+ index))
              (push
               (real-init-audit-generate--one-line
                `(real-init-audit--eval-one ,path ',kind ,index ,line
                                             ,(buffer-substring-no-properties
                                               start (point))))
               wrappers)
              (when (and (integerp limit) (>= index limit))
                (throw 'real-init-audit-generate-done nil)))))
        (nreverse wrappers)))))

(defun real-init-audit-generate--emit (output user-directory)
  "Write the audit setup, wrappers, and finish boundary to OUTPUT."
  (let* ((directory (file-name-as-directory
                     (file-truename user-directory)))
         (early (expand-file-name "early-init.el" directory))
         (init (expand-file-name "init.el" directory))
         (limit-text (getenv "NEMACS_REAL_INIT_STOP_AFTER"))
         (limit (and (stringp limit-text)
                     (string-match-p "^[1-9][0-9]*$" limit-text)
                     (string-to-number limit-text))))
    (with-temp-file output
      (let ((coding-system-for-write 'utf-8-unix))
        ;; The audit invokes --no-init-file.  Loadup and main are therefore
        ;; required here before the manual init sequence begins.
        (dolist (form
                 `((require 'nemacs-main)
                   (setq nemacs-user-emacs-directory ,directory
                         user-emacs-directory ,directory
                         init-file-user ""
                         package-enable-at-startup t)
                   (when (fboundp 'emacs-standalone-init)
                     (emacs-standalone-init))
                   (when (fboundp 'run-hooks)
                     (run-hooks 'before-init-hook))))
          (insert (real-init-audit-generate--one-line form) "\n"))
        (dolist (form (real-init-audit-generate--forms early 'early-init))
          (insert form "\n"))
        (dolist (form
                 '((when (fboundp 'nemacs-activate-packages-at-startup)
                     (nemacs-activate-packages-at-startup))
                   (when (fboundp 'run-hooks)
                     (run-hooks 'nemacs-package-activation-hook))))
          (insert (real-init-audit-generate--one-line form) "\n"))
        (dolist (form (real-init-audit-generate--forms init 'init limit))
          (insert form "\n"))
        (dolist (form
                 '((when (fboundp 'run-hooks)
                     (run-hooks 'after-init-hook))
                   (setq early-init-file nil
                         user-init-file nil
                         nemacs-init-file-loaded t
                         nemacs-initialized t)
                   (princ "AUDIT_DONE\n")))
          (insert
           (real-init-audit-generate--one-line
            (if (equal form
                       '(setq early-init-file nil
                              user-init-file nil
                              nemacs-init-file-loaded t
                              nemacs-initialized t))
                `(setq early-init-file ,(and (file-readable-p early) early)
                       user-init-file ,init
                       nemacs-init-file-loaded t
                       nemacs-initialized t)
              form))
           "\n"))))))

(let* ((arguments (if (equal (car command-line-args-left) "--")
                      (cdr command-line-args-left)
                    command-line-args-left))
       (user-directory (nth 0 arguments))
       (output (nth 1 arguments)))
  (unless (and user-directory output (= (length arguments) 2))
    (error "usage: emacs -Q --batch -l real-init-audit-generate.el -- USER-DIR OUTPUT"))
  (real-init-audit-generate--emit output user-directory))

;;; real-init-audit-generate.el ends here
