;;; driver.el --- Shared driver for daily-driver smoke probes -*- lexical-binding: t; -*-

(defvar smoke-case-directory (getenv "NELISP_SMOKE_SUITE"))
(defvar smoke-fixtures (file-name-as-directory (getenv "NELISP_SMOKE_FIXTURES")))
(defvar smoke-errors 0)
(defvar smoke-before-step-hook nil)

(setq user-emacs-directory (file-name-as-directory
                            (expand-file-name ".emacs.d" (getenv "HOME")))
      enable-local-variables nil
      enable-local-eval nil
      create-lockfiles nil
      inhibit-message t)
(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))
(dolist (directory (reverse (split-string (getenv "NELISP_SMOKE_PATH") ":" t)))
  (setq load-path (cons directory load-path)))

(defun smoke-emit (label value)
  (let ((print-length nil)
        (print-level nil)
        (print-escape-newlines t))
    (princ (format "\nSMOKE %s %S\n" label value)))
  value)

(defmacro smoke-step (label &rest body)
  "Record the outcome of BODY and continue independently after a Lisp error."
  (declare (indent 1))
  `(let ((step ,label))
     (princ (concat "STEP " step "\n"))
     (run-hooks 'smoke-before-step-hook)
     (condition-case fault
         (smoke-emit step (progn ,@body))
       ((error quit)
        (setq smoke-errors (1+ smoke-errors))
        (smoke-emit (concat "ERROR " step) fault)
        nil))))

(defun smoke-state ()
  (list major-mode (point)
        (buffer-substring-no-properties (point-min) (point-max))))

(defun smoke-read (file &optional coding)
  (with-temp-buffer
    (let ((coding-system-for-read coding))
      (insert-file-contents file))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun smoke-matching-lines (regexp)
  (save-excursion
    (goto-char (point-min))
    (let (lines)
      (while (re-search-forward regexp nil t)
        (push (buffer-substring-no-properties
               (line-beginning-position) (line-end-position)) lines)
        (forward-line 1))
      (nreverse lines))))

(defun smoke-await-process (process)
  (unless (processp process)
    (error "Expected a process, got %S" process))
  (let ((deadline (+ (float-time) 5)))
    (while (and (process-live-p process) (< (float-time) deadline))
      (accept-process-output process 0.05))
    (when (process-live-p process)
      (delete-process process)
      (error "Process exceeded five seconds"))
    (accept-process-output process 0.05)
    (process-exit-status process)))

(defun smoke-run-case (name)
  (let ((smoke-errors 0)
        (before (buffer-list))
        (original (current-buffer))
        (default-directory (file-name-as-directory
                            (expand-file-name name smoke-fixtures))))
    (smoke-emit "BEGIN" name)
    (unwind-protect
        (smoke-step "scenario"
          (let ((function (intern (concat "smoke-" name))))
            (unless (fboundp function)
              (load (expand-file-name (concat name ".el") smoke-case-directory) nil t t))
            (funcall function)
            'finished))
      (smoke-step "cleanup"
        (when (buffer-live-p original)
          (switch-to-buffer original))
        (dolist (buffer (buffer-list))
          (unless (memq buffer before)
            (let ((kill-buffer-query-functions nil))
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))
        'finished))
    (smoke-emit "END" (list name smoke-errors))
    smoke-errors))
