;;; nelisp-bytecode-cleanup.el --- Shared bytecode restoration policy -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(defun nelisp-bytecode-cleanup-source ()
  "Return the Lisp reference factory for buffer restoration entries."
  '(let ((current (symbol-function 'current-buffer))
         (live (symbol-function 'buffer-live-p))
         (select (symbol-function 'set-buffer))
         (point-marker-value (symbol-function 'point-marker))
         (copy (symbol-function 'copy-marker))
         (minimum (symbol-function 'point-min)) (maximum (symbol-function 'point-max))
         (position (symbol-function 'marker-position))
         (detach (symbol-function 'set-marker)) (go (symbol-function 'goto-char))
         (wide (symbol-function 'widen)) (narrow (symbol-function 'narrow-to-region)))
     (lambda (opcode)
       (let ((buffer (funcall current)))
         (cond
          ((memq opcode '(97 114))
           (lambda () (when (funcall live buffer) (funcall select buffer))))
          ((= opcode 138)
           (let ((marker (funcall point-marker-value)))
             (lambda ()
               (unwind-protect
                   (when (funcall live buffer)
                     (funcall select buffer) (funcall go (funcall position marker)))
                 (funcall detach marker nil)))))
          ((= opcode 140)
           (let ((begin (funcall copy (funcall minimum) nil))
                 (end (funcall copy (funcall maximum) t)))
             (lambda ()
               (let ((active (funcall current)))
                 (unwind-protect
                     (when (funcall live buffer)
                       (funcall select buffer) (funcall wide)
                       (funcall narrow (funcall position begin) (funcall position end)))
                   (when (funcall live active) (funcall select active))
                   (funcall detach begin nil) (funcall detach end nil))))))
          (t (error "Unknown bytecode restoration opcode")))))))
(defconst nelisp-bytecode-cleanup-factory (eval (nelisp-bytecode-cleanup-source) t))
(fset 'nelisp--bytecode-save-state nelisp-bytecode-cleanup-factory)
(let ((function-test (symbol-function 'functionp))
      (evaluate (symbol-function 'eval)))
  (defun nelisp-bytecode-cleanup-run (payload)
    "Call a function payload or evaluate its forms, as GNU prog_ignore does."
    (if (funcall function-test payload) (funcall payload)
      (dolist (form payload) (funcall evaluate form)))))
(defvar temp-buffer-setup-hook nil)
(defvar temp-buffer-show-hook nil)
(defvar temp-buffer-show-function nil)
(defun nelisp-bytecode-legacy-source ()
  "Return frozen evaluator providers for GNU's obsolete wrapper opcodes.
SETUP returns the buffer before the caller installs a specpdl binding; SHOW
runs while that binding is visible, before the caller pops exactly one entry."
  '(let ((evaluate (symbol-function 'eval))
         (current (symbol-function 'current-buffer))
         (select (symbol-function 'set-buffer))
         (live (symbol-function 'buffer-live-p))
         (create (symbol-function 'get-buffer-create))
         (erase (symbol-function 'erase-buffer))
         (wide (symbol-function 'widen)) (go (symbol-function 'goto-char))
         (minimum (symbol-function 'point-min))
         (hooks (symbol-function 'run-hooks))
         (kill-locals (symbol-function 'kill-all-local-variables))
         (string-test (symbol-function 'stringp))
         (raise (symbol-function 'signal)))
     (list
      (cons 'nelisp--bytecode-legacy-window
            (lambda (forms)
              ;; The batch reader has no windows. GNU batch restores the
              ;; current buffer, but retains point changes in the saved buffer.
              (let ((buffer (funcall current)))
                (unwind-protect (funcall evaluate (cons 'progn forms))
                  (when (funcall live buffer) (funcall select buffer))))))
      (cons 'nelisp--bytecode-legacy-catch
            (lambda (tag body) (catch tag (funcall evaluate body))))
      (cons 'nelisp--bytecode-legacy-condition
            (lambda (var body clauses)
              (funcall evaluate (cons 'condition-case (cons var (cons body clauses))))))
      (cons 'nelisp--bytecode-legacy-setup
            (lambda (name)
              (unless (funcall string-test name)
                (funcall raise 'wrong-type-argument (list 'stringp name)))
              (let ((buffer (funcall current)) (directory default-directory)
                    (output (funcall create name)))
                (unwind-protect
                    (progn
                      (funcall select output) (funcall kill-locals)
                      (setq default-directory directory buffer-read-only nil
                            buffer-file-name nil buffer-undo-list t)
                      (let ((inhibit-read-only t) (inhibit-modification-hooks t))
                        (funcall wide) (funcall erase)
                        (funcall hooks 'temp-buffer-setup-hook))
                      output)
                  (when (funcall live buffer) (funcall select buffer))))))
      (cons 'nelisp--bytecode-legacy-show
            (lambda (output value)
              (let ((buffer (funcall current)) (directory default-directory))
                (unwind-protect
                    (progn (funcall select output)
                           (setq default-directory directory)
                           (funcall wide) (funcall go (funcall minimum)))
                  (when (funcall live buffer) (funcall select buffer))))
              ;; GNU -Q --batch still has a live output window. The reader
              ;; models its hook's buffer context without a display object.
              (if temp-buffer-show-function
                  (funcall temp-buffer-show-function output)
                (let ((buffer (funcall current)))
                  (unwind-protect
                      (progn (funcall select output)
                             (funcall hooks 'temp-buffer-show-hook))
                    (when (funcall live buffer) (funcall select buffer)))))
              value)))))
(defconst nelisp-bytecode-legacy-providers (eval (nelisp-bytecode-legacy-source) t))
(dolist (entry nelisp-bytecode-legacy-providers) (fset (car entry) (cdr entry)))
(provide 'nelisp-bytecode-cleanup)
