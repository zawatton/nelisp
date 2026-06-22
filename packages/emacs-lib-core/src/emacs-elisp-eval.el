;;; emacs-elisp-eval.el --- Interactive Elisp eval commands for nelisp-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 02 v0.1 daily-driver §3.2.4 M2.4.
;;
;; Adds the interactive eval UI layer on top of the existing read/eval
;; primitives:
;;
;; - `eval-last-sexp'  (`C-x C-e')
;; - `eval-defun'      (`C-M-x')
;; - `eval-region'
;; - `eval-buffer'
;;
;; The commands read source from the current buffer, evaluate it, and
;; mirror the result to the echo area via `message' using
;; `prin1-to-string'.  Errors are caught and rendered as echo-area text
;; rather than escaping to the command loop.

;;; Code:

(require 'emacs-buffer-builtins)
(require 'emacs-eval)
(require 'emacs-keymap-builtins)

;; Capture the host runtime's `eval-buffer' / `eval-region' before this
;; module installs its interactive replacements.  Under host Emacs both
;; are C subrs that `load' calls internally to populate `load-history';
;; under standalone NeLisp neither symbol is fboundp yet.  Saving them
;; here lets the polyfills below delegate when invoked from `load',
;; preserving load-history bookkeeping for subsequently loaded files.
(defvar emacs-elisp-eval--orig-eval-buffer
  (and (fboundp 'eval-buffer)
       (subrp (symbol-function 'eval-buffer))
       (symbol-function 'eval-buffer))
  "Original `eval-buffer' subr captured at load time, or nil.")

(defvar emacs-elisp-eval--orig-eval-region
  (and (fboundp 'eval-region)
       (subrp (symbol-function 'eval-region))
       (symbol-function 'eval-region))
  "Original `eval-region' subr captured at load time, or nil.")

(defun emacs-elisp-eval--echo-result (value)
  "Render VALUE in the echo area and return it."
  (message "%s" (prin1-to-string value))
  value)

(defun emacs-elisp-eval--echo-error (err)
  "Render ERR in the echo area and return nil."
  (message "%s" (error-message-string err))
  nil)

(defun emacs-elisp-eval--char-at (pos)
  "Return the character at POS in the current buffer, or nil."
  (when (< pos (point-max))
    (aref (buffer-substring-no-properties pos (1+ pos)) 0)))

(defun emacs-elisp-eval--read-string (text)
  "Read one Lisp form from TEXT."
  (car (read-from-string text)))

(defun emacs-elisp-eval--eval-string (text)
  "Read and evaluate TEXT as one Lisp form."
  (eval (emacs-elisp-eval--read-string text)))

(defun emacs-elisp-eval--beginning-of-defun ()
  "Return the start position of the enclosing top-level form.

This follows the repo's MVP convention: a defun starts at a line whose
first character is `('."
  (save-excursion
    (let ((pmin (point-min))
          (p (point)))
      (catch 'done
        (while (> p pmin)
          (let ((bol p))
            (while (and (> bol pmin)
                        (not (eq (emacs-elisp-eval--char-at (1- bol)) ?\n)))
              (setq bol (1- bol)))
            (when (eq (emacs-elisp-eval--char-at bol) ?\()
              (throw 'done bol))
            (setq p (max pmin (1- bol)))))
        pmin))))

(defun emacs-elisp-eval--defun-bounds ()
  "Return `(START . END)' for the enclosing top-level form."
  (save-excursion
    (let ((start (emacs-elisp-eval--beginning-of-defun)))
      (goto-char start)
      (unless (eq (emacs-elisp-eval--char-at start) ?\()
        (error "No enclosing top-level form"))
      (let ((end (progn (forward-sexp) (point))))
        (cons start end)))))

(defun emacs-elisp-eval--eval-forms-in-string (text)
  "Read and evaluate every top-level form in TEXT.
Return the last value, or nil when TEXT contains no forms."
  (let ((pos 0)
        (limit (length text))
        (last nil))
    (while (< pos limit)
      (condition-case err
          (pcase-let ((`(,form . ,next-pos) (read-from-string text pos)))
            (setq last (eval form)
                  pos next-pos))
        (end-of-file
         (setq pos limit))
        (error
         (signal (car err) (cdr err)))))
    last))

(defun emacs-elisp-eval--global-map ()
  "Return the current global map, creating one if needed."
  (when (fboundp 'current-global-map)
    (or (current-global-map)
        (let ((map (make-sparse-keymap)))
          (when (fboundp 'use-global-map)
            (use-global-map map))
          map))))

(defun emacs-elisp-eval--install-bindings ()
  "Install the M2.4 eval bindings."
  (let ((global-map (emacs-elisp-eval--global-map)))
    (when global-map
      (define-key global-map (kbd "C-x C-e") #'eval-last-sexp)
      (define-key global-map (kbd "C-M-x") #'eval-defun))
    (when (and (boundp 'emacs-lisp-mode-map) (keymapp emacs-lisp-mode-map))
      (define-key emacs-lisp-mode-map (kbd "C-x C-e") #'eval-last-sexp)
      (define-key emacs-lisp-mode-map (kbd "C-M-x") #'eval-defun))))

;;;###autoload
(defun eval-last-sexp (&optional eval-last-sexp-arg)
  "Evaluate the sexp before point and echo the result."
  (interactive "P")
  (ignore eval-last-sexp-arg)
  (condition-case err
      (let ((value (save-excursion
                     (backward-sexp)
                     (emacs-elisp-eval--eval-string
                      (buffer-substring-no-properties (point)
                                                      (save-excursion
                                                        (forward-sexp)
                                                        (point)))))))
        (emacs-elisp-eval--echo-result value))
    (error
     (emacs-elisp-eval--echo-error err))))

;;;###autoload
(defun eval-defun (&optional edebug-it)
  "Evaluate the enclosing top-level form and echo the result."
  (interactive "P")
  (ignore edebug-it)
  (condition-case err
      (pcase-let* ((`(,start . ,end) (emacs-elisp-eval--defun-bounds))
                   (value (emacs-elisp-eval--eval-string
                           (buffer-substring-no-properties start end))))
        (emacs-elisp-eval--echo-result value))
    (error
     (emacs-elisp-eval--echo-error err))))

;;;###autoload
(defun emacs-elisp-eval-eval-region (beg end &optional stream read-function)
  "Evaluate each top-level form between BEG and END and echo the last result."
  (interactive "r")
  (if (and load-in-progress emacs-elisp-eval--orig-eval-region)
      ;; `load' depends on the host C subr's bookkeeping (load-history,
      ;; lexical-binding cookies, byte-compile cookies).  Delegate
      ;; transparently to preserve that surface.
      (funcall emacs-elisp-eval--orig-eval-region beg end stream read-function)
    (ignore stream read-function)
    (condition-case err
        (let ((value (emacs-elisp-eval--eval-forms-in-string
                      (buffer-substring-no-properties beg end))))
          (emacs-elisp-eval--echo-result value))
      (error
       (emacs-elisp-eval--echo-error err)))))

;;;###autoload
(defun emacs-elisp-eval-eval-buffer
    (&optional buffer printflag filename unibyte do-allow-print)
  "Evaluate every top-level form in the current buffer and echo the last result."
  (interactive)
  (if (and load-in-progress emacs-elisp-eval--orig-eval-buffer)
      ;; Same delegation as `eval-region': `load' calls `eval-buffer'
      ;; internally and depends on the C subr's load-history side
      ;; effects.  Routing through our polyfill there would silently
      ;; drop the per-file load-history entry (breaking `symbol-file'
      ;; lookups in every later-loaded file).
      (funcall emacs-elisp-eval--orig-eval-buffer
               buffer printflag filename unibyte do-allow-print)
    (ignore printflag filename unibyte do-allow-print)
    (save-current-buffer
      (when buffer
        (set-buffer buffer))
      (emacs-elisp-eval-eval-region (point-min) (point-max)))))

;; Host Emacs native-comp may install subr trampolines when replacing C
;; subrs.  Package smoke intentionally shadows host `cl-lib', so keep that
;; trampoline path off while installing the compatibility command names.
(let ((native-comp-enable-subr-trampolines nil))
  (fset 'eval-region #'emacs-elisp-eval-eval-region)
  (fset 'eval-buffer #'emacs-elisp-eval-eval-buffer))

(emacs-elisp-eval--install-bindings)

(provide 'emacs-elisp-eval)

;;; emacs-elisp-eval.el ends here
