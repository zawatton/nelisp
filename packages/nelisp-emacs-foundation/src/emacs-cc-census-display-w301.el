;;; emacs-cc-census-display-w301.el --- Batch display primitives  -*- lexical-binding: t; -*-

;;; Code:

(defvar executing-kbd-macro nil
  "Non-nil while executing a keyboard macro.")
(defvar ring-bell-function nil
  "Function called instead of ringing the terminal bell interactively.")

(defun emacs-cc-census-display-w301--arity (name arguments maximum)
  "Validate that NAME received at most MAXIMUM ARGUMENTS."
  (let ((count (length arguments)))
    (when (> count maximum)
      (signal 'wrong-number-of-arguments (list name count)))))

(when (or (not (fboundp 'current-message)) (fboundp 'nelisp--repr))
  (defun current-message (&rest arguments)
    "Return the current echo-area message, or nil when there is none.
The standalone batch message implementation writes to stderr and does
not create an echo-area message."
    (emacs-cc-census-display-w301--arity 'current-message arguments 0)
    (and (not noninteractive) (boundp 'emacs-special-buffers-echo-message)
         emacs-special-buffers-echo-message)))

(unless (fboundp 'ding)
  (defun ding (&rest arguments)
    "Ring the bell; a non-nil ARG permits keyboard macro execution.
Batch sessions emit an ASCII bell regardless of ARG or bell settings."
    (emacs-cc-census-display-w301--arity 'ding arguments 1)
    (cond
     (noninteractive
      (nelisp--write-stdout-bytes "\a"))
     ((and (not (car arguments))
           (boundp 'executing-kbd-macro) executing-kbd-macro)
      (signal 'user-error
              '("Keyboard macro terminated by a command ringing the bell")))
     ((and (boundp 'ring-bell-function) ring-bell-function)
      (let ((function ring-bell-function))
        (setq ring-bell-function nil)
        (funcall function)
        (setq ring-bell-function function)))
     (t (nelisp--write-stdout-bytes "\a")))
    nil))

(unless (fboundp 'redisplay)
  (defun redisplay (&rest arguments)
    "Perform redisplay and return t, unless executing a keyboard macro.
The optional FORCE argument is ignored.  A batch session has no display
output to update."
    (emacs-cc-census-display-w301--arity 'redisplay arguments 1)
    (not (and (boundp 'executing-kbd-macro) executing-kbd-macro))))

(unless (fboundp 'redraw-display)
  (defun redraw-display (&rest arguments)
    "Clear and redraw each visible frame; return nil."
    (interactive)
    (emacs-cc-census-display-w301--arity 'redraw-display arguments 0)
    (unless noninteractive
      (dolist (frame (frame-list))
        (when (eq (frame-visible-p frame) t)
          (redraw-frame frame))))
    nil))

;; The remaining assigned primitives need interfaces outside this unit:
;; evaluator backtrace frames, window glyph matrices, native-buffer window
;; support, and real horizontal-scroll and dedication accessors.  Keep their
;; existing bindings rather than manufacture state their readers cannot see.

(provide 'emacs-cc-census-display-w301)
;;; emacs-cc-census-display-w301.el ends here
