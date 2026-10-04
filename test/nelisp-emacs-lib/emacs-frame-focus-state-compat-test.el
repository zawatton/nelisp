;;; emacs-frame-focus-state-compat-test.el --- frame focus providers -*- lexical-binding: t; -*-

(require 'ert)
(defconst emacs-frame-focus-state-compat-test--terminal-source
  (expand-file-name
   "../../packages/nelisp-emacs-foundation/src/emacs-cc-terminal-focus-1.el"
   (file-name-directory (or load-file-name buffer-file-name))))
(defconst emacs-frame-focus-state-compat-test--state-source
  (expand-file-name
   "../../packages/nelisp-emacs-foundation/src/emacs-frame-focus-state-compat.el"
   (file-name-directory (or load-file-name buffer-file-name))))
(load emacs-frame-focus-state-compat-test--terminal-source nil t)
(load emacs-frame-focus-state-compat-test--state-source nil t)

(defun emacs-frame-focus-state-compat-test--with-providers (thunk)
  "Run THUNK with the providers installed, restoring GNU functions."
  (let ((originals (mapcar (lambda (name) (cons name (symbol-function name)))
                           '(tty-top-frame terminal-parameter frame-focus-state))))
    (unwind-protect
        (progn
          (dolist (entry originals) (fmakunbound (car entry)))
          (load emacs-frame-focus-state-compat-test--terminal-source nil t)
          (load emacs-frame-focus-state-compat-test--state-source nil t)
          (funcall thunk))
      (dolist (entry originals) (fset (car entry) (cdr entry))))))

(ert-deftest emacs-frame-focus-state-compat/guard-preserves-gnu-functions ()
  (let ((originals (mapcar (lambda (name) (cons name (symbol-function name)))
                           '(tty-top-frame terminal-parameter frame-focus-state))))
    (load emacs-frame-focus-state-compat-test--terminal-source nil t)
    (load emacs-frame-focus-state-compat-test--state-source nil t)
    (dolist (entry originals)
      (should (eq (cdr entry) (symbol-function (car entry)))))))

(ert-deftest emacs-frame-focus-state-compat/headless-frame-state-is-live-data ()
  (let* ((frame (selected-frame))
         (old (frame-parameter frame 'last-focus-update)))
    (unwind-protect
        (emacs-frame-focus-state-compat-test--with-providers
         (lambda ()
           (should-not window-system)
           (should-not (tty-top-frame frame))
           (should (= (terminal-parameter frame 'normal-erase-is-backspace) 0))
           (should-not (terminal-parameter frame 'tty-focus-state))
           (should-not (frame-focus-state))
           (should-not (frame-focus-state frame))
           (set-frame-parameter frame 'last-focus-update t)
           (should (eq (frame-focus-state frame) t))
           (set-frame-parameter frame 'last-focus-update nil)
           (should-not (frame-focus-state frame))
           (should-error (frame-focus-state 'bad-frame)
                         :type 'wrong-type-argument)
           (should-error (frame-focus-state frame frame)
                         :type 'wrong-number-of-arguments)))
      (set-frame-parameter frame 'last-focus-update old))))

(ert-deftest emacs-frame-focus-state-compat/invalid-terminals-and-arity ()
  (emacs-frame-focus-state-compat-test--with-providers
   (lambda ()
     (dolist (terminal '(t fake-terminal 7 "display"))
       (should-error (tty-top-frame terminal) :type 'wrong-type-argument)
       (should-error (terminal-parameter terminal 'tty-focus-state)
                     :type 'wrong-type-argument))
     (should-error (tty-top-frame nil nil)
                   :type 'wrong-number-of-arguments)
     (should-error (terminal-parameter nil) :type 'wrong-number-of-arguments)
     (should-error (terminal-parameter nil 'tty-focus-state 'focused)
                   :type 'wrong-number-of-arguments)
     (should-error (frame-focus-state nil nil)
                   :type 'wrong-number-of-arguments))))

(provide 'emacs-frame-focus-state-compat-test)
;;; emacs-frame-focus-state-compat-test.el ends here
