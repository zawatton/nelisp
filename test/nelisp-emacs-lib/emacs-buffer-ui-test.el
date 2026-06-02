;;; emacs-buffer-ui-test.el --- ERT tests for emacs-buffer-ui.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-buffer-ui)
(require 'emacs-mode)

(defmacro emacs-buffer-ui-test--with-fresh-world (&rest body)
  "Run BODY with clean buffer/window/minibuffer/file state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (emacs-buffer--state (make-hash-table :test 'eq))
         (emacs-buffer--variable-buffer-local nil)
         (emacs-buffer--default-values (make-hash-table :test 'eq))
         (emacs-fileio--buffer-files nil)
         (emacs-window--id-counter 0)
         (emacs-window--root nil)
         (emacs-window--selected nil)
         (emacs-minibuffer--depth 0)
         (emacs-minibuffer--buffers nil)
         (emacs-minibuffer--prompts nil)
         (emacs-minibuffer--prompt-ends nil)
         (emacs-minibuffer--window nil)
         (emacs-minibuffer--saved-window nil)
         (emacs-minibuffer--input-queue nil)
         (emacs-minibuffer-history nil)
         (emacs-minibuffer-default nil)
         (minibuffer-completion-table nil)
         (minibuffer-completion-confirm nil))
     (emacs-mode-reset)
     ,@body))

(ert-deftest switch-to-buffer-completes-existing ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha"))
          (b (nelisp-ec-generate-new-buffer "beta")))
      (emacs-window-set-window-buffer (emacs-window-selected-window) a)
      (nelisp-ec-set-buffer a)
      (emacs-minibuffer-feed-input "beta")
      (should (eq b (call-interactively #'switch-to-buffer-interactive)))
      (should (eq b (emacs-window-window-buffer)))
      (should (eq b (nelisp-ec-current-buffer))))))

(ert-deftest switch-to-buffer-creates-new-on-unknown-name ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha")))
      (emacs-window-set-window-buffer (emacs-window-selected-window) a)
      (nelisp-ec-set-buffer a)
      (emacs-minibuffer-feed-input "gamma")
      (let ((buf (call-interactively #'switch-to-buffer-interactive)))
        (should (equal "gamma" (nelisp-ec-buffer-name buf)))
        (should (memq buf (emacs-buffer-buffer-list)))
        (should (eq buf (emacs-window-window-buffer)))))))

(ert-deftest kill-buffer-removes-from-buffer-list ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha"))
          (b (nelisp-ec-generate-new-buffer "beta")))
      (emacs-window-set-window-buffer (emacs-window-selected-window) a)
      (nelisp-ec-set-buffer a)
      (should (memq a (emacs-buffer-buffer-list)))
      (should (eq t (kill-buffer-interactive a)))
      (should-not (memq a (emacs-buffer-buffer-list)))
      (should (eq b (emacs-window-window-buffer))))))

(ert-deftest kill-buffer-prompts-on-modified ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha")))
      (emacs-window-set-window-buffer (emacs-window-selected-window) a)
      (nelisp-ec-set-buffer a)
      (emacs-buffer-set-buffer-modified-p t a)
      (let ((prompt nil))
        (cl-letf (((symbol-function 'emacs-minibuffer-yes-or-no-p)
                   (lambda (message)
                     (setq prompt message)
                     t)))
          (should (eq t (kill-buffer-interactive a))))
        (should (string-match-p "modified; kill anyway" prompt))))))

(ert-deftest list-buffers-renders-four-columns ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "alpha"))
          (b (nelisp-ec-generate-new-buffer "beta")))
      (nelisp-ec-with-current-buffer a
        (nelisp-ec-insert "hello")
        (emacs-buffer-set-buffer-local-value 'major-mode a 'text-mode))
      (nelisp-ec-with-current-buffer b
        (emacs-buffer-set-buffer-local-value 'major-mode b 'emacs-lisp-mode))
      (setq emacs-fileio--buffer-files (list (cons b "/tmp/beta.txt")))
      (let ((buf (emacs-buffer-ui-list-buffers)))
        (should (equal "*Buffer List*" (nelisp-ec-buffer-name buf)))
        (let ((text (nelisp-ec-with-current-buffer buf
                      (nelisp-ec-buffer-string))))
          (should (string-match-p "^name[[:space:]]+size[[:space:]]+mode[[:space:]]+file$" text))
          (should (string-match-p "^alpha[[:space:]]+5[[:space:]]+text-mode[[:space:]]*$" text))
          (should (string-match-p "^beta[[:space:]]+0[[:space:]]+emacs-lisp-mode[[:space:]]+/tmp/beta.txt$" text)))))))

(ert-deftest list-buffers-renders-files-standalone-visited-file ()
  (emacs-buffer-ui-test--with-fresh-world
    (let ((buf (nelisp-ec-generate-new-buffer "standalone.txt")))
      (cl-letf (((symbol-function 'files--buffer-file-name)
                 (lambda (buffer)
                   (and (eq buffer buf) "/tmp/standalone.txt"))))
        (let ((out (emacs-buffer-ui-list-buffers)))
          (let ((text (nelisp-ec-with-current-buffer out
                        (nelisp-ec-buffer-string))))
            (should (string-match-p
                     "^standalone\\.txt[[:space:]]+0[[:space:]]+fundamental-mode[[:space:]]+/tmp/standalone\\.txt$"
                     text))))))))

(provide 'emacs-buffer-ui-test)

;;; emacs-buffer-ui-test.el ends here
