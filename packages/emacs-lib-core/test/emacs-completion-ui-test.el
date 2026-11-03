;;; emacs-completion-ui-test.el --- ERT tests for emacs-completion-ui.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-completion-ui)

(defmacro emacs-completion-ui-test--with-fresh-world (&rest body)
  "Run BODY with clean buffer/window/minibuffer/completion state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
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
         (emacs-minibuffer--read-fn nil)
         (emacs-minibuffer--key-fn nil)
         (emacs-minibuffer--y-or-n-fn nil)
         (minibuffer-completion-table nil)
         (minibuffer-completion-confirm nil)
         (emacs-completion-ui--completion-state nil))
     ,@body))

(defmacro emacs-completion-ui-test--with-active-minibuffer
    (initial table &rest body)
  "Run BODY inside an active minibuffer seeded with INITIAL and TABLE."
  (declare (indent 2) (debug (form form body)))
  `(let ((minibuffer-completion-table ,table))
     (emacs-minibuffer--with-frame
      "Prompt: " ,initial
      (lambda ()
        ,@body))))

(ert-deftest minibuffer-complete-prefix-match ()
  (emacs-completion-ui-test--with-fresh-world
    (emacs-completion-ui-test--with-active-minibuffer
        "foo" '("foobar" "food" "bar")
      (minibuffer-complete)
      (let* ((buf (plist-get emacs-completion-ui--completion-state :buffer))
             (text (nelisp-ec-with-current-buffer buf
                     (nelisp-ec-buffer-string))))
        (should (string-match-p "foobar" text))
        (should (string-match-p "food" text))
        (should-not (string-match-p "\\_<bar\\_>" text))))))

(ert-deftest minibuffer-complete-unique-auto-completes ()
  (emacs-completion-ui-test--with-fresh-world
    (emacs-completion-ui-test--with-active-minibuffer
        "foob" '("foobar" "food")
      (minibuffer-complete)
      (should (string-equal "foobar"
                            (emacs-minibuffer-minibuffer-contents))))))

(ert-deftest minibuffer-complete-multiple-shows-completions-buffer ()
  (emacs-completion-ui-test--with-fresh-world
    (emacs-completion-ui-test--with-active-minibuffer
        "fo" '("foobar" "food")
      (let ((buf (minibuffer-complete)))
        (should (equal "*Completions*" (nelisp-ec-buffer-name buf)))
        (should (eq buf (plist-get emacs-completion-ui--completion-state :buffer)))))))

(ert-deftest completions-buffer-rendering-shape ()
  (emacs-completion-ui-test--with-fresh-world
    (emacs-completion-ui-test--with-active-minibuffer
        "" '("alpha" "beta" "gamma" "delta")
      (minibuffer-complete)
      (let ((text (nelisp-ec-with-current-buffer
                      (plist-get emacs-completion-ui--completion-state :buffer)
                    (nelisp-ec-buffer-string))))
        (should (string-match-p "alpha[[:space:]]+beta" text))
        (should (string-match-p "\n" text))))))

(ert-deftest minibuffer-complete-and-exit-confirms-and-closes ()
  (emacs-completion-ui-test--with-fresh-world
    (emacs-completion-ui-test--with-active-minibuffer
        "foob" '("foobar" "food")
      (let ((exited nil))
        (cl-letf (((symbol-function 'emacs-minibuffer-exit-minibuffer)
                   (lambda ()
                     (setq exited t)
                     nil)))
          (minibuffer-complete-and-exit)
          (should exited)
          (should (string-equal "foobar"
                                (emacs-minibuffer-minibuffer-contents))))))))

(ert-deftest choose-completion-confirms-selected-candidate ()
  (emacs-completion-ui-test--with-fresh-world
    (emacs-completion-ui-test--with-active-minibuffer
        "fo" '("foobar" "food")
      (let ((exited nil))
        (cl-letf (((symbol-function 'emacs-minibuffer-exit-minibuffer)
                   (lambda ()
                     (setq exited t)
                     nil)))
          (minibuffer-complete)
          (switch-to-completions)
          (next-completion)
          (should (string-equal "food" (choose-completion)))
          (should exited)
          (should (string-equal "food"
                                (nelisp-ec-with-current-buffer
                                    (emacs-minibuffer--current-buffer)
                                  (emacs-minibuffer-minibuffer-contents)))))))))

(provide 'emacs-completion-ui-test)

;;; emacs-completion-ui-test.el ends here
