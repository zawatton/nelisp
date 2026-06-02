;;; emacs-help-test.el --- ERT for emacs-help -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-help)

(defvar emacs-help-test--sample-variable '(alpha beta)
  "Sample variable docstring for help tests.")

(defun emacs-help-test--sample-function (required &optional optional)
  "Sample function docstring for help tests."
  (list required optional))

(defun emacs-help-test--fresh-help-state ()
  "Reset mutable help state used by tests."
  (setq emacs-help--state (make-hash-table :test 'eq :weakness nil))
  (when (get-buffer emacs-help--buffer-name)
    (kill-buffer emacs-help--buffer-name))
  (let ((map (make-sparse-keymap)))
    (use-global-map map)
    (emacs-help--ensure-global-bindings)
    map))

(defmacro emacs-help-test--with-fresh-world (&rest body)
  "Run BODY with clean help/buffer/keymap state."
  (declare (indent 0) (debug (body)))
  `(let ((major-mode 'fundamental-mode)
         (mode-name "Fundamental"))
     (unwind-protect
         (progn
           (when (fboundp 'emacs-mode-reset)
             (emacs-mode-reset))
           (emacs-help-test--fresh-help-state)
           ,@body)
       (when (get-buffer emacs-help--buffer-name)
         (kill-buffer emacs-help--buffer-name))
       (when (fboundp 'emacs-mode-reset)
         (emacs-mode-reset)))))

(defun emacs-help-test--help-string ()
  "Return the current `*Help*' buffer string."
  (with-current-buffer (get-buffer emacs-help--buffer-name)
    (buffer-string)))

(ert-deftest describe-function-renders-signature-and-docstring ()
  (emacs-help-test--with-fresh-world
    (describe-function 'emacs-help-test--sample-function)
    (let ((text (emacs-help-test--help-string)))
      (should (string-match-p
               "(emacs-help-test--sample-function required &optional optional)"
               text))
      (should (string-match-p "Sample function docstring for help tests\\." text))
      (should (string-match-p "Defined in:" text)))))

(ert-deftest describe-function-handles-undefined-function ()
  (emacs-help-test--with-fresh-world
    (should-error (describe-function 'emacs-help-test--missing-function)
                  :type 'user-error)))

(ert-deftest describe-variable-renders-value-and-docstring ()
  (emacs-help-test--with-fresh-world
    (describe-variable 'emacs-help-test--sample-variable)
    (let ((text (emacs-help-test--help-string)))
      (should (string-match-p "emacs-help-test--sample-variable is a variable\\." text))
      (should (string-match-p "(alpha beta)" text))
      (should (string-match-p "Sample variable docstring for help tests\\." text)))))

(ert-deftest describe-variable-handles-unbound-variable ()
  (emacs-help-test--with-fresh-world
    (makunbound 'emacs-help-test--temporary-unbound)
    (should-error (describe-variable 'emacs-help-test--temporary-unbound)
                  :type 'user-error)))

(ert-deftest describe-symbol-dispatches-to-function-or-variable ()
  (emacs-help-test--with-fresh-world
    (describe-symbol 'emacs-help-test--sample-function)
    (should (string-match-p "is a function" (emacs-help-test--help-string)))
    (describe-symbol 'emacs-help-test--sample-variable)
    (should (string-match-p "is a variable" (emacs-help-test--help-string)))))

(ert-deftest help-mode-history-commands-report-unavailable ()
  (emacs-help-test--with-fresh-world
    (should-error (help-go-back) :type 'user-error)
    (should-error (help-go-forward) :type 'user-error)))

(ert-deftest describe-key-resolves-binding ()
  (emacs-help-test--with-fresh-world
    (define-key (current-global-map) (kbd "C-c h")
                #'emacs-help-test--sample-function)
    (describe-key (kbd "C-c h"))
    (let ((text (emacs-help-test--help-string)))
      (should (string-match-p "C-c h runs the command emacs-help-test--sample-function\\." text))
      (should (string-match-p "Sample function docstring for help tests\\." text)))))

(ert-deftest help-mode-q-buries-buffer ()
  (emacs-help-test--with-fresh-world
    (describe-function 'emacs-help-test--sample-function)
    (let ((quit-called nil))
      (cl-letf (((symbol-function 'quit-window)
                 (lambda (&rest _)
                   (setq quit-called t)
                   :quit)))
        (should (eq :quit (funcall (lookup-key help-mode-map (kbd "q")))))
        (should quit-called)))))

(ert-deftest help-mode-g-rerenders-last-description ()
  (emacs-help-test--with-fresh-world
    (let ((calls 0))
      (cl-letf (((symbol-function 'documentation)
                 (lambda (_symbol &optional _raw)
                   (setq calls (1+ calls))
                   (format "render %d" calls))))
        (describe-function 'emacs-help-test--sample-function)
        (should (string-match-p "render 1" (emacs-help-test--help-string)))
        (call-interactively (lookup-key help-mode-map (kbd "g")))
        (should (string-match-p "render 2" (emacs-help-test--help-string)))))))

(ert-deftest emacs-help-global-key-bindings-installed ()
  (emacs-help-test--with-fresh-world
    (let ((map (current-global-map)))
      (should (eq #'describe-function (lookup-key map (kbd "C-h f"))))
      (should (eq #'describe-variable (lookup-key map (kbd "C-h v"))))
      (should (eq #'describe-key (lookup-key map (kbd "C-h k")))))))

(provide 'emacs-help-test)

;;; emacs-help-test.el ends here
