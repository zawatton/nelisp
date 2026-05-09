;;; emacs-org-todo-test.el --- ERT for emacs-org-todo  -*- lexical-binding: t; -*-

;;; Commentary:

;; M3.2 TODO tests for `emacs-org-todo.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-fileio)
(require 'emacs-minibuffer)
(require 'emacs-org-outline)
(require 'emacs-org-todo)

(defmacro emacs-org-todo-test--with-fresh-world (&rest body)
  "Run BODY with clean mode, file, minibuffer, and buffer state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil)
         (emacs-fileio--buffer-files nil)
         (emacs-fileio--buffer-default-directories nil)
         (emacs-fileio--buffer-major-modes nil)
         (emacs-fileio--buffer-mode-names nil)
         (emacs-minibuffer--depth 0)
         (emacs-minibuffer--buffers nil)
         (emacs-minibuffer--prompts nil)
         (emacs-minibuffer--prompt-ends nil)
         (emacs-minibuffer--window nil)
         (emacs-minibuffer--saved-window nil)
         (emacs-minibuffer--input-queue nil)
         (auto-mode-alist nil)
         (default-directory "/tmp/")
         (major-mode 'fundamental-mode)
         (mode-name "Fundamental")
         (buffer-invisibility-spec nil))
     (unwind-protect
         (progn
           (when (fboundp 'emacs-mode-reset)
             (emacs-mode-reset))
           (org-outline--install-auto-mode)
           ,@body)
       (when (fboundp 'emacs-mode-reset)
         (emacs-mode-reset)))))

(defmacro emacs-org-todo-test--with-org-buffer (content &rest body)
  "Create a fresh Org buffer seeded with CONTENT, then run BODY."
  (declare (indent 1) (debug (form body)))
  `(emacs-org-todo-test--with-fresh-world
     (let ((buf (generate-new-buffer "*org-todo-test*")))
       (unwind-protect
           (with-current-buffer buf
             (insert ,content)
             (goto-char (point-min))
             (org-mode)
             ,@body)
         (when (buffer-live-p buf)
           (kill-buffer buf))))))

(defun emacs-org-todo-test--heading-line ()
  "Return the current heading line as a plain string."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun emacs-org-todo-test--line-containing (needle)
  "Return the full line containing NEEDLE."
  (save-excursion
    (goto-char (point-min))
    (search-forward needle nil t)
    (buffer-substring-no-properties
     (line-beginning-position)
     (line-end-position))))

(ert-deftest org-todo-cycles-through-keywords ()
  (emacs-org-todo-test--with-org-buffer
      "* Task\n"
    (let ((seen nil))
      (dotimes (_ 9)
        (push (emacs-org-todo-test--heading-line) seen)
        (org-todo))
      (should (equal
               (nreverse seen)
               '("* Task"
                 "* INBOX Task"
                 "* NEXT Task"
                 "* WAIT Task"
                 "* PROJECTS Task"
                 "* SCHEDULED Task"
                 "* SOMEDAY Task"
                 "* DONE Task"
                 "* CANCEL Task"))))))

(ert-deftest org-todo-with-prefix-arg-jumps-to-keyword ()
  (emacs-org-todo-test--with-org-buffer
      "* Task\n"
    (emacs-minibuffer-feed-input "WAIT")
    (org-todo t)
    (should (equal "* WAIT Task"
                   (emacs-org-todo-test--heading-line)))))

(ert-deftest org-todo-handles-non-heading-line-gracefully ()
  (emacs-org-todo-test--with-org-buffer
      "* Task\nbody\n"
    (forward-line 1)
    (let ((before (buffer-string)))
      (should-error (org-todo) :type 'user-error)
      (should (equal before (buffer-string))))))

(ert-deftest org-todo-done-inserts-closed-timestamp ()
  (emacs-org-todo-test--with-org-buffer
      "* SOMEDAY Task\nbody\n"
    (let ((org-log-done 'time))
      (org-todo)
      (should (equal "* DONE Task"
                     (emacs-org-todo-test--heading-line)))
      (should (string-match
               "^CLOSED: \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\]$"
               (emacs-org-todo-test--line-containing "CLOSED:"))))))

(ert-deftest org-todo-nil-removes-keyword ()
  (emacs-org-todo-test--with-org-buffer
      "* CANCEL Task\n"
    (org-todo)
    (should (equal "* Task"
                   (emacs-org-todo-test--heading-line)))))

(ert-deftest org-todo-six-keyword-sequence ()
  (emacs-org-todo-test--with-org-buffer
      "* Task\n"
    (let ((keywords nil))
      (dotimes (_ 8)
        (org-todo)
        (push (nth 1 (split-string (emacs-org-todo-test--heading-line) " "))
              keywords))
      (should (equal
               (nreverse keywords)
               '("INBOX" "NEXT" "WAIT" "PROJECTS"
                 "SCHEDULED" "SOMEDAY" "DONE" "CANCEL"))))))

(ert-deftest org-toggle-todo-toggles-checkboxes ()
  (emacs-org-todo-test--with-org-buffer
      "- [ ] item\n"
    (org-toggle-todo)
    (should (equal "- [X] item"
                   (emacs-org-todo-test--heading-line)))
    (org-toggle-todo)
    (should (equal "- [ ] item"
                   (emacs-org-todo-test--heading-line)))))

(ert-deftest org-todo-font-lock-faces-keywords ()
  (emacs-org-todo-test--with-org-buffer
      "* INBOX Task\n* DONE Finished\n"
    (font-lock-fontify-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "INBOX")
    (should (eq 'org-todo-keyword-todo
                (get-text-property (1- (point)) 'face)))
    (search-forward "DONE")
    (should (eq 'org-todo-keyword-done
                (get-text-property (1- (point)) 'face)))))

(provide 'emacs-org-todo-test)

;;; emacs-org-todo-test.el ends here
