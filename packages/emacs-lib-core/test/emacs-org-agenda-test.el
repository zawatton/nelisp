;;; emacs-org-agenda-test.el --- ERT for emacs-org-agenda -*- lexical-binding: t; -*-

;;; Commentary:

;; M3.4 agenda tests for `emacs-org-agenda.el'.

;;; Code:

(load-file
 (expand-file-name "../src/emacs-org-agenda.el"
                   (file-name-directory (or load-file-name buffer-file-name))))

(require 'ert)
(require 'cl-lib)
(require 'emacs-org-agenda)
(require 'emacs-org-capture)
(require 'emacs-org-todo)

(defvar emacs-org-agenda-test--tmp-counter 0)

(defun emacs-org-agenda-test--tmp-path (suffix)
  "Return a unique temp path ending with SUFFIX."
  (setq emacs-org-agenda-test--tmp-counter
        (1+ emacs-org-agenda-test--tmp-counter))
  (format "/tmp/emacs-org-agenda-test-%d-%d-%s"
          (emacs-pid)
          emacs-org-agenda-test--tmp-counter
          suffix))

(defun emacs-org-agenda-test--write-file (path content)
  "Write CONTENT to PATH."
  (with-temp-file path
    (insert content)))

(defmacro emacs-org-agenda-test--with-temp-files (bindings &rest body)
  "Create temp files from BINDINGS, then run BODY.
Each binding is (VAR SUFFIX CONTENT)."
  (declare (indent 1) (debug (sexp body)))
  (if (null bindings)
      `(progn ,@body)
    (let ((binding (car bindings)))
      `(let ((,(nth 0 binding)
              (emacs-org-agenda-test--tmp-path ,(nth 1 binding))))
         (unwind-protect
             (progn
               (emacs-org-agenda-test--write-file ,(nth 0 binding) ,(nth 2 binding))
               (emacs-org-agenda-test--with-temp-files ,(cdr bindings) ,@body))
           (when (file-exists-p ,(nth 0 binding))
             (delete-file ,(nth 0 binding))))))))

(defun emacs-org-agenda-test--fresh-state ()
  "Reset mutable agenda state used by tests."
  (setq org-agenda--state (make-hash-table :test 'eq :weakness nil))
  (setq org-agenda-files nil)
  (when (get-buffer org-agenda--buffer-name)
    (kill-buffer org-agenda--buffer-name))
  (let ((map (make-sparse-keymap)))
    (use-global-map map)
    (org-agenda--ensure-global-binding)))

(defmacro emacs-org-agenda-test--with-fresh-world (&rest body)
  "Run BODY with clean agenda/buffer/keymap state."
  (declare (indent 0) (debug (body)))
  `(let ((major-mode 'fundamental-mode)
         (mode-name "Fundamental")
         (org-todo-keywords '(("TODO" "NEXT" "|" "DONE"))))
     (unwind-protect
         (progn
           (when (fboundp 'emacs-mode-reset)
             (emacs-mode-reset))
           (emacs-org-agenda-test--fresh-state)
           ,@body)
       (when (get-buffer org-agenda--buffer-name)
         (kill-buffer org-agenda--buffer-name))
       (when (fboundp 'emacs-mode-reset)
         (emacs-mode-reset)))))

(defun emacs-org-agenda-test--agenda-string ()
  "Return the current agenda buffer contents."
  (with-current-buffer (get-buffer org-agenda--buffer-name)
    (buffer-string)))

(defun emacs-org-agenda-test--goto-line-containing (needle)
  "Move point to the first line containing NEEDLE."
  (goto-char (point-min))
  (search-forward needle nil t)
  (beginning-of-line))

(ert-deftest org-agenda-dispatches-a-shows-scheduled ()
  (emacs-org-agenda-test--with-fresh-world
    (emacs-org-agenda-test--with-temp-files
        ((journal "journal.org"
                  "* TODO Morning run\nSCHEDULED: <2026-05-09 14:30>\n* Write post\nDEADLINE: <2026-05-10>\n")
         (todo "todo.org"
               "* NEXT Review notes\nSCHEDULED: <2026-05-15>\n"))
      (let ((org-agenda-files (list journal todo)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda ()
                     (encode-time 0 0 8 9 5 2026))))
          (org-agenda ?a)
          (let ((text (emacs-org-agenda-test--agenda-string)))
            (should (string-match-p "Org Agenda: 2026-05-09 \\+6 days" text))
            (should (string-match-p "2026-05-09" text))
            (should (string-match-p "14:30 TODO Morning run" text))
            (should (string-match-p "2026-05-10" text))
            (should (string-match-p "Write post  \\[Deadline\\]" text))
            (should (string-match-p "NEXT Review notes" text))))))))

(ert-deftest org-agenda-dispatches-t-shows-todos ()
  (emacs-org-agenda-test--with-fresh-world
    (emacs-org-agenda-test--with-temp-files
        ((a "todos-a.org"
            "* TODO Ship feature\nBody\n* DONE Closed item\n")
         (b "todos-b.org"
            "* NEXT Keep moving :work:\n* Plain heading\n"))
      (let ((org-agenda-files (list a b)))
        (org-agenda ?t)
        (let ((text (emacs-org-agenda-test--agenda-string)))
          (should (string-match-p "Org Agenda: TODOs" text))
          (should (string-match-p "TODO Ship feature" text))
          (should (string-match-p "DONE Closed item" text))
          (should (string-match-p "NEXT Keep moving" text))
          (should-not (string-match-p "Plain heading" text)))))))

(ert-deftest org-agenda-dispatches-m-shows-matched-tag ()
  (emacs-org-agenda-test--with-fresh-world
    (emacs-org-agenda-test--with-temp-files
        ((a "tags.org"
            "* TODO Work item :work:\n* Personal item :home:\n* Dual tag :work:deep:\n"))
      (let ((org-agenda-files (list a)))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _args) ":work:")))
          (org-agenda ?m))
        (let ((text (emacs-org-agenda-test--agenda-string)))
          (should (string-match-p "Org Agenda: tag match work" text))
          (should (string-match-p "Work item" text))
          (should (string-match-p "Dual tag" text))
          (should-not (string-match-p "Personal item" text)))))))

(ert-deftest org-agenda-RET-jumps-to-source ()
  (emacs-org-agenda-test--with-fresh-world
    (emacs-org-agenda-test--with-temp-files
        ((a "jump.org"
            "* TODO Morning run\nSCHEDULED: <2026-05-09>\n* Other\n"))
      (let ((org-agenda-files (list a)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda ()
                     (encode-time 0 0 8 9 5 2026))))
          (org-agenda ?a))
        (let (target-buffer)
        (with-current-buffer (get-buffer org-agenda--buffer-name)
          (emacs-org-agenda-test--goto-line-containing "Morning run")
            (setq target-buffer (org-agenda-goto)))
          (with-current-buffer target-buffer
            (should (equal (expand-file-name a)
                           (expand-file-name (buffer-file-name (current-buffer)))))
            (should (= 1 (line-number-at-pos)))
            (should (looking-at "\\* TODO Morning run"))))))))

(ert-deftest org-agenda-goto-accepts-nelisp-buffer-source ()
  (emacs-org-agenda-test--with-fresh-world
    (let ((source (nelisp-ec-generate-new-buffer "agenda-source.org")))
      (unwind-protect
          (progn
            (nelisp-ec-with-current-buffer source
              (insert "* TODO Mirror source\nBody\n")
              (goto-char (point-min)))
            (let ((result (org-agenda--goto-entry
                           (list :file source :line 1))))
              (should (eq source result))
              (should (eq source (nelisp-ec-current-buffer)))
              (should (equal 1
                             (nelisp-ec-with-current-buffer source
                               (nelisp-ec-point))))))
        (when (and (fboundp 'nelisp-ec-kill-buffer)
                   (nelisp-ec-buffer-p source))
          (nelisp-ec-kill-buffer source))))))

(ert-deftest org-agenda-q-quits ()
  (emacs-org-agenda-test--with-fresh-world
    (emacs-org-agenda-test--with-temp-files
        ((a "quit.org" "* TODO Quit me\n"))
      (let ((org-agenda-files (list a))
            (quit-called nil))
        (org-agenda ?t)
        (cl-letf (((symbol-function 'quit-window)
                   (lambda (&rest _)
                     (setq quit-called t)
                     :quit)))
          (with-current-buffer (get-buffer org-agenda--buffer-name)
            (should (eq :quit (funcall
                               (lookup-key org-agenda-mode-map (kbd "q"))))))
          (should quit-called))))))

(ert-deftest org-agenda-g-reverts ()
  (emacs-org-agenda-test--with-fresh-world
    (emacs-org-agenda-test--with-temp-files
        ((a "revert.org"
            "* TODO First item\n"))
      (let ((org-agenda-files (list a)))
        (org-agenda ?t)
        (should (string-match-p "First item" (emacs-org-agenda-test--agenda-string)))
        (should-not (string-match-p "Second item" (emacs-org-agenda-test--agenda-string)))
        (emacs-org-agenda-test--write-file
         a
         "* TODO First item\n* TODO Second item\n")
        (with-current-buffer (get-buffer org-agenda--buffer-name)
          (call-interactively (lookup-key org-agenda-mode-map (kbd "g"))))
        (should (string-match-p "Second item" (emacs-org-agenda-test--agenda-string)))))))

(ert-deftest org-agenda-respects-org-agenda-files ()
  (emacs-org-agenda-test--with-fresh-world
    (emacs-org-agenda-test--with-temp-files
        ((a "one.org" "* TODO One\n")
         (b "two.org" "* TODO Two\n"))
      (let ((org-agenda-files (list a)))
        (org-agenda ?t)
        (should (string-match-p "One" (emacs-org-agenda-test--agenda-string)))
        (should-not (string-match-p "Two" (emacs-org-agenda-test--agenda-string))))
      (let ((org-agenda-files (list b)))
        (org-agenda ?t)
        (should-not (string-match-p "One" (emacs-org-agenda-test--agenda-string)))
        (should (string-match-p "Two" (emacs-org-agenda-test--agenda-string)))))))

(ert-deftest org-agenda-capture-todo-fixture-integration ()
  (emacs-org-agenda-test--with-fresh-world
    (emacs-org-agenda-test--with-temp-files
        ((todo "todo-fixture.org" "* Inbox\n"))
      (let ((org-capture-templates
             `(("t" "Todo" entry
                (file+headline ,todo "Inbox")
                "* TODO Captured task\n:PROPERTIES:\n:Effort: 0:30\n:END:\nCaptured body %T"
                :immediate-finish t)))
            (org-agenda-files (list todo)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda ()
                     (encode-time 0 34 12 30 6 2026))))
          (should (equal todo (org-capture "t"))))
        (with-temp-buffer
          (insert-file-contents todo)
          (org-mode)
          (goto-char (point-min))
          (search-forward "Captured task")
          (beginning-of-line)
          (should (equal "Scheduled to <2026-07-06 Mon>"
                         (org-schedule nil "2026-07-06")))
          (write-region (point-min) (point-max) todo nil 'silent))
        (let* ((scan (org-agenda--scan-file todo))
               (agenda (plist-get scan :agenda))
               (todos (plist-get scan :todos))
               (tree (with-temp-buffer
                       (insert-file-contents todo)
                       (org-mode)
                       (org-element-parse-buffer))))
          (should (= 1 (length todos)))
          (should (= 1 (length agenda)))
          (should (equal "Captured task"
                         (plist-get (car todos) :title)))
          (should (equal 'scheduled
                         (plist-get (car agenda) :kind)))
          (should (equal '("Effort")
                         (mapcar
                          (lambda (property)
                            (org-element-property :key property))
                          (org-element-map tree 'node-property
                            #'identity))))
          (should (= 2
                     (length (org-element-map tree 'timestamp
                               #'identity)))))
        (cl-letf (((symbol-function 'current-time)
                   (lambda ()
                     (encode-time 0 0 8 6 7 2026))))
          (org-agenda ?a))
        (let ((text (emacs-org-agenda-test--agenda-string)))
          (should (string-match-p "Org Agenda: 2026-07-06 \\+6 days" text))
          (should (string-match-p "TODO Captured task" text)))))))

(ert-deftest org-agenda-a-sorts-same-day-items-by-time ()
  (emacs-org-agenda-test--with-fresh-world
    (emacs-org-agenda-test--with-temp-files
        ((a "times.org"
            "* TODO Later\nSCHEDULED: <2026-05-09 18:00>\n* TODO Earlier\nSCHEDULED: <2026-05-09 09:00>\n* TODO Untimed\nSCHEDULED: <2026-05-09>\n"))
      (let ((org-agenda-files (list a)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda ()
                     (encode-time 0 0 8 9 5 2026))))
          (org-agenda ?a)
          (let* ((text (emacs-org-agenda-test--agenda-string))
                 (earlier (string-match "09:00 TODO Earlier" text))
                 (later (string-match "18:00 TODO Later" text))
                 (untimed (string-match "Untimed" text)))
            (should earlier)
            (should later)
            (should untimed)
            (should (< earlier later))
            (should (< later untimed))))))))

(ert-deftest org-agenda-f-and-b-shift-agenda-window ()
  (emacs-org-agenda-test--with-fresh-world
    (emacs-org-agenda-test--with-temp-files
        ((a "days.org"
            "* TODO Day one\nSCHEDULED: <2026-05-09>\n* TODO Day two\nSCHEDULED: <2026-05-10>\n"))
      (let ((org-agenda-files (list a)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda ()
                     (encode-time 0 0 8 9 5 2026))))
          (org-agenda ?a))
        (with-current-buffer (get-buffer org-agenda--buffer-name)
          (call-interactively (lookup-key org-agenda-mode-map (kbd "f"))))
        (should (string-match-p "Org Agenda: 2026-05-10 \\+6 days"
                                (emacs-org-agenda-test--agenda-string)))
        (with-current-buffer (get-buffer org-agenda--buffer-name)
          (call-interactively (lookup-key org-agenda-mode-map (kbd "b"))))
        (should (string-match-p "Org Agenda: 2026-05-09 \\+6 days"
                                (emacs-org-agenda-test--agenda-string)))))))

(provide 'emacs-org-agenda-test)

;;; emacs-org-agenda-test.el ends here
