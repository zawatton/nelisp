;;; emacs-org-capture-test.el --- ERT for emacs-org-capture  -*- lexical-binding: t; -*-

;;; Commentary:

;; M3.5 lightweight org-capture tests for `emacs-org-capture.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-fileio)
(require 'emacs-org-capture)

(defvar emacs-org-capture-test--tmp-counter 0)

(defun emacs-org-capture-test--tmp-path (suffix)
  "Return a unique temporary path ending with SUFFIX."
  (setq emacs-org-capture-test--tmp-counter
        (1+ emacs-org-capture-test--tmp-counter))
  (format "/tmp/emacs-org-capture-test-%d-%d-%s"
          (emacs-pid)
          emacs-org-capture-test--tmp-counter
          suffix))

(defmacro emacs-org-capture-test--with-fresh-world (&rest body)
  "Run BODY with clean capture, buffer, and fileio state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil)
         (emacs-fileio--buffer-files nil)
         (emacs-fileio--buffer-default-directories nil)
         (emacs-fileio--buffer-major-modes nil)
         (emacs-fileio--buffer-mode-names nil)
         (emacs-keymap-local-map nil)
         (auto-mode-alist nil)
         (default-directory "/tmp/")
         (major-mode 'fundamental-mode)
         (mode-name "Fundamental")
         (org-capture--state (make-hash-table :test 'eq :weakness nil))
         (org-capture-templates nil))
     (unwind-protect
         (progn
           (when (fboundp 'emacs-mode-reset)
             (emacs-mode-reset))
           ,@body)
       (when (get-buffer org-capture--buffer-name)
         (kill-buffer org-capture--buffer-name))
       (when (fboundp 'emacs-mode-reset)
         (emacs-mode-reset)))))

(defmacro emacs-org-capture-test--with-temp-file (var suffix content &rest body)
  "Bind VAR to a temp file ending in SUFFIX seeded with CONTENT."
  (declare (indent 3) (debug (symbolp form form body)))
  `(let ((,var (emacs-org-capture-test--tmp-path ,suffix)))
     (unwind-protect
         (progn
           (with-temp-file ,var
             (insert ,content))
           ,@body)
       (when (file-exists-p ,var)
         (delete-file ,var)))))

(defmacro emacs-org-capture-test--with-fixed-time (&rest body)
  "Run BODY with deterministic timestamp formatting."
  (declare (indent 0) (debug (body)))
  `(cl-letf (((symbol-function 'current-time)
              (lambda () '(0 0 0 0)))
             ((symbol-function 'format-time-string)
              (lambda (fmt &optional _time _zone)
                (pcase fmt
                  ("%Y" "2026")
                  ("%Y-%m" "2026-05")
                  ("%Y-%m-%d" "2026-05-09")
                  ("%Y/%m/%d" "2026/05/09")
                  ("%Y-%m-%d %a" "2026-05-09 Sat")
                  ("%Y-%m-%d %a %H:%M" "2026-05-09 Sat 12:34")
                  (_ (error "Unexpected format string: %s" fmt))))))
     ,@body))

(defun emacs-org-capture-test--read-file (path)
  "Return PATH contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(ert-deftest org-capture-shows-template-menu ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file path "menu.org" "* Inbox\n"
      (let ((prompt nil))
        (setq org-capture-templates
              `(("j" "Journal" entry (file+headline ,path "Inbox") "* %?")))
        (cl-letf (((symbol-function 'read-key)
                   (lambda (arg)
                     (setq prompt arg)
                     ?j)))
          (org-capture)
          (should (string-match-p "Org capture template:" prompt))
          (should (string-match-p "j Journal" prompt))
          (should (equal org-capture--buffer-name (buffer-name (current-buffer)))))))))

(ert-deftest org-capture-file+headline-inserts-at-headline-end ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-fixed-time
      (emacs-org-capture-test--with-temp-file
          path
          "headline.org"
          "* Inbox\n** Existing\n* Later\n"
        (setq org-capture-templates
              `(("t" "Task" entry (file+headline ,path "Inbox") "* New task")))
        (org-capture "t")
        (org-capture-finalize)
        (should
         (equal
          "* Inbox\n** Existing\n** New task\n* Later\n"
          (emacs-org-capture-test--read-file path)))))))

(ert-deftest org-capture-file-target-appends-top-level-entry ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file path "file-target.org"
        "* Existing\nbody\n"
      (setq org-capture-templates
            `(("f" "File" entry
               (file ,path)
               "* File task")))
      (org-capture "f")
      (org-capture-finalize)
      (should
       (equal
        "* Existing\nbody\n* File task\n"
        (emacs-org-capture-test--read-file path))))))

(ert-deftest org-capture-file-target-prepends-top-level-entry ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file path "file-target-prepend.org"
        "* Existing\nbody\n"
      (setq org-capture-templates
            `(("f" "File prepend" entry
               (file ,path)
               "* First file task"
               :prepend t)))
      (org-capture "f")
      (org-capture-finalize)
      (should
       (equal
        "* First file task\n* Existing\nbody\n"
        (emacs-org-capture-test--read-file path))))))

(ert-deftest org-capture-file+olp-creates-path-and-inserts-entry ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file path "file-olp.org"
        "* Projects\n"
      (setq org-capture-templates
            `(("o" "OLP" entry
               (file+olp ,path "Projects" "Alpha")
               "* Nested task")))
      (org-capture "o")
      (org-capture-finalize)
      (should
       (equal
        "* Projects\n** Alpha\n*** Nested task\n"
        (emacs-org-capture-test--read-file path))))))

(ert-deftest org-capture-file+olp-prepend-inserts-at-final-path-start ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file path "file-olp-prepend.org"
        "* Projects\n** Alpha\nbody\n*** Existing\n"
      (setq org-capture-templates
            `(("o" "OLP prepend" entry
               (file+olp ,path "Projects" "Alpha")
               "* First nested task"
               :prepend t)))
      (org-capture "o")
      (org-capture-finalize)
      (should
       (equal
        "* Projects\n** Alpha\n*** First nested task\nbody\n*** Existing\n"
        (emacs-org-capture-test--read-file path))))))

(ert-deftest org-capture-file+regexp-inserts-at-matching-location ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file path "file-regexp.org"
        "* Inbox\n# capture-here\n** Existing\n"
      (setq org-capture-templates
            `(("r" "Regexp" entry
               (file+regexp ,path "^# capture-here$")
               "* Regexp task")))
      (org-capture "r")
      (org-capture-finalize)
      (should
       (equal
        "* Inbox\n** Regexp task\n# capture-here\n** Existing\n"
        (emacs-org-capture-test--read-file path))))))

(ert-deftest org-capture-immediate-finish-finalizes-without-capture-buffer ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file path "immediate.org" "* Inbox\n"
      (setq org-capture-templates
            `(("i" "Immediate" entry
               (file+headline ,path "Inbox")
               "* Immediate task"
               :immediate-finish t)))
      (should (equal path (org-capture "i")))
      (should-not (get-buffer org-capture--buffer-name))
      (should
       (equal
        "* Inbox\n** Immediate task\n"
        (emacs-org-capture-test--read-file path))))))

(ert-deftest org-capture-empty-lines-properties-pad-finalized-entry ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file path "empty-lines.org" "* Inbox\n"
      (setq org-capture-templates
            `(("e" "Empty lines" entry
               (file+headline ,path "Inbox")
               "* Padded task"
               :empty-lines-before 1
               :empty-lines-after 2)))
      (org-capture "e")
      (org-capture-finalize)
      (should
       (equal
        "* Inbox\n\n** Padded task\n\n\n"
        (emacs-org-capture-test--read-file path))))))

(ert-deftest org-capture-prepend-property-inserts-at-target-start ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file path "prepend.org"
        "* Inbox\nbody\n** Existing\n* Later\n"
      (setq org-capture-templates
            `(("p" "Prepend" entry
               (file+headline ,path "Inbox")
               "* First task"
               :prepend t)))
      (org-capture "p")
      (org-capture-finalize)
      (should
       (equal
        "* Inbox\n** First task\nbody\n** Existing\n* Later\n"
        (emacs-org-capture-test--read-file path))))))

(ert-deftest org-capture-file+olp+datetree-creates-datetree ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-fixed-time
      (emacs-org-capture-test--with-temp-file path "datetree.org" ""
        (setq org-capture-templates
              `(("j" "Journal" entry (file+olp+datetree ,path) "* %?")))
        (org-capture "j")
        (insert "Daily note")
        (org-capture-finalize)
        (should
         (equal
          "* 2026\n** 2026-05\n*** 2026-05-09\n**** Daily note\n"
          (emacs-org-capture-test--read-file path)))))))

(ert-deftest org-capture-prepend-property-inserts-at-datetree-day-start ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-fixed-time
      (emacs-org-capture-test--with-temp-file path "datetree-prepend.org"
          "* 2026\n** 2026-05\n*** 2026-05-09\n**** Existing note\n"
        (setq org-capture-templates
              `(("j" "Journal" entry
                 (file+olp+datetree ,path)
                 "* First note"
                 :prepend t)))
        (org-capture "j")
        (org-capture-finalize)
        (should
         (equal
          "* 2026\n** 2026-05\n*** 2026-05-09\n**** First note\n**** Existing note\n"
          (emacs-org-capture-test--read-file path)))))))

(ert-deftest org-capture-percent-question-leaves-cursor ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-fixed-time
      (emacs-org-capture-test--with-temp-file path "cursor.org" "* Inbox\n"
        (setq org-capture-templates
              `(("c" "Cursor" entry (file+headline ,path "Inbox") "* before %? after")))
        (org-capture "c")
        (should (equal "** before  after\n" (buffer-string)))
        (should (= 11 (point)))))))

(ert-deftest org-capture-percent-T-and-custom-format-insert-timestamp ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-fixed-time
      (emacs-org-capture-test--with-temp-file path "timestamp.org" "* Inbox\n"
        (setq org-capture-templates
              `(("T" "Timed" entry
                 (file+headline ,path "Inbox")
                 "* %T custom %<%Y/%m/%d>")))
        (org-capture "T")
        (org-capture-finalize)
        (should
         (string-match-p
          (regexp-quote
           "** <2026-05-09 Sat 12:34> custom 2026/05/09\n")
          (emacs-org-capture-test--read-file path)))))))

(ert-deftest org-capture-percent-u-and-U-insert-inactive-timestamps ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-fixed-time
      (emacs-org-capture-test--with-temp-file path "inactive-timestamp.org"
          "* Inbox\n"
        (setq org-capture-templates
              `(("u" "Inactive" entry
                 (file+headline ,path "Inbox")
                 "* date %u datetime %U")))
        (org-capture "u")
        (org-capture-finalize)
        (should
         (string-match-p
          (regexp-quote
           "** date [2026-05-09 Sat] datetime [2026-05-09 Sat 12:34]\n")
          (emacs-org-capture-test--read-file path)))))))

(ert-deftest org-capture-percent-a-uses-org-store-link ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file source "source.org" "* Source\nbody\n"
      (emacs-org-capture-test--with-temp-file target "target.org" "* Inbox\n"
        (find-file source)
        (goto-char (point-min))
        (setq org-capture-templates
              `(("a" "Annotation" entry
                 (file+headline ,target "Inbox")
                 "* Captured from %a")))
        (org-capture "a")
        (org-capture-finalize)
        (should
         (string-match-p
          (regexp-quote
          (format "[[file:%s::1]]" (expand-file-name source)))
          (emacs-org-capture-test--read-file target)))))))

(ert-deftest org-capture-percent-l-and-L-use-literal-link ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file source "literal-source.org"
        "* Source\nbody\n"
      (emacs-org-capture-test--with-temp-file target "literal-target.org"
          "* Inbox\n"
        (find-file source)
        (goto-char (point-min))
        (setq org-capture-templates
              `(("l" "Literal link" entry
                 (file+headline ,target "Inbox")
                 "* Links %l raw %L")))
        (org-capture "l")
        (org-capture-finalize)
        (let ((raw-link (format "file:%s::1" (expand-file-name source))))
          (should
           (string-match-p
            (regexp-quote (format "** Links [[%s]] raw %s\n"
                                  raw-link raw-link))
            (emacs-org-capture-test--read-file target))))))))

(ert-deftest org-capture-percent-A-prompts-for-link-description ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file source "prompt-source.org"
        "* Source\nbody\n"
      (emacs-org-capture-test--with-temp-file target "prompt-target.org"
          "* Inbox\n"
        (find-file source)
        (goto-char (point-min))
        (setq org-capture-templates
              `(("A" "Prompted annotation" entry
                 (file+headline ,target "Inbox")
                 "* Prompted %A")))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (prompt &rest _args)
                     (should (equal "Link description: " prompt))
                     "Source description")))
          (org-capture "A"))
        (org-capture-finalize)
        (should
         (string-match-p
          (regexp-quote
           (format "** Prompted [[file:%s::1][Source description]]\n"
                   (expand-file-name source)))
          (emacs-org-capture-test--read-file target)))))))

(ert-deftest org-capture-percent-f-and-F-use-source-file-name ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file source "source-file.org"
        "* Source\nbody\n"
      (emacs-org-capture-test--with-temp-file target "target-file.org"
          "* Inbox\n"
        (find-file source)
        (goto-char (point-min))
        (setq org-capture-templates
              `(("f" "Source file" entry
                 (file+headline ,target "Inbox")
                 "* Source file %f full %F percent %%")))
        (org-capture "f")
        (org-capture-finalize)
        (should
         (string-match-p
          (regexp-quote
           (format "** Source file %s full %s percent %%\n"
                   (file-name-nondirectory source)
                   (expand-file-name source)))
          (emacs-org-capture-test--read-file target)))))))

(ert-deftest org-capture-percent-i-uses-active-source-region ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file source "source-region.org"
        "* Source\nalpha Selected text omega\n"
      (emacs-org-capture-test--with-temp-file target "target-region.org"
          "* Inbox\n"
        (find-file source)
        (goto-char (point-min))
        (search-forward "Selected text")
        (let ((region-end (point))
              (region-start (match-beginning 0)))
          (goto-char region-end)
          (set-mark region-start)
          (setq mark-active t))
        (setq org-capture-templates
              `(("i" "Initial" entry
                 (file+headline ,target "Inbox")
                 "* Initial %i")))
        (org-capture "i")
        (org-capture-finalize)
        (should
         (string-match-p
          (regexp-quote "** Initial Selected text\n")
          (emacs-org-capture-test--read-file target)))))))

(ert-deftest org-capture-percent-n-uses-user-full-name ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file path "user.org" "* Inbox\n"
      (let ((user-full-name "Ada Lovelace"))
        (setq org-capture-templates
              `(("n" "User" entry
                 (file+headline ,path "Inbox")
                 "* Author %n")))
        (org-capture "n")
        (org-capture-finalize)
        (should
         (string-match-p
          (regexp-quote "** Author Ada Lovelace\n")
          (emacs-org-capture-test--read-file path)))))))

(ert-deftest org-capture-percent-c-uses-current-kill ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file path "current-kill.org"
        "* Inbox\n"
      (let ((kill-ring '("Copied body" "Older body"))
            (kill-ring-yank-pointer nil))
        (setq org-capture-templates
              `(("c" "Current kill" entry
                 (file+headline ,path "Inbox")
                 "* Clipboard %c")))
        (org-capture "c")
        (org-capture-finalize)
        (should
         (string-match-p
          (regexp-quote "** Clipboard Copied body\n")
          (emacs-org-capture-test--read-file path)))))))

(ert-deftest org-capture-percent-x-uses-external-clipboard ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file path "clipboard.org"
        "* Inbox\n"
      (let ((interprogram-paste-function
             (lambda () "Clipboard body")))
        (setq org-capture-templates
              `(("x" "Clipboard" entry
                 (file+headline ,path "Inbox")
                 "* External %x")))
        (org-capture "x")
        (org-capture-finalize)
        (should
         (string-match-p
          (regexp-quote "** External Clipboard body\n")
          (emacs-org-capture-test--read-file path)))))))

(ert-deftest org-capture-percent-k-and-K-use-current-clock ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file source "clock-source.org"
        "* TODO Timed\nbody\n"
      (emacs-org-capture-test--with-temp-file target "clock-target.org"
          "* Inbox\n"
        (let ((source-buffer (find-file-noselect source)))
          (unwind-protect
              (with-current-buffer source-buffer
                (goto-char (point-min))
                (org-clock-in nil (encode-time 0 15 8 27 6 2026))
                (setq org-capture-templates
                      `(("k" "Clock" entry
                         (file+headline ,target "Inbox")
                         "* Clock %k link %K")))
                (org-capture "k")
                (org-capture-finalize)
                (should
                 (string-match-p
                  (regexp-quote
                   (format "** Clock TODO Timed link [[file:%s::1]]\n"
                           (expand-file-name source)))
                  (emacs-org-capture-test--read-file target))))
            (when org-clock-marker
              (condition-case nil
                  (org-clock-out nil t (encode-time 0 45 8 27 6 2026))
                (error
                 (setq org-clock-marker nil)
                 (setq org-clock-start-line-marker nil)
                 (setq org-clock-start-time nil))))))))))

(ert-deftest org-capture-percent-a-uses-nelisp-source-buffer ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-temp-file source "source-nelisp.org"
        "* Source\nbody\n"
      (emacs-org-capture-test--with-temp-file target "target-nelisp.org"
          "* Inbox\n"
        (let ((source-buffer (find-file-noselect source)))
          (setq org-capture-templates
                `(("a" "Annotation" entry
                   (file+headline ,target "Inbox")
                   "* Captured from %a")))
          (let ((capture-buffer
                 (if (and (fboundp 'nelisp-ec-buffer-p)
                          (nelisp-ec-buffer-p source-buffer))
                     (nelisp-ec-with-current-buffer source-buffer
                       (goto-char (point-min))
                       (org-capture "a"))
                   (with-current-buffer source-buffer
                     (goto-char (point-min))
                     (org-capture "a")))))
            (if (and (fboundp 'nelisp-ec-buffer-p)
                     (nelisp-ec-buffer-p capture-buffer))
                (nelisp-ec-with-current-buffer capture-buffer
                  (org-capture-finalize))
              (with-current-buffer capture-buffer
                (org-capture-finalize)))))
        (should
         (string-match-p
          (regexp-quote
           (format "[[file:%s::1]]" (expand-file-name source)))
          (emacs-org-capture-test--read-file target)))))))

(ert-deftest org-capture-C-c-C-k-aborts-without-modifying-target ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-fixed-time
      (emacs-org-capture-test--with-temp-file path "abort.org" "* Inbox\n"
        (let ((before (emacs-org-capture-test--read-file path)))
          (setq org-capture-templates
                `(("k" "Kill" entry (file+headline ,path "Inbox") "* %?")))
          (org-capture "k")
          (insert "discard me")
          (org-capture-kill)
          (should (equal before
                         (emacs-org-capture-test--read-file path))))))))

(provide 'emacs-org-capture-test)

;;; emacs-org-capture-test.el ends here
