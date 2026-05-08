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

(ert-deftest org-capture-percent-question-leaves-cursor ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-fixed-time
      (emacs-org-capture-test--with-temp-file path "cursor.org" "* Inbox\n"
        (setq org-capture-templates
              `(("c" "Cursor" entry (file+headline ,path "Inbox") "* before %? after")))
        (org-capture "c")
        (should (equal "** before  after\n" (buffer-string)))
        (should (= 11 (point)))))))

(ert-deftest org-capture-percent-T-inserts-timestamp ()
  (emacs-org-capture-test--with-fresh-world
    (emacs-org-capture-test--with-fixed-time
      (emacs-org-capture-test--with-temp-file path "timestamp.org" "* Inbox\n"
        (setq org-capture-templates
              `(("T" "Timed" entry (file+headline ,path "Inbox") "* %T")))
        (org-capture "T")
        (org-capture-finalize)
        (should
         (string-match-p
          (regexp-quote "** <2026-05-09 Sat 12:34>\n")
          (emacs-org-capture-test--read-file path)))))))

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
