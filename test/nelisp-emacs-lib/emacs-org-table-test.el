;;; emacs-org-table-test.el --- ERT for emacs-org-table  -*- lexical-binding: t; -*-

;;; Commentary:

;; M3.3 table tests for `emacs-org-table.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-fileio)
(require 'emacs-org-outline)
(require 'emacs-org-table)

(defmacro emacs-org-table-test--with-fresh-world (&rest body)
  "Run BODY with clean mode, file, and buffer state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil)
         (emacs-fileio--buffer-files nil)
         (emacs-fileio--buffer-default-directories nil)
         (emacs-fileio--buffer-major-modes nil)
         (emacs-fileio--buffer-mode-names nil)
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

(defmacro emacs-org-table-test--with-org-buffer (content &rest body)
  "Create a fresh Org buffer seeded with CONTENT, then run BODY."
  (declare (indent 1) (debug (form body)))
  `(emacs-org-table-test--with-fresh-world
     (let ((buf (generate-new-buffer "*org-table-test*")))
       (unwind-protect
           (with-current-buffer buf
             (insert ,content)
             (goto-char (point-min))
             (org-mode)
             ,@body)
         (when (buffer-live-p buf)
           (kill-buffer buf))))))

(defun emacs-org-table-test--goto (needle)
  "Move point to the first occurrence of NEEDLE."
  (goto-char (point-min))
  (search-forward needle nil t)
  (goto-char (match-beginning 0)))

(ert-deftest org-table-p-detects-leading-whitespace ()
  (emacs-org-table-test--with-org-buffer
      "  | a | b |\nplain\n"
    (should (org-table-p))
    (forward-line 1)
    (should-not (org-table-p))))

(ert-deftest org-table-align-pads-cells-to-column-width ()
  (emacs-org-table-test--with-org-buffer
      "| foo | x |\n| longer | yz |\n"
    (org-table-align)
    (should (equal "| foo    | x  |\n| longer | yz |\n"
                   (buffer-string)))))

(ert-deftest org-table-next-field-moves-to-next-cell ()
  (emacs-org-table-test--with-org-buffer
      "| foo | bar |\n| baz | qux |\n"
    (emacs-org-table-test--goto "foo")
    (org-table-next-field)
    (should (looking-at "bar"))
    (should (equal "| foo | bar |\n| baz | qux |\n"
                   (buffer-string)))))

(ert-deftest org-table-next-field-at-end-creates-row ()
  (emacs-org-table-test--with-org-buffer
      "| a | b |\n"
    (emacs-org-table-test--goto "b")
    (org-table-next-field)
    (should (looking-at " "))
    (should (equal "| a | b |\n|   |   |\n"
                   (buffer-string)))))

(ert-deftest org-table-previous-field-moves-back ()
  (emacs-org-table-test--with-org-buffer
      "| foo | bar |\n| baz | qux |\n"
    (emacs-org-table-test--goto "baz")
    (org-table-previous-field)
    (should (looking-at "bar"))))

(ert-deftest org-table-insert-column-shifts-cells ()
  (emacs-org-table-test--with-org-buffer
      "| a | b |\n| c | d |\n"
    (emacs-org-table-test--goto "b")
    (org-table-insert-column)
    (should (equal "| a |  | b |\n| c |  | d |\n"
                   (buffer-string)))))

(ert-deftest org-table-delete-row-removes-line ()
  (emacs-org-table-test--with-org-buffer
      "| a |\n| b |\n| c |\n"
    (emacs-org-table-test--goto "b")
    (org-table-delete-row)
    (should (equal "| a |\n| c |\n"
                   (buffer-string)))))

(ert-deftest org-table-delete-column-removes-current-field ()
  (emacs-org-table-test--with-org-buffer
      "| a | b | c |\n| d | e | f |\n"
    (emacs-org-table-test--goto "b")
    (org-table-delete-column)
    (should (equal "| a | c |\n| d | f |\n"
                   (buffer-string)))))

(ert-deftest org-tab-context-falls-through-to-org-cycle ()
  (emacs-org-table-test--with-org-buffer
      "* Heading\nbody\n"
    (let ((called nil))
      (cl-letf (((symbol-function 'org-cycle)
                 (lambda ()
                   (interactive)
                   (setq called 'org-cycle))))
        (goto-char (point-min))
        (org-tab-context)
        (should (eq called 'org-cycle))))))

(ert-deftest org-mode-map-tab-bindings-use-table-context-dispatch ()
  (should (eq (lookup-key org-mode-map (kbd "TAB")) #'org-tab-context))
  (should (eq (lookup-key org-mode-map (kbd "S-TAB")) #'org-shifttab-context)))

(provide 'emacs-org-table-test)

;;; emacs-org-table-test.el ends here
