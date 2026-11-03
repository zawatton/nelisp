;;; emacs-outline-test.el --- ERT for generic outline substrate  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-org-outline)

(defmacro emacs-outline-test--with-buffer (content &rest body)
  "Create a plain buffer with CONTENT and run BODY.
This intentionally does not enable `org-mode'; it exercises the generic
`outline-regexp' substrate."
  (declare (indent 1) (debug (form body)))
  `(let ((buf (generate-new-buffer "*emacs-outline-test*")))
     (unwind-protect
         (with-current-buffer buf
           (insert ,content)
           (goto-char (point-min))
           (setq-local outline-regexp "^\\(\\*+\\) ")
           ,@body)
       (when (buffer-live-p buf)
         (kill-buffer buf)))))

(defun emacs-outline-test--line-start (needle)
  "Return the line-start position of the first line containing NEEDLE."
  (save-excursion
    (goto-char (point-min))
    (search-forward needle nil t)
    (line-beginning-position)))

(ert-deftest emacs-outline-heading-detection-uses-outline-regexp ()
  (emacs-outline-test--with-buffer
      "intro\n* A\n** A.1\n"
    (should-not (outline-on-heading-p))
    (goto-char (emacs-outline-test--line-start "* A"))
    (should (outline-on-heading-p))
    (should (= 1 (outline-level)))
    (goto-char (emacs-outline-test--line-start "** A.1"))
    (should (= 2 (outline-level)))))

(ert-deftest emacs-outline-next-and-previous-heading-work ()
  (emacs-outline-test--with-buffer
      "* A\nbody\n** A.1\n** A.2\n* B\n"
    (goto-char (point-min))
    (should (= (emacs-outline-test--line-start "** A.1")
               (outline-next-heading)))
    (should (= (emacs-outline-test--line-start "** A.2")
               (outline-next-heading)))
    (should (= (emacs-outline-test--line-start "** A.1")
               (outline-previous-heading)))
    (should (= (emacs-outline-test--line-start "* A")
               (outline-previous-heading)))))

(ert-deftest emacs-outline-subtree-end-finds-next-same-or-higher-heading ()
  (emacs-outline-test--with-buffer
      "* A\nbody\n** A.1\nchild\n** A.2\nchild\n* B\n"
    (goto-char (point-min))
    (outline-end-of-subtree)
    (should (= (point) (emacs-outline-test--line-start "* B")))))

(ert-deftest emacs-outline-up-heading-finds-parent ()
  (emacs-outline-test--with-buffer
      "* A\n** A.1\nbody\n** A.2\n*** A.2.a\n* B\n"
    (goto-char (emacs-outline-test--line-start "*** A.2.a"))
    (outline-up-heading)
    (should (= (point) (emacs-outline-test--line-start "** A.2")))
    (outline-up-heading)
    (should (= (point) (emacs-outline-test--line-start "* A")))))

(ert-deftest emacs-outline-minimal-milestone-shape-is-identifiable ()
  (emacs-outline-test--with-buffer
      "* A\n** A.1\n** A.2\n* B\n"
    (let ((level-1 0)
          (children-of-a 0)
          a-end)
      (goto-char (point-min))
      (while (outline-on-heading-p)
        (cond
         ((= (outline-level) 1)
          (setq level-1 (1+ level-1)))
         ((= (outline-level) 2)
          (setq children-of-a (1+ children-of-a))))
        (unless (outline-next-heading)
          (goto-char (point-max))))
      (goto-char (point-min))
      (outline-end-of-subtree)
      (setq a-end (point))
      (should (= 2 level-1))
      (should (= 2 children-of-a))
      (should (= a-end (emacs-outline-test--line-start "* B"))))))

(provide 'emacs-outline-test)

;;; emacs-outline-test.el ends here
