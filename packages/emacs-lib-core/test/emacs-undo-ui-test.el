;;; emacs-undo-ui-test.el --- ERT for undo UI layer  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-edit-builtins)
(require 'emacs-undo-ui)

(defun emacs-undo-ui-test--insert (text)
  "Insert TEXT into the current nelisp buffer and record undo."
  (let ((beg (nelisp-ec-point)))
    (nelisp-ec-insert text)
    (emacs-undo-record-insert beg (nelisp-ec-point))))

(defmacro emacs-undo-ui-test--with-fresh-buffer (text &rest body)
  "Run BODY in a fresh nelisp buffer seeded with TEXT."
  (declare (indent 1) (debug (form body)))
  (let ((buf (make-symbol "buf")))
    `(let ((nelisp-ec--buffers nil)
           (nelisp-ec--current-buffer nil)
           (nelisp-ec--match-data nil)
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           (emacs-undo-ui--redos nil))
       (emacs-undo-reset)
       (let ((,buf (nelisp-ec-generate-new-buffer "undo-ui")))
         (unwind-protect
             (nelisp-ec-with-current-buffer ,buf
               (nelisp-ec-insert ,text)
               (nelisp-ec-goto-char (nelisp-ec-point-min))
               (emacs-undo-set-buffer-undo-list nil)
               ,@body)
           (emacs-undo-reset)
           (nelisp-ec-kill-buffer ,buf))))))

(ert-deftest undo-reverts-last-edit ()
  (emacs-undo-ui-test--with-fresh-buffer ""
    (emacs-undo-ui-test--insert "a")
    (undo-boundary)
    (should (equal "a" (nelisp-ec-buffer-string)))
    (undo)
    (should (equal "" (nelisp-ec-buffer-string)))))

(ert-deftest undo-multiple-times ()
  (emacs-undo-ui-test--with-fresh-buffer ""
    (emacs-undo-ui-test--insert "a")
    (undo-boundary)
    (emacs-undo-ui-test--insert "b")
    (undo-boundary)
    (emacs-undo-ui-test--insert "c")
    (undo-boundary)
    (undo 2)
    (should (equal "a" (nelisp-ec-buffer-string)))))

(ert-deftest undo-redo-restores-undone-edit ()
  (emacs-undo-ui-test--with-fresh-buffer ""
    (emacs-undo-ui-test--insert "a")
    (undo-boundary)
    (emacs-undo-ui-test--insert "b")
    (undo-boundary)
    (undo)
    (should (equal "a" (nelisp-ec-buffer-string)))
    (undo-redo)
    (should (equal "ab" (nelisp-ec-buffer-string)))))

(ert-deftest undo-on-empty-history-errors ()
  (emacs-undo-ui-test--with-fresh-buffer ""
    (should-error (undo) :type 'user-error)))

(ert-deftest undo-ui-installs-global-bindings ()
  (should (eq #'undo (key-binding (kbd "C-/"))))
  (should (eq #'undo (key-binding (kbd "C-_"))))
  (should (eq #'undo-redo (key-binding (kbd "C-x u")))))

(provide 'emacs-undo-ui-test)

;;; emacs-undo-ui-test.el ends here
