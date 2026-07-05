;;; simple-test.el --- ERT for lightweight simple.el shim  -*- lexical-binding: t; -*-

;;; Commentary:

;; Host Emacs preloads much of GNU simple.el, so these tests temporarily
;; unbind the shim-owned commands before loading src/simple.el.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst simple-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name default-directory))))
  "Repository root used to load the shim source directly.")

(defmacro simple-test--with-shim-functions (&rest body)
  "Load the lightweight simple shim with its command gates open."
  (declare (indent 0) (debug (body)))
  `(let ((open-line-original (and (fboundp 'open-line)
                                  (symbol-function 'open-line)))
         (quoted-insert-original (and (fboundp 'quoted-insert)
                                      (symbol-function 'quoted-insert)))
         (indent-for-tab-command-original
          (and (fboundp 'indent-for-tab-command)
               (symbol-function 'indent-for-tab-command)))
         (delete-and-extract-region-original
          (and (fboundp 'delete-and-extract-region)
               (symbol-function 'delete-and-extract-region)))
         (buffer-substring--filter-original
          (and (fboundp 'buffer-substring--filter)
               (symbol-function 'buffer-substring--filter)))
         (filter-buffer-substring-original
          (and (fboundp 'filter-buffer-substring)
               (symbol-function 'filter-buffer-substring)))
         (filter-buffer-substring-functions-bound
          (boundp 'filter-buffer-substring-functions))
         (filter-buffer-substring-functions-original
          (and (boundp 'filter-buffer-substring-functions)
               filter-buffer-substring-functions))
         (filter-buffer-substring-function-bound
          (boundp 'filter-buffer-substring-function))
         (filter-buffer-substring-function-original
          (and (boundp 'filter-buffer-substring-function)
               filter-buffer-substring-function)))
     (unwind-protect
         (progn
           (fmakunbound 'open-line)
           (fmakunbound 'quoted-insert)
           (fmakunbound 'indent-for-tab-command)
           (fmakunbound 'delete-and-extract-region)
           (fmakunbound 'buffer-substring--filter)
           (fmakunbound 'filter-buffer-substring)
           (makunbound 'filter-buffer-substring-functions)
           (makunbound 'filter-buffer-substring-function)
           (load (expand-file-name "src/simple.el" simple-test--root)
                 nil t)
           ,@body)
       (if open-line-original
           (fset 'open-line open-line-original)
         (fmakunbound 'open-line))
       (if quoted-insert-original
           (fset 'quoted-insert quoted-insert-original)
         (fmakunbound 'quoted-insert))
       (if indent-for-tab-command-original
           (fset 'indent-for-tab-command indent-for-tab-command-original)
         (fmakunbound 'indent-for-tab-command))
       (if delete-and-extract-region-original
           (fset 'delete-and-extract-region delete-and-extract-region-original)
         (fmakunbound 'delete-and-extract-region))
       (if buffer-substring--filter-original
           (fset 'buffer-substring--filter buffer-substring--filter-original)
         (fmakunbound 'buffer-substring--filter))
       (if filter-buffer-substring-original
           (fset 'filter-buffer-substring filter-buffer-substring-original)
         (fmakunbound 'filter-buffer-substring))
       (if filter-buffer-substring-functions-bound
           (setq filter-buffer-substring-functions
                 filter-buffer-substring-functions-original)
         (makunbound 'filter-buffer-substring-functions))
       (if filter-buffer-substring-function-bound
           (setq filter-buffer-substring-function
                 filter-buffer-substring-function-original)
         (makunbound 'filter-buffer-substring-function)))))

(ert-deftest simple-test/provides-daily-driver-surface ()
  (simple-test--with-shim-functions
    (should (featurep 'simple))
    (dolist (symbol '(open-line quoted-insert indent-for-tab-command
                                filter-buffer-substring
                                buffer-substring--filter
                                delete-and-extract-region
                                beginning-of-line end-of-line kill-line
                                newline))
      (should (fboundp symbol)))))

(ert-deftest simple-test/filter-buffer-substring-defaults ()
  (simple-test--with-shim-functions
    (should (eq filter-buffer-substring-function
                'buffer-substring--filter))
    (should (null filter-buffer-substring-functions))
    (with-temp-buffer
      (insert "abcdef")
      (should (equal "bcd" (filter-buffer-substring 2 5)))
      (should (equal "abcdef" (buffer-string)))
      (should (equal "bc" (filter-buffer-substring 2 4 t)))
      (should (equal "adef" (buffer-string))))))

(ert-deftest simple-test/open-line-leaves-point-before-newlines ()
  (simple-test--with-shim-functions
    (with-temp-buffer
      (insert "ab")
      (goto-char 2)
      (open-line 2)
      (should (equal "a\n\nb" (buffer-string)))
      (should (= 2 (point))))))

(ert-deftest simple-test/quoted-insert-reads-and-inserts-character ()
  (simple-test--with-shim-functions
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-char)
                 (lambda (&rest _args) ?x)))
        (quoted-insert 3))
      (should (equal "xxx" (buffer-string))))))

(ert-deftest simple-test/indent-for-tab-command-uses-line-function ()
  (simple-test--with-shim-functions
    (let ((called nil)
          (old-indent-line-function indent-line-function))
      (unwind-protect
          (progn
            (setq indent-line-function (lambda () (setq called t)))
            (indent-for-tab-command)
            (should called))
        (setq indent-line-function old-indent-line-function)))))

(ert-deftest simple-test/indent-for-tab-command-falls-back-to-tab ()
  (simple-test--with-shim-functions
    (with-temp-buffer
      (let ((indent-line-function nil))
        (indent-for-tab-command))
      (should (equal "\t" (buffer-string))))))

(provide 'simple-test)

;;; simple-test.el ends here
