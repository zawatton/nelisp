;;; emacs-fileio-test.el --- ERT for emacs-fileio  -*- lexical-binding: t; -*-

;;; Commentary:

;; M1 interactive file I/O tests for the higher-level layer in
;; `emacs-fileio.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-fileio)

(defvar emacs-fileio-test--tmp-counter 0)

(defun emacs-fileio-test--tmp-path (suffix)
  "Return a unique tmp path ending with SUFFIX."
  (setq emacs-fileio-test--tmp-counter
        (1+ emacs-fileio-test--tmp-counter))
  (format "/tmp/emacs-fileio-test-%d-%d-%s"
          (emacs-pid)
          emacs-fileio-test--tmp-counter
          suffix))

(defmacro emacs-fileio-test--with-fresh-world (&rest body)
  "Run BODY with clean buffer/fileio/mode state."
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
         (mode-name "Fundamental"))
     (unwind-protect
         (progn
           (when (fboundp 'emacs-mode-reset)
             (emacs-mode-reset))
           ,@body)
       (when (fboundp 'emacs-mode-reset)
         (emacs-mode-reset)))))

(defmacro emacs-fileio-test--with-temp-file (var content &rest body)
  "Bind VAR to a temp file seeded with CONTENT, then run BODY."
  (declare (indent 2) (debug (symbolp form body)))
  `(let ((,var (emacs-fileio-test--tmp-path "tmp.txt")))
     (unwind-protect
         (progn
           (with-temp-file ,var
             (insert ,content))
           ,@body)
       (when (file-exists-p ,var)
         (delete-file ,var)))))

(ert-deftest find-file-creates-buffer-and-loads-content ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "alpha\nbeta\n"
      (let ((buf (find-file path)))
        (should (eq buf (current-buffer)))
        (should (equal "alpha\nbeta\n" (with-current-buffer buf (buffer-string))))))))

(ert-deftest find-file-existing-buffer-switches-to-it ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "same"
      (let* ((other (generate-new-buffer "other"))
             (first (find-file path)))
        (set-buffer other)
        (should (eq first (find-file path)))
        (should (eq first (current-buffer)))))))

(ert-deftest find-file-sets-buffer-file-name ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "x"
      (let ((buf (find-file path)))
        (should (equal (expand-file-name path)
                       (buffer-file-name buf)))
        (should (equal (file-name-as-directory
                        (file-name-directory (expand-file-name path)))
                       default-directory))))))

(ert-deftest find-file-dispatches-major-mode-via-auto-mode-alist ()
  (emacs-fileio-test--with-fresh-world
    (let ((calls nil))
      (cl-letf (((symbol-function 'org-mode)
                 (lambda ()
                   (interactive)
                   (setq major-mode 'org-mode
                         mode-name "Org")
                   (push 'org-mode calls)
                   nil)))
        (let ((path (emacs-fileio-test--tmp-path "notes.org")))
          (unwind-protect
              (progn
                (with-temp-file path
                  (insert "* heading\n"))
                (find-file path)
                (should (eq 'org-mode major-mode))
                (should (equal '(org-mode) calls)))
            (when (file-exists-p path)
              (delete-file path))))))))

(ert-deftest find-file-noselect-does-not-switch-to-buffer ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "stay"
      (let ((current (generate-new-buffer "current")))
        (set-buffer current)
        (let ((buf (find-file-noselect path)))
          (should (eq current (current-buffer)))
          (should (not (eq current buf)))
          (should (equal "stay" (with-current-buffer buf (buffer-string)))))))))

(ert-deftest save-buffer-writes-back-to-disk ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "old"
      (let ((buf (find-file path)))
        (with-current-buffer buf
          (erase-buffer)
          (insert "new text")
          (should (buffer-modified-p))
          (save-buffer))
        (should (equal "new text"
                       (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string))))))))

(ert-deftest save-buffer-respects-utf-8 ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "before"
      (let ((buf (find-file path))
            (payload "日本語 café\n"))
        (with-current-buffer buf
          (erase-buffer)
          (insert payload)
          (save-buffer))
        (should (equal payload
                       (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string))))))))

(ert-deftest save-buffer-no-op-when-not-modified ()
  (emacs-fileio-test--with-fresh-world
    (emacs-fileio-test--with-temp-file path "steady"
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (find-file path)
          (should-not (buffer-modified-p))
          (should-not (save-buffer))
          (should (equal '("(No changes need to be saved)") messages)))))))

(ert-deftest write-file-changes-buffer-file-name-and-saves ()
  (emacs-fileio-test--with-fresh-world
    (let ((src (emacs-fileio-test--tmp-path "src.txt"))
          (dst (emacs-fileio-test--tmp-path "dst.org")))
      (unwind-protect
          (progn
            (with-temp-file src
              (insert "source"))
            (let ((buf (find-file src)))
              (with-current-buffer buf
                (erase-buffer)
                (insert "* moved\n")
                (write-file dst)
                (should (equal (expand-file-name dst) (buffer-file-name)))
                (should (eq 'fundamental-mode major-mode))
                (should
                 (equal "* moved\n"
                        (with-temp-buffer
                          (insert-file-contents dst)
                          (buffer-string)))))))
        (when (file-exists-p src)
          (delete-file src))
        (when (file-exists-p dst)
          (delete-file dst))))))

(ert-deftest emacs-fileio-global-key-bindings-installed ()
  (let ((map (current-global-map)))
    (should (eq #'find-file (lookup-key map (kbd "C-x C-f"))))
    (should (eq #'save-buffer (lookup-key map (kbd "C-x C-s"))))
    (should (eq #'write-file (lookup-key map (kbd "C-x C-w"))))))

(provide 'emacs-fileio-test)

;;; emacs-fileio-test.el ends here
