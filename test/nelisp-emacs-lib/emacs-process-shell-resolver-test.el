;;; emacs-process-shell-resolver-test.el --- ERT for shell resolver  -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused coverage for `emacs-process-resolve-shell-file-name'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-process)

(defmacro emacs-process-shell-resolver-test--with-fresh-cache (&rest body)
  "Run BODY with a cleared shell resolver cache."
  (declare (indent 0) (debug (body)))
  `(let ((emacs-process--resolved-shell-file-name nil))
     ,@body))

(ert-deftest emacs-process-shell-resolver-test/prefers-bin-sh-when-executable ()
  (emacs-process-shell-resolver-test--with-fresh-cache
    (cl-letf (((symbol-function 'file-executable-p)
               (lambda (path)
                 (equal path "/bin/sh")))
              ((symbol-function 'executable-find)
               (lambda (_name)
                 "c:/Program Files/Git/usr/bin/sh.exe")))
      (should (equal "/bin/sh"
                     (emacs-process-resolve-shell-file-name))))))

(ert-deftest emacs-process-shell-resolver-test/falls-back-to-executable-find ()
  (emacs-process-shell-resolver-test--with-fresh-cache
    (let ((shell-file-name "C:/fallback/bash.exe"))
      (cl-letf (((symbol-function 'file-executable-p)
                 (lambda (path)
                   (equal path "c:/Program Files/Git/usr/bin/sh.exe")))
                ((symbol-function 'executable-find)
                 (lambda (_name)
                   "c:/Program Files/Git/usr/bin/sh.exe")))
        (should (equal "c:/Program Files/Git/usr/bin/sh.exe"
                       (emacs-process-resolve-shell-file-name)))))))

(ert-deftest emacs-process-shell-resolver-test/falls-back-to-shell-file-name-then-literal ()
  (emacs-process-shell-resolver-test--with-fresh-cache
    (let ((shell-file-name "C:/Program Files/Git/bin/bash.exe"))
      (cl-letf (((symbol-function 'file-executable-p)
                 (lambda (path)
                   (equal path shell-file-name)))
                ((symbol-function 'executable-find)
                 (lambda (_name)
                   nil)))
        (should (equal shell-file-name
                       (emacs-process-resolve-shell-file-name)))))
    (emacs-process-clear-shell-file-name-cache)
    (let ((shell-file-name "C:/missing/bash.exe"))
      (cl-letf (((symbol-function 'file-executable-p)
                 (lambda (_path)
                   nil))
                ((symbol-function 'executable-find)
                 (lambda (_name)
                   nil)))
        (should (equal "/bin/sh"
                       (emacs-process-resolve-shell-file-name)))))))

(ert-deftest emacs-process-shell-resolver-test/result-is-non-empty-string ()
  (emacs-process-shell-resolver-test--with-fresh-cache
    (cl-letf (((symbol-function 'file-executable-p)
               (lambda (_path)
                 nil))
              ((symbol-function 'executable-find)
               (lambda (_name)
                 nil)))
      (let ((resolved (emacs-process-resolve-shell-file-name)))
        (should (stringp resolved))
        (should (> (length resolved) 0))))))

(ert-deftest emacs-process-shell-resolver-test/cache-clear-forces-reresolution ()
  (emacs-process-shell-resolver-test--with-fresh-cache
    (let ((count 0))
      (cl-letf (((symbol-function 'file-executable-p)
                 (lambda (path)
                   (setq count (1+ count))
                   (equal path "/bin/sh")))
                ((symbol-function 'executable-find)
                 (lambda (_name)
                   (error "cache should prevent executable-find"))))
        (should (equal "/bin/sh"
                       (emacs-process-resolve-shell-file-name)))
        (should (equal "/bin/sh"
                       (emacs-process-resolve-shell-file-name)))
        (should (= count 1))
        (emacs-process-clear-shell-file-name-cache)
        (should (equal "/bin/sh"
                       (emacs-process-resolve-shell-file-name)))
        (should (= count 2))))))

(provide 'emacs-process-shell-resolver-test)

;;; emacs-process-shell-resolver-test.el ends here

(ert-deftest emacs-process-shell-resolver-test/standalone-run-resolves-program-once ()
  "A standalone call-process walks the program search path only once."
  (let ((lookups 0) (started nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (setq lookups (1+ lookups))
                 (and (equal name "git") "/usr/bin/git")))
              ((symbol-function 'file-exists-p) (lambda (_f) t))
              ((symbol-function 'emacs-process--call-process-cwd) (lambda () nil))
              ((symbol-function 'nelisp-process-start)
               (lambda (&rest command) (setq started command) 'proc))
              ((symbol-function 'nelisp-process-wait) (lambda (_p) 0))
              ((symbol-function 'nelisp-process-read-output) (lambda (_p _n) nil)))
      (should (equal (emacs-process--standalone-run "git" nil t '("status"))
                     '(0 . "")))
      (should (= lookups 1))
      (should (equal (last started 2) '("/usr/bin/git" "status"))))))
