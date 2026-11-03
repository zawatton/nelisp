;;; emacs-file-name-handler-test.el --- ERT for emacs-file-name-handler  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 37 (Tramp ssh-only lane, task #16) regression coverage for the
;; `find-file-name-handler' dispatch substrate.  Under host Emacs,
;; `find-file-name-handler' is the real C primitive and every
;; `emacs-fnh-*' alias install in the module is `unless (fboundp ...)'
;; gated, so these tests exercise the prefixed `emacs-fnh-*' helpers
;; directly (the functions that matter on the standalone reader) rather
;; than relying on the module having replaced anything under host Emacs.

;;; Code:

(require 'ert)
(require 'emacs-file-name-handler)

(ert-deftest emacs-file-name-handler-test/require-loads-cleanly ()
  (should (featurep 'emacs-file-name-handler))
  (should (fboundp 'emacs-fnh-find-file-name-handler))
  (should (fboundp 'emacs-fnh-dispatch))
  (should (fboundp 'emacs-fnh-wrap)))

(ert-deftest emacs-file-name-handler-test/no-match-returns-nil ()
  (let ((file-name-handler-alist nil))
    (should (null (emacs-fnh-find-file-name-handler "/tmp/plain" 'file-exists-p))))
  (let ((file-name-handler-alist (list (cons "\\`/ssh:" #'ignore))))
    (should (null (emacs-fnh-find-file-name-handler "/tmp/plain" 'file-exists-p)))))

(ert-deftest emacs-file-name-handler-test/matching-handler-found ()
  (let* ((marker (lambda (_op &rest _args) 'handled))
         (file-name-handler-alist (list (cons "\\`/ssh:" marker))))
    (should (eq (emacs-fnh-find-file-name-handler "/ssh:host:/tmp/x" 'file-exists-p)
                marker))
    (should (null (emacs-fnh-find-file-name-handler "/tmp/x" 'file-exists-p)))))

(ert-deftest emacs-file-name-handler-test/non-string-filename-is-safe ()
  (let ((file-name-handler-alist (list (cons "\\`/ssh:" #'ignore))))
    (should (null (emacs-fnh-find-file-name-handler nil 'file-exists-p)))
    (should (null (emacs-fnh-find-file-name-handler 42 'file-exists-p)))))

(ert-deftest emacs-file-name-handler-test/inhibited-operation-is-skipped ()
  (let* ((marker (lambda (_op &rest _args) 'handled))
         (file-name-handler-alist (list (cons "\\`/ssh:" marker)))
         (inhibit-file-name-operation 'file-exists-p)
         (inhibit-file-name-handlers (list marker)))
    (should (null (emacs-fnh-find-file-name-handler "/ssh:host:/tmp/x" 'file-exists-p)))
    ;; A different operation is not inhibited.
    (should (eq (emacs-fnh-find-file-name-handler "/ssh:host:/tmp/x" 'insert-file-contents)
                marker))))

(ert-deftest emacs-file-name-handler-test/dispatch-routes-to-handler ()
  (let* ((calls nil)
         (handler (lambda (op &rest args)
                    (push (cons op args) calls)
                    'from-handler))
         (file-name-handler-alist (list (cons "\\`/ssh:" handler))))
    (should (eq (emacs-fnh-dispatch 'file-exists-p #'ignore "/ssh:host:/tmp/x")
                'from-handler))
    (should (equal calls (list (list 'file-exists-p "/ssh:host:/tmp/x"))))))

(ert-deftest emacs-file-name-handler-test/dispatch-falls-through-to-local ()
  (let ((file-name-handler-alist nil))
    (should (equal (emacs-fnh-dispatch 'file-exists-p (lambda (f) (list :local f))
                                       "/tmp/plain")
                   (list :local "/tmp/plain")))))

(ert-deftest emacs-file-name-handler-test/wrap-produces-dispatching-closure ()
  (let* ((handler (lambda (_op file) (list :handled file)))
         (file-name-handler-alist (list (cons "\\`/ssh:" handler)))
         (fn (emacs-fnh-wrap 'file-exists-p (lambda (f) (list :local f)))))
    (should (equal (funcall fn "/ssh:host:/tmp/x") (list :handled "/ssh:host:/tmp/x")))
    (should (equal (funcall fn "/tmp/plain") (list :local "/tmp/plain")))))

(ert-deftest emacs-file-name-handler-test/remote-fallbacks-default-local ()
  (let ((file-name-handler-alist nil))
    (should (null (emacs-fnh-file-remote-p "/tmp/plain")))
    (should (equal (emacs-fnh-file-local-name "/tmp/plain") "/tmp/plain"))))

(ert-deftest emacs-file-name-handler-test/remote-fallbacks-honour-handler ()
  (let* ((handler (lambda (op file &rest _)
                    (cond
                     ((eq op 'file-remote-p) "/ssh:host:")
                     ((eq op 'file-local-name) (substring file 10))
                     (t nil))))
         (file-name-handler-alist (list (cons "\\`/ssh:" handler))))
    (should (equal (emacs-fnh-file-remote-p "/ssh:host:/tmp/x") "/ssh:host:"))
    (should (equal (emacs-fnh-file-local-name "/ssh:host:/tmp/x") "/tmp/x"))))

;;; emacs-file-name-handler-test.el ends here
