;;; nelisp-socket-contract-test.el --- socket contract smoke tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (file-name-directory this))
       (root (file-name-directory (directory-file-name test-dir))))
  (dolist (dir '("lisp" "src" "scripts"))
    (add-to-list 'load-path (expand-file-name dir root))))

(require 'nelisp-standalone-build)

(defconst nelisp-socket-contract-test--calls
  '("nelisp-socket-listen" "nelisp-socket-accept"
    "nelisp-socket-connect" "nelisp-socket-send" "nelisp-socket-recv"
    "nelisp-socket-close" "nelisp-socket-poll" "nelisp-socket-connect-error"))

(defun nelisp-socket-contract-test--output (mode)
  "Return the output that a portable call-process stub emits for MODE."
  (let ((lines (mapcar (lambda (name)
                         (format "SOCKET-UNSUPPORTED=%s\n" name))
                       nelisp-socket-contract-test--calls)))
    (pcase mode
      ('missing-one (setq lines (cdr lines)))
      ('missing-done nil)
      ('wrong-error (push "WRONG-ERROR=(wrong-error)\n" lines))
      ('unexpected-success (push "UNEXPECTED-SUCCESS\n" lines)))
    (apply #'concat
           (append lines
                   (unless (eq mode 'missing-done)
                     '("SOCKET-UNSUPPORTED-DONE\n"))))))

(ert-deftest nelisp-socket-contract-smoke-portable-success ()
  "The contract accepts all eight markers and the completion marker."
  (let ((nelisp-standalone--reader-out "portable-socket-stub"))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _infile destination _display &rest _args)
                 (with-temp-file (cadr destination)
                   (insert ""))
                 (with-current-buffer (if (eq (car destination) t)
                                          (current-buffer)
                                        (car destination))
                   (insert (nelisp-socket-contract-test--output nil)))
                 0)))
      (should-not
       (condition-case err
           (progn
             (nelisp-standalone--reader-unsupported-socket-smoke)
             nil)
         (error err))))))

(dolist (mode '(missing-one missing-done wrong-error unexpected-success
                nonzero-exit stderr))
  (eval
   `(ert-deftest ,(intern (format "nelisp-socket-contract-smoke-rejects-%s" mode)) ()
      ,(format "The socket contract rejects the %s stub response." mode)
      (let ((nelisp-standalone--reader-out "portable-socket-stub"))
        (cl-letf (((symbol-function 'call-process)
                   (lambda (_program _infile destination _display &rest _args)
                     (with-temp-file (cadr destination)
                       (insert ""))
                     (when (eq ',mode 'stderr)
                       (with-temp-file (cadr destination)
                         (insert "unexpected stderr\\n")))
                     (when (and (memq ',mode '(nonzero-exit stderr))
                                (eq (car destination) t))
                       (insert (nelisp-socket-contract-test--output nil)))
                     (when (and (not (memq ',mode '(nonzero-exit stderr)))
                                (eq (car destination) t))
                       (insert (nelisp-socket-contract-test--output ',mode)))
                     (if (eq ',mode 'nonzero-exit) 7 0))))
          (should-error
           (nelisp-standalone--reader-unsupported-socket-smoke)))))))

(provide 'nelisp-socket-contract-test)

;;; nelisp-socket-contract-test.el ends here
