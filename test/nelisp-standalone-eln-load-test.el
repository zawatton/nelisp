;;; nelisp-standalone-eln-load-test.el --- generated .eln load bridge -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)

(defconst nelisp-standalone-eln-load-test--root
  (expand-file-name ".." (file-name-directory load-file-name)))

(defun nelisp-standalone-eln-load-test--install-generated-wrapper ()
  "Evaluate only the standalone after-load wrapper source generator."
  (let ((path (expand-file-name "scripts/nelisp-standalone-build.el"
                                nelisp-standalone-eln-load-test--root))
        form)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (unless (search-forward
               "(defun nelisp-standalone--after-load-runtime-src" nil t)
        (error "after-load source generator not found"))
      (goto-char (match-beginning 0))
      (setq form (read (current-buffer))))
    (eval form)
    (with-temp-buffer
      (insert (nelisp-standalone--after-load-runtime-src))
      (goto-char (point-min))
      (condition-case nil
          (while t (eval (read (current-buffer))))
        (end-of-file nil)))))

(defun nelisp-standalone-eln-load-test--restore-function (symbol old)
  (if old
      (fset symbol old)
    (fmakunbound symbol)))

(ert-deftest nelisp-standalone-eln-load-routes-and-binds-load-file-name ()
  (let* ((wrapper (symbol-function 'load))
         (provide (symbol-function 'provide))
         (require-fn (symbol-function 'require))
         (base-load-bound (boundp 'nelisp--base-load))
         (base-load (and base-load-bound nelisp--base-load))
         (base-provide-bound (boundp 'nelisp--base-provide))
         (base-provide (and base-provide-bound nelisp--base-provide))
         (generator-bound
          (fboundp 'nelisp-standalone--after-load-runtime-src))
         (generator
          (and generator-bound
               (symbol-function 'nelisp-standalone--after-load-runtime-src)))
         (driver-bound (fboundp 'nelisp-eln-registration-load))
         (driver (and driver-bound (symbol-function 'nelisp-eln-registration-load)))
         (after-bound (fboundp 'nelisp--after-load-feature))
         (after (and after-bound (symbol-function 'nelisp--after-load-feature)))
         (eln (make-temp-file "nelisp-eln-load" nil ".eln"))
         (ordinary (make-temp-file "nelisp-eln-load" nil ".el"))
         (driver-calls nil) (after-calls nil) (native-calls nil)
         (required nil))
    (unwind-protect
        (progn
          (nelisp-standalone-eln-load-test--install-generated-wrapper)
          (setq nelisp--base-load
                (lambda (&rest args)
                  (push args native-calls)
                  (if (equal (car args) "/missing/absent.eln") nil :native)))
          (fset 'nelisp--after-load-feature
                (lambda (path) (push path after-calls)))
          (fmakunbound 'nelisp-eln-registration-load)
          (fset 'require
                (lambda (feature &optional _filename _noerror)
                  (setq required (cons feature required))
                  (unless (eq feature 'nelisp-eln-registration)
                    (error "Unexpected require: %S" feature))
                  (fset 'nelisp-eln-registration-load
                        (lambda (path)
                          (push (cons path load-file-name) driver-calls)
                          nil))
                  feature))
          (let ((load-file-name "caller.el"))
            (should (eq (funcall (symbol-function 'load) eln) t))
            (should (equal load-file-name "caller.el")))
          (should (equal required '(nelisp-eln-registration)))
          (should (equal driver-calls
                         (list (cons (expand-file-name eln)
                                     (expand-file-name eln)))))
          (should (equal after-calls (list (expand-file-name eln))))
          (should-not native-calls)
          ;; Inaccessible explicit .eln paths retain the captured native
          ;; loader's noerror behavior and argument list.
          (should-not (funcall (symbol-function 'load)
                               "/missing/absent.eln" t))
          (should (equal (car native-calls) '("/missing/absent.eln" t)))
          ;; Ordinary source paths still use the native loader.
          (should (eq (funcall (symbol-function 'load) ordinary) :native))
          (should (equal (car native-calls) (list (expand-file-name ordinary))))
          ;; A driver error propagates and never triggers after-load.
          (fset 'nelisp-eln-registration-load
                (lambda (_path) (error "registration rejected")))
          (let ((before (length after-calls)))
            (should-error (funcall (symbol-function 'load) eln))
            (should (= before (length after-calls)))))
      (delete-file eln)
      (delete-file ordinary)
      (nelisp-standalone-eln-load-test--restore-function 'load wrapper)
      (nelisp-standalone-eln-load-test--restore-function 'provide provide)
      (nelisp-standalone-eln-load-test--restore-function 'require require-fn)
      (nelisp-standalone-eln-load-test--restore-function
       'nelisp--after-load-feature after)
      (nelisp-standalone-eln-load-test--restore-function
       'nelisp-eln-registration-load driver)
      (if base-load-bound
          (setq nelisp--base-load base-load)
        (makunbound 'nelisp--base-load))
      (if base-provide-bound
          (setq nelisp--base-provide base-provide)
        (makunbound 'nelisp--base-provide))
      (nelisp-standalone-eln-load-test--restore-function
       'nelisp-standalone--after-load-runtime-src generator))))

(provide 'nelisp-standalone-eln-load-test)

;;; nelisp-standalone-eln-load-test.el ends here
