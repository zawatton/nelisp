;;; nelisp-eval-after-load-test.el --- GNU after-load bridge tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'nelisp-load)

(defvar nelisp-eval-after-load-test--host-count 0)
(defvar nelisp-after-load-count 0)

(defun nelisp-eval-after-load-test--source (text)
  "Write TEXT to a temporary Elisp file and return its path."
  (let ((path (make-temp-file "nelisp-eval-after-load-" nil ".el")))
    (with-temp-file path
      (insert ";;; -*- lexical-binding: t; -*-\n" text))
    path))

(defun nelisp-eval-after-load-test--feature (suffix)
  "Return a unique interned feature symbol with SUFFIX."
  (intern (format "nelisp-after-load-%s-%s" suffix
                  (substring (md5 (format "%s:%s" (float-time) (random))) 0 12))))

(defun nelisp-eval-after-load-test--reset-runtime ()
  "Reset NeLisp state for a test."
  (nelisp--reset)
  (setq nelisp-load-prefer-artifacts nil
        nelisp-eval-after-load-test--host-count 0
        nelisp-after-load-count 0)
  (nelisp-eval-string "(setq nelisp-after-load-count 0)"))

(defun nelisp-eval-after-load-test--register-runtime (selector)
  "Register a NeLisp counter callback for SELECTOR through GNU's function."
  (nelisp-eval
   `(eval-after-load ,(if (symbolp selector) `(quote ,selector) selector)
      '(setq nelisp-after-load-count (1+ nelisp-after-load-count)))))

(defun nelisp-eval-after-load-test--register-host-feature (feature)
  "Register a host callback with GNU `with-eval-after-load'."
  (with-eval-after-load feature
    (setq nelisp-eval-after-load-test--host-count
          (1+ nelisp-eval-after-load-test--host-count))))

(ert-deftest nelisp-eval-after-load/gnu-feature-notifications-repeat ()
  "GNU feature callbacks run after source provide and again on reload."
  (let* ((feature (nelisp-eval-after-load-test--feature "repeat"))
         (path (nelisp-eval-after-load-test--source
                (format "(provide '%s)\n" feature))))
    (unwind-protect
        (progn
          (nelisp-eval-after-load-test--reset-runtime)
          (nelisp-eval-after-load-test--register-host-feature feature)
          (nelisp-eval-after-load-test--register-runtime feature)
          (nelisp-load-file path)
          (should (= nelisp-eval-after-load-test--host-count 1))
          (should (= (gethash 'nelisp-after-load-count nelisp--globals) 1))
          (load path nil 'nomessage)
          (should (= nelisp-eval-after-load-test--host-count 2))
          (should (= (gethash 'nelisp-after-load-count nelisp--globals) 2)))
      (delete-file path))))

(ert-deftest nelisp-eval-after-load/gnu-file-notifications-and-late-registration ()
  "GNU file callbacks run after source success and at registration if loaded."
  (let* ((path (nelisp-eval-after-load-test--source "(setq host-source-loaded t)\n")))
    (unwind-protect
        (progn
          (nelisp-eval-after-load-test--reset-runtime)
          (eval-after-load path
            (lambda () (setq nelisp-eval-after-load-test--host-count
                             (1+ nelisp-eval-after-load-test--host-count))))
          (nelisp-eval-after-load-test--register-runtime path)
          (nelisp-load-file path)
          (should (= nelisp-eval-after-load-test--host-count 1))
          (should (= (gethash 'nelisp-after-load-count nelisp--globals) 1))
          (eval-after-load path
            (lambda () (setq nelisp-eval-after-load-test--host-count
                             (1+ nelisp-eval-after-load-test--host-count))))
          (nelisp-eval-after-load-test--register-runtime path)
          (should (= nelisp-eval-after-load-test--host-count 2))
          (should (= (gethash 'nelisp-after-load-count nelisp--globals) 2)))
      (delete-file path))))

(ert-deftest nelisp-eval-after-load/failed-source-does-not-run-file-hooks ()
  "GNU file callbacks stay pending when NeLisp source evaluation fails."
  (let* ((feature (nelisp-eval-after-load-test--feature "failed"))
         (path (nelisp-eval-after-load-test--source
                (format "(provide '%s)\n(error \"load failed\")\n" feature))))
    (unwind-protect
        (progn
          (nelisp-eval-after-load-test--reset-runtime)
          (eval-after-load path
            (lambda () (setq nelisp-eval-after-load-test--host-count
                             (1+ nelisp-eval-after-load-test--host-count))))
          (nelisp-eval-after-load-test--register-runtime path)
          (nelisp-eval-after-load-test--register-host-feature feature)
          (nelisp-eval-after-load-test--register-runtime feature)
          (should-error (nelisp-load-file path) :type 'nelisp-load-error)
          (should (= nelisp-eval-after-load-test--host-count 0))
          (should (= (gethash 'nelisp-after-load-count nelisp--globals) 0))
          (should-not (load-history-filename-element (load-history-regexp path))))
      (delete-file path))))

(ert-deftest nelisp-eval-after-load/gnu-callback-errors-propagate ()
  "Errors from GNU after-load callbacks propagate after source success."
  (let* ((feature (nelisp-eval-after-load-test--feature "callback-error"))
         (path (nelisp-eval-after-load-test--source
                (format "(provide '%s)\n" feature))))
    (unwind-protect
        (progn
          (nelisp-eval-after-load-test--reset-runtime)
          (eval-after-load feature (lambda () (error "host callback failed")))
          (nelisp-eval
           `(eval-after-load ',feature '(error "NeLisp callback failed")))
          (should-error (nelisp-load-file path) :type 'error)
          (should (memq feature features))
          (should (member (cons 'provide feature)
                          (cdr (assoc (file-truename path) load-history)))))
      (delete-file path))))

(ert-deftest nelisp-eval-after-load/registration-after-provide-is-immediate ()
  "GNU `eval-after-load' runs immediately after a feature is provided."
  (let ((feature (nelisp-eval-after-load-test--feature "late")))
    (nelisp-eval-after-load-test--reset-runtime)
    (nelisp-eval-after-load-test--register-host-feature feature)
    (nelisp-eval-after-load-test--register-runtime feature)
    (provide feature)
    (nelisp--builtin-provide feature)
    (nelisp-eval-after-load-test--register-host-feature feature)
    (nelisp-eval-after-load-test--register-runtime feature)
    (should (= nelisp-eval-after-load-test--host-count 3))
    (should (= (gethash 'nelisp-after-load-count nelisp--globals) 3))))

(provide 'nelisp-eval-after-load-test)

;;; nelisp-eval-after-load-test.el ends here
