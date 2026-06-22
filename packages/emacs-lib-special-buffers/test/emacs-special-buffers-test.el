;;; emacs-special-buffers-test.el --- tests for special buffers  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-special-buffers)

(defmacro emacs-special-buffers-test--with-fresh-world (&rest body)
  "Run BODY with clean core buffer and special-buffer backend state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (emacs-buffer--state (make-hash-table :test 'eq))
         (emacs-special-buffers-backend nil))
     ,@body))

(defun emacs-special-buffers-test--buffer-string (buffer)
  "Return BUFFER contents for host or NeLisp core buffer substrates."
  (if (and (fboundp 'nelisp-ec-buffer-p)
           (nelisp-ec-buffer-p buffer))
      (nelisp-ec-with-current-buffer buffer
        (nelisp-ec-buffer-string))
    (with-current-buffer buffer
      (buffer-string))))

(ert-deftest emacs-special-buffers-test/ensure-standard-core-buffers ()
  (emacs-special-buffers-test--with-fresh-world
    (should (emacs-special-buffers-ensure-standard-buffers))
    (let ((scratch (emacs-special-buffers--find-buffer "*scratch*"))
          (messages (emacs-special-buffers--find-buffer "*Messages*"))
          (warnings (emacs-special-buffers--find-buffer "*Warnings*")))
      (should scratch)
      (should messages)
      (should warnings)
      (should (string-match-p
               "This buffer is for text"
               (emacs-special-buffers-test--buffer-string scratch)))
      (when (emacs-special-buffers--core-buffer-substrate-p)
        (should (equal ""
                       (emacs-special-buffers-test--buffer-string messages)))
        (should (equal ""
                       (emacs-special-buffers-test--buffer-string warnings))))
      (should-not (emacs-special-buffers-read-only-p "*scratch*"))
      (should (emacs-special-buffers-read-only-p "*Messages*"))
      (should (emacs-special-buffers-read-only-p "*Warnings*")))))

(ert-deftest emacs-special-buffers-test/names-and-default-policy ()
  (should (equal "*scratch*" emacs-special-buffers-scratch-name))
  (should (equal "*Messages*" emacs-special-buffers-messages-name))
  (should (equal "*Warnings*" emacs-special-buffers-warnings-name))
  (should (emacs-special-buffers-special-buffer-p
           emacs-special-buffers-scratch-name))
  (should (emacs-special-buffers-special-buffer-p
           emacs-special-buffers-messages-name))
  (should (emacs-special-buffers-special-buffer-p
           emacs-special-buffers-warnings-name))
  (should-not (emacs-special-buffers-special-buffer-p "*ordinary*"))
  (should (string-match-p
           "This buffer is for text"
           emacs-special-buffers-scratch-initial-message))
  (should (equal emacs-special-buffers-scratch-initial-message
                 (emacs-special-buffers-default-text
                  emacs-special-buffers-scratch-name)))
  (should (equal "" (emacs-special-buffers-default-text
                     emacs-special-buffers-messages-name)))
  (should (equal "" (emacs-special-buffers-default-text
                     emacs-special-buffers-warnings-name))))

(ert-deftest emacs-special-buffers-test/message-and-warning-append ()
  (emacs-special-buffers-test--with-fresh-world
    (should (equal "hello world"
                   (emacs-special-buffers-message "hello %s" "world")))
    (should (equal "Warning [nemacs]: careful"
                   (emacs-special-buffers-display-warning
                    'nemacs "careful" 'warning)))
    (let ((messages (emacs-special-buffers-test--buffer-string
                     (emacs-special-buffers--find-buffer "*Messages*")))
          (warnings (emacs-special-buffers-test--buffer-string
                     (emacs-special-buffers--find-buffer "*Warnings*"))))
      (should (string-match-p "hello world" messages))
      (should (string-match-p "Warning \\[nemacs\\]: careful" messages))
      (should (string-match-p "Warning \\[nemacs\\]: careful" warnings)))))

(ert-deftest emacs-special-buffers-test/warn-and-lwarn-append ()
  (emacs-special-buffers-test--with-fresh-world
    (should (equal "Warning [pkg]: first"
                   (emacs-special-buffers-lwarn 'pkg 'warning
                                                 "first")))
    (should (equal "Warning [emacs]: second"
                   (emacs-special-buffers-warn "second")))
    (let ((messages (emacs-special-buffers-test--buffer-string
                     (emacs-special-buffers--find-buffer
                      emacs-special-buffers-messages-name)))
          (warnings (emacs-special-buffers-test--buffer-string
                     (emacs-special-buffers--find-buffer
                      emacs-special-buffers-warnings-name))))
      (should (string-match-p "Warning \\[pkg\\]: first" messages))
      (should (string-match-p "Warning \\[emacs\\]: second" messages))
      (should (string-match-p "Warning \\[pkg\\]: first" warnings))
      (should (string-match-p "Warning \\[emacs\\]: second" warnings)))))

(ert-deftest emacs-special-buffers-test/display-plan-ensures-buffer ()
  (emacs-special-buffers-test--with-fresh-world
    (let ((plan (emacs-special-buffers-display-plan
                 "*scratch*" "scratch-buffer")))
      (should (eq 'ok (plist-get plan :status)))
      (should (plist-get plan :buffer))
      (should (equal "*scratch*" (plist-get plan :buffer-name)))
      (should (equal 0 (plist-get plan :scroll-offset)))
      (should (equal "scratch-buffer" (plist-get plan :message)))
      (should (string-match-p
               "This buffer is for text"
               (emacs-special-buffers-test--buffer-string
                (plist-get plan :buffer)))))))

(ert-deftest emacs-special-buffers-test/get-scratch-buffer-create-ensures-scratch ()
  (emacs-special-buffers-test--with-fresh-world
    (let ((scratch (get-scratch-buffer-create)))
      (should scratch)
      (should (eq scratch (emacs-special-buffers--find-buffer "*scratch*")))
      (should (string-match-p
               "This buffer is for text"
               (emacs-special-buffers-test--buffer-string scratch))))))

(ert-deftest emacs-special-buffers-test/backend-receives-operations ()
  (let ((calls nil))
    (emacs-special-buffers-register-backend
     :ensure (lambda (name)
               (push (list :ensure name) calls)
               name)
     :append (lambda (name text)
               (push (list :append name text) calls)
               text)
     :switch (lambda (name)
               (push (list :switch name) calls)
               name))
    (should (equal "*scratch*"
                   (emacs-special-buffers-ensure-buffer "*scratch*")))
    (should (equal "line\n"
                   (emacs-special-buffers-append-to-buffer
                    "*Messages*" "line\n")))
    (should (equal "*Warnings*"
                   (emacs-special-buffers-switch-to-buffer "*Warnings*")))
    (should (equal '((:switch "*Warnings*")
                     (:append "*Messages*" "line\n")
                     (:ensure "*scratch*"))
                   calls))))

(provide 'emacs-special-buffers-test)

;;; emacs-special-buffers-test.el ends here
