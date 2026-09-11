;;; nelisp-dev-failure-test.el --- failure adapter contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-dev-failure)

(defun nelisp-dev-failure-test--request (operation arguments)
  (list (cons "schema_version" "1") (cons "operation" operation)
        (cons "request_id" "failure-test") (cons "arguments" arguments)
        (cons "target" "host-emacs") (cons "session_id" "live-failure-test")
        (cons "limits" nil)))

(ert-deftest nelisp-dev-failure-capture-diagnose-explicit-retry ()
  (let ((nelisp-repl-session--failures nil)
        (nelisp-repl-session--next-id 1)
        (nelisp-dev-failure--handles (make-hash-table :test #'equal))
        (attempts 0)
        (value nil)
        (context '(:target "host-emacs" :session-id "live-failure-test"
                   :effects-policy "explicit-only")))
    (condition-case nil
        (nelisp-repl-session-call
         (lambda () (setq attempts (1+ attempts))
           (if (= attempts 1) (error "first attempt") 42)))
      (error nil))
    (let* ((failure (car (nelisp-repl-session-failures)))
           (id (plist-get failure :id))
           (diagnosed (nelisp-dev-failure-dispatch
                       (nelisp-dev-failure-test--request
                        "diagnose" (list (cons "failure_id" id))) context))
           (data (cdr (assoc "data" diagnosed)))
           (handle (cdr (assoc "failure_handle" data)))
           (retried (nelisp-dev-failure-dispatch
                     (nelisp-dev-failure-test--request
                      "retry" (list (cons "failure_handle" handle))) context)))
      (should (equal "ok" (cdr (assoc "status" diagnosed))))
      (should (stringp handle))
      (should (equal "ok" (cdr (assoc "status" retried))))
      (setq value (cdr (assoc "result_type" (cdr (assoc "data" retried)))))
      (should (equal value "number"))
      (should (= attempts 2)))))

(ert-deftest nelisp-dev-failure-rejections-have-no-side-effects ()
  (let ((nelisp-repl-session--failures nil)
        (nelisp-repl-session--next-id 1)
        (nelisp-dev-failure--handles (make-hash-table :test #'equal))
        (attempts 0)
        (context '(:target "host-emacs" :session-id "live-failure-test")))
    (condition-case nil
        (nelisp-repl-session-call (lambda () (setq attempts (1+ attempts)) (error "saved")))
      (error nil))
    (let* ((id (plist-get (car (nelisp-repl-session-failures)) :id))
           (diagnosed (nelisp-dev-failure-dispatch
                       (nelisp-dev-failure-test--request
                        "diagnose" (list (cons "failure_id" id))) context))
           (handle (cdr (assoc "failure_handle" (cdr (assoc "data" diagnosed)))))
           (rejected (nelisp-dev-failure-dispatch
                      (nelisp-dev-failure-test--request
                       "retry" (list (cons "failure_handle" handle))) context)))
      (should (equal "failed" (cdr (assoc "status" rejected))))
      (should (= attempts 1))
      (nelisp-dev-failure-clear)
      (let ((stale (nelisp-dev-failure-dispatch
                    (nelisp-dev-failure-test--request
                     "retry" (list (cons "failure_handle" handle)))
                    (plist-put (copy-sequence context) :effects-policy "explicit-only"))))
        (should (equal "failed" (cdr (assoc "status" stale))))
        (should (= attempts 1))))))

(ert-deftest nelisp-dev-failure-handle-is-stale-after-clear-and-id-reuse ()
  (let ((nelisp-repl-session--failures nil)
        (nelisp-repl-session--next-id 1)
        (nelisp-dev-failure--handles (make-hash-table :test #'equal))
        (context '(:target "host-emacs" :session-id "live-failure-test"
                   :effects-policy "explicit-only")))
    (dolist (_ '(1))
      (condition-case nil
          (nelisp-repl-session-call (lambda () (error "same")))
        (error nil)))
    (let* ((id (plist-get (car (nelisp-repl-session-failures)) :id))
           (first (nelisp-dev-failure-dispatch
                   (nelisp-dev-failure-test--request "diagnose"
                                                    (list (cons "failure_id" id))) context))
           (handle (cdr (assoc "failure_handle" (cdr (assoc "data" first))))))
      (nelisp-repl-session-clear)
      (condition-case nil
          (nelisp-repl-session-call (lambda () (error "same")))
        (error nil))
      (should (equal "failed"
                     (cdr (assoc "status"
                                 (nelisp-dev-failure-dispatch
                                  (nelisp-dev-failure-test--request
                                   "retry" (list (cons "failure_handle" handle)))
                                  context))))))))

(ert-deftest nelisp-dev-failure-stable-bounded-wire-handles ()
  (let ((nelisp-repl-session--failures nil)
        (nelisp-repl-session--records nil)
        (nelisp-repl-session--next-id 1)
        (nelisp-dev-failure--handles (make-hash-table :test #'equal))
        (context '(:target "host-emacs" :session-id "live-failure-test")))
    (condition-case nil (nelisp-repl-session-call (lambda () (error "saved")))
      (error nil))
    (let* ((request (nelisp-dev-failure-test--request "diagnose" '(("failure_id" . 1))))
           (first (nelisp-dev-failure-dispatch request context))
           (handle (cdr (assoc "failure_handle" (cdr (assoc "data" first))))))
      (dotimes (_ 70)
        (let ((next (nelisp-dev-failure-dispatch request context)))
          (should (equal handle (cdr (assoc "failure_handle" (cdr (assoc "data" next))))))
          (should (equal "ok" (cdr (assoc "status" next))))
          (should (stringp (nelisp-dev-protocol-json next)))))
      (should (= 1 (hash-table-count nelisp-dev-failure--handles)))
      (nelisp-repl-session-clear)
      (condition-case nil (nelisp-repl-session-call (lambda () (error "saved")))
        (error nil))
      (let ((new (nelisp-dev-failure-dispatch request context)))
        (should-not (equal handle (cdr (assoc "failure_handle" (cdr (assoc "data" new)))))))
      (dolist (arguments (list
                         '(("failure_id" . 1) ("effects_policy" . "explicit-only"))
                         (list (cons "failure_handle" handle) '("failure_id" . 1)
                               '("effects_policy" . "explicit-only"))))
        (should (equal "failed" (cdr (assoc "status"
          (nelisp-dev-failure-dispatch
           (nelisp-dev-failure-test--request "retry" arguments) context)))))))))

(provide 'nelisp-dev-failure-test)
;;; nelisp-dev-failure-test.el ends here
