;;; nelisp-dev-clear-test.el --- live session.clear contracts -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-dev)
(require 'nelisp-dev-failure)
(require 'nelisp-dev-gc)
(require 'nelisp-repl-code)

(defmacro nelisp-dev-clear-test--isolated (&rest body)
  (declare (indent 0))
  `(let ((nelisp-repl-session--failures nil)
         (nelisp-repl-session--records nil)
         (nelisp-repl-session--next-id 1)
         (nelisp-repl-code--records (make-hash-table :test #'eq))
         (nelisp-dev-protocol--details (make-hash-table :test #'equal))
         (nelisp-dev-protocol--detail-bytes 0)
         (nelisp-dev-protocol--epoch 0)
         (nelisp-dev-gc--snapshots (make-hash-table :test #'equal))
         (nelisp-dev-gc--epochs (make-hash-table :test #'equal))
         (nelisp-dev-failure--handles (make-hash-table :test #'equal)))
     ,@body))

(defun nelisp-dev-clear-test--request (&optional session)
  (list '("schema_version" . "1") '("operation" . "session.clear")
        '("request_id" . "clear-test") '("arguments")
        '("target" . "host-emacs") (cons "session_id" (or session :null))
        '("limits")))

(ert-deftest nelisp-dev-clear-live-registration-and-disconnected-refusal ()
  (let ((adapters (plist-get (nelisp-dev-session-context) :adapters)))
    (dolist (operation '("session.clear" "diagnose" "retry"))
      (should (functionp (cdr (assoc operation adapters))))))
  (should (equal "unsupported"
                 (cdr (assoc "status" (nelisp-dev-dispatch
                                       (nelisp-dev-clear-test--request)
                                       (nelisp-dev-context)))))))

(ert-deftest nelisp-dev-clear-releases-all-registered-stores ()
  (nelisp-dev-clear-test--isolated
    (let* ((context (nelisp-dev-session-context))
           (session (plist-get context :session-id)))
      (setq nelisp-repl-session--failures '((:id 7 :args (retained)))
            nelisp-repl-session--records '((:kind :form :form "7")))
      (puthash 'demo 'provenance nelisp-repl-code--records)
      (puthash "handle" 'retained nelisp-dev-failure--handles)
      (puthash "snapshot" (list 0 :session-id session) nelisp-dev-gc--snapshots)
      (puthash session '(1) nelisp-dev-gc--epochs)
      (nelisp-dev-protocol-detail-put "detail" 'retained)
      (let* ((result (nelisp-dev-dispatch (nelisp-dev-clear-test--request) context))
             (summary (cdr (assoc "summary" result))))
        (should (equal "ok" (cdr (assoc "status" result))))
        (dolist (key '("failures" "replay_records" "code_provenance"
                       "protocol_details" "gc_snapshots" "gc_epochs" "failure_handles"))
          (should (= 1 (cdr (assoc key summary)))))
        (should (stringp (nelisp-dev-protocol-json result))))
      (should-not nelisp-repl-session--failures)
      (should-not nelisp-repl-session--records)
      (dolist (table (list nelisp-repl-code--records nelisp-dev-failure--handles
                           nelisp-dev-gc--snapshots nelisp-dev-gc--epochs
                           nelisp-dev-protocol--details))
        (should (= 0 (hash-table-count table))))
      (should (= 1 nelisp-dev-protocol--epoch)))))

(ert-deftest nelisp-dev-clear-stale-context-preserves-records ()
  (nelisp-dev-clear-test--isolated
    (let ((context (nelisp-dev-session-context)))
      (setq nelisp-repl-session--records '((:kind :form :form "7")))
      (plist-put context :session-id "stale")
      (should (equal "unsupported" (cdr (assoc "status"
        (nelisp-dev-dispatch (nelisp-dev-clear-test--request "stale") context)))))
      (should (= 1 (length nelisp-repl-session--records))))))

(defun nelisp-dev-clear-test--capture-weak ()
  "Return only a weak table after actual failure capture."
  (let ((weak (make-hash-table :test #'eq :weakness 'value))
        (object (list 'failure-only-reference)))
    (puthash 'object object weak)
    (condition-case nil
        (nelisp-repl-session-call (lambda (_arg) (error "retention probe")) object)
      (error nil))
    weak))

(ert-deftest nelisp-dev-clear-releases-failure-argument-root ()
  (nelisp-dev-clear-test--isolated
    (let ((weak (nelisp-dev-clear-test--capture-weak))
          (context (nelisp-dev-session-context)))
      (garbage-collect)
      (should (= 1 (hash-table-count weak)))
      (nelisp-dev-dispatch (nelisp-dev-clear-test--request) context)
      (garbage-collect)
      (should (= 0 (hash-table-count weak))))))

(provide 'nelisp-dev-clear-test)
