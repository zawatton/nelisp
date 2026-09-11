;;; -*- lexical-binding: t; -*-
;;; nelisp-dev-replay-test.el --- explicit host replay process tests
(require 'ert)
(require 'cl-lib)
(require 'json)
(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../lisp" here)))
(require 'nelisp-dev-session)
(require 'nelisp-dev-replay)

(defun nelisp-dev-replay-test--request (manifest &optional policy timeout)
  (list (cons "operation" "session.replay") (cons "request_id" "replay-test")
        (cons "target" "host-emacs")
        (cons "arguments"
              (append (list (cons "manifest" manifest))
                      (and policy (list (cons "effects-policy" policy)))
                      (and timeout (list (cons "timeout" timeout)))))))
(defun nelisp-dev-replay-test--status (result) (cdr (assoc "status" result)))

(defmacro nelisp-dev-replay-test--session (source-text &rest body)
  `(let* ((directory (make-temp-file "nelisp-dev-replay-test-" t))
          (source (expand-file-name "source-utf8.el" directory))
          (recipe (expand-file-name "recipe.el" directory))
          (manifest (expand-file-name "recipe.manifest.json" directory))
          (marker (expand-file-name "child-marker" directory)))
     (unwind-protect
         (progn
           (let ((coding-system-for-write 'utf-8-unix))
             (with-temp-file source
               (insert (replace-regexp-in-string "%MARKER%" marker ,source-text t t))))
           (let ((nelisp-repl-session--records nil)
                 (nelisp-repl-session--next-id 1))
             (nelisp-repl-session-record-load source)
             (nelisp-dev-session-dispatch "session.export"
              (list (cons "session-id" "replay-test") (cons "recipe" recipe)
                    (cons "manifest" manifest))))
           ,@body)
       (delete-directory directory t))))

(ert-deftest nelisp-dev-replay/explicit-record-load-runs-in-fresh-process ()
  (nelisp-dev-replay-test--session
   "(with-temp-file \"%MARKER%\" (insert \"child\")) (princ \"日本語\")\n"
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only")
                  (list :target "host-emacs" :root directory))))
     (should (equal "ok" (nelisp-dev-replay-test--status result)))
     (should (file-exists-p marker))
     (should (string-match-p "日本語"
                            (or (cdr (assoc "stdout" (cdr (assoc "data" result)))) "")))
     (should (= 1 (cdr (assoc "executed_forms" (cdr (assoc "data" result))))))
     (should (= 0 (cdr (assoc "worker_exit_code" (cdr (assoc "data" result))))))
     (should (null (bound-and-true-p nelisp-dev-replay-test-child))))))

(ert-deftest nelisp-dev-replay/missing-policy-does-not-run ()
  (nelisp-dev-replay-test--session
   "(with-temp-file \"%MARKER%\" (insert \"unexpected\"))\n"
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest nil)
                  (list :target "host-emacs" :root directory))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should-not (file-exists-p marker)))))

(ert-deftest nelisp-dev-replay/tampered-source-is-refused-before-execution ()
  (nelisp-dev-replay-test--session
   "(with-temp-file \"%MARKER%\" (insert \"executed\"))\n"
   (with-temp-file source
     (insert (replace-regexp-in-string "%MARKER%" marker
                                       "(with-temp-file \"%MARKER%\" (insert \"tampered\"))\n"
                                       t t)))
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only")
                  (list :target "host-emacs" :root directory))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should-not (file-exists-p marker)))))

(ert-deftest nelisp-dev-replay/error-before-success-marker-is-failure ()
  (nelisp-dev-replay-test--session
   "(error \"intentional replay failure\")\n(setq nelisp-dev-replay-test-success-marker t)\n"
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only")
                  (list :target "host-emacs" :root directory))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should (null (bound-and-true-p nelisp-dev-replay-test-success-marker))))))

(ert-deftest nelisp-dev-replay/timeout-kills-worker ()
  (nelisp-dev-replay-test--session
   "(while t)\n"
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only" 1)
                  (list :target "host-emacs" :root directory))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should (equal "timeout"
                    (cdr (assoc "phase" (cdr (assoc "summary" result)))))))))

(ert-deftest nelisp-dev-replay/output-is-bounded ()
  (nelisp-dev-replay-test--session
   "(princ (make-string 100000 ?x))\n"
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only")
                  (list :target "host-emacs" :root directory))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (let ((data (cdr (assoc "data" result))))
       (should (cdr (assoc "output_truncated" data)))
       (should (< (string-bytes (or (cdr (assoc "stdout" data)) "")) 70000))))))

(ert-deftest nelisp-dev-replay/early-zero-exit-has-no-terminal-success ()
  (nelisp-dev-replay-test--session
   "(princ \"SUCCESS\") (kill-emacs 0)\n"
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only")
                  (list :target "host-emacs" :root directory))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should (equal "NELISP-DEV-REPLAY-INCOMPLETE"
                    (cdr (assoc "code" (aref (cdr (assoc "diagnostics" result)) 0))))))))

(ert-deftest nelisp-dev-replay/buffer-edits-cannot-skip-later-forms ()
  (nelisp-dev-replay-test--session
   "nil\n"
   (let ((nelisp-repl-session--records nil))
     (nelisp-repl-session-record '(erase-buffer))
     (nelisp-repl-session-record '(error "must still execute"))
     (nelisp-dev-session-dispatch "session.export"
       (list :session-id "fixture" :recipe recipe :manifest manifest)))
   (let* ((result (nelisp-dev-replay-dispatch
                   (nelisp-dev-replay-test--request manifest "explicit-only")
                   (list :target "host-emacs" :root directory)))
          (data (cdr (assoc "data" result))))
     (should (equal "failed" (nelisp-dev-replay-test--status result)))
     (should (= 1 (cdr (assoc "executed_forms" data)))))))

(ert-deftest nelisp-dev-replay/empty-recipe-is-not-a-successful-run ()
  (nelisp-dev-replay-test--session
   "nil\n"
   (let ((nelisp-repl-session--records nil))
     (nelisp-dev-session-dispatch "session.export"
       (list :session-id "fixture" :recipe recipe :manifest manifest)))
   (let ((result (nelisp-dev-replay-dispatch
                  (nelisp-dev-replay-test--request manifest "explicit-only")
                  (list :target "host-emacs" :root directory))))
     (should (equal "inconclusive" (nelisp-dev-replay-test--status result)))
     (should (= 0 (cdr (assoc "executed_forms" (cdr (assoc "data" result)))))))))

(ert-deftest nelisp-dev-replay/non-utf8-locale-preserves-worker-output ()
  (nelisp-dev-replay-test--session
   "(princ \"日本語\")\n"
   (let ((original (symbol-function 'make-process)))
     (cl-letf (((symbol-function 'make-process)
                (lambda (&rest arguments)
                  (let ((command (plist-get arguments :command)))
                    (setq arguments
                          (plist-put arguments :command
                            (append (cl-subseq command 0 3)
                                    '("--eval" "(progn (setq locale-coding-system 'us-ascii coding-system-for-write 'us-ascii) (set-terminal-coding-system 'us-ascii))")
                                    (nthcdr 3 command))))
                    (apply original arguments)))))
       (let ((result (nelisp-dev-replay-dispatch
                      (nelisp-dev-replay-test--request manifest "explicit-only")
                      (list :target "host-emacs" :root directory))))
         (should (equal "ok" (nelisp-dev-replay-test--status result)))
         (should (equal "日本語" (cdr (assoc "stdout" (cdr (assoc "data" result)))))))))))

(provide 'nelisp-dev-replay-test)
