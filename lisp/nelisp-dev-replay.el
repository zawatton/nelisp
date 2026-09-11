;;; nelisp-dev-replay.el --- explicit host session replay adapter -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'nelisp-dev-session)
(require 'nelisp-dev-protocol)

(defconst nelisp-dev-replay--output-budget 65536)
(defconst nelisp-dev-replay--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defun nelisp-dev-replay--prefix (text bytes)
  "Return the longest character prefix of TEXT fitting in BYTES."
  (let ((lo 0) (hi (min (length text) bytes)))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi 1) 2)))
        (if (<= (string-bytes (substring text 0 mid)) bytes)
            (setq lo mid) (setq hi (1- mid)))))
    (substring text 0 lo)))

(defun nelisp-dev-replay--result (request status phase code data)
  (nelisp-dev-protocol-envelope
   "session.replay" (cdr (assoc "request_id" request)) status
   '(("target" . "host-emacs") ("session_id" . :null)
     ("source_revision" . :null) ("source_content_hash" . :null)
     ("runtime_artifact_hash" . :null) ("generation" . :null))
   (list (cons "phase" phase))
   (if code (vector (list (cons "code" code))) [])
   data
   ["The fresh host worker does not modify the parent REPL's Lisp state."
    "Temporary working directory and separate memory do not restrict network, process or absolute-path file effects."
    "Registered source hashes are preflight checks, not frozen dependencies."
    "Native runtime, build options and dependency compatibility remain unverified."]))

(defun nelisp-dev-replay--worker (request manifest timeout)
  "Run only our child process, with bounded streamed output and a deadline."
  (let* ((manifest-hash (nelisp-dev-session--sha256 manifest 1048576))
         (scratch (make-temp-file "nelisp-dev-replay-" t))
         (default-directory (file-name-as-directory scratch))
         (result-file (expand-file-name "terminal.json" scratch))
         (worker (expand-file-name "../scripts/nelisp-dev-replay-worker.el"
                                   nelisp-dev-replay--directory))
         (nonce (secure-hash 'sha256 (format "%S:%S" (current-time) (random))))
         (stdout "") (stderr "") (used 0) limited timed-out process error-pipe)
    (unwind-protect
        (progn
          (setq error-pipe
                (make-pipe-process
                 :name "nelisp-dev-replay-stderr" :noquery t :coding 'utf-8-unix
                 :filter (lambda (_pipe text)
                           (let ((part (nelisp-dev-replay--prefix text
                                        (- nelisp-dev-replay--output-budget used))))
                             (setq stderr (concat stderr part)
                                   used (+ used (string-bytes part)))
                             (unless (= (length part) (length text))
                               (setq limited t)
                               (when (and process (process-live-p process))
                                 (delete-process process)))))))
          (setq process
                (make-process
                 :name "nelisp-dev-replay" :noquery t :connection-type 'pipe
                 :coding 'utf-8-unix :stderr error-pipe
                 :command (list (expand-file-name invocation-name invocation-directory)
                                "-Q" "--batch" "--eval" "(setq load-prefer-newer t)"
                                "-L" nelisp-dev-replay--directory
                                "-l" worker "-f" "nelisp-dev-replay-worker-main"
                                manifest result-file manifest-hash nonce)
                 :filter (lambda (_process text)
                           (let ((part (nelisp-dev-replay--prefix text
                                        (- nelisp-dev-replay--output-budget used))))
                             (setq stdout (concat stdout part)
                                   used (+ used (string-bytes part)))
                             (unless (= (length part) (length text))
                               (setq limited t)
                               (when (and process (process-live-p process))
                                 (delete-process process)))))))
          (let ((deadline (+ (float-time) timeout)))
            (while (and (process-live-p process) (not limited)
                        (< (float-time) deadline))
              (accept-process-output process 0.02))
            (when (process-live-p process)
              (setq timed-out (not limited))
              (delete-process process)))
          ;; Bounded drain: a recipe can spawn a child retaining these pipes.
          (dotimes (_ 5)
            (accept-process-output process 0.01)
            (accept-process-output error-pipe 0.01))
          (let* ((read-result (and (not limited) (not timed-out)
                                   (file-readable-p result-file)
                                   (nelisp-dev-session--read-json result-file)))
                 (terminal (plist-get read-result :data))
                 (status (cdr (assoc "status" terminal)))
                 (exit-code (process-exit-status process))
                 (count (cdr (assoc "executed_forms" terminal)))
                 (valid (and (equal nonce (cdr (assoc "nonce" terminal)))
                             (integerp count) (>= count 0)
                             (member status '("ok" "failed" "inconclusive"))
                             (= exit-code (cond ((equal status "ok") 0)
                                                 ((equal status "inconclusive") 3)
                                                 (t 1))))))
            (nelisp-dev-replay--result
             request (if valid status "failed")
             (cond (timed-out "timeout") (limited "output-limit")
                   (valid (cdr (assoc "phase" terminal))) (t "worker"))
             (cond (timed-out "NELISP-DEV-REPLAY-TIMEOUT")
                   (limited "NELISP-DEV-REPLAY-OUTPUT-LIMIT")
                   ((not valid) "NELISP-DEV-REPLAY-INCOMPLETE")
                   ((equal status "failed") "NELISP-DEV-REPLAY-FAILED"))
             (append (list (cons "executed_forms" (if valid count :null))
                           (cons "worker_exit_code" exit-code)
                           (cons "stdout" (nelisp-dev-replay--prefix stdout 1024))
                           (cons "stderr" (nelisp-dev-replay--prefix stderr 1024))
                           (cons "stdout_bytes" (string-bytes stdout))
                           (cons "stderr_bytes" (string-bytes stderr))
                           (cons "output_truncated"
                                 (if (or limited (> (string-bytes stdout) 1024)
                                         (> (string-bytes stderr) 1024)) t :false))
                           (cons "terminal" (if valid terminal :null)))
                     (list (cons "manifest_sha256" manifest-hash))))))
      (when (and process (process-live-p process)) (delete-process process))
      (when (and error-pipe (process-live-p error-pipe)) (delete-process error-pipe))
      (delete-directory scratch t))))

(defun nelisp-dev-replay-dispatch (request context)
  "Replay a validated host manifest only after an explicit effects policy."
  (let* ((arguments (cdr (assoc "arguments" request)))
         (manifest (cdr (assoc "manifest" arguments)))
         (policy (or (cdr (assoc "effects-policy" arguments))
                     (cdr (assoc "effects_policy" arguments))))
         (timeout (or (cdr (assoc "timeout" arguments)) 10))
         (target (cdr (assoc "target" request))))
    (cond
     ((or (not (equal (plist-get context :target) "host-emacs"))
          (not (or (eq target :null) (equal target "host-emacs"))))
      (nelisp-dev-replay--result request "unsupported" "target"
                                 "NELISP-DEV-REPLAY-TARGET" nil))
     ((not (and (stringp manifest) (> (length manifest) 0)
                (equal policy "explicit-only") (numberp timeout) (<= 1 timeout 60)
                (let ((cursor (cdr (assoc "cursor" (cdr (assoc "limits" request))))))
                  (or (null cursor) (eq cursor :null)))))
      (nelisp-dev-replay--result request "failed" "arguments"
                                 "NELISP-DEV-INVALID-REQUEST"
                                 '(("executed_forms" . 0))))
     (t
      (setq manifest (expand-file-name manifest (or (plist-get context :root)
                                                     default-directory)))
      (condition-case err
          (let ((validation (nelisp-dev-session--validate (list :manifest manifest))))
            (if (eq (plist-get validation :status) :valid)
                (nelisp-dev-replay--worker request manifest timeout)
              (nelisp-dev-replay--result
               request "failed" "validate" "NELISP-DEV-REPLAY-INVALID-MANIFEST"
               (list '("executed_forms" . 0)
                     (cons "validation" (nelisp-dev-protocol-value validation))))))
        (error
         (nelisp-dev-replay--result
          request "failed" "worker" "NELISP-DEV-REPLAY-WORKER-ERROR"
          (list (cons "condition" (symbol-name (car err)))))))))))

(provide 'nelisp-dev-replay)
