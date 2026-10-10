;;; nelisp-service-test.el --- ERT for nelisp-service -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 213.  Host-Emacs ERT for the shared plumbing, the worker pool and
;; the daemon/client pair.  Workers are real child processes (host Emacs
;; children here; the standalone smokes in tools/ cover NeLisp children).
;; The daemon tests run daemon and client in this one process: the
;; client's waits service the daemon's sockets too.

;;; Code:

(require 'ert)
(require 'nelisp-service)
(require 'nelisp-service-worker)
(require 'nelisp-service-daemon)
(require 'nelisp-service-client)

(defmacro nelisp-service-test--with-state-dir (&rest body)
  "Run BODY with a fresh, private state directory."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "nelisp-service-test-" t))
          (nelisp-service-state-directory dir))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

;;; Framing and files

(ert-deftest nelisp-service-test-encode-is-one-line ()
  (let* ((object (list 'res 1 "a\nb\r\"c\" 日本\\"))
         (line (nelisp-service-encode object)))
    (should (= 1 (cl-count ?\n line)))
    (should (equal (nelisp-service-decode (substring line 0 -1)) object))))

(ert-deftest nelisp-service-test-blob-round-trip-in-pieces ()
  (let* ((big (concat (make-string 400 ?x) "\"q\" \\ 日本\n\r" (make-string 400 ?y)))
         (wire (concat (nelisp-service-encode (list 'res 7 big))
                       (nelisp-service-encode '(pong (:n 1)))))
         (bytes (encode-coding-string wire 'utf-8 t))
         (reader (nelisp-service-reader-create))
         (got nil))
    ;; The long string travels as a blob, not as an escaped literal.
    (should (string-match-p "(nelisp-service-blob [0-9]+)" wire))
    ;; Feed the bytes in awkward pieces, splitting UTF-8 sequences too.
    (let ((i 0))
      (while (< i (length bytes))
        (let ((j (min (length bytes) (+ i 7))))
          (setq got (append got (nelisp-service-reader-feed
                                 reader (substring bytes i j))))
          (setq i j))))
    (should (equal got (list (list 'res 7 big) '(pong (:n 1)))))))

(ert-deftest nelisp-service-test-pool-returns-big-string ()
  (let ((pool (nelisp-service-test--pool :max-workers 1)))
    (unwind-protect
        (should (equal (concat (make-string 3000 ?a) "日本")
                       (nelisp-service-pool-call
                        pool '(concat (make-string 3000 ?a) "日本") 60)))
      (nelisp-service-pool-shutdown pool))))

(ert-deftest nelisp-service-test-decode-rejects-garbage ()
  (should (null (nelisp-service-decode "(unbalanced")))
  (should (null (nelisp-service-decode ""))))

(ert-deftest nelisp-service-test-splitter-joins-chunks ()
  (let ((s (nelisp-service-splitter-create)))
    (should (null (nelisp-service-splitter-feed s "ab")))
    (should (equal (nelisp-service-splitter-feed s "c\r\nd\ne") '("abc" "d")))
    (should (equal (nelisp-service-splitter-pending s) "e"))))

(ert-deftest nelisp-service-test-exclusive-plist-file ()
  (nelisp-service-test--with-state-dir
    (let ((file (nelisp-service-state-file "x" "lock")))
      (should (nelisp-service-write-plist file '(:a 1) t))
      (should-not (nelisp-service-write-plist file '(:a 2) t))
      (should (equal (nelisp-service-read-plist file) '(:a 1))))))

(ert-deftest nelisp-service-test-tokens-differ ()
  (should-not (equal (nelisp-service-make-token) (nelisp-service-make-token))))

(ert-deftest nelisp-service-test-script-command-binds-args ()
  (nelisp-service-test--with-state-dir
    (let* ((script (make-temp-file "nelisp-service-script-" nil ".el"
                                   "(princ (format \"%S\" nelisp-service-bootstrap-args))"))
           (cmd (nelisp-service-script-command script '(:name "n" :n 3))))
      (unwind-protect
          (with-temp-buffer
            (should (= 0 (apply #'call-process (car cmd) nil t nil (cdr cmd))))
            (should (string-match-p "(:name \"n\" :n 3)" (buffer-string))))
        (delete-file script)))))

;;; Worker pool

(defun nelisp-service-test--pool (&rest args)
  "Create a pool from ARGS with host-Emacs workers."
  (apply #'nelisp-service-pool-create :name "ert" args))

(ert-deftest nelisp-service-test-pool-call-and-errors ()
  (let ((pool (nelisp-service-test--pool :max-workers 1)))
    (unwind-protect
        (progn
          (should (= 3 (nelisp-service-pool-call pool '(+ 1 2) 60)))
          (should (equal "a\nb日本"
                         (nelisp-service-pool-call pool '(concat "a\nb" "日本") 60)))
          (should-error (nelisp-service-pool-call pool '(car 5) 60))
          ;; An unreadable value comes back as its printed string.
          (should (stringp (nelisp-service-pool-call pool '(current-buffer) 60)))
          ;; The worker survives a signalling request.
          (should (= 1 (plist-get (nelisp-service-pool-stats pool) :spawned))))
      (nelisp-service-pool-shutdown pool))))

(ert-deftest nelisp-service-test-pool-ceiling-and-recycle ()
  (let ((pool (nelisp-service-test--pool :max-workers 2 :recycle-after 2))
        (done 0) (peak 0))
    (unwind-protect
        (progn
          (dotimes (i 8)
            (nelisp-service-pool-submit pool `(* ,i 2)
                                        (lambda (_s _v) (setq done (1+ done)))))
          (should (nelisp-service-wait-until
                   (lambda ()
                     (setq peak (max peak (length (nelisp-service-pool-processes pool))))
                     (= done 8))
                   120))
          (should (<= peak 2))
          (should (>= (plist-get (nelisp-service-pool-stats pool) :retired) 2)))
      (nelisp-service-pool-shutdown pool))))

(ert-deftest nelisp-service-test-pool-crash-fails-only-inflight ()
  (let ((pool (nelisp-service-test--pool :max-workers 1))
        (result nil))
    (unwind-protect
        (progn
          (nelisp-service-pool-call pool 1 60)
          (nelisp-service-pool-submit pool '(while t (sleep-for 0.1))
                                      (lambda (s v) (setq result (list s v))))
          (should (nelisp-service-wait-until
                   (lambda () (nelisp-service-get
                               (car (nelisp-service-get pool :workers)) :busy))
                   30))
          (delete-process (car (nelisp-service-pool-processes pool)))
          (should (nelisp-service-wait-until (lambda () result) 30))
          (should (eq (car result) 'crashed))
          (should (= 42 (nelisp-service-pool-call pool '(+ 40 2) 60))))
      (nelisp-service-pool-shutdown pool))))

(ert-deftest nelisp-service-test-pool-version-replaces-workers ()
  (let ((pool (nelisp-service-test--pool :max-workers 1 :version 1)))
    (unwind-protect
        (progn
          (nelisp-service-pool-call pool 1 60)
          (nelisp-service-pool-set-version pool 2)
          (nelisp-service-pool-call pool 2 60)
          (should (= 2 (plist-get (nelisp-service-pool-stats pool) :spawned))))
      (nelisp-service-pool-shutdown pool))))

(ert-deftest nelisp-service-test-pool-init-forms ()
  (let ((pool (nelisp-service-test--pool
               :max-workers 1 :init-forms '((defvar nelisp-service-test--seed 7)))))
    (unwind-protect
        (should (= 7 (nelisp-service-pool-call pool 'nelisp-service-test--seed 60)))
      (nelisp-service-pool-shutdown pool))))

(ert-deftest nelisp-service-test-pool-gives-up-on-broken-command ()
  (let ((pool (nelisp-service-test--pool
               :max-workers 1
               :command (list (nelisp-service-self-command) "--batch" "-Q"
                              "--eval" "(kill-emacs 3)")))
        (result nil))
    (unwind-protect
        (progn
          (nelisp-service-pool-submit pool 1 (lambda (s _v) (setq result s)))
          (should (nelisp-service-wait-until (lambda () result) 60))
          (should (eq result 'spawn-failed)))
      (nelisp-service-pool-shutdown pool))))

(ert-deftest nelisp-service-test-pool-init-error-fails-with-reason ()
  (let ((pool (nelisp-service-test--pool
               :max-workers 1 :init-forms '((error "No dictionary here"))))
        (result nil))
    (unwind-protect
        (progn
          (nelisp-service-pool-submit pool 1 (lambda (s v) (setq result (list s v))))
          (should (nelisp-service-wait-until (lambda () result) 120))
          (should (eq (car result) 'spawn-failed))
          (should (string-match-p "No dictionary here" (cadr result)))
          (should (= 3 (plist-get (nelisp-service-pool-stats pool) :spawned))))
      (nelisp-service-pool-shutdown pool))))

(ert-deftest nelisp-service-test-worker-exits-on-stdin-eof ()
  (let ((pool (nelisp-service-test--pool :max-workers 1)))
    (nelisp-service-pool-call pool 1 60)
    (let ((proc (car (nelisp-service-pool-processes pool))))
      (process-send-eof proc)
      (should (nelisp-service-wait-until
               (lambda () (not (process-live-p proc))) 30)))
    (nelisp-service-pool-shutdown pool)))

;;; Daemon and client

(defun nelisp-service-test--echo (_conn payload reply)
  "Test handler: echo PAYLOAD back, prefixed."
  (funcall reply (if (stringp payload) (concat "echo:" payload) payload)))

(ert-deftest nelisp-service-test-daemon-round-trip-and-sharing ()
  (nelisp-service-test--with-state-dir
    (let ((daemon (nelisp-service-daemon-start
                   "t1" :handler #'nelisp-service-test--echo :version "v1")))
      (unwind-protect
          (let ((a (nelisp-service-client-connect "t1" :version "v1" :timeout 20))
                (b (nelisp-service-client-connect "t1" :version "v1" :timeout 20)))
            (should (equal "echo:a\nb日本"
                           (nelisp-service-client-request a "a\nb日本" 20)))
            (should (= 2 (plist-get (nelisp-service-client-ping b) :connections)))
            (nelisp-service-client-close a)
            (nelisp-service-client-close b))
        (nelisp-service-daemon-close daemon)))))

(ert-deftest nelisp-service-test-daemon-single-instance ()
  (nelisp-service-test--with-state-dir
    (let ((first (nelisp-service-daemon-start "t2" :handler #'ignore)))
      (unwind-protect
          (should-not (nelisp-service-daemon-start "t2" :handler #'ignore))
        (nelisp-service-daemon-close first)))))

(ert-deftest nelisp-service-test-daemon-rejects-bad-token ()
  (nelisp-service-test--with-state-dir
    (let ((daemon (nelisp-service-daemon-start "t3" :handler #'ignore)))
      (unwind-protect
          (should (eq 'token
                      (nelisp-service-client--open
                       "t3" (list :port (nelisp-service-get daemon :port)
                                  :token "wrong")
                       nil)))
        (nelisp-service-daemon-close daemon)))))

(ert-deftest nelisp-service-test-daemon-version-mismatch-stops-daemon ()
  (nelisp-service-test--with-state-dir
    (let ((daemon (nelisp-service-daemon-start
                   "t4" :handler #'ignore :version "old")))
      (unwind-protect
          (let ((state (nelisp-service-read-plist
                        (nelisp-service-state-file "t4" "state"))))
            ;; A client older than the daemon (a session left running
            ;; across an update) is served, and the daemon stays.
            (let ((nelisp-service-client--started (- (float-time) 1000)))
              (let ((old (nelisp-service-client--open "t4" state "older")))
                (should (and old (not (symbolp old))))
                (nelisp-service-client-close old)))
            (should-not (nelisp-service-get daemon :stopped))
            ;; A client newer than the daemon replaces it.
            (let ((nelisp-service-client--started (+ (float-time) 1000)))
              (should (eq 'version
                          (nelisp-service-client--open "t4" state "new"))))
            (should (nelisp-service-get daemon :stopped)))
        (nelisp-service-daemon-close daemon)))))

(ert-deftest nelisp-service-test-daemon-handler-error-reaches-client ()
  (nelisp-service-test--with-state-dir
    (let ((daemon (nelisp-service-daemon-start
                   "t5" :handler (lambda (_c _p _r) (error "Boom")))))
      (unwind-protect
          (let ((client (nelisp-service-client-connect "t5" :timeout 20)))
            (should (string-match-p "Boom"
                                    (cadr (should-error
                                           (nelisp-service-client-request
                                            client "x" 20)))))
            (nelisp-service-client-close client))
        (nelisp-service-daemon-close daemon)))))

(ert-deftest nelisp-service-test-daemon-idle-exit ()
  (nelisp-service-test--with-state-dir
    (let ((daemon (nelisp-service-daemon-start
                   "t6" :handler #'ignore :idle-timeout 0.5)))
      (should (eq 'idle (nelisp-service-daemon-run daemon 0.05)))
      (should-not (file-exists-p (nelisp-service-state-file "t6" "lock")))
      (should-not (file-exists-p (nelisp-service-state-file "t6" "state"))))))

(ert-deftest nelisp-service-test-stale-lock-is-reclaimed ()
  (nelisp-service-test--with-state-dir
    (nelisp-service-write-plist (nelisp-service-state-file "t7" "lock")
                                (list :started (- (float-time) 1000)) t)
    (let ((daemon (nelisp-service-daemon-start "t7" :handler #'ignore
                                               :stale-after 10)))
      (should daemon)
      (nelisp-service-daemon-close daemon))))

(ert-deftest nelisp-service-test-client-autostarts-daemon ()
  (nelisp-service-test--with-state-dir
    (let* ((script (expand-file-name
                    "../bin/nelisp-service-echo-daemon.el"
                    (file-name-directory
                     (locate-library "nelisp-service-worker"))))
           (client (nelisp-service-client-connect
                    "auto" :version "v"
                    :start-command (nelisp-service-script-command
                                    script '(:name "auto" :version "v"
                                             :idle-timeout 30))
                    :timeout 60)))
      (should (equal "echo:x" (nelisp-service-client-request client "x" 20)))
      (should (nelisp-service-client-shutdown client))
      (should (nelisp-service-wait-until
               (lambda () (not (file-exists-p
                                (nelisp-service-state-file "auto" "lock"))))
               20)))))

(ert-deftest nelisp-service-test-early-lock-and-lock-held ()
  (nelisp-service-test--with-state-dir
    (should (nelisp-service-daemon-acquire-lock "t8"))
    ;; A second starter loses while the first is still loading.
    (should-not (nelisp-service-daemon-acquire-lock "t8"))
    (should-not (nelisp-service-daemon-start "t8" :handler #'ignore))
    (let ((daemon (nelisp-service-daemon-start "t8" :handler #'ignore
                                               :lock-held t)))
      (should daemon)
      (nelisp-service-daemon-close daemon)
      (should-not (file-exists-p (nelisp-service-state-file "t8" "lock"))))))

(ert-deftest nelisp-service-test-ensure-started-spawns-once ()
  (nelisp-service-test--with-state-dir
    (let* ((spawned 0)
           (nelisp-service-client-spawn-function
            (lambda (_name _command) (setq spawned (1+ spawned)))))
      (should (nelisp-service-client-ensure-started "t9" :start-command '("x")))
      (should (= 1 spawned))
      ;; A lock means a daemon is starting: do not spawn another.
      (nelisp-service-daemon-acquire-lock "t9")
      (should-not (nelisp-service-client-ensure-started "t9" :start-command '("x")))
      (should (= 1 spawned)))))

(ert-deftest nelisp-service-test-mcp-framing-parse ()
  (should (= 12 (nelisp-service-client--content-length "Content-Length: 12")))
  (should (= 7 (nelisp-service-client--content-length "content-length:7")))
  (should-not (nelisp-service-client--content-length "{\"jsonrpc\":\"2.0\"}")))

(provide 'nelisp-service-test)

;;; nelisp-service-test.el ends here
