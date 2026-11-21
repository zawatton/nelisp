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

(defvar nelisp-service--retirement-metrics-fixture nil)
(defvar nelisp-service--retirement-metrics-active
  (equal (getenv "NELISP_SERVICE_RETIREMENT_METRICS") "1"))
(defvar nelisp-service--retirement-metrics
  (let ((metrics (make-hash-table :test 'eq)))
    (dolist (key '(eof forced direct max_eof_ms max_forced_ms))
      (puthash key 0 metrics))
    metrics))

(defun nelisp-service-test--retirement-metrics-around (original &rest args)
  "Call ORIGINAL once and record its result and elapsed time."
  (if (or (not nelisp-service--retirement-metrics-active)
          nelisp-service--retirement-metrics-fixture)
      (apply original args)
    (let ((started (float-time))
          (result nil)
          (failure nil))
      (condition-case err
          (setq result (apply original args))
        (error (setq failure err)))
      (let ((elapsed-ms (* 1000.0 (- (float-time) started))))
        (when (memq result '(eof forced direct))
          (puthash result (1+ (gethash result nelisp-service--retirement-metrics 0))
                   nelisp-service--retirement-metrics)
          (when (memq result '(eof forced))
            (let ((key (if (eq result 'eof) 'max_eof_ms 'max_forced_ms)))
              (puthash key (max elapsed-ms
                                 (gethash key nelisp-service--retirement-metrics 0))
                       nelisp-service--retirement-metrics)))))
      (if failure
          (signal (car failure) (cdr failure))
        result))))

(defun nelisp-service-test--retirement-metrics-report ()
  "Print one aggregate line for the optional retirement diagnostic."
  (condition-case nil
      (message
       "RETIREMENT_METRICS eof=%d forced=%d direct=%d max_eof_ms=%.1f max_forced_ms=%.1f"
       (gethash 'eof nelisp-service--retirement-metrics 0)
       (gethash 'forced nelisp-service--retirement-metrics 0)
       (gethash 'direct nelisp-service--retirement-metrics 0)
       (gethash 'max_eof_ms nelisp-service--retirement-metrics 0)
       (gethash 'max_forced_ms nelisp-service--retirement-metrics 0))
    (error nil)))

(when (and nelisp-service--retirement-metrics-active
           (fboundp 'advice-add))
  (advice-add 'nelisp-service-retire-connection :around
              #'nelisp-service-test--retirement-metrics-around)
  (add-hook 'kill-emacs-hook
            #'nelisp-service-test--retirement-metrics-report))

(defmacro nelisp-service-test--with-state-dir (&rest body)
  "Run BODY with a fresh, private state directory."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "nelisp-service-test-" t))
          (nelisp-service-state-directory dir))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defmacro nelisp-service-test--with-fake-network-process (&rest body)
  "Run BODY with a deterministic fake network process named PROCESS."
  (declare (indent 0))
  `(let* ((process (make-symbol "fake-network-process"))
          (nelisp-service--retirement-metrics-fixture t)
          (status 'open)
          (kind 'network)
          (delete-count 0)
          (eof-count 0)
          (delete-observed-status nil)
          (process-on-delete nil))
     (cl-letf (((symbol-function 'processp)
                (lambda (candidate) (eq candidate process)))
               ((symbol-function 'process-type)
                (lambda (candidate) (and (eq candidate process) kind)))
               ((symbol-function 'process-status)
                (lambda (candidate) (and (eq candidate process) status)))
               ((symbol-function 'process-live-p)
                (lambda (candidate)
                  (and (eq candidate process) (memq status '(open run)))))
               ((symbol-function 'process-send-eof)
                (lambda (candidate)
                  (when (eq candidate process)
                    (setq eof-count (1+ eof-count)))))
               ((symbol-function 'delete-process)
                (lambda (candidate)
                  (when (eq candidate process)
                    (setq delete-count (1+ delete-count)
                          delete-observed-status status
                          status 'closed)
                    (when process-on-delete (funcall process-on-delete)))))
               ((symbol-function 'accept-process-output)
                (lambda (&rest _args) nil)))
       ,@body)))

(ert-deftest nelisp-service-test-retirement-drains-idle-eof-before-delete ()
  (nelisp-service-test--with-fake-network-process
    (let ((system-type 'windows-nt)
          (waits 0)
          (max-slice 0))
      (cl-letf (((symbol-function 'accept-process-output)
                 (lambda (_process seconds)
                   (setq waits (1+ waits)
                         max-slice (max max-slice seconds)
                         status 'closed)
                   (should (= 1 eof-count))
                   t)))
        (should (eq 'eof (nelisp-service-retire-connection process)))
        (should (= waits 1))
        (should (= 1 eof-count))
        (should (<= max-slice 0.05))
        (should (= 1 delete-count))
        (should (eq 'closed delete-observed-status))
        (should-not (process-live-p process))))))

(ert-deftest nelisp-service-test-retirement-times-out-and-deletes-process ()
  (nelisp-service-test--with-fake-network-process
    (let ((now 1000.0)
          (waits 0)
          (max-slice 0))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _time) now))
                ((symbol-function 'accept-process-output)
                 (lambda (_process seconds)
                   (setq waits (1+ waits)
                         max-slice (max max-slice seconds)
                         now (+ now seconds))
                   nil)))
        (let ((system-type 'windows-nt))
          (should (eq 'forced
                      (nelisp-service-retire-connection process 0.2)))
          (should (<= 4 waits 5))
          (should (< (abs (- now 1000.2)) 0.000001))
          (should (<= max-slice 0.05))
          (should (= 1 eof-count))
          (should (= 1 delete-count))
          (should-not (process-live-p process)))))))

(ert-deftest nelisp-service-test-retirement-clamps-deadline-crossing-sample ()
  (nelisp-service-test--with-fake-network-process
    (let ((clock-values '(1000.0 1000.19 1000.21 1000.21))
          (wait-slices nil))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _time) (pop clock-values)))
                ((symbol-function 'accept-process-output)
                 (lambda (_process seconds) (push seconds wait-slices) nil)))
        (let ((system-type 'windows-nt))
          (should (eq 'forced
                      (nelisp-service-retire-connection process 0.2)))
          (should (equal '(0) (nreverse wait-slices)))
          (should (= 1 eof-count))
          (should (= 1 delete-count)))))))

(ert-deftest nelisp-service-test-retirement-wait-error-still-deletes-process ()
  (nelisp-service-test--with-fake-network-process
    (let ((system-type 'windows-nt)
          (waits 0))
      (cl-letf (((symbol-function 'accept-process-output)
                 (lambda (&rest _args)
                   (setq waits (1+ waits))
                   (error "simulated process wait failure"))))
        (should (eq 'forced
                    (nelisp-service-retire-connection process 0.2)))
        (should (= 1 waits))
        (should (= 1 eof-count))
        (should (= 1 delete-count))
        (should-not (process-live-p process))))))

(ert-deftest nelisp-service-test-retirement-eof-error-still-deletes-process ()
  (nelisp-service-test--with-fake-network-process
    (let ((system-type 'windows-nt)
          (waits 0))
      (cl-letf (((symbol-function 'process-send-eof)
                 (lambda (_process) (error "simulated EOF send failure")))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _args) (setq waits (1+ waits)) nil)))
        (should (eq 'forced
                    (nelisp-service-retire-connection process 0.2)))
        (should (= 0 waits))
        (should (= 1 delete-count))
        (should-not (process-live-p process))))))

(ert-deftest nelisp-service-test-retirement-bypasses-ineligible-processes ()
  (let ((waits 0)
        (type-queries 0)
        (eof-sends 0))
    (cl-letf (((symbol-function 'accept-process-output)
               (lambda (&rest _args) (setq waits (1+ waits)) nil)))
      (nelisp-service-test--with-fake-network-process
        (let ((system-type 'gnu/linux))
          (cl-letf (((symbol-function 'process-type)
                     (lambda (&rest _args)
                       (setq type-queries (1+ type-queries)) 'network))
                    ((symbol-function 'process-send-eof)
                     (lambda (&rest _args) (setq eof-sends (1+ eof-sends))))
                    ((symbol-function 'accept-process-output)
                     (lambda (&rest _args) (setq waits (1+ waits)))))
            (should (eq 'direct (nelisp-service-retire-connection process))))
          (should-not (process-live-p process))
          (should (= 0 type-queries))
          (should (= 0 eof-sends))
          (should (= 0 waits)))
      (nelisp-service-test--with-fake-network-process
        (let ((system-type 'windows-nt))
          (cl-letf (((symbol-function 'read-stdin-bytes) #'ignore)
                    ((symbol-function 'process-type)
                     (lambda (&rest _args)
                       (setq type-queries (1+ type-queries)) 'network))
                    ((symbol-function 'process-send-eof)
                     (lambda (&rest _args) (setq eof-sends (1+ eof-sends))))
                    ((symbol-function 'accept-process-output)
                     (lambda (&rest _args) (setq waits (1+ waits)))))
            (should (eq 'direct (nelisp-service-retire-connection process)))
            (should-not (process-live-p process))
            (should (= 0 type-queries))
            (should (= 0 eof-sends))
            (should (= 0 waits))))))
      (nelisp-service-test--with-fake-network-process
        (let ((system-type 'windows-nt))
          (cl-letf (((symbol-function 'process-type) (lambda (_proc) 'pipe)))
            (should (eq 'direct (nelisp-service-retire-connection process)))
            (should-not (process-live-p process)))))
      (nelisp-service-test--with-fake-network-process
        (let ((system-type 'windows-nt))
          (setq status 'closed)
          (should (eq 'direct (nelisp-service-retire-connection process)))))
      (nelisp-service-test--with-fake-network-process
        (let ((system-type 'windows-nt))
          (should (eq 'direct (nelisp-service-retire-connection process 0)))
          (should-not (process-live-p process)))))
    (should (= 0 waits))))

(ert-deftest nelisp-service-test-retirement-reentrancy-defers-nested-delete ()
  (nelisp-service-test--with-fake-network-process
    (let ((system-type 'windows-nt)
          (nested nil)
          (waits 0))
      (cl-letf (((symbol-function 'accept-process-output)
                 (lambda (_process _seconds)
                   (setq waits (1+ waits))
                   (setq nested (nelisp-service-retire-connection process 0))
                   (should (process-live-p process))
                   (setq status 'closed)
                   nil)))
        (should (eq 'eof (nelisp-service-retire-connection process)))
        (should (eq 'direct nested))
        (should (= waits 1))
        (should (= 1 delete-count))
        (should-not (process-live-p process))))))

(ert-deftest nelisp-service-test-client-close-marks-idle-handle-before-pump ()
  (nelisp-service-test--with-fake-network-process
    (let* ((client-record
            (nelisp-service-record 'nelisp-service-client
                                   :process process :pending (make-hash-table)))
           (system-type 'windows-nt)
           (waits 0))
      (cl-letf (((symbol-function 'accept-process-output)
                 (lambda (_process _seconds)
                   (setq waits (1+ waits))
                   (should (nelisp-service-get client-record :closed))
                   (should (= 1 eof-count))
                   (setq status 'closed)
                   nil)))
        (nelisp-service-client-close client-record)
        (should (nelisp-service-get client-record :closed))
        (should (= 1 waits))
        (should (eq 'closed delete-observed-status))
        (should-not (process-live-p process))))))

(ert-deftest nelisp-service-test-client-close-with-pending-skips-pump ()
  (nelisp-service-test--with-fake-network-process
    (let* ((calls 0)
           (reply-status nil)
           (client-record
            (nelisp-service-record 'nelisp-service-client
                                   :process process :pending (make-hash-table)))
           (system-type 'windows-nt))
      (puthash 1 (lambda (result _value)
                   (setq calls (1+ calls) reply-status result))
               (nelisp-service-get client-record :pending))
      (setq process-on-delete
            (lambda () (nelisp-service-client--on-close client-record process)))
      (cl-letf (((symbol-function 'accept-process-output)
                 (lambda (&rest _args) (error "pending close must not pump"))))
        (nelisp-service-client-close client-record))
      (should (= 1 calls))
      (should (eq 'disconnected reply-status))
      (should (nelisp-service-get client-record :closed))
      (should (= 1 delete-count))
      (should-not (process-live-p process)))))

(ert-deftest nelisp-service-test-open-failure-retires-created-process-once ()
  (nelisp-service-test--with-fake-network-process
    (let ((retired nil))
      (cl-letf (((symbol-function 'make-network-process)
                 (lambda (&rest _args) process))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _args) (error "simulated handshake send failure")))
                ((symbol-function 'nelisp-service-retire-connection)
                 (lambda (candidate &optional _wait)
                   (should (eq process candidate))
                   (setq retired (1+ (or retired 0)))
                   'direct)))
        (should-not
         (nelisp-service-client--open
          "failed-open" (list :port 12345 :token "token") nil))
        (should (= 1 retired))
        (should (= 0 delete-count))))))

(ert-deftest nelisp-service-test-port-probe-retires-created-process ()
  (nelisp-service-test--with-fake-network-process
    (let ((retired nil))
      (cl-letf (((symbol-function 'make-network-process)
                 (lambda (&rest _args) process))
                ((symbol-function 'nelisp-service-retire-connection)
                 (lambda (candidate &optional _wait)
                   (should (eq process candidate))
                   (setq retired t)
                   'direct)))
        (should (nelisp-service-port-in-use-p 12345))
        (should retired)
        (should (= 0 delete-count))))))

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

(ert-deftest nelisp-service-test-script-command-can-be-replayed ()
  (let* ((dir (make-temp-file "nelisp-service-replay-" t))
         (nelisp-service-state-directory dir)
         (script (make-temp-file
                  "nelisp-service-replay-script-" nil ".el"
                  "(princ (format \"%S\\n%S\\n\" nelisp-service-bootstrap-args nelisp-service-state-directory))"))
         (args '(:name "replay" :values (1 "two")))
         (command (nelisp-service-script-command script args))
         (bootstrap (car (last command)))
         (first-output nil))
    (unwind-protect
        (progn
          (should (file-exists-p bootstrap))
          (with-temp-buffer
            (should (= 0 (apply #'call-process
                                (car command) nil t nil (cdr command))))
            (setq first-output (buffer-string)))
          (should (file-exists-p bootstrap))
          (with-temp-buffer
            (should (= 0 (apply #'call-process
                                (car command) nil t nil (cdr command))))
            (should (equal first-output (buffer-string))))
          (should (string-match-p (regexp-quote (format "%S" args))
                                  first-output))
          (should (string-match-p (regexp-quote (format "%S" dir))
                                  first-output)))
      (delete-file script)
      (delete-directory dir t))
    (should-not (file-exists-p dir))))

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

(ert-deftest nelisp-service-test-open-drains-reply-seen-after-close ()
  "A rejection whose close is observed first is still reported."
  (nelisp-service-test--with-state-dir
    (let ((daemon (nelisp-service-daemon-start
                   "t5" :handler #'ignore :version "old"))
          (held nil)
          (real-input (symbol-function 'nelisp-service-client--on-input))
          (real-close (symbol-function 'nelisp-service-client--on-close)))
      (unwind-protect
          (let ((state (nelisp-service-read-plist
                        (nelisp-service-state-file "t5" "state")))
                (nelisp-service-client--started (+ (float-time) 1000)))
            ;; Hold every chunk back; deliver it from a timer once the
            ;; sentinel has run, so the close is seen before the reply.
            (cl-letf (((symbol-function 'nelisp-service-client--on-input)
                       (lambda (client chunk) (push (cons client chunk) held)))
                      ((symbol-function 'nelisp-service-client--on-close)
                       (lambda (client proc)
                         (funcall real-close client proc)
                         (run-at-time 0.05 nil
                                      (lambda ()
                                        (dolist (h (nreverse held))
                                          (funcall real-input (car h) (cdr h))))))))
              (should (eq 'version
                          (nelisp-service-client--open "t5" state "new")))))
        (nelisp-service-daemon-close daemon)))))

(ert-deftest nelisp-service-test-open-drain-survives-unrelated-wakeups ()
  "A delayed rejection survives immediate unrelated process wakeups."
  (nelisp-service-test--with-state-dir
    (let ((daemon (nelisp-service-daemon-start
                   "t6" :handler #'ignore :version "old"))
          (held nil)
          (closed nil)
          (drain-wakeups 0)
          (real-input (symbol-function 'nelisp-service-client--on-input))
          (real-close (symbol-function 'nelisp-service-client--on-close))
          (real-accept (symbol-function 'accept-process-output)))
      (unwind-protect
          (let ((state (nelisp-service-read-plist
                        (nelisp-service-state-file "t6" "state")))
                (nelisp-service-client--started (+ (float-time) 1000)))
            (cl-letf (((symbol-function 'nelisp-service-client--on-input)
                       (lambda (client chunk)
                         (push (cons client chunk) held)))
                      ((symbol-function 'nelisp-service-client--on-close)
                       (lambda (client proc)
                         (funcall real-close client proc)
                         (setq closed t)))
                      ((symbol-function 'accept-process-output)
                       (lambda (&rest args)
                         (if (not closed)
                             (apply real-accept args)
                           (setq drain-wakeups (1+ drain-wakeups))
                           (when (= drain-wakeups 4)
                             (dolist (h (nreverse held))
                               (funcall real-input (car h) (cdr h)))
                             (setq held nil))
                           t))))
              (should (eq 'version
                          (nelisp-service-client--open "t6" state "new")))
              (should (>= drain-wakeups 4))))
        (nelisp-service-daemon-close daemon)))))

(ert-deftest nelisp-service-test-open-does-not-return-closed-welcome ()
  "A welcome drained after close must not return a dead client."
  (nelisp-service-test--with-state-dir
    (let ((daemon (nelisp-service-daemon-start
                   "t7" :handler #'ignore :version "v1"))
          (held nil)
          (closed nil)
          (closed-client nil)
          (welcome-decoded nil)
          (real-input (symbol-function 'nelisp-service-client--on-input))
          (real-close (symbol-function 'nelisp-service-client--on-close))
          (real-accept (symbol-function 'accept-process-output)))
      (unwind-protect
          (let ((state (nelisp-service-read-plist
                        (nelisp-service-state-file "t7" "state"))))
            (cl-letf (((symbol-function 'nelisp-service-client--on-input)
                       (lambda (client chunk)
                         (push (cons client chunk) held)
                         (delete-process
                          (nelisp-service-get client :process))))
                      ((symbol-function 'nelisp-service-client--on-close)
                       (lambda (client proc)
                         (funcall real-close client proc)
                         (setq closed-client client)
                         (setq closed t)))
                      ((symbol-function 'accept-process-output)
                       (lambda (&rest args)
                         (if (not closed)
                             (apply real-accept args)
                           (dolist (h (nreverse held))
                             (funcall real-input (car h) (cdr h)))
                           (setq welcome-decoded
                                 (nelisp-service-get (caar held) :welcome))
                           (setq held nil)
                           t))))
              (let ((result (nelisp-service-client--open "t7" state "v1")))
                (should closed)
                (should (nelisp-service-get closed-client :closed))
                (should welcome-decoded)
                (should-not result))))
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

;; Intermittent Windows failure: `process-send-string' signalled on a
;; request frame.  Nothing reached the daemon, so one resend is safe.
(defmacro nelisp-service-test--with-failing-req-send (failures &rest body)
  "Run BODY with the first FAILURES request sends signalling an error.
Hello frames are untouched.  Bind `req-sends' (all request sends seen) in BODY."
  (declare (indent 1))
  `(let ((req-sends 0)
         (real (symbol-function 'process-send-string)))
     (cl-letf (((symbol-function 'process-send-string)
                (lambda (proc string)
                  (if (not (string-prefix-p "(req " string))
                      (funcall real proc string)
                    (setq req-sends (1+ req-sends))
                    (when (<= req-sends ,failures)
                      (signal 'file-error '("Process send failed" "boom")))
                    (funcall real proc string)))))
       ,@body)))

(ert-deftest nelisp-service-test-request-resends-once-after-send-failure ()
  (nelisp-service-test--with-state-dir
    (let* ((runs 0)
           (daemon (nelisp-service-daemon-start
                    "r1" :handler (lambda (_c payload reply)
                                    (setq runs (1+ runs))
                                    (funcall reply (concat "ok:" payload)))))
           (client nil))
      (unwind-protect
          (progn
            (setq client (nelisp-service-client-connect "r1" :timeout 20))
            (nelisp-service-test--with-failing-req-send 1
              (should (equal "ok:x" (nelisp-service-client-request client "x" 20)))
              (should (= 2 req-sends)))
            (should (= 1 runs))
            ;; The old handle forwards to the fresh connection afterwards.
            (should (nelisp-service-client-live-p client))
            (should (equal "ok:y" (nelisp-service-client-request client "y" 20))))
        (when client (nelisp-service-client-close
                      (nelisp-service-client--current client)))
        (nelisp-service-daemon-close daemon)))))

(ert-deftest nelisp-service-test-superseded-handle-follows-successor ()
  (nelisp-service-test--with-state-dir
    (let* ((daemon (nelisp-service-daemon-start
                    "r3" :handler (lambda (_c payload reply)
                                    (funcall reply (concat "ok:" payload)))))
           (client nil))
      (unwind-protect
          (progn
            (setq client (nelisp-service-client-connect "r3" :timeout 20))
            (nelisp-service-test--with-failing-req-send 1
              (should (equal "ok:x" (nelisp-service-client-request client "x" 20))))
            (should (nelisp-service-get client :successor))
            (should (nelisp-service-client-ping client 10))
            (should (nelisp-service-client-shutdown client 10))
            (nelisp-service-client-close client)
            (nelisp-service-client-close client))
        (when client (nelisp-service-client-close client))
        (nelisp-service-daemon-close daemon)))))

(ert-deftest nelisp-service-test-request-second-send-failure-surfaces ()
  (nelisp-service-test--with-state-dir
    (let* ((runs 0)
           (daemon (nelisp-service-daemon-start
                    "r2" :handler (lambda (_c _p reply)
                                    (setq runs (1+ runs))
                                    (funcall reply "ok"))))
           (client nil))
      (unwind-protect
          (progn
            (setq client (nelisp-service-client-connect "r2" :timeout 20))
            (nelisp-service-test--with-failing-req-send 2
              (let ((err (should-error (nelisp-service-client-request
                                        client "x" 20))))
                (should (string-match-p "disconnected: send failed: .*boom"
                                        (cadr err)))
                (should (eq 'disconnected (nth 2 err)))
                (should (= 2 req-sends))))
            (should (= 0 runs)))
        (when client (nelisp-service-client-close
                      (nelisp-service-client--current client)))
        (nelisp-service-daemon-close daemon)))))

(ert-deftest nelisp-service-test-request-not-resent-after-successful-send ()
  (nelisp-service-test--with-state-dir
    (let* ((daemon (nelisp-service-daemon-start
                    "r3" :handler (lambda (_c _p reply) (funcall reply "ok"))))
           (client nil)
           (req-sends 0)
           (real (symbol-function 'process-send-string)))
      (unwind-protect
          (progn
            (setq client (nelisp-service-client-connect "r3" :timeout 20))
            ;; The send succeeds, then the connection is lost before a reply.
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (proc string)
                         (funcall real proc string)
                         (when (string-prefix-p "(req " string)
                           (setq req-sends (1+ req-sends))
                           (delete-process proc)))))
              (let ((err (should-error (nelisp-service-client-request
                                        client "x" 20))))
                (should (string-match-p "connection to daemon closed"
                                        (cadr err)))
                (should-not (string-match-p "send failed" (cadr err)))))
            (should (= 1 req-sends)))
        (when client (nelisp-service-client-close client))
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

;; A daemon that misses the hello timeout but still accepts connections
;; is busy, not dead: the client must not clear its files and spawn a
;; second one (Doc 213; three anvil daemons were seen running at once).
(ert-deftest nelisp-service-test-busy-daemon-is-not-replaced ()
  (nelisp-service-test--with-state-dir
    (let* ((mute (make-network-process
                  :name "nelisp-service-test-mute" :server t :host "127.0.0.1"
                  ;; An ephemeral port: a fixed one collides when several
                  ;; copies of this test run at once (windows-service-loop).
                  :service t :family 'ipv4 :noquery t
                  :filter #'ignore))
           (port (process-contact mute :service))
           (spawned 0)
           (nelisp-service-client-hello-timeout 0.3)
           (nelisp-service-client-spawn-function
            (lambda (_name _command) (setq spawned (1+ spawned)))))
      (unwind-protect
          (progn
            (nelisp-service-write-plist
             (nelisp-service-state-file "t10" "lock")
             (list :started (- (float-time) 1000)) t)
            (nelisp-service-write-plist
             (nelisp-service-state-file "t10" "state")
             (list :port port :token "tok" :version "v"))
            (should-error (nelisp-service-client-connect
                           "t10" :version "v" :start-command '("x")
                           :stale-after 1 :timeout 2))
            (should (= 0 spawned))
            (should (file-exists-p (nelisp-service-state-file "t10" "state")))
            (should (file-exists-p (nelisp-service-state-file "t10" "lock"))))
        (delete-process mute)))))

(ert-deftest nelisp-service-test-respawns-after-published-state-disappears ()
  (nelisp-service-test--with-state-dir
    (let* ((now 1000.0)
           (spawned 0)
           (opens 0)
           (state-file (nelisp-service-state-file "recovery" "state"))
           (lock-file (nelisp-service-state-file "recovery" "lock"))
           (replacement (cons 'fake-client nil))
           (nelisp-service-client-spawn-function
            (lambda (_name _command)
              (setq spawned (1+ spawned))
              (nelisp-service-write-plist lock-file (list :started now))
              (nelisp-service-write-plist
               state-file (list :port 12345 :token (number-to-string spawned)))))
           (nelisp-service-client-hello-timeout 0.01))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _time) now))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _args)
                   (setq now (+ now 0.1))
                   (when (and (= opens 1)
                              (file-exists-p state-file)
                              (file-exists-p lock-file))
                     (delete-file state-file)
                     (delete-file lock-file))
                   nil))
                ((symbol-function 'nelisp-service-client--lock-age)
                 (lambda (_name) (and (file-exists-p lock-file) 0)))
                ((symbol-function 'nelisp-service-port-in-use-p)
                 (lambda (_port) nil))
                ((symbol-function 'nelisp-service-client--open)
                 (lambda (_name _state _version)
                   (setq opens (1+ opens))
                   (when (= opens 2) replacement)))
                ((symbol-function 'nelisp-service-put) (lambda (&rest _args) nil)))
        (let ((result
               (condition-case nil
                   (nelisp-service-client-connect
                    "recovery" :version "v" :start-command '("unused")
                    :timeout 1 :stale-after 10)
                 (error nil))))
          (should (= 2 spawned))
          (should (= 2 opens))
          (should (eq replacement result)))))))

(ert-deftest nelisp-service-test-unpublished-start-keeps-cooldown ()
  (nelisp-service-test--with-state-dir
    (let* ((now 1000.0)
           (spawned 0)
           (nelisp-service-client-spawn-function
            (lambda (_name _command) (setq spawned (1+ spawned)))))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _time) now))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _args) (setq now (+ now 0.1)) nil))
                ((symbol-function 'nelisp-service-client--lock-age)
                 (lambda (_name) nil)))
        (should-error
         (nelisp-service-client-connect
          "unpublished" :start-command '("unused")
          :timeout 0.5 :stale-after 10))
        (should (= 1 spawned))
        (should-not (file-exists-p
                     (nelisp-service-state-file "unpublished" "state")))
        (should-not (file-exists-p
                     (nelisp-service-state-file "unpublished" "lock")))))))

(ert-deftest nelisp-service-test-disappeared-state-with-lock-keeps-cooldown ()
  (nelisp-service-test--with-state-dir
    (let* ((now 1000.0)
           (spawned 0)
           (opened nil)
           (state-file (nelisp-service-state-file "locked" "state"))
           (lock-file (nelisp-service-state-file "locked" "lock"))
           (nelisp-service-client-spawn-function
            (lambda (_name _command)
              (setq spawned (1+ spawned))
              (nelisp-service-write-plist lock-file (list :started now))
              (nelisp-service-write-plist
               state-file (list :port 12345 :token "first")))))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _time) now))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _args) (setq now (+ now 0.1)) nil))
                ((symbol-function 'nelisp-service-client--lock-age)
                 (lambda (_name) nil))
                ((symbol-function 'nelisp-service-port-in-use-p)
                 (lambda (_port) t))
                ((symbol-function 'nelisp-service-client--open)
                 (lambda (&rest _args)
                   (unless opened
                     (setq opened t)
                     (delete-file state-file))
                   nil)))
        (should-error
         (nelisp-service-client-connect
          "locked" :start-command '("unused")
          :timeout 0.5 :stale-after 10))
        (should (= 1 spawned))
        (should-not (file-exists-p state-file))
        (should (file-exists-p lock-file))))))

(ert-deftest nelisp-service-test-busy-published-daemon-keeps-files-and-cooldown ()
  (nelisp-service-test--with-state-dir
    (let* ((now 1000.0)
           (spawned 0)
           (state-file (nelisp-service-state-file "busy-recovery" "state"))
           (lock-file (nelisp-service-state-file "busy-recovery" "lock"))
           (nelisp-service-client-spawn-function
            (lambda (_name _command)
              (setq spawned (1+ spawned))
              (nelisp-service-write-plist lock-file (list :started now))
              (nelisp-service-write-plist
               state-file (list :port 12345 :token "first")))))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _time) now))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _args) (setq now (+ now 0.1)) nil))
                ((symbol-function 'nelisp-service-client--lock-age)
                 (lambda (_name) (and (> spawned 0) 0)))
                ((symbol-function 'nelisp-service-port-in-use-p)
                 (lambda (_port) t))
                ((symbol-function 'nelisp-service-client--open)
                 (lambda (&rest _args) nil)))
        (should-error
         (nelisp-service-client-connect
          "busy-recovery" :start-command '("unused")
          :timeout 0.5 :stale-after 10))
        (should (= 1 spawned))
        (should (file-exists-p state-file))
        (should (file-exists-p lock-file))))))

(ert-deftest nelisp-service-test-malformed-published-files-keep-cooldown ()
  (nelisp-service-test--with-state-dir
    (let* ((now 1000.0)
           (spawned 0)
           (state-file (nelisp-service-state-file "malformed" "state"))
           (lock-file (nelisp-service-state-file "malformed" "lock"))
           (nelisp-service-client-spawn-function
            (lambda (_name _command)
              (setq spawned (1+ spawned))
              (nelisp-service-write-plist lock-file (list :started now))
              (nelisp-service-write-plist
               state-file (list :port 12345 :token "first")))))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _time) now))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _args) (setq now (+ now 0.1)) nil))
                ((symbol-function 'nelisp-service-port-in-use-p)
                 (lambda (_port) nil))
                ((symbol-function 'nelisp-service-client--open)
                 (lambda (&rest _args)
                   (with-temp-file state-file (insert "("))
                   (with-temp-file lock-file (insert "("))
                   nil)))
        (should-error
         (nelisp-service-client-connect
          "malformed" :start-command '("unused")
          :timeout 0.5 :stale-after 10))
        (should (= 1 spawned))
        (should (file-exists-p state-file))
        (should (file-exists-p lock-file))
        (should-not (nelisp-service-read-plist state-file))
        (should-not (nelisp-service-read-plist lock-file))
        (should (equal "("
                       (with-temp-buffer
                         (insert-file-contents state-file)
                         (buffer-string))))
        (should (equal "("
                       (with-temp-buffer
                         (insert-file-contents lock-file)
                         (buffer-string))))))))

(ert-deftest nelisp-service-test-superseded-daemon-exits-and-keeps-files ()
  (nelisp-service-test--with-state-dir
    (let* ((nelisp-service-daemon-superseded-interval 0.1)
           (daemon (nelisp-service-daemon-start "t11" :handler #'ignore))
           (state (nelisp-service-state-file "t11" "state"))
           (lock (nelisp-service-state-file "t11" "lock")))
      ;; A newer daemon took over: its lock and state replace ours.
      (nelisp-service-delete-file lock)
      (nelisp-service-write-plist lock (list :started (+ (float-time) 5)) t)
      (nelisp-service-write-plist state (list :port 1 :token "newer"))
      (should (eq 'superseded (nelisp-service-daemon-run daemon 0.05)))
      (should (equal "newer" (plist-get (nelisp-service-read-plist state) :token)))
      (should (file-exists-p lock)))))

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
