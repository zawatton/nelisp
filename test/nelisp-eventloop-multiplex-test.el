;;; nelisp-eventloop-multiplex-test.el --- Phase 9d.K ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for Phase 9d.K — `src/nelisp-eventloop-multiplex.el'
;; (Doc 39 LOCKED-2026-04-25-v2 §3.K).
;;
;; Coverage (>= 12 tests, per Doc 39 §3.2 ERT smoke list):
;;   - select-fd readiness on a live pipe
;;   - select-fd timeout when no fd is ready
;;   - select-fd validation (negative timeout)
;;   - register-fd argument validation
;;   - register / fire / unregister cycle
;;   - multi-fd multiplex: 3 registered, 2 written, 2 callbacks fire
;;   - unregister stops callback firing
;;   - in-tick register/unregister deferred to next tick (reentrancy safety)
;;   - tick with no fds returns 0 immediately (timeout 0 fast-path)
;;   - Doc 34 §2.4 pull-fn adapter returns event-shape on ready fd
;;   - Doc 34 §2.4 pull-fn adapter returns nil on no-data
;;   - run-until-stop loop honours `multiplex-stop'
;;   - backend selector validation
;;   - rust-ffi backend signals todo-error (Phase 7.5 reservation)
;;   - fd-proxy register / unregister / lookup
;;   - select-fd with closed process is read-ready (EOF semantics)
;;   - contract version constant matches Doc 34 LOCKED v2

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eventloop)
(require 'nelisp-eventloop-multiplex)

;; ---------------------------------------------------------------------
;; Test fixture helpers.
;; ---------------------------------------------------------------------

(defvar nelisp-eventloop-multiplex-test--owned-procs nil
  "Procs created during the current test that must be killed at teardown.")

(defun nelisp-eventloop-multiplex-test--reset ()
  "Reset all multiplexer state for a fresh test."
  (nelisp-eventloop-multiplex--reset-registry)
  (nelisp-eventloop-multiplex--reset-fd-proxy)
  (setq nelisp-eventloop-multiplex--running nil
        nelisp-eventloop-multiplex-backend 'host-bridge)
  ;; Tear down any test-owned host processes.
  (dolist (p nelisp-eventloop-multiplex-test--owned-procs)
    (when (and (processp p) (process-live-p p))
      (ignore-errors (delete-process p)))
    (let ((b (and (processp p) (process-buffer p))))
      (when (and b (buffer-live-p b))
        (kill-buffer b))))
  (setq nelisp-eventloop-multiplex-test--owned-procs nil))

(defmacro nelisp-eventloop-multiplex-test--fresh (&rest body)
  "Run BODY under a freshly reset multiplexer."
  (declare (indent 0))
  `(progn
     (nelisp-eventloop-multiplex-test--reset)
     (unwind-protect
         (progn ,@body)
       (nelisp-eventloop-multiplex-test--reset))))

(defun nelisp-eventloop-multiplex-test--make-pipe-pair (name)
  "Create a host pipe-process named NAME with an attached buffer.
Returns the proc.  The proc is registered for teardown."
  (let* ((buf (generate-new-buffer (format " *evloop-pipe-%s*" name)))
         (proc (make-pipe-process
                :name name
                :buffer buf
                :noquery t
                :coding 'no-conversion)))
    (push proc nelisp-eventloop-multiplex-test--owned-procs)
    proc))

(defun nelisp-eventloop-multiplex-test--make-cat ()
  "Spawn `cat' as a child process bound to a fresh buffer.
Acts as a pipe whose stdin we can write to and whose stdout we can
read.  Returns the proc."
  (let* ((buf (generate-new-buffer " *evloop-cat*"))
         (proc (make-process
                :name "evloop-cat"
                :buffer buf
                :command '("cat")
                :coding 'no-conversion
                :connection-type 'pipe
                :noquery t)))
    (push proc nelisp-eventloop-multiplex-test--owned-procs)
    proc))

;; ---------------------------------------------------------------------
;; Phase 1 — select-fd primitive.
;; ---------------------------------------------------------------------

(ert-deftest nelisp-eventloop-multiplex-select-pipe-ready ()
  "select-fd returns :ready when the bound process has buffered data."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((proc (nelisp-eventloop-multiplex-test--make-cat))
          (fd (nelisp-eventloop-multiplex-register-process-fd proc)))
     (process-send-string proc "hello\n")
     ;; Give cat a moment to echo.
     (let ((deadline (+ (float-time) 2.0)))
       (while (and (with-current-buffer (process-buffer proc)
                     (zerop (buffer-size)))
                   (< (float-time) deadline))
         (accept-process-output proc 0.05)))
     (let ((sel (nelisp-eventloop-select-fd (list fd) nil nil 100)))
       (should (eq (plist-get sel :status) :ready))
       (should (memq fd (plist-get sel :ready)))))))

(ert-deftest nelisp-eventloop-multiplex-select-timeout ()
  "select-fd returns :timeout when no fd is ready within budget."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((proc (nelisp-eventloop-multiplex-test--make-cat))
          (fd (nelisp-eventloop-multiplex-register-process-fd proc))
          (start (float-time))
          (sel (nelisp-eventloop-select-fd (list fd) nil nil 50))
          (elapsed-ms (* (- (float-time) start) 1000.0)))
     (should (eq (plist-get sel :status) :timeout))
     (should (null (plist-get sel :ready)))
     ;; ≥ 40ms (allow 10ms slop), ≤ 1000ms (would indicate runaway).
     (should (>= elapsed-ms 40))
     (should (<= elapsed-ms 1000)))))

(ert-deftest nelisp-eventloop-multiplex-select-rejects-negative-timeout ()
  "select-fd validates timeout-ms ≥ 0."
  (nelisp-eventloop-multiplex-test--fresh
   (should-error
    (nelisp-eventloop-select-fd nil nil nil -1)
    :type 'wrong-type-argument)))

(ert-deftest nelisp-eventloop-multiplex-select-zero-timeout-is-poll ()
  "select-fd with timeout 0 returns immediately."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((proc (nelisp-eventloop-multiplex-test--make-cat))
          (fd (nelisp-eventloop-multiplex-register-process-fd proc))
          (start (float-time))
          (sel (nelisp-eventloop-select-fd (list fd) nil nil 0))
          (elapsed-ms (* (- (float-time) start) 1000.0)))
     (should (eq (plist-get sel :status) :timeout))
     (should (< elapsed-ms 50)))))

(ert-deftest nelisp-eventloop-multiplex-select-closed-fd-is-read-ready ()
  "A closed proc is read-ready (EOF semantics, matches select(2))."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((proc (nelisp-eventloop-multiplex-test--make-cat))
          (fd (nelisp-eventloop-multiplex-register-process-fd proc)))
     (delete-process proc)
     (let ((sel (nelisp-eventloop-select-fd (list fd) nil nil 0)))
       (should (eq (plist-get sel :status) :ready))
       (should (memq fd (plist-get sel :ready)))))))

;; ---------------------------------------------------------------------
;; Phase 2 — fd registration table.
;; ---------------------------------------------------------------------

(ert-deftest nelisp-eventloop-multiplex-register-validation ()
  "register-fd validates fd / type / callback shape."
  (nelisp-eventloop-multiplex-test--fresh
   (should-error (nelisp-eventloop-multiplex-register-fd "fd" 'read #'ignore)
                 :type 'wrong-type-argument)
   (should-error (nelisp-eventloop-multiplex-register-fd 100 'bogus #'ignore)
                 :type 'wrong-type-argument)
   (should-error (nelisp-eventloop-multiplex-register-fd 100 'read 42)
                 :type 'wrong-type-argument)))

(ert-deftest nelisp-eventloop-multiplex-register-fd-and-fire ()
  "register-fd → write → tick → callback fires once with (fd ctx)."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((proc (nelisp-eventloop-multiplex-test--make-cat))
          (fd (nelisp-eventloop-multiplex-register-process-fd proc))
          (seen nil)
          (h (nelisp-eventloop-multiplex-register-fd
              fd 'read
              (lambda (got-fd ctx) (push (cons got-fd ctx) seen))
              'context-tag)))
     (should (vectorp h))
     (process-send-string proc "abc\n")
     ;; Drive a few ticks with non-zero timeout so cat has time to echo.
     (let ((deadline (+ (float-time) 2.0)))
       (while (and (null seen) (< (float-time) deadline))
         (nelisp-eventloop-multiplex-tick 50)))
     (should (equal seen (list (cons fd 'context-tag)))))))

(ert-deftest nelisp-eventloop-multiplex-fd-proxy-register-unregister ()
  "register-process-fd allocates increasing ids, unregister drops them."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((p1 (nelisp-eventloop-multiplex-test--make-cat))
          (p2 (nelisp-eventloop-multiplex-test--make-cat))
          (fd1 (nelisp-eventloop-multiplex-register-process-fd p1))
          (fd2 (nelisp-eventloop-multiplex-register-process-fd p2)))
     (should (integerp fd1))
     (should (integerp fd2))
     (should (> fd2 fd1))
     (should (eq (nelisp-eventloop-multiplex--fd-proc fd1) p1))
     (should (eq (nelisp-eventloop-multiplex--fd-proc fd2) p2))
     (nelisp-eventloop-multiplex-unregister-process-fd fd1)
     (should (null (nelisp-eventloop-multiplex--fd-proc fd1)))
     (should (eq (nelisp-eventloop-multiplex--fd-proc fd2) p2)))))

;; ---------------------------------------------------------------------
;; Phase 3 — dispatch tick.
;; ---------------------------------------------------------------------

(ert-deftest nelisp-eventloop-multiplex-multi-fd-multiplex ()
  "3 fds registered, 2 receive data → 2 callbacks fire."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((p1 (nelisp-eventloop-multiplex-test--make-cat))
          (p2 (nelisp-eventloop-multiplex-test--make-cat))
          (p3 (nelisp-eventloop-multiplex-test--make-cat))
          (fd1 (nelisp-eventloop-multiplex-register-process-fd p1))
          (fd2 (nelisp-eventloop-multiplex-register-process-fd p2))
          (fd3 (nelisp-eventloop-multiplex-register-process-fd p3))
          (fired (make-hash-table :test 'eql)))
     (dolist (pair (list (cons fd1 'a) (cons fd2 'b) (cons fd3 'c)))
       (let ((fd (car pair)) (tag (cdr pair)))
         (nelisp-eventloop-multiplex-register-fd
          fd 'read
          (lambda (got-fd _ctx) (puthash got-fd tag fired)))))
     (process-send-string p1 "x\n")
     (process-send-string p3 "y\n")
     (let ((deadline (+ (float-time) 2.0)))
       (while (and (< (hash-table-count fired) 2)
                   (< (float-time) deadline))
         (nelisp-eventloop-multiplex-tick 50)))
     (should (eq (gethash fd1 fired) 'a))
     (should (eq (gethash fd3 fired) 'c))
     (should (null (gethash fd2 fired))))))

(ert-deftest nelisp-eventloop-multiplex-unregister-stops-callbacks ()
  "After unregister-fd, subsequent ticks must NOT call the callback."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((proc (nelisp-eventloop-multiplex-test--make-cat))
          (fd (nelisp-eventloop-multiplex-register-process-fd proc))
          (count 0)
          (h (nelisp-eventloop-multiplex-register-fd
              fd 'read (lambda (_fd _ctx) (cl-incf count)))))
     ;; First tick — fires once.
     (process-send-string proc "1\n")
     (let ((deadline (+ (float-time) 2.0)))
       (while (and (zerop count) (< (float-time) deadline))
         (nelisp-eventloop-multiplex-tick 50)))
     (should (= count 1))
     ;; Unregister and feed more — callback must NOT fire again.
     (should (eq t (nelisp-eventloop-multiplex-unregister-fd h)))
     (process-send-string proc "2\n")
     (dotimes (_ 3) (nelisp-eventloop-multiplex-tick 30))
     (should (= count 1))
     ;; Re-unregister returns nil (no-op).
     (should (null (nelisp-eventloop-multiplex-unregister-fd h))))))

(ert-deftest nelisp-eventloop-multiplex-tick-no-fds-returns-zero ()
  "Tick with empty registry returns 0 fired callbacks."
  (nelisp-eventloop-multiplex-test--fresh
   (should (= 0 (nelisp-eventloop-multiplex-tick 0)))))

(ert-deftest nelisp-eventloop-multiplex-in-tick-register-deferred ()
  "Registering inside a callback is deferred to the next tick.
This is the Doc 39 §3.K reentrancy guarantee — the live registration
list cannot grow mid-walk."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((proc (nelisp-eventloop-multiplex-test--make-cat))
          (fd (nelisp-eventloop-multiplex-register-process-fd proc))
          (outer-fired 0)
          (inner-fired 0)
          (inner-handle nil))
     (nelisp-eventloop-multiplex-register-fd
      fd 'read
      (lambda (_fd _ctx)
        (cl-incf outer-fired)
        ;; Register a *new* read-handler on the same fd.  Per spec,
        ;; the new handle must NOT fire during this same tick.
        (setq inner-handle
              (nelisp-eventloop-multiplex-register-fd
               fd 'read
               (lambda (_fd _ctx) (cl-incf inner-fired))))))
     (process-send-string proc "ping\n")
     (let ((deadline (+ (float-time) 2.0)))
       (while (and (zerop outer-fired) (< (float-time) deadline))
         (nelisp-eventloop-multiplex-tick 50)))
     ;; First tick: outer fires, inner is still in pending queue.
     (should (= outer-fired 1))
     (should (= inner-fired 0))
     (should (vectorp inner-handle))
     ;; Drive a follow-up tick — *now* both are live.  Feed more
     ;; data so that the next tick observes a ready fd.
     (process-send-string proc "pong\n")
     (let ((before-outer outer-fired)
           (before-inner inner-fired)
           (deadline (+ (float-time) 2.0)))
       (while (and (= inner-fired before-inner)
                   (< (float-time) deadline))
         (nelisp-eventloop-multiplex-tick 50))
       (should (>= inner-fired (1+ before-inner)))
       (should (>= outer-fired (1+ before-outer)))))))

;; ---------------------------------------------------------------------
;; Phase 4 — Doc 34 §2.4 pull-fn adapter.
;; ---------------------------------------------------------------------

(ert-deftest nelisp-eventloop-multiplex-pull-fn-shape ()
  "Adapter returns a 0-arg function (pull-fn contract version 1)."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((proc (nelisp-eventloop-multiplex-test--make-cat))
          (fd (nelisp-eventloop-multiplex-register-process-fd proc))
          (pf (nelisp-eventloop-as-event-source-pull-fn fd)))
     (should (functionp pf))
     ;; arity check — 0-arg.
     (should (equal (func-arity pf) '(0 . 0))))))

(ert-deftest nelisp-eventloop-multiplex-pull-fn-emits-event-on-ready ()
  "pull-fn returns a `nelisp-event' of kind `fd-data' when bytes wait."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((proc (nelisp-eventloop-multiplex-test--make-cat))
          (fd (nelisp-eventloop-multiplex-register-process-fd proc))
          (pf (nelisp-eventloop-as-event-source-pull-fn fd)))
     (process-send-string proc "doc34\n")
     (let ((deadline (+ (float-time) 2.0))
           (ev nil))
       (while (and (null ev) (< (float-time) deadline))
         (setq ev (funcall pf))
         (unless ev (sit-for 0.02)))
       (should (nelisp-event-p ev))
       (should (eq (nelisp-event-kind ev) 'fd-data))
       (let ((data (nelisp-event-data ev)))
         (should (eql (plist-get data :fd) fd))
         (should (stringp (plist-get data :payload)))
         (should (string-match-p "doc34" (plist-get data :payload))))))))

(ert-deftest nelisp-eventloop-multiplex-pull-fn-nil-on-no-data ()
  "pull-fn returns nil when no data is buffered (non-blocking)."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((proc (nelisp-eventloop-multiplex-test--make-cat))
          (fd (nelisp-eventloop-multiplex-register-process-fd proc))
          (pf (nelisp-eventloop-as-event-source-pull-fn fd))
          (start (float-time))
          (ev (funcall pf))
          (elapsed-ms (* (- (float-time) start) 1000.0)))
     (should (null ev))
     (should (< elapsed-ms 50)))))

(ert-deftest nelisp-eventloop-multiplex-pull-fn-validation ()
  "Adapter validates fd argument."
  (nelisp-eventloop-multiplex-test--fresh
   (should-error
    (nelisp-eventloop-as-event-source-pull-fn "not-an-fd")
    :type 'wrong-type-argument)))

(ert-deftest nelisp-eventloop-multiplex-event-source-contract-version ()
  "Adapter declares Doc 34 §2.4 EVENT_SOURCE_CONTRACT_VERSION = 1."
  (should
   (= 1 nelisp-eventloop-multiplex-event-source-contract-version)))

;; ---------------------------------------------------------------------
;; Phase 5 — backend selector + run loop.
;; ---------------------------------------------------------------------

(ert-deftest nelisp-eventloop-multiplex-backend-validation ()
  "set-backend rejects unknown backends."
  (nelisp-eventloop-multiplex-test--fresh
   (should-error
    (nelisp-eventloop-multiplex-set-backend 'epoll)
    :type 'wrong-type-argument)
   ;; valid switches succeed.
   (nelisp-eventloop-multiplex-set-backend 'host-bridge)
   (should (eq nelisp-eventloop-multiplex-backend 'host-bridge))
   (nelisp-eventloop-multiplex-set-backend 'rust-ffi)
   (should (eq nelisp-eventloop-multiplex-backend 'rust-ffi))))

(ert-deftest nelisp-eventloop-multiplex-rust-ffi-signals-todo ()
  "rust-ffi backend signals todo-error (Phase 7.5 reservation)."
  (nelisp-eventloop-multiplex-test--fresh
   (nelisp-eventloop-multiplex-set-backend 'rust-ffi)
   (should-error
    (nelisp-eventloop-select-fd (list 100) nil nil 0)
    :type 'nelisp-eventloop-multiplex-todo)))

(ert-deftest nelisp-eventloop-multiplex-run-honours-stop ()
  "run loop exits when stop is invoked from a callback."
  (nelisp-eventloop-multiplex-test--fresh
   (let* ((proc (nelisp-eventloop-multiplex-test--make-cat))
          (fd (nelisp-eventloop-multiplex-register-process-fd proc))
          (fired 0))
     (nelisp-eventloop-multiplex-register-fd
      fd 'read
      (lambda (_fd _ctx)
        (cl-incf fired)
        (nelisp-eventloop-multiplex-stop)))
     (process-send-string proc "stop-me\n")
     (let ((total (nelisp-eventloop-multiplex-run 30)))
       (should (>= fired 1))
       (should (= total fired))
       (should (null nelisp-eventloop-multiplex--running))))))

(provide 'nelisp-eventloop-multiplex-test)
;;; nelisp-eventloop-multiplex-test.el ends here
