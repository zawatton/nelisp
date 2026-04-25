;;; nelisp-process-test.el --- Phase 5-C.1 ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for Phase 5-C.1 — `src/nelisp-process.el'.
;;
;; Coverage:
;;   - spawn via `nelisp-make-process' with pipe / PTY
;;   - lifecycle: live-p / status / exit-code / wait-for-exit
;;   - I/O: send-string + send-eof + process-buffer capture
;;   - sentinel / filter trampoline dispatches host lambdas
;;   - sentinel mirrors exit-code onto the wrap
;;   - filter preserves default "append to process-buffer" behaviour
;;   - per-process actor receives `process-state' / `process-output'
;;     events via `nelisp-event' message protocol
;;   - registry list add / delete / find-by-host
;;   - get/put plist accessors
;;   - kill-process flips live-p to nil

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-actor)
(require 'nelisp-eventloop)
(require 'nelisp-process)

(defmacro nelisp-process-test--fresh (&rest body)
  "Run BODY with a pristine process registry + actor registry."
  (declare (indent 0))
  `(progn
     (nelisp-process--reset-registry)
     (nelisp-actor--reset)
     (nelisp-eventloop-drain)
     (unwind-protect
         (progn ,@body)
       (dolist (wrap (nelisp-process-list))
         (ignore-errors (nelisp-delete-process wrap)))
       (nelisp-process--reset-registry)
       (nelisp-actor--reset))))

;;; Basic lifecycle ---------------------------------------------------

(ert-deftest nelisp-process-spawn-true-exits-zero ()
  (nelisp-process-test--fresh
   (let* ((p (nelisp-make-process :name "t" :command '("true")))
          (code (nelisp-process-wait-for-exit p 2.0)))
     (should (nelisp-process-p p))
     (should (= 0 code))
     (should (null (nelisp-process-live-p p))))))

(ert-deftest nelisp-process-spawn-false-exits-nonzero ()
  (nelisp-process-test--fresh
   (let* ((p (nelisp-make-process :name "f" :command '("false")))
          (code (nelisp-process-wait-for-exit p 2.0)))
     (should (not (zerop code))))))

(ert-deftest nelisp-process-name-and-command-of ()
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process :name "nm" :command '("true"))))
     (should (string-match-p "nm" (nelisp-process-name-of p)))
     (should (equal '("true") (nelisp-process-command-of p)))
     (nelisp-process-wait-for-exit p 2.0))))

(ert-deftest nelisp-process-id-integer ()
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process :name "pid" :command '("sleep" "0.2"))))
     (should (integerp (nelisp-process-id-of p)))
     (nelisp-process-wait-for-exit p 2.0))))

(ert-deftest nelisp-process-command-missing-signals ()
  (nelisp-process-test--fresh
   (should-error (nelisp-make-process :name "no-cmd"))))

;;; I/O + buffer ------------------------------------------------------

(ert-deftest nelisp-process-send-string-roundtrips-via-buffer ()
  "cat echoes stdin back; we read it from the provided buffer."
  (nelisp-process-test--fresh
   (let* ((buf (generate-new-buffer " *np-cat*"))
          (p (nelisp-make-process
              :name "cat" :command '("cat") :buffer buf)))
     (unwind-protect
         (progn
           (nelisp-process-send-string p "roundtrip\n")
           (nelisp-process-send-eof p)
           (nelisp-process-wait-for-exit p 2.0)
           (should (string-match-p "roundtrip"
                                   (with-current-buffer buf
                                     (buffer-string)))))
       (kill-buffer buf)))))

(ert-deftest nelisp-process-buffer-of-returns-provided ()
  (nelisp-process-test--fresh
   (let* ((buf (generate-new-buffer " *np-pb*"))
          (p (nelisp-make-process
              :name "pb" :command '("true") :buffer buf)))
     (should (eq buf (nelisp-process-buffer-of p)))
     (nelisp-process-wait-for-exit p 2.0)
     (kill-buffer buf))))

;;; Sentinel / filter -------------------------------------------------

(ert-deftest nelisp-process-sentinel-trampoline-dispatches-host-lambda ()
  (nelisp-process-test--fresh
   (let* ((seen (list))
          (p (nelisp-make-process
              :name "sen" :command '("true")
              :sentinel (lambda (_wrap ev) (push ev seen)))))
     (nelisp-process-wait-for-exit p 2.0)
     (should (cl-some (lambda (s) (and (stringp s)
                                       (string-match-p
                                        "finished\\|exited"
                                        s)))
                      seen)))))

(ert-deftest nelisp-process-sentinel-updates-exit-code-slot ()
  "The trampoline mirrors `process-exit-status' into the wrap at
exit time (before the user sentinel fires)."
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process :name "ec" :command '("true"))))
     (nelisp-process-wait-for-exit p 2.0)
     (should (eql 0 (nelisp-process-exit-code p)))
     (should (memq (nelisp-process-status p)
                   '(exit signal closed failed))))))

(ert-deftest nelisp-process-filter-trampoline-invokes-host-lambda ()
  (nelisp-process-test--fresh
   (let* ((got (list))
          (p (nelisp-make-process
              :name "flt"
              :command '("sh" "-c" "printf hello")
              :filter (lambda (_wrap chunk) (push chunk got)))))
     (nelisp-process-wait-for-exit p 2.0)
     (should (cl-some (lambda (s)
                        (and (stringp s) (string-match-p "hello" s)))
                      got)))))

(ert-deftest nelisp-process-filter-preserves-buffer-append ()
  "Even with a user filter, the default \"append to buffer\" is kept."
  (nelisp-process-test--fresh
   (let* ((buf (generate-new-buffer " *np-flt-buf*"))
          (p (nelisp-make-process
              :name "flt2"
              :command '("sh" "-c" "printf cargo")
              :buffer buf
              :filter (lambda (_w _c) nil))))
     (nelisp-process-wait-for-exit p 2.0)
     (should (string-match-p "cargo"
                             (with-current-buffer buf
                               (buffer-string))))
     (kill-buffer buf))))

;;; Actor integration -------------------------------------------------

(ert-deftest nelisp-process-actor-receives-process-state-event ()
  "When :actor is given, the sentinel posts a `process-state' event."
  (nelisp-process-test--fresh
   (let* ((collector-seen (list))
          (collector
           (nelisp-spawn
            (nelisp-actor-lambda
              (let ((running t))
                (while running
                  (let ((msg (nelisp-receive)))
                    (cond
                     ((and (nelisp-event-p msg)
                           (eq (nelisp-event-kind msg) 'quit))
                      (setq running nil))
                     ((nelisp-event-p msg)
                      (push (nelisp-event-kind msg) collector-seen)
                      (nelisp-yield))
                     (t (nelisp-yield))))))))))
     (let ((p (nelisp-make-process
               :name "act"
               :command '("true")
               :actor collector)))
       (nelisp-process-wait-for-exit p 2.0)
       (nelisp-actor-run-until-idle)
       (nelisp-send collector (nelisp-make-event 'quit nil))
       (nelisp-actor-run-until-idle)
       (should (memq 'process-state collector-seen))))))

(ert-deftest nelisp-process-actor-receives-process-output-event ()
  (nelisp-process-test--fresh
   (let* ((chunks (list))
          (collector
           (nelisp-spawn
            (nelisp-actor-lambda
              (let ((running t))
                (while running
                  (let ((msg (nelisp-receive)))
                    (cond
                     ((and (nelisp-event-p msg)
                           (eq (nelisp-event-kind msg) 'quit))
                      (setq running nil))
                     ((and (nelisp-event-p msg)
                           (eq (nelisp-event-kind msg) 'process-output))
                      (push (nelisp-event-data msg) chunks)
                      (nelisp-yield))
                     (t (nelisp-yield))))))))))
     (let ((p (nelisp-make-process
               :name "act-out"
               :command '("sh" "-c" "printf streamed")
               :actor collector)))
       (nelisp-process-wait-for-exit p 2.0)
       (nelisp-actor-run-until-idle)
       (nelisp-send collector (nelisp-make-event 'quit nil))
       (nelisp-actor-run-until-idle)
       (should (cl-some (lambda (d)
                          (and (plist-get d :chunk)
                               (string-match-p "streamed"
                                               (plist-get d :chunk))))
                        chunks))))))

;;; Kill / delete / registry -----------------------------------------

(ert-deftest nelisp-process-kill-stops-live-process ()
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process
             :name "k" :command '("sleep" "5"))))
     (should (nelisp-process-live-p p))
     (nelisp-kill-process p)
     (nelisp-process-wait-for-exit p 2.0)
     (should (null (nelisp-process-live-p p))))))

(ert-deftest nelisp-process-delete-removes-from-registry ()
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process
             :name "d" :command '("sleep" "5"))))
     (should (memq p (nelisp-process-list)))
     (nelisp-delete-process p)
     (should (null (memq p (nelisp-process-list)))))))

(ert-deftest nelisp-process-list-tracks-multiple ()
  (nelisp-process-test--fresh
   (let ((p1 (nelisp-make-process :name "m1" :command '("sleep" "2")))
         (p2 (nelisp-make-process :name "m2" :command '("sleep" "2"))))
     (should (memq p1 (nelisp-process-list)))
     (should (memq p2 (nelisp-process-list)))
     (should (= 2 (length (nelisp-process-list))))
     (nelisp-kill-process p1)
     (nelisp-kill-process p2)
     (nelisp-process-wait-for-exit p1 2.0)
     (nelisp-process-wait-for-exit p2 2.0))))

(ert-deftest nelisp-process-find-by-host-locates-wrap ()
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process :name "fb" :command '("sleep" "2"))))
     (should (eq p (nelisp-process--find-by-host
                    (nelisp-process-host-proc p))))
     (nelisp-kill-process p)
     (nelisp-process-wait-for-exit p 2.0))))

;;; Plist -------------------------------------------------------------

(ert-deftest nelisp-process-get-put-roundtrip ()
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process :name "g" :command '("true"))))
     (nelisp-process-put p :owner 'anvil)
     (should (eq 'anvil (nelisp-process-get p :owner)))
     (nelisp-process-wait-for-exit p 2.0))))

(ert-deftest nelisp-process-initial-props ()
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process
             :name "ip" :command '("true")
             :props (list :label "seed" :owner 'test))))
     (should (equal "seed" (nelisp-process-get p :label)))
     (should (eq 'test (nelisp-process-get p :owner)))
     (nelisp-process-wait-for-exit p 2.0))))

;;; PTY ---------------------------------------------------------------

(ert-deftest nelisp-process-pty-spawn-roundtrip ()
  "`:connection-type 'pty' is forwarded; echo via cat should still
flow through the trampoline + buffer capture path."
  (skip-unless (member system-type '(gnu/linux darwin berkeley-unix)))
  (nelisp-process-test--fresh
   (let* ((buf (generate-new-buffer " *np-pty*"))
          (p (nelisp-make-process
              :name "pty" :command '("cat")
              :connection-type 'pty
              :buffer buf)))
     (unwind-protect
         (progn
           (nelisp-process-send-string p "pty-ok\n")
           (nelisp-process-send-eof p)
           (nelisp-process-wait-for-exit p 2.0)
           (should (string-match-p "pty-ok"
                                   (with-current-buffer buf
                                     (buffer-string)))))
       (kill-buffer buf)))))

;;; Status slot -------------------------------------------------------

(ert-deftest nelisp-process-status-of-matches-host ()
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process :name "st" :command '("sleep" "2"))))
     (should (memq (nelisp-process-status-of p) '(run open)))
     (nelisp-kill-process p)
     (nelisp-process-wait-for-exit p 2.0)
     (should (memq (nelisp-process-status-of p)
                   '(exit signal closed failed))))))

;;; ============================================================
;;; Phase 9d.L (T106) Layer B — Emacs-compatible API surface
;;; ============================================================

;;; processp / accessor aliases ---------------------------------------

(ert-deftest nelisp-process-processp-recognises-wrap-and-rejects-others ()
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process :name "pp" :command '("true"))))
     (should (nelisp-processp p))
     (should (not (nelisp-processp 42)))
     (should (not (nelisp-processp "string")))
     (should (not (nelisp-processp nil)))
     (nelisp-process-wait-for-exit p 2.0))))

(ert-deftest nelisp-process-emacs-compat-aliases-roundtrip ()
  "`nelisp-process-name-string' / -pid / -command-list / -stdout-buffer /
-exit-code-value should each go through the host primitive each call,
giving Emacs `process-NAME' parity at the call surface."
  (nelisp-process-test--fresh
   (let* ((buf (generate-new-buffer " *np-aliases*"))
          (p (nelisp-make-process
              :name "alias" :command '("true") :buffer buf)))
     (unwind-protect
         (progn
           (nelisp-process-wait-for-exit p 2.0)
           (should (string-match-p "alias" (nelisp-process-name-string p)))
           (should (equal '("true") (nelisp-process-command-list p)))
           (should (eq buf (nelisp-process-stdout-buffer p)))
           (should (eql 0 (nelisp-process-exit-code-value p)))
           ;; PID is queryable until host releases it; either an int
           ;; or nil after teardown is acceptable.
           (let ((pid (nelisp-process-pid p)))
             (should (or (null pid) (integerp pid)))))
       (kill-buffer buf)))))

;;; nelisp-process-current-status (Emacs parity, signals on bad arg) -

(ert-deftest nelisp-process-current-status-signals-on-non-wrap ()
  (nelisp-process-test--fresh
   (should-error (nelisp-process-current-status 'not-a-wrap))))

(ert-deftest nelisp-process-current-status-returns-symbol ()
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process :name "ss" :command '("sleep" "2"))))
     (should (memq (nelisp-process-current-status p) '(run open)))
     (nelisp-kill-process p)
     (nelisp-process-wait-for-exit p 2.0)
     (should (memq (nelisp-process-current-status p)
                   '(exit signal closed failed))))))

;;; set-process-filter / set-process-sentinel -----------------------

(ert-deftest nelisp-process-set-process-filter-replaces-callback ()
  (nelisp-process-test--fresh
   (let* ((seen (list))
          (p (nelisp-make-process
              :name "sf"
              :command '("sh" "-c" "printf abc"))))
     (nelisp-set-process-filter p (lambda (_w c) (push c seen)))
     (should (eq (nelisp-process-filter p)
                 (nelisp-process-user-filter p)))
     (nelisp-process-wait-for-exit p 2.0)
     (should (cl-some (lambda (s) (and (stringp s)
                                       (string-match-p "abc" s)))
                      seen)))))

(ert-deftest nelisp-process-set-process-sentinel-replaces-callback ()
  (nelisp-process-test--fresh
   (let* ((seen (list))
          (p (nelisp-make-process :name "ss" :command '("true"))))
     (nelisp-set-process-sentinel p (lambda (_w ev) (push ev seen)))
     (should (eq (nelisp-process-sentinel p)
                 (nelisp-process-user-sentinel p)))
     (nelisp-process-wait-for-exit p 2.0)
     (should (cl-some #'stringp seen)))))

(ert-deftest nelisp-process-set-process-filter-rejects-non-fn ()
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process :name "rj" :command '("true"))))
     (should-error (nelisp-set-process-filter p 42))
     (nelisp-process-wait-for-exit p 2.0))))

;;; nelisp-start-process --------------------------------------------

(ert-deftest nelisp-process-start-process-spawns-named-process ()
  (nelisp-process-test--fresh
   (let* ((buf (generate-new-buffer " *np-sp*"))
          (p (nelisp-start-process "spname" buf "sh" "-c" "printf out")))
     (unwind-protect
         (progn
           (should (nelisp-processp p))
           (should (string-match-p "spname" (nelisp-process-name-string p)))
           (nelisp-process-wait-for-exit p 2.0)
           (should (string-match-p
                    "out" (with-current-buffer buf (buffer-string)))))
       (kill-buffer buf)))))

(ert-deftest nelisp-process-start-process-rejects-bad-args ()
  (nelisp-process-test--fresh
   (should-error (nelisp-start-process 42 nil "true"))
   (should-error (nelisp-start-process "x" nil nil))))

;;; nelisp-call-process ---------------------------------------------

(ert-deftest nelisp-process-call-process-true-returns-zero ()
  (nelisp-process-test--fresh
   (should (eql 0 (nelisp-call-process "true")))))

(ert-deftest nelisp-process-call-process-false-returns-nonzero ()
  (nelisp-process-test--fresh
   (let ((rc (nelisp-call-process "false")))
     (should (integerp rc))
     (should (not (zerop rc))))))

(ert-deftest nelisp-process-call-process-captures-stdout ()
  (nelisp-process-test--fresh
   (let* ((buf (generate-new-buffer " *np-cp-out*")))
     (unwind-protect
         (let ((rc (nelisp-call-process "sh" nil buf nil "-c"
                                        "printf cap-out")))
           (should (eql 0 rc))
           (should (string-match-p
                    "cap-out" (with-current-buffer buf (buffer-string)))))
       (kill-buffer buf)))))

(ert-deftest nelisp-process-call-process-stderr-split-target ()
  "When DESTINATION is (REAL-DEST ERROR-DEST), stderr lands in the
second buffer."
  (nelisp-process-test--fresh
   (let* ((out (generate-new-buffer " *np-cp-out2*"))
          (err (generate-new-buffer " *np-cp-err2*")))
     (unwind-protect
         (let ((rc (nelisp-call-process
                    "sh" nil (list out err) nil "-c"
                    "printf good; printf bad >&2")))
           (should (eql 0 rc))
           (should (string-match-p "good"
                                   (with-current-buffer out (buffer-string))))
           (should (string-match-p "bad"
                                   (with-current-buffer err (buffer-string)))))
       (kill-buffer out)
       (kill-buffer err)))))

(ert-deftest nelisp-process-call-process-rejects-non-string-program ()
  (nelisp-process-test--fresh
   (should-error (nelisp-call-process 42))))

(ert-deftest nelisp-process-call-process-region-pipes-stdin ()
  (nelisp-process-test--fresh
   (let* ((src (generate-new-buffer " *np-cpr-src*"))
          (out (generate-new-buffer " *np-cpr-out*")))
     (unwind-protect
         (with-current-buffer src
           (insert "alpha-beta")
           (let ((rc (nelisp-call-process-region
                      (point-min) (point-max) "cat" nil out)))
             (should (eql 0 rc))
             (should (string-match-p
                      "alpha-beta"
                      (with-current-buffer out (buffer-string))))))
       (kill-buffer src)
       (kill-buffer out)))))

;;; nelisp-accept-process-output -----------------------------------

(ert-deftest nelisp-process-accept-process-output-drains-buffer ()
  (nelisp-process-test--fresh
   (let* ((buf (generate-new-buffer " *np-apo*"))
          ;; Use a small sleep to guarantee the wrap is still live when
          ;; the loop starts — otherwise `printf' alone races the
          ;; spawn-vs-loop and the test exits the body before any
          ;; accept-process-output hop fires.
          (p (nelisp-make-process
              :name "apo"
              :command '("sh" "-c" "sleep 0.05; printf streamed-data")
              :buffer buf)))
     (unwind-protect
         (progn
           ;; Drive the multiplexer / host-bridge tick a few times.
           (let ((deadline (+ (float-time) 2.0)))
             (while (and (nelisp-process-live-p p)
                         (< (float-time) deadline))
               (nelisp-accept-process-output p 0 50)))
           (nelisp-process-wait-for-exit p 2.0)
           ;; Ensure all queued output has been drained into the buffer.
           (accept-process-output (nelisp-process-host-proc p) 0 100)
           (should (string-match-p
                    "streamed-data"
                    (with-current-buffer buf (buffer-string)))))
       (kill-buffer buf)))))

(ert-deftest nelisp-process-accept-process-output-nil-process-drains-any ()
  "Calling with WRAP=nil drains output from any registered wrap."
  (nelisp-process-test--fresh
   (let* ((b1 (generate-new-buffer " *np-apo-any-1*"))
          (b2 (generate-new-buffer " *np-apo-any-2*"))
          (p1 (nelisp-make-process
               :name "any1"
               :command '("sh" "-c" "printf one")
               :buffer b1))
          (p2 (nelisp-make-process
               :name "any2"
               :command '("sh" "-c" "printf two")
               :buffer b2)))
     (unwind-protect
         (progn
           (let ((deadline (+ (float-time) 2.0)))
             (while (and (or (nelisp-process-live-p p1)
                             (nelisp-process-live-p p2))
                         (< (float-time) deadline))
               (nelisp-accept-process-output nil 0 50)))
           (nelisp-process-wait-for-exit p1 2.0)
           (nelisp-process-wait-for-exit p2 2.0)
           (should (string-match-p "one" (with-current-buffer b1 (buffer-string))))
           (should (string-match-p "two" (with-current-buffer b2 (buffer-string)))))
       (kill-buffer b1)
       (kill-buffer b2)))))

(ert-deftest nelisp-process-accept-process-output-rejects-bad-wrap ()
  (nelisp-process-test--fresh
   (should-error (nelisp-accept-process-output 'not-a-wrap))))

;;; nelisp-delete-process kill cascade ------------------------------

(ert-deftest nelisp-process-delete-process-cascade-kills-and-cleans ()
  "SIGTERM → grace → SIGKILL → registry drop, all via nelisp-delete-process."
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process
             :name "dp" :command '("sleep" "30"))))
     (should (memq p (nelisp-process-list)))
     (should (nelisp-process-live-p p))
     (nelisp-delete-process p)
     ;; Cascade should drop us out of the registry.
     (should (null (memq p (nelisp-process-list))))
     ;; And the host process should be gone.
     (should (null (nelisp-process-live-p p))))))

(ert-deftest nelisp-process-delete-process-handles-already-dead ()
  "Calling delete on an already-exited process is a no-op (idempotent)."
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process :name "dpd" :command '("true"))))
     (nelisp-process-wait-for-exit p 2.0)
     (should (null (nelisp-process-live-p p)))
     ;; Should not signal even though the host is already dead.
     (nelisp-delete-process p)
     (should (null (memq p (nelisp-process-list)))))))

;;; query-on-exit flag (noquery) ------------------------------------

(ert-deftest nelisp-process-set-query-on-exit-flag-noquery ()
  "noquery=t suppresses the host prompt; mirrored via Emacs flag."
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process
             :name "nq" :command '("sleep" "5"))))
     (nelisp-set-process-query-on-exit-flag p t)
     (should (not (nelisp-process-query-on-exit-flag p)))
     (nelisp-set-process-query-on-exit-flag p nil)
     (should (nelisp-process-query-on-exit-flag p))
     (nelisp-kill-process p)
     (nelisp-process-wait-for-exit p 2.0))))

;;; Eventloop multiplexer integration ------------------------------

(ert-deftest nelisp-process-multiplexer-handle-registered-on-spawn ()
  "When `nelisp-eventloop-multiplex' is loaded, every spawn registers
its host fd with the multiplexer; the wrap then carries an opaque
handle + synthetic fd integer until sentinel / delete."
  (require 'nelisp-eventloop-multiplex)
  (nelisp-process-test--fresh
   (let ((p (nelisp-make-process :name "mx" :command '("sleep" "2"))))
     (should (nelisp-process-eventloop-handle p))
     (should (integerp (nelisp-process-eventloop-fd p)))
     (nelisp-kill-process p)
     (nelisp-process-wait-for-exit p 2.0)
     ;; Sentinel should drop the registration.
     (should (null (nelisp-process-eventloop-handle p)))
     (should (null (nelisp-process-eventloop-fd p))))))

;;; stats / instrumentation ---------------------------------------

(ert-deftest nelisp-process-list-stats-reports-live-and-zombie ()
  (nelisp-process-test--fresh
   (let* ((stats0 (nelisp-process-list-stats))
          (live-p (nelisp-make-process
                   :name "lst-live" :command '("sleep" "2")))
          (dead-p (nelisp-make-process
                   :name "lst-dead" :command '("true"))))
     (should (eql 0 (plist-get stats0 :live-count)))
     (nelisp-process-wait-for-exit dead-p 2.0)
     (let ((stats (nelisp-process-list-stats)))
       (should (>= (plist-get stats :total-count) 2))
       (should (>= (plist-get stats :live-count) 1))
       (should (>= (plist-get stats :zombie-count) 1)))
     (nelisp-kill-process live-p)
     (nelisp-process-wait-for-exit live-p 2.0))))

;;; Multi-process concurrent ---------------------------------------

(ert-deftest nelisp-process-multi-spawn-concurrent-three ()
  "Three children, each with own filter; verify each filter saw its
own payload and all three exit cleanly."
  (nelisp-process-test--fresh
   (let* ((seen (make-vector 3 nil))
          (procs (cl-loop for i from 0 below 3
                          collect
                          (let ((idx i))
                            (nelisp-make-process
                             :name (format "c%d" i)
                             :command (list "sh" "-c"
                                            (format "printf ch%d" i))
                             :filter (lambda (_w c)
                                       (aset seen idx c)))))))
     (dolist (p procs) (nelisp-process-wait-for-exit p 2.0))
     (should (string-match-p "ch0" (or (aref seen 0) "")))
     (should (string-match-p "ch1" (or (aref seen 1) "")))
     (should (string-match-p "ch2" (or (aref seen 2) ""))))))

;;; Send-string roundtrip via Layer B ------------------------------

(ert-deftest nelisp-process-large-stdin-write-roundtrip ()
  "Send a 64KB blob through cat and confirm full roundtrip via buffer.
Smaller than the §3.L 1MB target but covers the same path; pure-CI
budget reservation."
  (nelisp-process-test--fresh
   (let* ((buf (generate-new-buffer " *np-big*"))
          (p (nelisp-make-process
              :name "big" :command '("cat") :buffer buf))
          (payload (make-string (* 64 1024) ?A)))
     (unwind-protect
         (progn
           (nelisp-process-send-string p payload)
           (nelisp-process-send-eof p)
           (nelisp-process-wait-for-exit p 5.0)
           (should (= (length payload)
                      (with-current-buffer buf (buffer-size)))))
       (kill-buffer buf)))))

;;; FFI bridge availability probe -----------------------------------

(ert-deftest nelisp-process-rust-ffi-availability-probe-returns-bool ()
  "The probe must return a boolean — Phase 7.5 will flip this to t,
today it must return nil because the FFI is not wired."
  (let ((v (nelisp-process--rust-ffi-available-p)))
    (should (or (eq v t) (null v)))
    ;; Pre-7.5: must be nil so call-sites take the host-bridge path.
    (should (null v))))

(provide 'nelisp-process-test)
;;; nelisp-process-test.el ends here
