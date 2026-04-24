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

(provide 'nelisp-process-test)
;;; nelisp-process-test.el ends here
