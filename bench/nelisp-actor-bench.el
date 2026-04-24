;;; nelisp-actor-bench.el --- Phase 4.7 actor runtime bench  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 4.7 companion: advisory wall-clock numbers for three actor
;; runtime workloads — `spawn-heavy', `ping-pong', `mailbox-fill'.
;; Same policy as `nelisp-gc-bench': numbers are reported for the
;; reader; there is no speed floor and a regression doesn't fail
;; the merge gate (§2.8 A was already green in Phase 4.6).
;;
;; `make actor-bench' is the entry point.

;;; Code:

(require 'nelisp)
(require 'nelisp-actor)

(defun nelisp-actor-bench--time-spawn-heavy (n)
  "Spawn N trivial actors, drive them to completion, return timing plist.
Trivial = thunk with no body; each actor iterator exhausts on its
first `iter-next' and transitions to `:dead'.  Exercises the spawn
+ registry + `nelisp-gc-actor-boundary' snapshot + scheduler drain
path at scale."
  (nelisp-actor--reset)
  (let* ((t0 (current-time))
         _)
    (dotimes (_ n)
      (nelisp-spawn (nelisp-actor-lambda)))
    (let ((ran (nelisp-actor-run-until-idle)))
      (list :scenario 'spawn-heavy
            :n n
            :ran ran
            :seconds (float-time (time-since t0))))))

(defun nelisp-actor-bench--time-ping-pong (n)
  "Round-trip N messages between two actors; return timing plist.
Uses an unbuffered channel pattern via plain `nelisp-send': the
server echoes every incoming message back to the client, and the
client counts responses.  Drives 2*N scheduler steps roughly."
  (nelisp-actor--reset)
  (let ((n n)
        server client t0)
    (setq server
          (nelisp-spawn
           (let ((n n))
             (nelisp-actor-lambda
               (dotimes (_ n)
                 (let ((msg (nelisp-receive)))
                   (nelisp-send (car msg) (cdr msg))))))))
    (setq client
          (nelisp-spawn
           (let ((server server)
                 (n n))
             (nelisp-actor-lambda
               (let ((self (nelisp-self)))
                 (dotimes (_ n)
                   (nelisp-send server (cons self 'ping))
                   (nelisp-receive)))))))
    (ignore client)
    (setq t0 (current-time))
    (nelisp-actor-run-until-idle)
    (list :scenario 'ping-pong
          :n n
          :seconds (float-time (time-since t0)))))

(defun nelisp-actor-bench--time-mailbox-fill (n)
  "Push N messages onto one actor's mailbox then drain; return timing plist.
Pushes happen from the host (outside any actor) via `nelisp-send',
which exercises the per-send deep-copy path on symbols (shareable
fast path).  The receiver then pops all N inside a single actor
body — its `iter-next' resume loop drains the mailbox in one
scheduler pass."
  (nelisp-actor--reset)
  (let ((n n)
        receiver t0)
    (setq receiver
          (nelisp-spawn
           (let ((n n))
             (nelisp-actor-lambda
               (dotimes (_ n) (nelisp-receive))))))
    (setq t0 (current-time))
    (dotimes (_ n)
      (nelisp-send receiver 'tick))
    (nelisp-actor-run-until-idle)
    (list :scenario 'mailbox-fill
          :n n
          :seconds (float-time (time-since t0)))))

;;;###autoload
(defun nelisp-actor-bench-batch ()
  "`make actor-bench' entry point.
Run each scenario once and print a one-line summary.  Advisory
only; never asserts.  Resets the actor runtime between runs so one
scenario cannot contaminate another's scheduler state or registry."
  (message "==== nelisp-actor-bench Phase 4.7 ====")
  (let ((spawn (nelisp-actor-bench--time-spawn-heavy 1000))
        (pong  (nelisp-actor-bench--time-ping-pong 10000))
        (fill  (nelisp-actor-bench--time-mailbox-fill 10000)))
    (message "spawn-heavy  n=%5d ran=%5d  elapsed=%.4fs"
             (plist-get spawn :n)
             (plist-get spawn :ran)
             (plist-get spawn :seconds))
    (message "ping-pong    n=%5d          elapsed=%.4fs"
             (plist-get pong :n)
             (plist-get pong :seconds))
    (message "mailbox-fill n=%5d          elapsed=%.4fs"
             (plist-get fill :n)
             (plist-get fill :seconds)))
  (message "==== done ===="))

(provide 'nelisp-actor-bench)

;;; nelisp-actor-bench.el ends here
