;;; two-actor-exchange.el --- nelisp-actor demo: two actors trading messages -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Distinctive feature of `nelisp-actor' (docs/design/10-phase4-actor.org
;; Phase 4.2): cooperative message-passing concurrency on a single OS
;; thread.  `nelisp-receive' genuinely SUSPENDS an actor's thunk (CPS-
;; transformed by `generator.el' via `nelisp-actor-lambda') and resumes
;; it later from the same source position when a message arrives --
;; there is no OS-level blocking, no polling, and no second thread.
;;
;; Host Emacs only.  `generator.el' is not vendored for the NeLisp
;; standalone binary: loading this package there fails loudly with
;; `(file-missing . generator)' (measured 2026-08-22, target/nelisp
;; built from 06ed9bd3a; see packages/nelisp-actor/README.org).  This
;; example is therefore exercised only through host-Emacs ERT
;; (`ert-full'), never through a standalone smoke.
;;
;; Two actors, "ping" and "pong", bounce a counter back and forth:
;; ping kicks off with 0, and each actor increments whatever it
;; receives and sends it back, until the counter reaches HOPS.  Both
;; actors block on `nelisp-receive' between turns -- the trail below
;; is produced entirely by suspend/resume handoffs, not by either
;; actor polling the other.

;;; Code:

(require 'nelisp)
(require 'cl-lib)
(require 'nelisp-actor)

(defun nelisp-demo-ping-pong (hops)
  "Bounce a counter between two actors until it reaches HOPS.
Returns a plist:

  :trail       ((ACTOR . N) ...) in the order each hop was received
  :ping-status `nelisp-actor-status' of the ping actor once the
               scheduler runs out of runnable actors
  :pong-status likewise for pong

Ping always ends `:blocked-receive' (it is waiting for one more
reply that never comes, since pong stops replying once the count
reaches HOPS) and pong ends `:dead' (its thunk runs to completion)."
  (nelisp-actor--reset)
  (let (ping pong trail)
    (setq pong
          (nelisp-spawn
           (nelisp-actor-lambda ()
             (let ((n (nelisp-receive)))
               (while (< n hops)
                 (push (cons 'pong n) trail)
                 (nelisp-send ping (1+ n))
                 (setq n (nelisp-receive)))
               (push (cons 'pong n) trail)))))
    (setq ping
          (nelisp-spawn
           (nelisp-actor-lambda ()
             (nelisp-send pong 0)
             (let ((n (nelisp-receive)))
               (while (< n hops)
                 (push (cons 'ping n) trail)
                 (nelisp-send pong (1+ n))
                 (setq n (nelisp-receive)))
               (push (cons 'ping n) trail)))))
    (nelisp-actor-run-until-idle)
    (list :trail (nreverse trail)
          :ping-status (nelisp-actor-status ping)
          :pong-status (nelisp-actor-status pong))))

(provide 'nelisp-actor-two-actor-exchange-demo)

;;; two-actor-exchange.el ends here
