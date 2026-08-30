;;; nelisp-actor-standalone-smoke.el --- run nelisp-actor on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 193 §4.4 Phase 1: the standalone acceptance gate `nelisp-actor'
;; could not have before this doc -- `generator.el' is not vendored
;; for the standalone substrate, so `(require 'nelisp-actor)' failed
;; loudly with `file-missing: generator' on ANY standalone build
;; before this (measured, verbatim, running `target/nelisp --load
;; packages/nelisp-actor/src/nelisp-actor.el' -- the against-the-bug
;; check this smoke's first section performs).
;;
;; Unlike `nl-condition'/`nl-safe''s standalone smokes, this one does
;; NOT replay `nelisp-actor-test.el's ERT bodies: most of that suite
;; spawns actors via `(nelisp-actor-lambda ...)' directly, which
;; genuinely needs `generator.el' to macroexpand and stays host-Emacs-
;; only by design (see packages/nelisp-actor/README.org and this
;; package's own Commentary).  What runs standalone is specifically
;; `packages/nelisp-actor/generated/two-actor-exchange-cps.el' -- the
;; build-time CPS transform's output -- exercising the actor runtime's
;; core happy path: spawn, mailbox send/receive, cooperative
;; yield/resume, and run-to-idle scheduling, all without ever asking
;; the standalone reader to see `iter-lambda'/`iter-yield'.  The
;; host-Emacs half of this same claim (the generated forms behave
;; IDENTICALLY to what real `generator.el' would have produced, not
;; just "it runs") is
;; `packages/nelisp-actor/test/nelisp-actor-cps-parity-test.el'.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load packages/nelisp-actor/test/nelisp-actor-standalone-smoke.el

;;; Code:

(defvar nelisp-actor-smoke--checked 0)
(defvar nelisp-actor-smoke--failures nil)

(defun nelisp-actor-smoke--check (label actual expected)
  "Record one check: LABEL passes when ACTUAL `equal's EXPECTED."
  (setq nelisp-actor-smoke--checked (1+ nelisp-actor-smoke--checked))
  (unless (equal actual expected)
    (setq nelisp-actor-smoke--failures
          (cons (format "%s: expected %S, got %S" label expected actual)
                nelisp-actor-smoke--failures))))

;; Against-the-bug, section 1: `nelisp-actor' itself loads standalone.
;; Before Doc 193 §4.4 Phase 1's soft `require'/runtime shim
;; (packages/nelisp-actor/src/nelisp-actor.el), THIS `load' is exactly
;; what failed with `nelisp: uncaught error: file-missing: generator'.
;; If it had regressed back to that, this whole file would already be
;; a `--load' failure by the time execution reaches here -- there is
;; no `condition-case' around it, on purpose: a hard failure here
;; should look exactly like the original bug looked, not like a
;; softened, harder-to-recognize smoke failure.
(load "packages/nelisp-actor/src/nelisp-actor.el")
(setq nelisp-actor-smoke--checked (1+ nelisp-actor-smoke--checked))

;; Section 2: the build-time-transformed ping-pong demo -- spawn,
;; send, cooperative receive/yield, run-to-idle -- via the generated,
;; generator-free closures.
(load "packages/nelisp-actor/generated/two-actor-exchange-cps.el")

;; Expected values below are the exact host-Emacs results (`make
;; test-one FILE=packages/nelisp-actor/test/nelisp-actor-example-test.el',
;; and `nelisp-actor-cps-parity-test.el' on host Emacs) for the SAME
;; `nelisp-demo-ping-pong'/`-standalone' HOPS values -- this smoke does
;; not invent its own expectations, it reuses the ones the host-Emacs
;; suite already established and pins them here.
(nelisp-actor-smoke--check
 "hops=4 trail"
 (plist-get (nelisp-demo-ping-pong-standalone 4) :trail)
 '((pong . 0) (ping . 1) (pong . 2) (ping . 3) (pong . 4)))
(nelisp-actor-smoke--check
 "hops=4 pong-status"
 (plist-get (nelisp-demo-ping-pong-standalone 4) :pong-status)
 :dead)
(nelisp-actor-smoke--check
 "hops=4 ping-status"
 (plist-get (nelisp-demo-ping-pong-standalone 4) :ping-status)
 :blocked-receive)
(nelisp-actor-smoke--check
 "hops=2 trail"
 (plist-get (nelisp-demo-ping-pong-standalone 2) :trail)
 '((pong . 0) (ping . 1) (pong . 2)))
(nelisp-actor-smoke--check
 "hops=0 trail"
 (plist-get (nelisp-demo-ping-pong-standalone 0) :trail)
 '((pong . 0)))
(nelisp-actor-smoke--check
 "hops=9 (odd) trail length"
 (length (plist-get (nelisp-demo-ping-pong-standalone 9) :trail))
 10)

;; `tools/ai/nelisp-ai.sh gate NAME -- ...' requires this exact line to
;; report what the gate checked; its absence is itself a hard failure
;; there (see tools/ai/nelisp-ai.sh's `cmd_gate', and nl-condition-
;; standalone-smoke.el's identical comment).
(princ (format "GATE-COUNT checked=%d findings=%d\n"
               nelisp-actor-smoke--checked (length nelisp-actor-smoke--failures)))
(if nelisp-actor-smoke--failures
    (progn
      (dolist (f (reverse nelisp-actor-smoke--failures))
        (princ (format "FAIL %s\n" f)))
      (error "nelisp-actor-standalone-smoke: %d failure(s), %d checked"
             (length nelisp-actor-smoke--failures) nelisp-actor-smoke--checked))
  (princ (format "nelisp-actor-standalone-smoke: PASS (%d checks)\n"
                 nelisp-actor-smoke--checked)))

;;; nelisp-actor-standalone-smoke.el ends here
