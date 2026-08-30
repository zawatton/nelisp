;;; nelisp-actor-cps-parity-test.el --- host-vs-generated CPS parity for nelisp-actor -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 193 §8's verification design for §4 (nelisp-actor build-time
;; transform): "the same actor-thunk test corpus run once through real
;; generator.el on host Emacs and once through the build-time-
;; transformed forms on target/nelisp, behavior compared, not just it
;; runs -- since the whole point of choosing host Emacs as the
;; transform oracle (§4.3) is fidelity to real semantics, and that
;; claim needs its own check, not an assumption."
;;
;; This file is the host-Emacs half of that: it loads BOTH
;; `examples/nelisp-actor/two-actor-exchange.el's `nelisp-demo-ping-
;; pong' (the original, `nelisp-actor-lambda'/real `generator.el') and
;; `packages/nelisp-actor/generated/two-actor-exchange-cps.el's
;; `nelisp-demo-ping-pong-standalone' (`nelisp-actor-cps-dump.el's
;; build-time-transformed output, same functions real `generator.el'
;; would have produced -- Doc 193 §4.3/§4.4 Phase 1) in the SAME
;; process and asserts their results are identical across several
;; `hops' values.  The standalone half (running the generated function
;; ON `target/nelisp', where the original cannot run at all) is
;; `packages/nelisp-actor/test/nelisp-actor-standalone-smoke.el'.

;;; Code:

(require 'ert)
(require 'nelisp)
(require 'cl-lib)
(require 'nelisp-actor)
;; Plain `load' with a relative FILE searches `load-path' in host
;; Emacs rather than falling back to `default-directory'; an absolute
;; path resolves unambiguously regardless of `load-path' contents.
(load (expand-file-name "examples/nelisp-actor/two-actor-exchange.el"))
(load (expand-file-name
       "packages/nelisp-actor/generated/two-actor-exchange-cps.el"))

(defun nelisp-actor-cps-parity--run-pair (hops)
  "Run both the macro-built and generated ping-pong demos for HOPS.
Returns (ORIGINAL . GENERATED)."
  (cons (nelisp-demo-ping-pong hops)
        (nelisp-demo-ping-pong-standalone hops)))

(ert-deftest nelisp-actor-cps-parity-hops-4 ()
  "The build-time-transformed thunks behave identically to the
`nelisp-actor-lambda'/real-`generator.el' originals for HOPS=4 --
same trail, same final statuses for both actors."
  (let ((pair (nelisp-actor-cps-parity--run-pair 4)))
    (should (equal (car pair) (cdr pair)))))

(ert-deftest nelisp-actor-cps-parity-hops-2 ()
  "Same as `nelisp-actor-cps-parity-hops-4', a different HOPS."
  (let ((pair (nelisp-actor-cps-parity--run-pair 2)))
    (should (equal (car pair) (cdr pair)))))

(ert-deftest nelisp-actor-cps-parity-hops-0 ()
  "HOPS=0: pong's `while' body never runs, an edge case worth its own
row rather than trusting the HOPS=2/4 cases to cover it."
  (let ((pair (nelisp-actor-cps-parity--run-pair 0)))
    (should (equal (car pair) (cdr pair)))))

(ert-deftest nelisp-actor-cps-parity-hops-9-odd ()
  "An odd HOPS, so the loop's last iteration lands on the OTHER actor
than the even cases above -- both parities of \"who sends last\" get a
row."
  (let ((pair (nelisp-actor-cps-parity--run-pair 9)))
    (should (equal (car pair) (cdr pair)))))

(provide 'nelisp-actor-cps-parity-test)

;;; nelisp-actor-cps-parity-test.el ends here
