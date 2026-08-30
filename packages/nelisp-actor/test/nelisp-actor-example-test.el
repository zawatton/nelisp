;;; nelisp-actor-example-test.el --- ERT coverage for the two-actor exchange demo -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercises `examples/nelisp-actor/two-actor-exchange.el' so the
;; example cannot silently rot.  This file's name matches `make
;; test's `packages/*/test/nelisp*-test.el' glob (host Emacs, part of
;; the `ert-full' gate) -- the only substrate this package supports,
;; since `generator.el' is unavailable on the standalone binary (see
;; the example file's Commentary and packages/nelisp-actor/README.org).

;;; Code:

(require 'ert)
(require 'nelisp)
(require 'cl-lib)
(require 'nelisp-actor)
;; Plain `load' with a relative FILE searches `load-path' in host
;; Emacs rather than falling back to `default-directory'; an absolute
;; path resolves unambiguously regardless of `load-path' contents.
(load (expand-file-name "examples/nelisp-actor/two-actor-exchange.el"))

(ert-deftest nelisp-actor-example-trail-alternates-and-increments ()
  "The counter is handed off strictly ping/pong/ping/... up to HOPS."
  (let ((result (nelisp-demo-ping-pong 4)))
    (should (equal (plist-get result :trail)
                    '((pong . 0) (ping . 1) (pong . 2) (ping . 3) (pong . 4))))))

(ert-deftest nelisp-actor-example-pong-completes-ping-waits ()
  "Pong's thunk runs to completion; ping is left waiting for one more
reply that never comes -- both are live evidence of suspend/resume,
not of either actor busy-polling the other."
  (let ((result (nelisp-demo-ping-pong 4)))
    (should (eq (plist-get result :pong-status) :dead))
    (should (eq (plist-get result :ping-status) :blocked-receive))))

(ert-deftest nelisp-actor-example-hop-count-scales ()
  "A different HOPS produces a proportionally longer trail."
  (let ((result (nelisp-demo-ping-pong 2)))
    (should (equal (plist-get result :trail)
                    '((pong . 0) (ping . 1) (pong . 2))))))

(provide 'nelisp-actor-example-test)

;;; nelisp-actor-example-test.el ends here
