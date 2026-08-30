;;; go-ping-pong.el --- nl-clj-async demo: ping/pong over channels and go blocks -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 (docs/design/195-clojure-compat-library.org) §4.6's own
;; honest limit, inherited from `nelisp-actor' (§2.4): a NEW `nl-clj-
;; go' form written and loaded fresh does not run on `target/nelisp'
;; without the same build-time CPS-transform + bake step `nelisp-
;; actor' itself requires -- "write a go block and ship it standalone"
;; is a two-step workflow, not a plain `require'.  This file is that
;; first step: an ordinary, host-Emacs example using `nl-clj-go'/
;; `nl-clj-chan'/`nl-clj-<!'/`nl-clj->!' -- deliberately mirroring
;; `examples/nelisp-actor/two-actor-exchange.el''s own ping-pong shape
;; so the standalone smoke's claim ("a go block IS a CPS-transformed
;; cooperative coroutine, same as a raw actor") is visibly the same
;; demo, one layer up.
;;
;; Resolution of the former named gap: the direct raw-channel
;; workaround is gone.  The two minimal fixtures below each execute
;; one wrapper park point twice from the same `while' (raw operation
;; only on the opposite side), and the full ping/pong uses both
;; wrappers throughout.  On the source-matched standalone binary all
;; three complete, including two calls to the baked ping/pong in one
;; process.  Rebaking the take fixture with the former internal-`let'
;; expansion also completed with `(:first :second)', ruling out the
;; claimed continuation-state defect.  There was no defect in
;; `nl-clj-<!'/`nl-clj->!' to fix.
;;
;; The earlier "hang" was memory exhaustion, not a deadlock and not
;; elapsed time misread as one.  Measured on this tree with the OLD
;; `nl-clj-async.el' restored, sampling CPU% and RSS every 3s while the
;; widened smoke drove it: with Doc 152 Stage 5's mid-form collector on
;; (today's default) peak RSS is ~586 MB, CPU stays ~100%, and the run
;; finishes in ~63s; with the collector disarmed -- the pre-Stage-5
;; default this bug was originally reported under -- RSS reaches about
;; 23 GB in 18s, plateaus, and the run never finishes.  The original
;; report's "0% CPU, stable RSS" is the signature of a process in swap:
;; RSS looks stable because physical memory caps it, and CPU reads near
;; zero because the time is iowait.  Stage 5 fixed this, not anything
;; in this file; the widened smoke now waits for and asserts completion.
;;
;; `packages/nl-clj/scripts/nl-clj-async-cps-dump.el' (run under host
;; Emacs, real `generator.el') transforms the two minimal wrapper
;; fixtures and `nl-clj-async-demo-ping-pong' below -- together with
;; `nl-clj-async--make-chan-1' itself, since a channel a `go' block
;; creates also embeds one `nelisp-actor-lambda' -- into `packages/
;; nl-clj/generated/go-ping-pong-cps.el', the checked-in build-time
;; bake `nl-clj-async-standalone-smoke.el' actually loads.  See `make
;; nl-clj-async-cps-baseline'.
;;
;; Unlike the two-actor-exchange demo (which deliberately leaves ping
;; permanently `:blocked-receive', waiting for a reply pong stops
;; sending), this one is written so BOTH sides terminate cleanly: each
;; side does exactly HOPS receive/reply cycles (a plain counter, not a
;; `close!'-triggered nil-unpark), symmetric on both sides -- the
;; shape this session measured to survive standalone repeatedly.

;;; Code:

(require 'nelisp)
(require 'cl-lib)
(require 'nelisp-actor)
(require 'nl-clj-core)
(require 'nl-safe)
(require 'nl-clj-async)

(defun nl-clj-async-demo-repeated-take ()
  "Exercise one `nl-clj-<!' park point twice from the same `while'.
The producer deliberately uses the raw `nelisp-chan-send' primitive,
so this is a minimal take-side reproducer independent of `nl-clj-go',
`nl-clj->!', channel close, and conditional branching."
  (nelisp-actor--reset)
  (let ((ch (nl-clj-chan))
        (seen nil))
    (nelisp-spawn
     (nelisp-actor-lambda
       (let ((i 0))
         (while (< i 2)
           (let ((v (nl-clj-<! ch)))
             (setq seen (cons v seen)))
           (setq i (1+ i))))))
    (nelisp-spawn
     (nelisp-actor-lambda
       (nelisp-chan-send ch :first)
       (nelisp-chan-send ch :second)))
    (nelisp-actor-run-until-idle)
    (nreverse seen)))

(defun nl-clj-async-demo-repeated-take-raw-control ()
  "Raw-receive control for `nl-clj-async-demo-repeated-take'."
  (nelisp-actor--reset)
  (let ((ch (nl-clj-chan))
        (seen nil))
    (nelisp-spawn
     (nelisp-actor-lambda
       (let ((i 0))
         (while (< i 2)
           (let ((v (cadr (nelisp-chan-recv ch))))
             (setq seen (cons v seen)))
           (setq i (1+ i))))))
    (nelisp-spawn
     (nelisp-actor-lambda
       (nelisp-chan-send ch :first)
       (nelisp-chan-send ch :second)))
    (nelisp-actor-run-until-idle)
    (nreverse seen)))

(defun nl-clj-async-demo-repeated-put ()
  "Exercise one `nl-clj->!' park point twice from the same `while'.
The consumer deliberately uses the raw `nelisp-chan-recv' primitive,
so this is the put-side counterpart of the minimal repeated-take gate."
  (nelisp-actor--reset)
  (let ((ch (nl-clj-chan))
        (accepted nil)
        (seen nil))
    (nelisp-spawn
     (nelisp-actor-lambda
       (let ((i 0))
         (while (< i 2)
           (let ((ok (nl-clj->! ch i)))
             (setq accepted (cons ok accepted)))
           (setq i (1+ i))))))
    (nelisp-spawn
     (nelisp-actor-lambda
       (let ((i 0))
         (while (< i 2)
           (let ((v (cadr (nelisp-chan-recv ch))))
             (setq seen (cons v seen)))
           (setq i (1+ i))))))
    (nelisp-actor-run-until-idle)
    (list :accepted (nreverse accepted) :seen (nreverse seen))))

(defun nl-clj-async-demo-ping-pong (hops)
  "Bounce a counter between two `nl-clj-go' blocks, each talking over
its own channel, for HOPS round trips.  HOPS must be >= 1 -- ping's
own initial rendezvous send requires pong to execute at least one
receive/reply cycle.  Returns a plist:

  :trail        ((ACTOR . N) ...) in the order each hop was received --
                same shape as `nelisp-demo-ping-pong''s own :trail
  :ping-result  the ping go-block's own return value (`:ping-done')
  :pong-result  likewise for pong (`:pong-done')

Both sides terminate cleanly (unlike the raw-actor demo this mirrors,
which deliberately leaves ping dangling for every HOPS value): pong
always replies exactly HOPS times; ping sends the initial value plus
exactly (HOPS - 1) replies, so the last exchange is pong's alone --
symmetric, counter-bounded termination on both sides, no `close!'
needed to unpark a final dangling receive."
  (nelisp-actor--reset)
  (let ((to-ping (nl-clj-chan))
        (to-pong (nl-clj-chan))
        (trail nil)
        pong-chan ping-chan)
    (setq pong-chan
          (nl-clj-go
            (let ((i 0))
              (while (< i hops)
                (let ((n (nl-clj-<! to-pong)))
                  (setq trail (cons (cons 'pong n) trail))
                  (nl-clj->! to-ping (1+ n)))
                (setq i (1+ i))))
            :pong-done))
    (setq ping-chan
          (nl-clj-go
            (nl-clj->! to-pong 0)
            (let ((i 0))
              (while (< i hops)
                (let ((n (nl-clj-<! to-ping)))
                  (setq trail (cons (cons 'ping n) trail))
                  (when (< (1+ i) hops)
                    (nl-clj->! to-pong (1+ n))))
                (setq i (1+ i))))
            :ping-done))
    (nelisp-actor-run-until-idle)
    (list :trail (nreverse trail)
          :ping-result (nl-clj-<!! ping-chan)
          :pong-result (nl-clj-<!! pong-chan))))

(provide 'nl-clj-async-go-ping-pong-demo)

;;; go-ping-pong.el ends here
