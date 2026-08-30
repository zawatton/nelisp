;;; nl-clj-async-test.el --- Tests for nl-clj-async.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 (docs/design/195-clojure-compat-library.org) §4.6's own
;; verification design: unlike almost everything else in this package,
;; host Emacs IS a real control here -- `nl-clj-chan'/`go'/`>!'/`<!'
;; running under host Emacs (real `generator.el', nothing about the
;; build-time transform needed) exercises the identical code path this
;; package's standalone bake (`nl-clj-async-cps-baseline') later proves
;; standalone.  Every assertion below is checked against Clojure's own
;; documented `core.async' reference contract by hand -- plain Emacs
;; has no channels to differentially compare against at all.
;;
;; `nelisp-actor--reset' before every test (same fixture
;; `nelisp-actor-test.el' uses): actor/channel state is global, so a
;; leftover actor from a previous test would corrupt this one's IDs
;; and mailbox contents.
;;
;; Against-the-bug shapes this file specifically covers, named because
;; each is exactly the sort of thing a casual smoke would skip and
;; this package's own development caught real (documented in
;; nl-clj-async.el's own Commentary at each fix site):
;;   - alts! picks EXACTLY one ready port -- the untouched one must
;;     still be independently takeable afterward, unharmed (`nl-clj-
;;     async-test-alts-does-not-touch-the-loser').
;;   - close! is REPEATABLE: `<!'/`<!!' on an already-closed, already-
;;     drained channel must keep answering nil forever, not just once
;;     (`nl-clj-async-test-close-take-repeatable*') -- the mediator
;;     dying on close (matching `nelisp-make-chan''s own shutdown
;;     condition) was a real, measured defect this package's own test
;;     suite found and nl-clj-async.el's own Commentary fixes.
;;   - `>!' on a closed channel returns false, never signals, even
;;     nested inside `nl-clj-go''s own `let'-wrapped body -- the
;;     `condition-case'-around-a-yield-inside-a-let generator.el defect
;;     this package's own test suite found and nl-clj-async.el's own
;;     Commentary documents and works around.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp)
(require 'nelisp-actor)
(require 'nl-clj-async)

(defun nl-clj-async-test--fixture ()
  "Reset actor/channel runtime state before each test."
  (nelisp-actor--reset))

;;;; chan / close! -------------------------------------------------------

(ert-deftest nl-clj-async-test-chan-is-a-nelisp-chan ()
  (nl-clj-async-test--fixture)
  (should (nelisp-chan-p (nl-clj-chan))))

(ert-deftest nl-clj-async-test-unbuffered-rendezvous ()
  "Unbuffered channel: send and recv hand off directly (Doc 195 §4.6:
same rendezvous contract as `nelisp-make-chan')."
  (nl-clj-async-test--fixture)
  (let* ((ch (nl-clj-chan))
         (received nil))
    (nelisp-spawn (let ((ch ch)) (nelisp-actor-lambda (nl-clj->! ch 'hello))))
    (nelisp-spawn (let ((ch ch)) (nelisp-actor-lambda (setq received (nl-clj-<! ch)))))
    (nelisp-actor-run-until-idle)
    (should (eq received 'hello))))

(ert-deftest nl-clj-async-test-unbuffered-nil-message-distinct-from-closed ()
  "A real nil message and a closed channel must both unwrap to nil from
`nl-clj-<!', but the SEND must still have genuinely happened (Doc 195
§4.6's `>!'/`<!' unwrap of `nelisp-chan-recv''s own `(:value V)' vs
`(:closed)' list-wrapper)."
  (nl-clj-async-test--fixture)
  (let* ((ch (nl-clj-chan)) (sent nil) (received :unset))
    (nelisp-spawn (let ((ch ch)) (nelisp-actor-lambda (setq sent (nl-clj->! ch nil)))))
    (nelisp-spawn (let ((ch ch)) (nelisp-actor-lambda (setq received (nl-clj-<! ch)))))
    (nelisp-actor-run-until-idle)
    (should (eq sent t))
    (should (null received))))

(ert-deftest nl-clj-async-test-buffered-send-without-block ()
  "Buffered channel: sender completes without a receiver, up to capacity
(Doc 195 §4.6, matching `nelisp-actor-test.el's own channel section)."
  (nl-clj-async-test--fixture)
  (let* ((ch (nl-clj-chan 3))
         (sender (nelisp-spawn
                  (let ((ch ch))
                    (nelisp-actor-lambda
                      (nl-clj->! ch 'a)
                      (nl-clj->! ch 'b)
                      (nl-clj->! ch 'c))))))
    (nelisp-actor-run-until-idle)
    (should (eq (nelisp-actor-status sender) :dead))))

(ert-deftest nl-clj-async-test-buffered-send-blocks-when-full ()
  "Buffered channel: the (n+1)th send blocks until a recv drains one."
  (nl-clj-async-test--fixture)
  (let* ((ch (nl-clj-chan 1))
         (received nil)
         (sender (nelisp-spawn
                  (let ((ch ch))
                    (nelisp-actor-lambda
                      (nl-clj->! ch 'first)
                      (nl-clj->! ch 'second))))))
    (nelisp-actor-run-until-idle)
    (should (memq (nelisp-actor-status sender) '(:blocked-receive :runnable)))
    (nelisp-spawn
     (let ((ch ch))
       (nelisp-actor-lambda
         (push (nl-clj-<! ch) received)
         (push (nl-clj-<! ch) received))))
    (nelisp-actor-run-until-idle)
    (should (equal (nreverse received) '(first second)))
    (should (eq (nelisp-actor-status sender) :dead))))

(ert-deftest nl-clj-async-test-recv-blocks-until-send ()
  (nl-clj-async-test--fixture)
  (let* ((ch (nl-clj-chan))
         (received :unset)
         (receiver (nelisp-spawn
                    (let ((ch ch))
                      (nelisp-actor-lambda (setq received (nl-clj-<! ch)))))))
    (nelisp-actor-run-until-idle)
    (should (eq received :unset))
    (should (memq (nelisp-actor-status receiver) '(:blocked-receive :runnable)))
    (nelisp-spawn (let ((ch ch)) (nelisp-actor-lambda (nl-clj->! ch 'delayed))))
    (nelisp-actor-run-until-idle)
    (should (eq received 'delayed))
    (should (eq (nelisp-actor-status receiver) :dead))))

;;;; close! semantics (DoD: take returns nil after close, repeatably;
;;;; put after close returns false per Clojure, never signals) ---------

(ert-deftest nl-clj-async-test-close-wakes-pending-recv ()
  (nl-clj-async-test--fixture)
  (let* ((ch (nl-clj-chan))
         (received :unset)
         (receiver (nelisp-spawn
                    (let ((ch ch))
                      (nelisp-actor-lambda (setq received (nl-clj-<! ch)))))))
    (nelisp-actor-run-until-idle)
    (should (eq received :unset))
    (nl-clj-close! ch)
    (nelisp-actor-run-until-idle)
    (should (null received))
    (should (eq (nelisp-actor-status receiver) :dead))))

(ert-deftest nl-clj-async-test-close-take-repeatable ()
  "The DoD's own wording: take returns nil after close -- and must keep
doing so, not just once.  Against-the-bug: an earlier version of this
package's mediator died the moment it was closed with nothing pending
\(matching `nelisp-make-chan''s own shutdown condition exactly\), so a
SECOND take after close crashed the caller with `send-to-dead-actor'
instead of returning nil -- fixed in `nl-clj-async--make-chan-1''s own
Commentary; this test is the regression guard."
  (nl-clj-async-test--fixture)
  (let ((ch (nl-clj-chan)))
    (nl-clj-close! ch)
    (dotimes (_ 5)
      (let ((r (nl-clj-go (nl-clj-<! ch))))
        (nelisp-actor-run-until-idle)
        (should (null (nl-clj-<!! r)))))))

(ert-deftest nl-clj-async-test-close-put-returns-false-not-signal ()
  "Real core.async: `>!' on a closed channel is a well-defined false, not
an exception.  Against-the-bug: an earlier version of `nl-clj->!' used
`condition-case' around `nelisp-chan-send' (which itself signals on
this exact case) and lost the handler once nested inside `nl-clj-go''s
own `let'-wrapped body -- the raw error condition leaked out as if it
were a normal return VALUE instead of being caught.  Repeated 3x: the
fix (speaking the `:send' protocol directly, no `condition-case' at
all) must not merely work once by accident."
  (nl-clj-async-test--fixture)
  (let ((ch (nl-clj-chan)))
    (nl-clj-close! ch)
    (dotimes (_ 3)
      (let ((r (nl-clj-go (nl-clj->! ch :x))))
        (nelisp-actor-run-until-idle)
        (should (eq (nl-clj-<!! r) nil))))))

(ert-deftest nl-clj-async-test-close-put-false-does-not-crash-actor ()
  "The regression this guards is specifically an UNCAUGHT-looking crash
disguised as a normal value; assert the go BLOCK's own actor (not
either channel's mediator, which never dies -- see
`nl-clj-async--make-chan-1's own Commentary) is genuinely `:dead', not
`:crashed', after a put-on-closed inside it."
  (nl-clj-async-test--fixture)
  (let* ((ch (nl-clj-chan)))
    (nl-clj-close! ch)
    (let* ((known (list (nelisp-chan-actor ch)))
           (r (nl-clj-go (nl-clj->! ch :x))))
      (push (nelisp-chan-actor r) known)
      (nelisp-actor-run-until-idle)
      (should (eq (nl-clj-<!! r) nil))
      (let ((go-actor (car (cl-remove-if (lambda (a) (memq a known)) (nelisp-actor-list)))))
        (should go-actor)
        (should (eq (nelisp-actor-status go-actor) :dead))))))

(ert-deftest nl-clj-async-test-close-errors-pending-sender-via-blocking-recv ()
  "Closing a full buffered channel fails a pending sender with false, not
by crashing it -- `nl-clj->!' inside a go block that is currently
parked mid-send when close happens."
  (nl-clj-async-test--fixture)
  (let* ((ch (nl-clj-chan 1))
         (results nil))
    (let ((r (nl-clj-go
               (push (nl-clj->! ch 'fits) results)
               (push (nl-clj->! ch 'blocks) results)
               :done)))
      (nelisp-actor-run-until-idle)
      (nl-clj-close! ch)
      (nelisp-actor-run-until-idle)
      (should (eq (nl-clj-<!! r) :done))
      (should (equal (nreverse results) (list t nil))))))

;;;; producer/consumer go pair exchanging N values (DoD) ----------------

(ert-deftest nl-clj-async-test-producer-consumer-exchange-n-values ()
  "A `go' pair exchanging N values over one channel, then closing --
`nl-clj-go' is genuinely a spawned, CPS-parked coroutine: the consumer
suspends on every `<!' and resumes only once the producer's matching
`>!' hands off, never polling."
  (nl-clj-async-test--fixture)
  (let* ((n 25)
         (ch (nl-clj-chan))
         producer consumer)
    (setq producer
          (nl-clj-go
            (dotimes (i n) (nl-clj->! ch i))
            (nl-clj-close! ch)
            :producer-done))
    (setq consumer
          (nl-clj-go
            (let (acc (v (nl-clj-<! ch)))
              (while v
                (push v acc)
                (setq v (nl-clj-<! ch)))
              (nreverse acc))))
    (nelisp-actor-run-until-idle)
    (should (eq (nl-clj-<!! producer) :producer-done))
    (should (equal (nl-clj-<!! consumer) (number-sequence 0 (1- n))))))

(ert-deftest nl-clj-async-test-producer-consumer-buffered-channel ()
  "Same exchange, but over a buffered channel -- the producer should not
need to interleave with the consumer's own scheduling as tightly."
  (nl-clj-async-test--fixture)
  (let* ((n 10) (ch (nl-clj-chan 4)) producer consumer)
    (setq producer (nl-clj-go (dotimes (i n) (nl-clj->! ch (* i i))) (nl-clj-close! ch) n))
    (setq consumer (nl-clj-go
                     (let (acc (v (nl-clj-<! ch)))
                       (while v (push v acc) (setq v (nl-clj-<! ch)))
                       (nreverse acc))))
    (nelisp-actor-run-until-idle)
    (should (= (nl-clj-<!! producer) n))
    (should (equal (nl-clj-<!! consumer) (mapcar (lambda (i) (* i i)) (number-sequence 0 (1- n)))))))

;;;; go's own return channel ----------------------------------------------

(ert-deftest nl-clj-async-test-go-returns-a-channel ()
  (nl-clj-async-test--fixture)
  (should (nelisp-chan-p (nl-clj-go 42))))

(ert-deftest nl-clj-async-test-go-result-delivered ()
  (nl-clj-async-test--fixture)
  (let ((r (nl-clj-go (+ 1 2 3))))
    (nelisp-actor-run-until-idle)
    (should (= (nl-clj-<!! r) 6))))

(ert-deftest nl-clj-async-test-go-body-runs-under-actor-context ()
  "A `go' body can use `nelisp-self'/`nelisp-yield' like any actor body
-- `nl-clj-go' is a thin wrapper over `nelisp-spawn'/`nelisp-actor-
lambda', not a new scheduling mechanism (Doc 195 §4.6)."
  (nl-clj-async-test--fixture)
  (let ((r (nl-clj-go (nelisp-yield) (if (nelisp-self) :has-self :no-self))))
    (nelisp-actor-run-until-idle)
    (should (eq (nl-clj-<!! r) :has-self))))

;;;; >!! / <!! -- blocking, outside any go/actor context (DoD: new
;;;; plumbing named explicitly by Doc 195 §4.6) --------------------------

(ert-deftest nl-clj-async-test-blocking-take-from-top-level ()
  (nl-clj-async-test--fixture)
  (let ((ch (nl-clj-chan)))
    (nelisp-spawn (let ((ch ch)) (nelisp-actor-lambda (nl-clj->! ch :top-level))))
    (should (eq (nl-clj-<!! ch) :top-level))))

(ert-deftest nl-clj-async-test-blocking-put-from-top-level ()
  (nl-clj-async-test--fixture)
  (let* ((ch (nl-clj-chan)) (received :unset))
    (nelisp-spawn (let ((ch ch)) (nelisp-actor-lambda (setq received (nl-clj-<! ch)))))
    (should (eq (nl-clj->!! ch :top-level) t))
    (should (eq received :top-level))))

(ert-deftest nl-clj-async-test-blocking-take-buffered-no-actor-needed ()
  "A value already buffered needs no counterpart actor at all."
  (nl-clj-async-test--fixture)
  (let ((ch (nl-clj-chan 1)))
    (should (eq (nl-clj->!! ch :buffered) t))
    (should (eq (nl-clj-<!! ch) :buffered))))

(ert-deftest nl-clj-async-test-blocking-take-signals-when-nothing-ever-arrives ()
  "Against-the-bug: `<!!' must not hang the process forever when nothing
can ever satisfy it (Doc 195 §2.7's own single-thread-model divergence,
named in `nl-clj-<!!''s own Commentary) -- it must signal, loudly and
promptly, not loop."
  (nl-clj-async-test--fixture)
  (let ((ch (nl-clj-chan)))
    (should-error (nl-clj-<!! ch) :type 'nl-clj-async-error)))

(ert-deftest nl-clj-async-test-blocking-put-signals-when-nothing-ever-arrives ()
  (nl-clj-async-test--fixture)
  (let ((ch (nl-clj-chan)))
    (should-error (nl-clj->!! ch :x) :type 'nl-clj-async-error)))

;;;; alts! -- picking exactly one ready channel (DoD) --------------------

(ert-deftest nl-clj-async-test-alts-picks-a-ready-port ()
  (nl-clj-async-test--fixture)
  (let* ((ca (nl-clj-chan 1)))
    (nl-clj->!! ca :only)
    (let ((r (nl-clj-go (nl-clj-alts! (list ca)))))
      (nelisp-actor-run-until-idle)
      (should (equal (nl-clj-<!! r) (list :only ca))))))

(ert-deftest nl-clj-async-test-alts-two-ready-picks-exactly-one ()
  "Doc 195 §4.6's own against-the-bug shape verbatim: two channels each
with a pending value, `alts!' must consume exactly one, never both."
  (nl-clj-async-test--fixture)
  (let* ((ca (nl-clj-chan 1)) (cb (nl-clj-chan 1)))
    (nl-clj->!! ca :a)
    (nl-clj->!! cb :b)
    (let ((r (nl-clj-go (nl-clj-alts! (list ca cb)))))
      (nelisp-actor-run-until-idle)
      (let ((result (nl-clj-<!! r)))
        (should (memq (car result) '(:a :b)))
        (should (memq (cadr result) (list ca cb)))))))

(ert-deftest nl-clj-async-test-alts-does-not-touch-the-loser ()
  "The other half of the same DoD case: the port `alts!' did NOT pick
must still hold its value, completely untouched, independently
takeable afterward -- this is the property that would catch a claim
mechanism that let both mediators deliver."
  (nl-clj-async-test--fixture)
  (let* ((ca (nl-clj-chan 1)) (cb (nl-clj-chan 1)))
    (nl-clj->!! ca :a)
    (nl-clj->!! cb :b)
    (let ((r (nl-clj-go (nl-clj-alts! (list ca cb)))))
      (nelisp-actor-run-until-idle)
      (let* ((result (nl-clj-<!! r))
             (winner (cadr result))
             (loser (if (eq winner ca) cb ca))
             (loser-expected (if (eq winner ca) :b :a)))
        (should (eq (nl-clj-<!! loser) loser-expected))))))

(ert-deftest nl-clj-async-test-alts-never-delivers-neither ()
  "The third leg of Doc 195 §4.6's own wording (\"never both, never
neither\"): with two ready ports, `alts!' must resolve to a real value,
not park forever nor error."
  (nl-clj-async-test--fixture)
  (let* ((ca (nl-clj-chan 1)) (cb (nl-clj-chan 1)))
    (nl-clj->!! ca :a)
    (nl-clj->!! cb :b)
    (let ((r (nl-clj-go (nl-clj-alts! (list ca cb)))))
      (nelisp-actor-run-until-idle)
      (should (nl-clj-<!! r)))))

(ert-deftest nl-clj-async-test-alts-parks-until-one-becomes-ready ()
  "Nothing is ready when `alts!' is called; it genuinely PARKS (the actor
is `:blocked-receive', not busy-polling) and resumes once a later `>!'
on one of the ports satisfies it."
  (nl-clj-async-test--fixture)
  (let* ((ca (nl-clj-chan)) (cb (nl-clj-chan)) (r (nl-clj-go (nl-clj-alts! (list ca cb)))))
    (nelisp-actor-run-until-idle)
    (should (eq (nelisp-actor-status (nelisp-chan-actor r)) :blocked-receive))
    (nl-clj->!! cb :late)
    (nelisp-actor-run-until-idle)
    (should (equal (nl-clj-<!! r) (list :late cb)))))

(ert-deftest nl-clj-async-test-alts-put-port-picks-when-ready ()
  "PORTS may also be `(CHAN VAL)' puts, real core.async `alts!' syntax --
here a pending receiver makes the put immediately ready."
  (nl-clj-async-test--fixture)
  (let* ((ch (nl-clj-chan)) (received :unset))
    (nelisp-spawn (let ((ch ch)) (nelisp-actor-lambda (setq received (nl-clj-<! ch)))))
    (nelisp-actor-run-until-idle)
    (let ((r (nl-clj-go (nl-clj-alts! (list (list ch :put-value))))))
      (nelisp-actor-run-until-idle)
      (should (equal (nl-clj-<!! r) (list t ch)))
      (should (eq received :put-value)))))

(ert-deftest nl-clj-async-test-alts-closed-port-resolves-with-nil ()
  "A closed port is itself immediately \"ready\" -- `alts!' resolves to
nil for it, exactly like a direct `<!' would."
  (nl-clj-async-test--fixture)
  (let* ((ca (nl-clj-chan)) (cb (nl-clj-chan)))
    (nl-clj-close! cb)
    (let ((r (nl-clj-go (nl-clj-alts! (list ca cb)))))
      (nelisp-actor-run-until-idle)
      (should (equal (nl-clj-<!! r) (list nil cb))))))

(ert-deftest nl-clj-async-test-alts-mixed-take-and-put-ports ()
  "PORTS can mix take-ports and put-ports in one call; whichever is ready
first wins."
  (nl-clj-async-test--fixture)
  (let* ((take-ch (nl-clj-chan 1)) (put-ch (nl-clj-chan 1)))
    (nl-clj->!! take-ch :ready-to-take)
    (let ((r (nl-clj-go (nl-clj-alts! (list take-ch (list put-ch :ready-to-put))))))
      (nelisp-actor-run-until-idle)
      (should (equal (nl-clj-<!! r) (list :ready-to-take take-ch))))))

(provide 'nl-clj-async-test)

;;; nl-clj-async-test.el ends here
