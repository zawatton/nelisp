;;; nl-clj-async.el --- Channels and go blocks for nl-clj -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 (docs/design/195-clojure-compat-library.org) §4.6: core.async
;; channels and `go' blocks over `packages/nelisp-actor/src/
;; nelisp-actor.el''s already-shipped cooperative CPS scheduler.  Per
;; §4.6's own asset-mapping table:
;;
;;   core.async       nelisp-actor                  reuse shape
;;   chan             nelisp-make-chan / spawn+recv  see below (*)
;;   close!           nelisp-chan-close              direct, renamed wrapper
;;   go               nelisp-spawn + nelisp-actor-lambda   thin wrapper
;;   >!/<!            nelisp-chan-send/-recv         wire-compatible wrapper
;;   >!!/<!!          (none)                         new plumbing
;;   alts!            (none)                         new plumbing
;;
;; (*) One deliberate divergence from the doc's own literal table,
;; named here rather than left implicit: `nl-clj-chan' is NOT literally
;; `nelisp-make-chan'.  §4.6's own alts! design explicitly calls for "a
;; new :recv-alt/:send-alt message variant per channel, not a rewrite
;; of the mediator itself" -- but that mediator lives INSIDE
;; `nelisp-make-chan''s own closure in nelisp-actor.el, a shipped,
;; standalone-proven, already-baked (packages/nelisp-actor/generated/
;; two-actor-exchange-cps.el) foundation this package builds ON, not a
;; file this phase edits.  Editing it would force re-baking and re-
;; verifying that unrelated generated artifact for a change this
;; package's own scope does not need.  `nl-clj-async--make-chan-1'
;; below is therefore THIS package's own mediator: the exact same
;; :send/:recv/:close protocol and semantics as `nelisp-make-chan'
;; (verified byte-for-byte by this package's own test suite mirroring
;; nelisp-actor-test.el's channel section), plus the two new alts!
;; message types.  It returns a real `nelisp-chan' struct (from
;; nelisp-actor.el, via that package's own `nelisp-chan--make'), so
;; `nelisp-chan-send'/`nelisp-chan-recv'/`nelisp-chan-close' work on an
;; `nl-clj-chan' exactly as they do on a `nelisp-make-chan' channel.
;; `nl-clj-<!' adds only reply unwrapping, `nl-clj->!' speaks the same
;; send protocol directly so closed puts return nil instead of
;; signaling, and `nl-clj-close!' is a literal renamed wrapper.  Only
;; channel CREATION goes through this package's own mediator.
;;
;; A second, load-bearing constraint this whole file is built around
;; (Doc 195 §2.4's own "generator.el is not vendored standalone" fact,
;; worked through concretely for the first time here): `nelisp-receive'/
;; `nelisp-chan-send'/`nelisp-chan-recv'/`iter-yield' only correctly
;; suspend-and-resume when they are LEXICALLY, TEXTUALLY present inside
;; the `nelisp-actor-lambda' body that `generator.el''s CPS transform
;; macroexpands -- `catch'/`throw' (what `iter-yield' compiles to) are
;; dynamic, so calling a park point from an ordinary function a
;; `nelisp-actor-lambda' body merely FUNCALLS still correctly unwinds
;; to the right `catch', but a LATER `iter-next' resume can only jump
;; back into the CPS state machine's own dispatch table, never back
;; into that plain function's own suspended call frame.  Every macro
;; below that genuinely parks (`nl-clj-<!'/`nl-clj->!'/`nl-clj-alts!')
;; is therefore a MACRO whose expansion is inlined at the call site,
;; not a function; every helper that does NOT itself need to park
;; (`nl-clj-async--alts-sweep', `nl-clj-async--alts-satisfy', the claim
;; check) is free to be a plain function, since nothing about
;; suspending is asked of it.
;;
;; Package graph (Doc 195 §3.4): this file, and only this file inside
;; `packages/nl-clj/', depends on `packages/nelisp-actor/'.  The
;; umbrella `nl-clj.el' deliberately does NOT `(require 'nl-clj-async)'
;; -- a caller who only wants persistent vectors/maps/atoms should not
;; be forced to pull in the actor scheduler.  `(require 'nl-clj-async)'
;; explicitly, same as `(require 'nl-clj)' itself is explicit.

;;; Code:

(require 'cl-lib)
(require 'nl-clj-core)
(require 'nl-safe)
(require 'nelisp-actor)

(define-error 'nl-clj-async-error "nl-clj-async error" 'nl-clj-error)

;;;; chan / close! (Doc 195 §4.6 table) --------------------------------

(defvar nl-clj-async--chan-ctor-standalone nil
  "Build-time-CPS-baked replacement for `nl-clj-async--make-chan-1',
installed by loading the generated standalone bake
\(packages/nl-clj/generated/go-ping-pong-cps.el, via `make
nl-clj-async-cps-baseline').  Doc 195 §2.4/§4.6's own honest limit: a
NEW `nelisp-actor-lambda' form -- and `nl-clj-async--make-chan-1'
contains exactly one -- does not run on `target/nelisp' without this
same two-step bake `nelisp-actor' itself requires (packages/
nelisp-actor/README.org).  nil on host Emacs, where real `generator.el'
loads and `nl-clj-async--make-chan-1' needs no help; nil also on a
standalone build that never loaded the generated bake, in which case
`nl-clj-chan' fails exactly the way calling `nelisp-make-chan' directly
would -- loudly, not silently.")


(cl-defstruct (nl-clj-async--chan-state
               (:constructor nl-clj-async--chan-state--make)
               (:copier nil))
  "Mutable mediator state for one `nl-clj-async--make-chan-1' channel.
`cl-defstruct' is fine here even though §2.1/§3.2 rule it out for any
nl-clj COLLECTION type: this struct is purely internal, created and
consumed entirely inside this actor's own body, never returned to a
caller, never `prin1'd -- none of §2.1's print/read asymmetry applies
to a value nothing ever tries to round-trip.  A flat struct (rather
than the deeply nested `let'-bound locals an earlier draft of this
file used) keeps every message-type handler below a small, ordinary,
independently-checkable function instead of one very deep nested form
-- the earlier draft's own hand-counted parens went wrong exactly
because that nesting was too deep to verify by eye."
  (buf nil)
  (pending-senders nil)
  (pending-receivers nil)
  (alts-pending-receivers nil)
  (alts-pending-senders nil)
  (closed nil))

(defun nl-clj-async--chan-handle-send (state cap payload reply-to)
  "Handle a `:send' request: byte-identical semantics to
`nelisp-make-chan''s own `:send' case, extended to also try an
alts!-pending receiver (via `nl-clj-async--alts-satisfy') before
falling back to buffering or queuing as an ordinary pending sender."
  (cond
   ((nl-clj-async--chan-state-closed state)
    (nelisp-send reply-to :error-closed))
   ((nl-clj-async--chan-state-pending-receivers state)
    (let ((r (pop (nl-clj-async--chan-state-pending-receivers state))))
      (nelisp-send r (list :value payload))
      (nelisp-send reply-to :ok)))
   (t
    (let ((delivered nil))
      (setf (nl-clj-async--chan-state-alts-pending-receivers state)
            (nl-clj-async--alts-satisfy
             (nl-clj-async--chan-state-alts-pending-receivers state)
             reply-to
             (lambda (entry)
               (nelisp-send (nth 2 entry)
                            (list :alts-value (nth 1 entry) (list :value payload)))
               (setq delivered t))))
      (cond
       (delivered (nelisp-send reply-to :ok))
       ((< (length (nl-clj-async--chan-state-buf state)) cap)
        (setf (nl-clj-async--chan-state-buf state)
              (append (nl-clj-async--chan-state-buf state) (list payload)))
        (nelisp-send reply-to :ok))
       (t
        (setf (nl-clj-async--chan-state-pending-senders state)
              (append (nl-clj-async--chan-state-pending-senders state)
                      (list (cons payload reply-to))))))))))

(defun nl-clj-async--chan-handle-recv (state reply-to)
  "Handle a `:recv' request: byte-identical semantics to
`nelisp-make-chan''s own `:recv' case, extended to also try an
alts!-pending sender when neither a buffered value nor an ordinary
pending sender can satisfy this request."
  (cond
   ((nl-clj-async--chan-state-buf state)
    (let ((v (pop (nl-clj-async--chan-state-buf state))))
      (nelisp-send reply-to (list :value v)))
    (cond
     ((nl-clj-async--chan-state-pending-senders state)
      (let* ((s (pop (nl-clj-async--chan-state-pending-senders state)))
             (p (car s)) (rt (cdr s)))
        (setf (nl-clj-async--chan-state-buf state)
              (append (nl-clj-async--chan-state-buf state) (list p)))
        (nelisp-send rt :ok)))
     (t
      (setf (nl-clj-async--chan-state-alts-pending-senders state)
            (nl-clj-async--alts-satisfy
             (nl-clj-async--chan-state-alts-pending-senders state)
             reply-to
             (lambda (entry)
               (setf (nl-clj-async--chan-state-buf state)
                     (append (nl-clj-async--chan-state-buf state)
                             (list (nth 2 entry))))
               (nelisp-send (nth 3 entry) (list :alts-value (nth 1 entry) :ok))))))))
   ((nl-clj-async--chan-state-pending-senders state)
    (let* ((s (pop (nl-clj-async--chan-state-pending-senders state)))
           (p (car s)) (rt (cdr s)))
      (nelisp-send reply-to (list :value p))
      (nelisp-send rt :ok)))
   (t
    (let ((delivered nil))
      (setf (nl-clj-async--chan-state-alts-pending-senders state)
            (nl-clj-async--alts-satisfy
             (nl-clj-async--chan-state-alts-pending-senders state)
             reply-to
             (lambda (entry)
               (nelisp-send reply-to (list :value (nth 2 entry)))
               (nelisp-send (nth 3 entry) (list :alts-value (nth 1 entry) :ok))
               (setq delivered t))))
      (unless delivered
        (if (nl-clj-async--chan-state-closed state)
            (nelisp-send reply-to (list :closed))
          (setf (nl-clj-async--chan-state-pending-receivers state)
                (append (nl-clj-async--chan-state-pending-receivers state)
                        (list reply-to)))))))))

(defun nl-clj-async--chan-handle-recv-alt (state cell port-echo reply-to)
  "Handle a `:recv-alt' probe (Doc 195 §4.6's own new plumbing for
`alts!'): claim-checked immediate delivery when a value is already
available, or register onto `alts-pending-receivers' for later --
never replies at all unless it actually wins a claim (see
`nl-clj-async--alts-claim' and `nl-clj-async--alts-satisfy')."
  (cond
   ((nl-clj-async--chan-state-buf state)
    (when (nl-clj-async--alts-claim cell reply-to)
      (let ((v (pop (nl-clj-async--chan-state-buf state))))
        (nelisp-send reply-to (list :alts-value port-echo (list :value v))))
      (when (nl-clj-async--chan-state-pending-senders state)
        (let* ((s (pop (nl-clj-async--chan-state-pending-senders state)))
               (p (car s)) (rt (cdr s)))
          (setf (nl-clj-async--chan-state-buf state)
                (append (nl-clj-async--chan-state-buf state) (list p)))
          (nelisp-send rt :ok)))))
   ((nl-clj-async--chan-state-pending-senders state)
    (when (nl-clj-async--alts-claim cell reply-to)
      (let* ((s (pop (nl-clj-async--chan-state-pending-senders state)))
             (p (car s)) (rt (cdr s)))
        (nelisp-send reply-to (list :alts-value port-echo (list :value p)))
        (nelisp-send rt :ok))))
   ((nl-clj-async--chan-state-closed state)
    (when (nl-clj-async--alts-claim cell reply-to)
      (nelisp-send reply-to (list :alts-value port-echo (list :closed)))))
   (t
    (setf (nl-clj-async--chan-state-alts-pending-receivers state)
          (append (nl-clj-async--chan-state-alts-pending-receivers state)
                  (list (list cell port-echo reply-to)))))))

(defun nl-clj-async--chan-handle-send-alt (state cap cell port-echo payload reply-to)
  "Handle a `:send-alt' probe -- the put-side symmetric twin of
`nl-clj-async--chan-handle-recv-alt'."
  (cond
   ((nl-clj-async--chan-state-closed state)
    (when (nl-clj-async--alts-claim cell reply-to)
      (nelisp-send reply-to (list :alts-value port-echo :error-closed))))
   ((nl-clj-async--chan-state-pending-receivers state)
    (when (nl-clj-async--alts-claim cell reply-to)
      (let ((r (pop (nl-clj-async--chan-state-pending-receivers state))))
        (nelisp-send r (list :value payload))
        (nelisp-send reply-to (list :alts-value port-echo :ok)))))
   ((< (length (nl-clj-async--chan-state-buf state)) cap)
    (when (nl-clj-async--alts-claim cell reply-to)
      (setf (nl-clj-async--chan-state-buf state)
            (append (nl-clj-async--chan-state-buf state) (list payload)))
      (nelisp-send reply-to (list :alts-value port-echo :ok))))
   (t
    (setf (nl-clj-async--chan-state-alts-pending-senders state)
          (append (nl-clj-async--chan-state-alts-pending-senders state)
                  (list (list cell port-echo payload reply-to)))))))

(defun nl-clj-async--chan-handle-close (state)
  "Handle `:close': byte-identical semantics to `nelisp-make-chan''s own
`:close' case (wake pending receivers with `(:closed)', fail pending
senders), extended to also wake every alts!-pending entry -- still
claim-checked, so two channels in the same `alts!' set closing in the
same scheduler pass still resolve to exactly one winner."
  (setf (nl-clj-async--chan-state-closed state) t)
  (dolist (r (nl-clj-async--chan-state-pending-receivers state))
    (nelisp-send r (list :closed)))
  (setf (nl-clj-async--chan-state-pending-receivers state) nil)
  (dolist (s (nl-clj-async--chan-state-pending-senders state))
    (nelisp-send (cdr s) :error-closed))
  (setf (nl-clj-async--chan-state-pending-senders state) nil)
  (dolist (entry (nl-clj-async--chan-state-alts-pending-receivers state))
    (when (nl-clj-async--alts-claim (nth 0 entry) (nth 2 entry))
      (nelisp-send (nth 2 entry) (list :alts-value (nth 1 entry) (list :closed)))))
  (setf (nl-clj-async--chan-state-alts-pending-receivers state) nil)
  (dolist (entry (nl-clj-async--chan-state-alts-pending-senders state))
    (when (nl-clj-async--alts-claim (nth 0 entry) (nth 3 entry))
      (nelisp-send (nth 3 entry) (list :alts-value (nth 1 entry) :error-closed))))
  (setf (nl-clj-async--chan-state-alts-pending-senders state) nil))

(defun nl-clj-async--chan-dispatch (state cap req)
  "Route one mediator REQ to its handler.  A plain function, not a
macro or anything CPS-sensitive -- none of the handlers above ever
park (they only `nelisp-send' and mutate STATE), so calling them via
ordinary `funcall' from inside the actor body below is exactly as safe
as `nelisp-send'/`nelisp-chan-actor' themselves already are (see this
file's Commentary on why parking primitives specifically have to be
macros, and why non-parking helpers do not)."
  (pcase req
    (`(:send ,payload ,reply-to)
     (nl-clj-async--chan-handle-send state cap payload reply-to))
    (`(:recv ,reply-to)
     (nl-clj-async--chan-handle-recv state reply-to))
    (`(:recv-alt ,cell ,port-echo ,reply-to)
     (nl-clj-async--chan-handle-recv-alt state cell port-echo reply-to))
    (`(:send-alt ,cell ,port-echo ,payload ,reply-to)
     (nl-clj-async--chan-handle-send-alt state cap cell port-echo payload reply-to))
    (`(:close)
     (nl-clj-async--chan-handle-close state))))

(defun nl-clj-async--make-chan-1 (capacity)
  "The real, un-baked channel-mediator constructor.  See this file's
Commentary for why this is not literally `nelisp-make-chan'.  CAPACITY
nil or 0 gives an unbuffered (rendezvous) channel; a positive integer
permits that many values to queue without blocking a sender -- same
contract as `nelisp-make-chan'.  Returns an ordinary `nelisp-chan'
struct (from nelisp-actor.el); `nelisp-chan-send'/`nelisp-chan-recv'/
`nelisp-chan-close' -- and this file's own `nl-clj->!'/`nl-clj-<!'/
`nl-clj-close!', which are exactly those -- work on the result exactly
as they do on a `nelisp-make-chan' channel, since
`nl-clj-async--chan-dispatch's `:send'/`:recv'/`:close' handling is
byte-identical to that mediator's own.  `:recv-alt'/`:send-alt' are
the two new message types this package's own alts! design (Doc 195
§4.6) adds, claim-checked through `nl-clj-async--alts-claim' (`nl-safe''s
`nl-cell' reused as a claim-once race adjudicator, per that section's
own text).  The actor body itself is deliberately tiny -- see
`nl-clj-async--chan-state'/`nl-clj-async--chan-dispatch' above for why:
this is the ONE `nelisp-actor-lambda' this file's standalone bake
needs to transform (`nl-clj-async-cps-baseline'), and a small body is
both easier to verify by hand and a smaller CPS-transform surface.

Against-the-bug, ONE deliberate divergence from `nelisp-make-chan'
found (and fixed here, not inherited) by this package's own test
suite: `nelisp-make-chan''s own mediator loop condition -- `(while
(not (and closed (null buf) (null pending-senders))))' -- lets the
actor terminate (`:dead') the moment a channel is closed AND has
nothing buffered or pending.  That is fine for every existing
`nelisp-actor-test.el' channel case (none of them `:recv'/`:send' a
SECOND time after that point), but it directly breaks Clojure's own
`<!'/`>!' contract on a closed channel: a real closed channel answers
`<!' with nil, and `>!' with false, REPEATEDLY, forever -- not once.
A dead mediator cannot do that; the very next `nelisp-chan-recv' on it
signals `nelisp-actor-error' `send-to-dead-actor' instead of returning
`(:closed)' -- measured directly, running this package's own close!
tests before this fix: closing an unbuffered channel with nothing
pending killed the mediator immediately, and a `nl-clj-<!' called on
it even once more crashed the caller.  The fix: this actor's own loop
never exits (`(while t ...)') -- `closed' is state the dispatch
handlers already consult on every request (see `nl-clj-async--chan-
handle-recv'/`-send'/`-recv-alt'/`-send-alt' above), not a shutdown
signal, so serving `(:closed)'/`:error-closed' indefinitely falls out
of the existing per-request logic with no new code; only the loop's
own willingness to keep asking `(nelisp-receive)' for one more request
had to change.  The mediator actor for a channel therefore lives for
as long as the channel itself is reachable, exactly matching a real
core.async channel's own lifecycle (an object that keeps answering
correctly for its whole lifetime, not one that vanishes on close)."
  (let* ((cap (or capacity 0))
         (act (nelisp-spawn
               (nelisp-actor-lambda
                 (let ((state (nl-clj-async--chan-state--make)))
                   (while t
                     (nl-clj-async--chan-dispatch state cap (nelisp-receive))))))))
    (nelisp-chan--make :actor act :capacity cap)))

(defun nl-clj-async--make-chan (capacity)
  "Dispatch to the real mediator constructor, or the standalone bake
when that is what is actually loaded -- see
`nl-clj-async--chan-ctor-standalone'."
  (if (and nl-clj-async--chan-ctor-standalone (not (featurep 'generator)))
      (funcall nl-clj-async--chan-ctor-standalone capacity)
    (nl-clj-async--make-chan-1 capacity)))

(defun nl-clj-chan (&optional buf-or-n)
  "Return a new channel.  BUF-OR-N nil or 0 is unbuffered (rendezvous);
a positive integer permits that many buffered values.  See this file's
Commentary for why this is `nl-clj-async--make-chan', not literally
`nelisp-make-chan' (Doc 195 §4.6 table)."
  (nl-clj-async--make-chan (and (integerp buf-or-n) (> buf-or-n 0) buf-or-n)))

(defun nl-clj-close! (chan)
  "Close CHAN.  Direct wrapper over `nelisp-chan-close' (Doc 195 §4.6
table: `close!' <-> `nelisp-chan-close', direct renamed wrapper)."
  (nelisp-chan-close chan))

;;;; >! / <! -- parking, inside go (Doc 195 §4.6 table) -----------------

(defun nl-clj-async--unwrap-recv (v)
  "Translate `nelisp-chan-recv''s own `(:value V)'/`(:closed)' reply V
into core.async `<!''s contract: the taken value, or nil.  A plain
function, not a macro: the parking receive remains textually inside
`nl-clj-<!''s expansion while this non-parking translation stays
independently testable and mutation-addressable."
  (if (eq (car v) :closed) nil (cadr v)))

(defmacro nl-clj-<! (chan)
  "Parking take from CHAN; only valid inside `nl-clj-go' (an actor
body).  Direct wrapper over `nelisp-chan-recv' (Doc 195 §4.6 table),
with core.async's own `<!' contract layered on top: returns the taken
value, or nil once CHAN is closed and drained, via
`nl-clj-async--unwrap-recv'.

Repeated-resumption evidence is pinned by the standalone smoke's
minimal `nl-clj-async-demo-repeated-take-standalone' fixture: one
textual call site parks and resumes twice from the same `while'.  The
earlier claim that an internal `let' expansion hung on its second
resumption was rechecked against the same source-matched standalone
binary and was false: both that former expansion and this helper-call
expansion returned `(:first :second)'.  The smoke now exercises this
actual expansion directly, so a future regression is observed rather
than inferred from elapsed time."
  `(nl-clj-async--unwrap-recv (nelisp-chan-recv ,chan)))

(defmacro nl-clj->! (chan val)
  "Parking put of VAL onto CHAN; only valid inside `nl-clj-go' (an
actor body).  Same `:send' wire protocol as `nelisp-chan-send' (Doc
195 §4.6 table), with core.async's own `>!' contract layered on top:
returns t if accepted, nil if CHAN was already closed -- real
core.async's `>!' on a closed channel is a well-defined false, not a
signal.

Against-the-bug, found by this package's own test suite: an earlier
version of this macro called `nelisp-chan-send' (which SIGNALS
`nelisp-actor-error' on the closed case) wrapped in a `condition-case'
to translate that signal to nil.  Measured directly, this does not
work reliably once the call sits inside ANY enclosing `let' in the
surrounding actor body -- and `nl-clj-go' always wraps its own body in
one (`(let ((nl-clj-async--go-result (progn ,@body))) ...)'), so every
realistic caller hit this: real `generator.el''s CPS transform of
`condition-case' around a form that yields (`nelisp-chan-send' embeds
exactly one, in the ack wait) loses the handler's dynamic extent the
moment the whole thing is nested inside a `let' -- even an EMPTY
`(let nil ...)' reproduces it -- so the signal escapes uncaught up to
`nelisp-actor--step''s own top-level `condition-case', which does not
crash the actor (the ack IS a normal return, not an error, by the time
CATCH there is reached in a way this bug's own shape allows) but
instead lets the raw `(nelisp-actor-error chan-send-on-closed)'
condition object flow onward as an ordinary VALUE -- observed
literally, as a `nl-clj->!' \"result\" of `(nelisp-actor-error
chan-send-on-closed)' instead of nil.  The fix removes the signal (and
therefore the need for `condition-case') from this macro's own path
entirely: it speaks the `:send' wire protocol directly and reads the
ack as plain data, exactly as `nelisp-chan-recv'/`nl-clj-<!' already
do for the symmetric take-side case (which never needed a
`condition-case' and never hit this)."
  `(progn
     (nelisp-send (nelisp-chan-actor ,chan) (list :send ,val (nelisp-self)))
     (not (eq (nelisp-receive) :error-closed))))

;;;; >!! / <!! -- blocking, any context (Doc 195 §4.6: new plumbing) ---

;; Doc 195 §2.4/§4.6's own honest limit applies to these two exactly as
;; it does to `nl-clj-async--make-chan-1': each embeds one `nelisp-
;; actor-lambda' (its own throwaway actor), so calling either fresh,
;; un-baked, standalone hits the identical `void-function: iter-lambda'
;; a fresh `nl-clj-go' would -- against-the-bug, measured directly:
;; `nl-clj-async-demo-ping-pong-standalone' (baked, see `nl-clj-async-
;; cps-dump.el') still calls plain `nl-clj-<!!' by ordinary function
;; name at its own top level (outside any actor, exactly the context
;; `<!!' exists for) to retrieve each go-block's own result, and this
;; package's own dump script has no reason to rewrite an ordinary
;; function call the way it rewrites `nl-clj-go' -- so `nl-clj-<!!'
;; itself needed the SAME dispatch-on-`(featurep 'generator)' pattern
;; `nl-clj-async--make-chan' already uses, not just the mediator.

(defvar nl-clj-async--blocking-take-standalone nil
  "Build-time-CPS-baked replacement for `nl-clj-async--blocking-take-1',
installed by loading the generated standalone bake -- see
`nl-clj-async--chan-ctor-standalone's identical Commentary, which this
mirrors exactly for `nl-clj-<!!' instead of `nl-clj-chan'.")

(defvar nl-clj-async--blocking-put-standalone nil
  "Build-time-CPS-baked replacement for `nl-clj-async--blocking-put-1',
the `nl-clj->!!' counterpart of `nl-clj-async--blocking-take-standalone'.")

(defun nl-clj-async--blocking-take-1 (chan)
  "The real, un-baked body of `nl-clj-<!!'.  See that function and this
file's Commentary."
  (let (result got)
    (nelisp-spawn
     (let ((chan chan))
       (nelisp-actor-lambda
         (let ((v (nelisp-chan-recv chan)))
           (setq result (if (eq (car v) :closed) nil (cadr v))
                 got t)))))
    (nelisp-actor-run-until-idle)
    (unless got
      (signal 'nl-clj-async-error
              (list 'nl-clj-<!! "channel never became ready; scheduler idle with no runnable actor left to satisfy it" chan)))
    result))

(defun nl-clj-<!! (chan)
  "Blocking take from CHAN; callable from ordinary top-level code, not
only inside `nl-clj-go' -- Doc 195 §4.6's own design for this item:
spawn a throwaway, single-purpose actor that performs exactly one
`nelisp-chan-recv', then drive the WHOLE scheduler synchronously via
`nelisp-actor-run-until-idle' until every currently-runnable actor
\(this throwaway one among them) has run to completion or blocked --
built entirely from already-standalone-proven primitives, no new
scheduler behavior.  Returns the value, or nil once CHAN is closed and
drained -- same unwrap contract as `nl-clj-<!'.

Divergence, named rather than hidden: real core.async's `<!!' parks
the CALLING THREAD while everything else keeps running on other real
threads; NeLisp has exactly one thread of Lisp-level control (Doc 195
§2.7), so \"drive the cooperative scheduler to idle\" is the closest
available translation -- and unlike a real blocked OS thread, a
`run-until-idle' that returns with this throwaway actor still parked
(nothing else was left runnable to ever satisfy it) has no way to keep
waiting, so this signals `nl-clj-async-error' rather than hanging the
process forever.

Dispatches to the standalone bake when that is what is actually loaded
-- see `nl-clj-async--blocking-take-standalone' and this file's
Commentary on why this function needs the same dispatch `nl-clj-chan'
does."
  (if (and nl-clj-async--blocking-take-standalone (not (featurep 'generator)))
      (funcall nl-clj-async--blocking-take-standalone chan)
    (nl-clj-async--blocking-take-1 chan)))

(defun nl-clj-async--blocking-put-1 (chan val)
  "The real, un-baked body of `nl-clj->!!'.  See that function and this
file's Commentary."
  (let (result got)
    (nelisp-spawn
     (let ((chan chan) (val val))
       (nelisp-actor-lambda
         (setq result (nl-clj->! chan val)
               got t))))
    (nelisp-actor-run-until-idle)
    (unless got
      (signal 'nl-clj-async-error
              (list 'nl-clj->!! "channel put never completed; scheduler idle with no runnable actor left to satisfy it" chan val)))
    result))

(defun nl-clj->!! (chan val)
  "Blocking put of VAL onto CHAN; callable from ordinary top-level code.
See `nl-clj-<!!' for the throwaway-actor + drive-to-idle mechanism and
its named divergence from a genuinely blocked OS thread.  Returns t if
accepted, nil if CHAN was already closed -- `nl-clj->!''s same
false-not-signal translation, reused here via that same macro.
Dispatches to the standalone bake the same way `nl-clj-<!!' does."
  (if (and nl-clj-async--blocking-put-standalone (not (featurep 'generator)))
      (funcall nl-clj-async--blocking-put-standalone chan val)
    (nl-clj-async--blocking-put-1 chan val)))

;;;; go (Doc 195 §4.6 table) --------------------------------------------

(defmacro nl-clj-go (&rest body)
  "Spawn an actor executing BODY; return a channel that receives BODY's
own return value exactly once.  Composes two already-shipped
primitives (`nelisp-spawn', and `nl-clj-chan' itself over
`nl-clj-async--make-chan') -- no new scheduling mechanism (Doc 195
§4.6's own framing for this item).  BODY runs under `nelisp-actor-
lambda', so `nl-clj-<!'/`nl-clj->!'/`nl-clj-alts!' inside it genuinely
suspend and resume via the actor scheduler -- the CPS-transformed-
coroutine mapping Doc 195 §4.6 names directly (\"a go block IS a
CPS-transformed cooperative coroutine\").

The result channel is created with capacity 1 (a deliberate, named
divergence from an unbuffered channel): a go block whose return value
nobody ever takes would otherwise park forever on its own final,
internal `nelisp-chan-send' -- a buffered slot lets the block finish
and become `:dead' regardless of whether anything ever reads the
result, matching the spirit (not the literal unbuffered-everywhere
default) of real core.async's own `go', which uses a non-blocking put
for exactly this reason.

This macro does NOT drive the scheduler itself; callers still need
`nelisp-actor-run-until-idle' (directly, or transitively through
`nl-clj-<!!'/`nl-clj->!!') to make progress, exactly like `nelisp-spawn'
itself.

Doc 195 §4.6's own honest limit, inherited unchanged from
`nelisp-actor-lambda' (Doc 195 §2.4): a NEW `nl-clj-go' form written
and loaded fresh does not run on `target/nelisp' without the same
build-time CPS-transform + bake step `nelisp-actor' itself requires --
see `packages/nl-clj/scripts/nl-clj-async-cps-dump.el' and `make
nl-clj-async-cps-baseline'.

Against-the-bug, found (and fixed here) by this package's own
multi-round-trip go-block testing -- the single most expensive defect
this whole package's standalone bake surfaced, taking several
narrowing rounds to isolate cleanly.  Two earlier, plausible-looking
fixes were tried and both measured wrong on `target/nelisp' (both
correct on host Emacs the entire time, which is what made this slow to
pin down):

  1. `(let ((result (progn ,@body))) (nelisp-chan-send chan result)
     (nelisp-chan-close chan))' -- binding BODY's return value via
     `let' before sending it.  Hangs indefinitely (0% CPU, stable RSS)
     the moment BODY contains a `while' loop parking 2+ times, which
     is this macro's own headline use case (a real two-`nl-clj-go'
     channel bounce).
  2. Factoring the send+close into a plain helper function and passing
     BODY's result to it as an ordinary argument -- avoids the `let',
     but `nelisp-chan-send' (called from inside that helper) itself
     parks, and a plain function is opaque to the CPS transform:
     `macroexpand-all' never sees inside it, so no state in the
     generated machine corresponds to \"resume partway through this
     helper.\"  Also hangs.

The actual, measured-working fix is neither \"remove the `let'\" nor
\"keep `nelisp-chan-send' textually inline\" alone (both partial fixes
above still nest BODY as a SUBEXPRESSION -- a `let' init-form or a
function-call argument -- of some other form): BODY's own forms must
remain literal, direct, TOP-LEVEL statements of the actor lambda,
exactly as `,@body' would place them with no wrapper at all.  Isolated
directly: a `while' loop spliced in as a genuine top-level statement,
immediately followed by a SEPARATE, later `nelisp-chan-send' statement
naming only the FINAL body form's value, ran correctly on
`target/nelisp' across many round trips; the identical loop nested one
level deeper -- as an argument, whether to `let' or to a plain function
call -- did not, every time this was tested.  Only the LAST body form
(the one whose value becomes the go block's own result) is nested, as
`nelisp-chan-send''s own message argument; every form before it,
including any loop, is spliced at the top level via `butlast'."
  (declare (indent 0) (debug (&rest form)))
  (let ((chan (gensym "nl-clj-go-chan")))
    `(let ((,chan (nl-clj-chan 1)))
       (nelisp-spawn
        (nelisp-actor-lambda
          ,@(butlast body)
          (nelisp-chan-send ,chan ,(car (last body)))
          (nelisp-chan-close ,chan)))
       ,chan)))

;;;; alts! (Doc 195 §4.6: new plumbing, the most expensive item) -------

(cl-defstruct (nl-clj-async--alts-token
               (:constructor nl-clj-async--alts-token--make)
               (:copier nil))
  "A `cl-defstruct' (record) wrapper around one `alts!' claim `nl-cell'.

Against-the-bug, found by this package's own ERT suite, not assumed:
`nl-cell' IS a plain vector (`nl-safe.el': `(vector 'nl--cell value
0)'), and `nelisp-send''s own Phase 4.3 shared-immutable message
policy (`nelisp-actor--shareable-p' / `nelisp-actor--copy-message')
DEEP-COPIES plain vectors on every send -- deliberately, so two actors
can never see each other's mutations through a message they were both
handed.  That is exactly the right default for an ordinary message and
exactly wrong for this one deliberately-shared cell: `nl-clj-alts!'
sends the SAME cell to N different channels' mediators so they can
race to claim it, and a vector arrives at each as an independent copy
-- measured directly: two channels each holding a ready value, `alts!'
between them, and BOTH mediators' claims succeeded (against their own,
private copy of a cell that read nil both times), delivering two
`:alts-value' replies into one mailbox instead of one, corrupting the
very next, unrelated `nelisp-receive' in that same actor body.  A
`cl-defstruct' instance -- a record -- is the one representation
`nelisp-actor--shareable-p' classifies as shareable (passed by
reference, never recursed into or copied), so wrapping the cell in
one, and mutating only the *cell inside it* (never replacing the
wrapper), is the minimal fix: the wrapper survives the copy unchanged,
and its one slot still points at the identical `nl-cell' vector every
recipient's own copy of the wrapper shares."
  cell)

(defun nl-clj-async--alts-claim (token claimant)
  "Claim-once check-and-set on TOKEN (an `nl-clj-async--alts-token'
wrapping the real claim cell -- see that struct's own Commentary for
why the indirection is load-bearing, not decorative): `nl-safe''s
`nl-cell' reused as a race adjudicator (Doc 195 §4.6's own design for
`alts!').  CLAIMANT is stored as the winner's identity if this call
wins (a diagnostic nicety -- nothing reads it back today).  Return
non-nil iff this call won.

Not needed for correctness under today's strictly-serialized scheduler
by the same triviality argument Doc 195 §4.5 makes for
`compare-and-set!' -- ANY single mediator's own :recv-alt/:send-alt
handler runs to completion, uninterrupted, before the next actor's
turn, so within ONE handler invocation there is nothing to race.  It
IS load-bearing across handlers, though: when `nl-clj-alts!' sends its
probe to N different channels' mediators at once, each is a SEPARATE
actor that independently decides \"I am ready\" without knowing whether
another one already decided the same thing -- serialization guarantees
ORDER (mediator A's handler runs to completion before mediator B's),
not MUTUAL EXCLUSION across their independent decisions.  This cell is
what supplies that exclusion.  Adopted anyway, per Doc 195 §4.5's own
reasoning for `compare-and-set!', because it is free and gives a
future concurrent backport of this exact code a defined, loud failure
\(`nl-borrow-error') instead of an undefined one (silent double
delivery) the day two mediators really could run in parallel."
  (let ((cell (nl-clj-async--alts-token-cell token)))
    (nl-with-borrow-mut (v cell)
      (if v
          nil
        (nl-cell-set cell claimant)
        t))))

(defun nl-clj-async--alts-satisfy (entries claimant deliver-fn)
  "Try to deliver to the first of ENTRIES that can still claim its own
cell.  Each entry's `car' is its claim cell (see the two pending-list
shapes `nl-clj-async--make-chan-1' builds: `(cell port-echo reply-to)'
for a pending alts! take, `(cell port-echo payload reply-to)' for a
pending alts! put).  CLAIMANT identifies the caller for the claim.
DELIVER-FN is called with the winning entry -- and NOTHING else, not
even for a losing entry -- to perform the actual state mutation plus
reply exactly once.  Return ENTRIES with the winner, and every entry
whose cell is already claimed by some OTHER alts! resolution,
permanently removed: a losing claim attempt backs off for good and is
never reconsidered (Doc 195 §4.6's own text: \"a losing mediator's own
claim attempt finds the cell already holding a non-nil value and backs
off\").  An entry that is neither winner nor loser -- its cell still
reads nil because a DIFFERENT, unrelated `alts!' call registered it and
nothing has tried to satisfy it yet -- is impossible here: every entry
in one of these lists was registered by exactly one `alts!' call whose
own cell it alone carries, so \"still free\" and \"this call's own
still-pending entry\" are the same thing, kept unless it is the winner
being delivered to right now."
  (let (kept won)
    (dolist (entry entries)
      (cond
       (won (push entry kept))
       ((nl-clj-async--alts-claim (car entry) claimant) (setq won entry))
       (t nil)))
    (when won (funcall deliver-fn won))
    (nreverse kept)))

(defun nl-clj-async--alts-sweep (ports cell self)
  "Send one `:recv-alt'/`:send-alt' probe per PORTS entry, tagged CELL,
replying to actor SELF.  A bare channel in PORTS is a take; a
2-element list `(CHAN VAL)' is a put -- real core.async's own `alts!'
port syntax.  Every probe either gets an immediate winning reply (this
port was ready and won the claim), or none at all -- a losing or
not-yet-ready probe never replies (see `nl-clj-async--make-chan-1's
`:recv-alt'/`:send-alt' handlers and `nl-clj-async--alts-satisfy's own
contract above), so the caller needs exactly one `(nelisp-receive)',
never a fixed count to drain.

The port-echo sent with each probe -- and returned unchanged by
`nl-clj-async--alts-unwrap' as the `PORT' half of `alts!''s own `(list
VALUE PORT)' result -- is always the CHANNEL, never the 2-element
`(CHAN VAL)' pair even for a put: real core.async's own `alts!'
returns just the channel as `port' for a winning put, matching what a
caller would pass to `close!'/a later `alts!' call, not the transient
put-request pair that only ever made sense as this one call's own
input shape."
  (dolist (port ports)
    (if (and (consp port) (nelisp-chan-p (car port)))
        (nelisp-send (nelisp-chan-actor (car port))
                     (list :send-alt cell (car port) (cadr port) self))
      (nelisp-send (nelisp-chan-actor port) (list :recv-alt cell port self))))
  nil)

(defun nl-clj-async--alts-unwrap (reply)
  "Translate one mediator `:alts-value' REPLY into (list VALUE PORT),
matching real core.async `alts!''s own `[val port]' shape (a 2-element
list here, not a vector -- this package has no vector-return
convention to match; every other nl-clj-async return shape is already
a plain value or a list)."
  (pcase reply
    (`(:alts-value ,port (:value ,v)) (list v port))
    (`(:alts-value ,port (:closed)) (list nil port))
    (`(:alts-value ,port :ok) (list t port))
    (`(:alts-value ,port :error-closed) (list nil port))
    (_ (signal 'nl-clj-async-error (list 'nl-clj-alts! "unexpected reply" reply)))))

(defmacro nl-clj-alts! (ports)
  "Park until exactly one operation among PORTS completes; return
`(list VALUE PORT)'.  Only valid inside `nl-clj-go' (an actor body).
PORTS is a list, evaluated once: each element is either a channel (a
take) or a 2-element list `(CHAN VAL)' (a put) -- real core.async's own
`alts!' port syntax, without the `:default'/`:priority' options (Doc
195 §4.6 names both as real, separable extensions, not designed here).

Registers one probe per port via `nl-clj-async--alts-sweep' (a plain
function call, no park point) and then genuinely parks on a single
`(nelisp-receive)', textually inlined right here so the enclosing
`nl-clj-go' block's own CPS transform sees it -- see this file's
Commentary for why every parking primitive in this package has to be a
macro, not a function, for exactly this reason.  Because every losing
or not-yet-ready probe never replies (see `nl-clj-async--alts-sweep'),
this one `nelisp-receive' is guaranteed to receive exactly the winning
port's `:alts-value' reply, whenever it arrives -- immediately, if a
port was already ready when `alts!' was called, or later, once one
becomes ready, in which case this parks for real, exactly like
`nl-clj-<!' does."
  (let ((cellv (gensym "nl-clj-alts-token")))
    `(let ((,cellv (nl-clj-async--alts-token--make :cell (nl-cell nil))))
       (nl-clj-async--alts-sweep ,ports ,cellv (nelisp-self))
       (nl-clj-async--alts-unwrap (nelisp-receive)))))

(provide 'nl-clj-async)

;;; nl-clj-async.el ends here
