;;; nl-clj-async-standalone-smoke.el --- run nl-clj-async on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 (docs/design/195-clojure-compat-library.org) §4.6's own
;; honest limit, inherited from `nelisp-actor' (§2.4): "writing a NEW
;; nelisp-actor-lambda form and expecting it to run standalone directly
;; does not work yet -- only pre-transformed, checked-in thunks do,"
;; and `nl-clj-go' compiles directly to one.  This smoke is the proof
;; that limit is a two-step WORKFLOW (write, then bake via `make
;; nl-clj-async-cps-baseline'), not a wall this package's own go/chan
;; primitives hit that `nelisp-actor' itself did not already -- it
;; loads `nl-clj' + `nelisp-actor' + `nl-clj-async' on `target/nelisp'
;; itself and runs minimal repeated-take/repeated-put fixtures plus a
;; real ping/pong exchange entirely over `nl-clj-go'/`nl-clj-chan'/
;; `nl-clj-<!'/`nl-clj->!', proving this package's channel mediator and
;; both parking wrappers survive repeated resumption on the same
;; standalone substrate `nelisp-actor' does.
;;
;; Resolution of the former named gap: the raw `nelisp-chan-recv'/
;; `nelisp-chan-send' workaround and the one-call limit are both gone.
;; On the source-matched standalone binary, the isolated `nl-clj-<!'
;; fixture, its raw control, the symmetric `nl-clj->!' fixture, and two
;; wrapper-based ping/pong calls in one process all completed.  The
;; former internal-`let' expansion of `nl-clj-<!' also completed when
;; rebaked into the same minimal fixture.  Those measurements rule out
;; reinitialized park state, a reused continuation closure, re-consed
;; sentinel identity, and catch/throw tag identity -- none of which was
;; ever the cause, and `nl-clj-<!' had no defect to fix.
;;
;; What the original "hang" actually was, measured directly on this
;; tree with the OLD `nl-clj-async.el' restored and this same widened
;; smoke driving it (CPU% and RSS sampled every 3s):
;;
;;   mid-form collector ON  (today's default, Doc 152 Stage 5):
;;     peak RSS ~586 MB, CPU pinned at ~100%, completes in ~63s,
;;     `GATE-COUNT checked=10 findings=0'.
;;   mid-form collector OFF (the pre-Stage-5 default):
;;     RSS reaches 23,137,904 KiB -- about 23 GB -- within 18s, then
;;     plateaus there; still running at 180s and killed, never finishing.
;;
;; So this fixture allocates roughly 40x more than it retains, and
;; before Doc 152 Stage 5 turned the collector on by default nothing
;; ever reclaimed it.  The original report's own signature -- "0% CPU,
;; stable RSS" -- is what a process deep in swap looks like: RSS is
;; stable because it is capped by physical memory, and CPU reads near
;; zero because the time is iowait, not user time.  It was memory
;; exhaustion misread as a deadlock, not elapsed time misread as one,
;; and it was fixed by Stage 5 rather than by anything in this file.
;; This widened smoke now waits for actual completion and asserts the
;; values from both resumptions and both top-level calls.
;;
;; Practical consequence worth keeping: this smoke is only viable at
;; all because the collector is on by default.  If a future change
;; disarms it (`(nelisp--debug-switch 6)'), this smoke does not fail
;; with a clear message -- it eats the machine's memory first.
;;
;; Unlike `nl-clj-standalone-smoke.el' (which replays every `nl-clj-*-
;; test.el' ERT body directly, since none of Tier 1's atom/vector/
;; hash/seq code needs `generator.el' at all), this smoke does NOT
;; replay `nl-clj-async-test.el's ERT bodies -- most of that suite
;; spawns actors via `nl-clj-go'/raw `nelisp-actor-lambda' directly,
;; which genuinely needs `generator.el' to macroexpand and stays
;; host-Emacs-only by design (same reasoning as `nelisp-actor-
;; standalone-smoke.el's own Commentary).  What runs standalone is
;; specifically `packages/nl-clj/generated/go-ping-pong-cps.el' -- the
;; build-time CPS transform's output -- exercising this package's own
;; channel mediator, both repeated-resumption fixtures, and `nl-clj-go'
;; itself, all without asking the standalone reader to see `iter-
;; lambda'/`iter-yield'/`nl-clj-go' at all.  Host behavior is checked
;; separately by `nl-clj-async-test.el'; `make nl-clj-async-cps-
;; baseline' regenerates the checked-in transform but does not itself
;; claim host-vs-generated parity.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load packages/nl-clj/test/nl-clj-async-standalone-smoke.el

;;; Code:

(defvar nl-clj-async-smoke--checked 0)
(defvar nl-clj-async-smoke--failures nil)
(defvar nl-clj-async-smoke--result nil)

(defun nl-clj-async-smoke--check (label actual expected)
  "Record one check: LABEL passes when ACTUAL `equal's EXPECTED."
  (setq nl-clj-async-smoke--checked (1+ nl-clj-async-smoke--checked))
  (unless (equal actual expected)
    (setq nl-clj-async-smoke--failures
          (cons (format "%s: expected %S, got %S" label expected actual)
                nl-clj-async-smoke--failures))))

;; Against-the-bug, section 1: `nl-clj-async' itself loads standalone.
;; Before this package existed there was nothing here to regress to;
;; this is the same "no condition-case around it" discipline
;; `nelisp-actor-standalone-smoke.el' uses for its own first `load' --
;; a hard failure here should look exactly like a plain load failure,
;; not a softened, harder-to-recognize smoke failure.
(load "packages/nl-prelude/src/nl-prelude-trampoline.el") ; wave8: nl-prelude requires it
(load "packages/nl-prelude/src/nl-prelude.el")
(load "packages/nl-safe/src/nl-safe.el")
(load "packages/nl-clj/src/nl-clj-core.el")
(load "packages/nl-clj/src/nl-clj-atom.el")
(load "packages/nl-clj/src/nl-clj-vector.el")
(load "packages/nl-clj/src/nl-clj-hash.el")
(load "packages/nl-clj/src/nl-clj-seq.el")
(load "packages/nl-clj/src/nl-clj.el")
(load "packages/nelisp-actor/src/nelisp-actor.el")
(load "packages/nl-clj/src/nl-clj-async.el")
(setq nl-clj-async-smoke--checked (1+ nl-clj-async-smoke--checked))

;; Section 2: build-time-transformed minimal wrapper fixtures and the
;; go-block ping/pong demo, all via generated, generator-free closures.
(load "packages/nl-clj/generated/go-ping-pong-cps.el")

(nl-clj-async-smoke--check
 "same nl-clj-<! park point resumes twice"
 (nl-clj-async-demo-repeated-take-standalone)
 '(:first :second))

(setq nl-clj-async-smoke--result
      (nl-clj-async-demo-repeated-put-standalone))
(nl-clj-async-smoke--check
 "same nl-clj->! park point resumes twice"
 (plist-get nl-clj-async-smoke--result :accepted)
 '(t t))
(nl-clj-async-smoke--check
 "repeated nl-clj->! values reach the raw receiver"
 (plist-get nl-clj-async-smoke--result :seen)
 '(0 1))

(defun nl-clj-async-smoke--check-ping-pong (label result expected-trail)
  "Check one LABEL ping/pong RESULT against EXPECTED-TRAIL."
  (nl-clj-async-smoke--check
   (format "%s trail" label)
   (plist-get result :trail)
   expected-trail)
  (nl-clj-async-smoke--check
   (format "%s results" label)
   (list (plist-get result :ping-result) (plist-get result :pong-result))
   '(:ping-done :pong-done))
  (nl-clj-async-smoke--check
   (format "%s trail length" label)
   (length (plist-get result :trail))
   (length expected-trail)))

;; Two real calls in the same process are deliberate: `nelisp-actor--
;; reset' reuses actor ids, so this directly disproves the former
;; immortal-mediator/id-collision hypothesis while both calls also
;; resume `nl-clj-<!'/`nl-clj->!' repeatedly inside their loops.
(setq nl-clj-async-smoke--result
      (nl-clj-async-demo-ping-pong-standalone 4))
(nl-clj-async-smoke--check-ping-pong
 "first call, hops=4"
 nl-clj-async-smoke--result
 '((pong . 0) (ping . 1) (pong . 2) (ping . 3)
   (pong . 4) (ping . 5) (pong . 6) (ping . 7)))

(setq nl-clj-async-smoke--result
      (nl-clj-async-demo-ping-pong-standalone 2))
(nl-clj-async-smoke--check-ping-pong
 "second call, hops=2"
 nl-clj-async-smoke--result
 '((pong . 0) (ping . 1) (pong . 2) (ping . 3)))

;; `tools/ai/nelisp-ai.sh gate NAME -- ...' requires this exact line to
;; report what the gate checked; its absence is itself a hard failure
;; there (see tools/ai/nelisp-ai.sh's `cmd_gate', and nl-clj-standalone-
;; smoke.el's identical comment).
(princ (format "GATE-COUNT checked=%d findings=%d\n"
               nl-clj-async-smoke--checked (length nl-clj-async-smoke--failures)))
(if nl-clj-async-smoke--failures
    (progn
      (dolist (f (reverse nl-clj-async-smoke--failures))
        (princ (format "FAIL %s\n" f)))
      (error "nl-clj-async-standalone-smoke: %d failure(s), %d checked"
             (length nl-clj-async-smoke--failures) nl-clj-async-smoke--checked))
  (princ (format "nl-clj-async-standalone-smoke: PASS (%d checks)\n"
                 nl-clj-async-smoke--checked)))

;;; nl-clj-async-standalone-smoke.el ends here
