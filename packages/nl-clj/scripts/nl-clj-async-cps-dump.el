;;; nl-clj-async-cps-dump.el --- Build-time CPS transform for nl-clj-async -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 (docs/design/195-clojure-compat-library.org) §4.6's own
;; honest limit, inherited unchanged from `nelisp-actor' (§2.4):
;; `generator.el' is not vendored for the standalone substrate, so no
;; freshly-loaded `(nelisp-actor-lambda ...)' form can macroexpand
;; there -- and `nl-clj-go' compiles directly to one.  This script
;; applies `packages/nelisp-actor/scripts/nelisp-actor-cps-dump.el''s
;; own build-time transform (run under a REAL Emacs, where
;; `generator.el' unquestionably works) a second time, over a
;; different source shape, reusing that script's small reader/setf-
;; rewrite helpers directly but NOT its `nelisp-actor'-only struct-
;; accessor suppression (see the two-part reason below):
;;
;;   1. `nl-clj-async-cps-dump--substitute' is the `nl-clj-go'-aware
;;      sibling of `nelisp-actor-cps-dump--substitute': that walker
;;      only ever recognizes the LITERAL symbol `nelisp-actor-lambda'
;;      -- it has no reason to know about `nl-clj-go', a macro this
;;      package adds that WRAPS one.  This one ALSO walks looking for
;;      `nl-clj-go' specifically, replaces each occurrence with ITS
;;      OWN `macroexpand-1' (a real, one-level expansion under this
;;      same host Emacs, where `nl-clj-async.el' is loaded normally),
;;      revealing the `nelisp-actor-lambda' subform inside.
;;   2. Whichever way a `nelisp-actor-lambda' subform was found, it is
;;      expanded via `nl-clj-async-cps-dump--expand-actor-lambda', NOT
;;      the reused `nelisp-actor-cps-dump--substitute' directly --
;;      against-the-bug, found by this package's own first standalone
;;      run: the reused struct-accessor suppression only ever covers
;;      `nelisp-actor''s own accessors, but this package's CPS bodies
;;      ALSO call `nelisp-chan' accessors (a SECOND `cl-defstruct' from
;;      the same file, via `nelisp-chan-recv'/-send'/`nl-clj-<!'/
;;      `nl-clj->!') plus this package's own two new structs.  Leaving
;;      any of those un-suppressed baked in real Emacs's own inlined
;;      `aref'/`cl-struct-NAME-tags' accessor body, which the
;;      standalone's differently-shaped struct representation cannot
;;      read correctly (Doc 195 §2.1) -- measured directly as a SILENT
;;      hang (0% CPU, stable RSS -- genuinely blocked, not a runaway
;;      loop), not a crash, since `nelisp-chan-actor' read the wrong
;;      slot instead of erroring.  See `nl-clj-async-cps-dump--call-
;;      without-struct-inlining's own Commentary below for the full
;;      measurement.
;;
;; This bakes SIX defuns into one generated output file.  Three are
;; the runtime support below; two are the minimal repeated-resumption
;; gates; the last is the full wrapper-based ping/pong demo.  A
;; `go' block that CREATES a channel needs a working `nl-clj-chan' too,
;; and one that reads a RESULT back via `nl-clj-<!!'/`nl-clj->!!' from
;; ordinary (non-actor) code needs those working as well: `nl-clj-
;; async--make-chan-1' (the real, un-baked mediator constructor),
;; `nl-clj-async--blocking-take-1', and `nl-clj-async--blocking-put-1'
;; (nl-clj-async.el's own Commentary on all three) each embed their OWN
;; `nelisp-actor-lambda' and need the identical treatment, or calling
;; the un-baked one standalone fails exactly the way calling it before
;; this baking existed does (see `nl-clj-async--chan-ctor-standalone'/
;; `-blocking-take-standalone'/`-blocking-put-standalone' in
;; nl-clj-async.el -- the three runtime dispatch hooks this generated
;; file's own trailing `setq' lines install into).
;;
;; Run from the repository root, under a REAL Emacs:
;;
;;   emacs -Q --batch -L src -L packages/nelisp-actor/src \
;;     -L packages/nl-prelude/src -L packages/nl-safe/src \
;;     -L packages/nl-clj/src \
;;     -l packages/nelisp-actor/scripts/nelisp-actor-cps-dump.el \
;;     -l packages/nl-clj/scripts/nl-clj-async-cps-dump.el \
;;     --eval "(nl-clj-async-cps-dump-write)"
;;
;; or: make nl-clj-async-cps-baseline
;;
;; AI.md rule 7 ("record generated data recipes next to the
;; generator"): the recipe is the `make nl-clj-async-cps-baseline'
;; target in the top-level Makefile, next to this file; do not
;; hand-edit `packages/nl-clj/generated/go-ping-pong-cps.el' --
;; regenerate it.

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'nelisp-actor)
(require 'nelisp-actor-cps-dump)
(require 'nl-clj-core)
(require 'nl-safe)
(require 'nl-clj-async)

(unless (featurep 'generator)
  (error "nl-clj-async-cps-dump: real generator.el did not load -- \
refusing to bake a transform whose macro-expansion oracle is unavailable"))

;; Against-the-bug, found (and fixed here) by this package's own first
;; standalone run of the baked demo: `nelisp-actor-cps-dump--call-
;; without-struct-inlining' -- the reused helper that suppresses
;; `cl-defstruct' accessors' `compiler-macro' property before
;; `macroexpand-all' runs, so a getter stays a plain function call
;; portable across substrates (see that function's own Commentary in
;; nelisp-actor.el's package) -- is hardcoded to `nelisp-actor' ONLY.
;; This package's own CPS-transformed bodies also call `nelisp-chan'
;; accessors (`nelisp-chan-actor', via `nelisp-chan-recv'/-send' and
;; this package's own `nl-clj-<!'/`nl-clj->!') -- a SECOND `cl-defstruct'
;; from the same nelisp-actor.el, never suppressed by the reused
;; helper, since that package's own code never itself macroexpands a
;; body touching one -- plus this package's own two new structs
;; (`nl-clj-async--chan-state', `nl-clj-async--alts-token').  Measured
;; directly: an earlier version of `packages/nl-clj/generated/
;; go-ping-pong-cps.el' baked `nelisp-chan-actor' calls down to real
;; Emacs's own `aref'/`cl-struct-nelisp-chan-tags' inlining (11 such
;; occurrences, `grep -c cl-struct-nelisp-chan-tags' on that file) --
;; and running it standalone did not crash (unlike the `nelisp-actor'
;; case this same defect shape produces `void-variable: cl-struct-
;; nelisp-actor-tags' for, per that package's own Commentary) but
;; silently misrouted messages: `nl-clj->!!' on a channel with a
;; waiting mediator hung indefinitely (0% CPU, stable RSS -- genuinely
;; blocked, not a runaway loop) instead of completing, because the
;; inlined accessor read the wrong slot on the standalone's
;; differently-shaped struct representation (Doc 195 §2.1's own
;; finding: `type-of'/`cl-typep' are unreliable there, and real
;; Emacs's own inlined accessor body type-checks via exactly those).

(defun nl-clj-async-cps-dump--struct-names ()
  "Every struct name whose accessors AND constructor need `compiler-
macro' suppression before this package's own CPS-transformed bodies
are `macroexpand-all'ed -- see the Commentary immediately above."
  '(nelisp-actor nelisp-chan nl-clj-async--chan-state nl-clj-async--alts-token))

(defun nl-clj-async-cps-dump--struct-constructors ()
  "The one non-derivable-from-slot-names function per struct in
`nl-clj-async-cps-dump--struct-names': its OWN `:constructor', which
can carry a `compiler-macro' property (inlining straight to `record')
exactly like an accessor can -- `nelisp-actor-cps-dump--struct-
accessors' (reused below for the accessor half) only ever reads slot
names, so the constructor needs listing here by hand, once, next to
where each struct is actually defined:
`nelisp-actor--make' (nelisp-actor.el), `nelisp-chan--make' (same
file), `nl-clj-async--chan-state--make'/`nl-clj-async--alts-token--make'
(nl-clj-async.el)."
  '(nelisp-actor--make nelisp-chan--make
    nl-clj-async--chan-state--make nl-clj-async--alts-token--make))

(defun nl-clj-async-cps-dump--call-without-struct-inlining (thunk)
  "Like `nelisp-actor-cps-dump--call-without-struct-inlining' (whose own
Commentary explains the WHY in full), generalized to every struct
`nl-clj-async-cps-dump--struct-names' lists plus every constructor
`nl-clj-async-cps-dump--struct-constructors' lists, not just
`nelisp-actor''s own accessors."
  (let* ((fns (append (apply #'append
                              (mapcar #'nelisp-actor-cps-dump--struct-accessors
                                      (nl-clj-async-cps-dump--struct-names)))
                       (nl-clj-async-cps-dump--struct-constructors)))
         (saved (mapcar (lambda (fn) (cons fn (function-get fn 'compiler-macro))) fns)))
    (unwind-protect
        (progn
          (dolist (fn fns) (function-put fn 'compiler-macro nil))
          (funcall thunk))
      (dolist (pair saved) (function-put (car pair) 'compiler-macro (cdr pair))))))

(defun nl-clj-async-cps-dump--expand-actor-lambda (form)
  "Return FORM's `macroexpand-all' expansion, struct-accessor inlining
suppressed per `nl-clj-async-cps-dump--call-without-struct-inlining',
then run through the reused `nelisp-actor-cps-dump--fix-setf-calls'
(harmless here -- this package never `setf's a `nelisp-actor'/
`nelisp-chan' field inside a CPS-transformed body, but the rewrite is a
no-op when it finds nothing to rewrite, and reusing it costs nothing
against maybe needing it later)."
  (nelisp-actor-cps-dump--fix-setf-calls
   (nl-clj-async-cps-dump--call-without-struct-inlining
    (lambda () (macroexpand-all form)))))

(defun nl-clj-async-cps-dump--substitute (form)
  "Return FORM with every `(nl-clj-go ...)' subform first replaced by
its one-level `macroexpand-1' (revealing the `nelisp-actor-lambda'
subform that macro's own expansion contains), and every literal
`nelisp-actor-lambda' subform -- whether it was already there or was
just revealed by that expansion -- replaced by its own full
`macroexpand-all' expansion via `nl-clj-async-cps-dump--expand-actor-
lambda' (this package's own struct-suppression-generalized sibling of
`nelisp-actor-cps-dump--substitute', not that function itself -- see
this file's Commentary on why the reused helper's `nelisp-actor'-only
suppression is not enough here).  Recurses into the rest of FORM
unchanged, the same shape as the function it generalizes."
  (cond
   ((and (consp form) (eq (car form) 'nl-clj-go))
    (nl-clj-async-cps-dump--substitute (macroexpand-1 form)))
   ((and (consp form) (eq (car form) 'nelisp-actor-lambda))
    (nl-clj-async-cps-dump--expand-actor-lambda form))
   ((consp form)
    (cons (nl-clj-async-cps-dump--substitute (car form))
          (nl-clj-async-cps-dump--substitute (cdr form))))
   (t form)))

(defun nl-clj-async-cps-dump--transform-defun (source-path defun-name new-name)
  "Read DEFUN-NAME from SOURCE-PATH, transform it, rename to NEW-NAME.
Returns the transformed `(defun NEW-NAME ...)' form; does not write
anything.  Reuses `nelisp-actor-cps-dump--read-defun' (a plain reader,
nothing transform-specific about it)."
  (let* ((form (nelisp-actor-cps-dump--read-defun source-path defun-name))
         (transformed (nl-clj-async-cps-dump--substitute form)))
    (append (list 'defun new-name) (cddr transformed))))

(defun nl-clj-async-cps-dump-write ()
  "Bake six defuns into `packages/nl-clj/generated/go-ping-pong-cps.el':

  `nl-clj-async--make-chan-1'      (nl-clj-async.el) -- the channel
                                    mediator `nl-clj-chan' creates
  `nl-clj-async--blocking-take-1'  (nl-clj-async.el) -- `nl-clj-<!!''s
                                    own throwaway-actor body
  `nl-clj-async--blocking-put-1'   (nl-clj-async.el) -- `nl-clj->!!''s
                                    own throwaway-actor body
  `nl-clj-async-demo-repeated-take' (examples/nl-clj-async/
                                     go-ping-pong.el) -- minimal `<!' gate
  `nl-clj-async-demo-repeated-put'  (examples/nl-clj-async/
                                     go-ping-pong.el) -- minimal `>!' gate
  `nl-clj-async-demo-ping-pong'    (examples/nl-clj-async/
                                    go-ping-pong.el) -- the demo itself

each renamed with a `-standalone' suffix, plus three trailing `setq'
lines wiring the first three into their own runtime dispatch hooks
\(`nl-clj-async--chan-ctor-standalone' / `-blocking-take-standalone' /
`-blocking-put-standalone', see nl-clj-async.el's own Commentary on
each\).  All three of the first are needed, not just the mediator:
against-the-bug, measured directly building this file for the first
time -- the demo's own top-level `(nl-clj-<!! ping-chan)' calls
(outside any actor, exactly the context `<!!' exists for) still
resolve to the ORDINARY `nl-clj-<!!' function name, which this
transform has no reason to rewrite (it only rewrites `nl-clj-go'/
`nelisp-actor-lambda' subforms, not arbitrary function calls) -- so
`nl-clj-<!!'/`nl-clj->!!' themselves needed the exact same bake-and-
dispatch treatment `nl-clj-chan' already gets, or calling the baked
demo function standalone hit `void-function: iter-lambda' one level
deeper, inside a function this transform's own walker never looked at."
  (let* ((chan-form
          (nl-clj-async-cps-dump--transform-defun
           "packages/nl-clj/src/nl-clj-async.el"
           'nl-clj-async--make-chan-1
           'nl-clj-async--make-chan-1-standalone))
         (take-form
          (nl-clj-async-cps-dump--transform-defun
           "packages/nl-clj/src/nl-clj-async.el"
           'nl-clj-async--blocking-take-1
           'nl-clj-async--blocking-take-1-standalone))
         (put-form
          (nl-clj-async-cps-dump--transform-defun
           "packages/nl-clj/src/nl-clj-async.el"
           'nl-clj-async--blocking-put-1
           'nl-clj-async--blocking-put-1-standalone))
         (repeated-take-form
          (nl-clj-async-cps-dump--transform-defun
           "examples/nl-clj-async/go-ping-pong.el"
           'nl-clj-async-demo-repeated-take
           'nl-clj-async-demo-repeated-take-standalone))
         (repeated-put-form
          (nl-clj-async-cps-dump--transform-defun
           "examples/nl-clj-async/go-ping-pong.el"
           'nl-clj-async-demo-repeated-put
           'nl-clj-async-demo-repeated-put-standalone))
         (demo-form
          (nl-clj-async-cps-dump--transform-defun
           "examples/nl-clj-async/go-ping-pong.el"
           'nl-clj-async-demo-ping-pong
           'nl-clj-async-demo-ping-pong-standalone))
         (out-path "packages/nl-clj/generated/go-ping-pong-cps.el"))
    (with-temp-file out-path
      (insert
       (format
        ";;; %s --- GENERATED: build-time CPS transform of nl-clj-async's go/chan demo -*- lexical-binding: t; -*-\n\n"
        (file-name-nondirectory out-path)))
      (insert ";; SPDX-License-Identifier: GPL-3.0-or-later\n\n")
      (insert ";;; Commentary:\n\n")
      (insert
       (format
        ";; GENERATED by `packages/nl-clj/scripts/nl-clj-async-cps-dump.el'
;; (run under Emacs %s) from `nl-clj-async.el's `nl-clj-async--make-
;; chan-1'/`nl-clj-async--blocking-take-1'/`nl-clj-async--blocking-
;; put-1' and `examples/nl-clj-async/go-ping-pong.el's two repeated-
;; resumption fixtures plus `nl-clj-async-demo-ping-pong'.  Doc 195
;; §4.6.
;;
;; Every `nl-clj-go'/`nelisp-actor-lambda' form has been replaced by
;; its `macroexpand-all' output under REAL `generator.el': ordinary
;; closures implementing the CPS state machine directly.  No
;; `iter-lambda'/`iter-yield'/`nl-clj-go' macro call remains anywhere
;; in this file -- `nelisp-spawn' just `funcall's whatever thunk it is
;; given and does not care how the resulting iterator was built, so
;; these plug into `nelisp-actor.el' exactly as the macro-built ones
;; do, and run on the standalone substrate via the `iter-next'/`iter-
;; close'/`iter-end-of-sequence' shim `nelisp-actor.el' installs when
;; `generator.el' is absent.
;;
;; DO NOT EDIT BY HAND.  Regenerate with `make nl-clj-async-cps-baseline'.\n\n"
        emacs-version))
      (insert ";;; Code:\n\n")
      (let ((print-length nil)
            (print-level nil)
            (print-circle nil))
        (pp chan-form (current-buffer))
        (insert "\n")
        (pp take-form (current-buffer))
        (insert "\n")
        (pp put-form (current-buffer))
        (insert "\n")
        (pp repeated-take-form (current-buffer))
        (insert "\n")
        (pp repeated-put-form (current-buffer))
        (insert "\n")
        (pp demo-form (current-buffer)))
      (insert "\n(setq nl-clj-async--chan-ctor-standalone
      #'nl-clj-async--make-chan-1-standalone)
(setq nl-clj-async--blocking-take-standalone
      #'nl-clj-async--blocking-take-1-standalone)
(setq nl-clj-async--blocking-put-standalone
      #'nl-clj-async--blocking-put-1-standalone)\n")
      (insert (format "\n(provide '%s)\n" (file-name-base out-path)))
      (insert (format "\n;;; %s ends here\n" (file-name-nondirectory out-path))))
    (princ (format "nl-clj-async-cps-dump: wrote %s\n" out-path))))

(provide 'nl-clj-async-cps-dump)

;;; nl-clj-async-cps-dump.el ends here
