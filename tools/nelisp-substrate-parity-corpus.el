;;; nelisp-substrate-parity-corpus.el --- shared probe corpus for the substrate-parity gate  -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure data, read by `tools/nelisp-substrate-parity.el' and never evaluated
;; directly by this file.  Each entry is (INDEX TAG FORM):
;;
;;   INDEX  a stable integer identity for the entry.  Ledger keys and
;;          findings name a form by this number, so once assigned an
;;          index is never reused for a different form -- add new entries
;;          at the end, at the next unused index, rather than renumbering.
;;   TAG    `shared' -- also run against host Emacs and compared to it;
;;          `standalone-only' -- run only across the NeLisp substrates,
;;          because the form probes a NeLisp-internal name or a behavior
;;          host Emacs has no reason to share.
;;   FORM   an Elisp form.  Every form here evaluates to 0 or 1 (never a
;;          raw value): the point of this corpus is cross-substrate
;;          identity checking, not measuring an interesting number, and a
;;          normalized boolean is exactly comparable everywhere a printer
;;          exists and exactly encodable in a process exit status where
;;          one does not (see the driver's bare-substrate fallback).
;;          Nondeterministic primitives (`emacs-pid', `random',
;;          `make-temp-name') are wrapped in a property check, never
;;          compared by raw value -- this is the exact shape of the
;;          regression this gate exists for: a primitive probed in one
;;          substrate and generalized to another.

(defconst nelisp-substrate-parity-corpus
  '(
    ;; -- intern / intern-soft / read-from-string canonicality for "nil"
    ;; and "t" (fixed 2026-08-19 at the native level, commit 342d098cc;
    ;; these pin that the fix holds across every entry point, not just
    ;; the one it was measured through).
    (0  shared (if (eq (intern "nil") nil) 1 0))
    (1  shared (if (eq (intern "t") t) 1 0))
    (2  shared (if (eq (intern-soft "nil") nil) 1 0))
    (3  shared (if (eq (intern-soft "t") t) 1 0))
    (4  shared (if (eq (car (read-from-string "nil")) nil) 1 0))
    (5  shared (if (eq (car (read-from-string "t")) t) 1 0))
    ;; An uninterned symbol named "nil" is a different object from the
    ;; canonical `nil'; `eq' must say so.
    (6  shared (if (eq (make-symbol "nil") nil) 0 1))

    ;; -- length vs string-bytes / aref on a string with a raw byte >= 128.
    ;; Built with `unibyte-string' rather than a literal non-ASCII source
    ;; character, so the corpus file itself stays plain ASCII and this
    ;; probes string representation, not source-file decoding.
    (7  shared (let ((s (unibyte-string 233)))
                 (if (= (length s) (string-bytes s)) 1 0)))
    (8  shared (let ((s (unibyte-string 233)))
                 (if (= (aref s 0) 233) 1 0)))

    ;; -- mod/% sign conventions and floor/truncate pairs.
    (9  shared (if (= (mod 7 -3) -2) 1 0))
    (10 shared (if (= (% 7 -3) 1) 1 0))
    (11 shared (if (= (mod -7 3) 2) 1 0))
    (12 shared (if (= (% -7 3) -1) 1 0))
    (13 shared (if (and (= (floor 7 2) 3) (= (truncate 7 2) 3)) 1 0))
    (14 shared (if (and (= (floor -7 2) -4) (= (truncate -7 2) -3)) 1 0))

    ;; -- match-data survival across an intervening `string-match-p' call.
    ;; Fixed 2026-08-22: `string-match-p' used to share `string-match''s
    ;; body and clobber match-data, unlike host Emacs's documented
    ;; contract that `-p' leaves it untouched.  This form pins the real
    ;; correctness contract now -- md1 and md2 must be `equal' on every
    ;; substrate, matching host Emacs, not merely agreeing with each
    ;; other on a shared wrong answer.
    (15 shared (let (md1 md2)
                 (string-match "b" "abc")
                 (setq md1 (match-data))
                 (string-match-p "x" "xyz")
                 (setq md2 (match-data))
                 (if (equal md1 md2) 1 0)))

    ;; -- condition-case's error/t handler must not eat a throw meant for
    ;; an enclosing catch (fixed 2026-08-19; see
    ;; feedback_nelisp_condition_case_eats_throw).
    (16 shared (if (equal (catch 'parity-tag
                            (condition-case nil
                                (throw 'parity-tag 'ok)
                              (error 'caught-wrongly)))
                          'ok)
                   1 0))

    ;; -- equal/eq on shared quoted literals.  17 is unrelated conses
    ;; (equal, not eq).  18 is the real trap: a lambda closing over one
    ;; quoted literal, called twice, where the first call's destructive
    ;; `nreverse' corrupts what the second call sees (see
    ;; feedback_elisp_quoted_literal_nreverse_share).  On host Emacs the
    ;; second call does NOT reproduce "(3 2 1)" -- it sees the mutated
    ;; remnant of the first call's `nreverse' -- so this pins CURRENT
    ;; (surprising) behavior rather than asserting a "correct" one; the
    ;; value that matters is that every substrate lands on the same side.
    (17 shared (if (equal '(1 2 3) (list 1 2 3)) 1 0))
    (18 shared (if (equal (let ((f (lambda () (nreverse '(1 2 3)))))
                            (list (funcall f) (funcall f)))
                          (list '(3 2 1) '(3 2 1)))
                   1 0))

    ;; -- format %S print-depth behavior on a deep acyclic structure.
    ;; Built by a loop rather than a literal nested quote so the depth is
    ;; not hostage to hand-counted parens; checked by round-tripping
    ;; through `read' and by the absence of a truncation marker, which
    ;; catches an early print-level cutoff without needing the exact
    ;; printed string spelled out here.
    (19 shared (let ((data nil) (i 8))
                 (while (> i 0)
                   (setq data (list i data))
                   (setq i (1- i)))
                 (if (and (equal (read (format "%S" data)) data)
                         (not (string-match "\\.\\.\\." (format "%S" data))))
                     1 0)))

    ;; -- the WHY: emacs-pid / random probed in one substrate and
    ;; generalized to another was this branch's costliest misdiagnosis.
    ;; Normalized to a property, run through every substrate below.
    (20 shared (if (integerp (emacs-pid)) 1 0))
    (21 shared (let ((r (random 1000000)))
                 (if (and (integerp r) (>= r 0) (< r 1000000)) 1 0)))
    (22 shared (if (and (stringp (make-temp-name "x"))
                       (not (string= (make-temp-name "x") (make-temp-name "x"))))
                   1 0))

    ;; -- fboundp for a pinned list of load-bearing primitives.  This is
    ;; the substrate-divergence signature itself: the bare positional
    ;; FILE entry point does not pull in the same prelude `--load' does
    ;; (see the ADDENDA known divergence), and this is how many functions
    ;; that gap actually covers, named one at a time instead of folded
    ;; into a single aggregate count nobody can attribute.
    (23 shared (if (fboundp 'format) 1 0))
    (24 shared (if (fboundp 'princ) 1 0))
    (25 shared (if (fboundp 'prin1) 1 0))
    (26 shared (if (fboundp 'message) 1 0))
    (27 shared (if (fboundp 'intern-soft) 1 0))
    (28 shared (if (fboundp 'read-from-string) 1 0))
    (29 shared (if (fboundp 'require) 1 0))
    (30 shared (if (fboundp 'provide) 1 0))
    (31 shared (if (fboundp 'symbol-name) 1 0))
    (32 shared (if (fboundp 'match-data) 1 0))
    (33 shared (if (fboundp 'string-match) 1 0))
    ;; NeLisp-internal raw stdout primitive: host Emacs never has this
    ;; name, so it is standalone-only rather than diffed against (f).
    (34 standalone-only (if (fboundp 'nelisp--write-stdout-bytes) 1 0))

    ;; -- defvaralias, all four directions plus the documented return
    ;; value (THE nelix GATES, PART 2: `nelix-core.el:95' called this,
    ;; ordinary idiomatic Elisp, and NeLisp's standalone runtime had never
    ;; implemented it -- `void-function').  `shared': host Emacs aliases
    ;; the same way for the value cell, which is everything this form
    ;; checks; it never touches the function cell or plist, where
    ;; NeLisp's record-sharing implementation intentionally diverges from
    ;; host Emacs (see the `nl_sf_defvaralias' commentary in
    ;; `scripts/nelisp-standalone-build.el' for that trade-off).
    (35 shared (if (and (eq (defvaralias 'nelisp-parity-dva-a 'nelisp-parity-dva-b)
                            'nelisp-parity-dva-b)
                       (progn (setq nelisp-parity-dva-b 7)
                              (eq (symbol-value 'nelisp-parity-dva-a) 7))
                       (progn (setq nelisp-parity-dva-a 9)
                              (eq (symbol-value 'nelisp-parity-dva-b) 9)))
                  1 0))

    ;; -- unwind-protect (Task C, the survey's gap #6): three behavioral
    ;; forms, none of which the corpus exercised before.  All `shared' --
    ;; unwind-protect/condition-case/catch/throw ordering is standard
    ;; Elisp, pinned against host, not a NeLisp-only behavior.
    ;;
    ;; 36: cleanup runs on an error escaping the protected body, BEFORE the
    ;; error reaches an enclosing `condition-case' handler -- the cleanup
    ;; fires during unwind, not after the handler resumes normal flow.
    ;; `log' ends up (caught cleanup): cleanup pushed first, caught second.
    (36 shared (let ((log nil))
                 (if (progn
                       (condition-case nil
                           (unwind-protect (error "boom")
                             (setq log (cons 'cleanup log)))
                         (error (setq log (cons 'caught log))))
                       (equal log '(caught cleanup)))
                     1 0)))

    ;; 37: `throw' crossing an unwind-protect to an OUTER `catch' still
    ;; runs the cleanup -- a throw-based non-local exit, not the
    ;; error-based one form 36 and form 16 (condition-case vs. an
    ;; enclosing catch) already cover.  `catch' must not see `thrown'
    ;; before `cleanup' has run.
    (37 shared (let ((log nil))
                 (if (progn
                       (catch 'outer
                         (unwind-protect (throw 'outer 'thrown)
                           (setq log (cons 'cleanup log))))
                       (equal log '(cleanup)))
                     1 0)))

    ;; 38: nested unwind-protect cleanup order is LIFO -- the innermost
    ;; frame's cleanup runs first, so `log' ends up (outer inner): `inner'
    ;; pushed first (innermost cleanup, closest to the error), `outer'
    ;; pushed second as unwinding continues outward.  `condition-case'
    ;; here only swallows the error so the form reduces to 0/1; the
    ;; primitive under test is unwind-protect ordering, not error
    ;; handling, so `ignore-errors' is deliberately avoided -- it is a
    ;; prelude macro, not a core primitive, and using it here would
    ;; conflate "is ignore-errors defined" with "is nesting ordered
    ;; right" in a single finding.
    (38 shared (let ((log nil))
                 (if (progn
                       (condition-case nil
                           (unwind-protect
                               (unwind-protect (error "boom")
                                 (setq log (cons 'inner log)))
                             (setq log (cons 'outer log)))
                         (error nil))
                       (equal log '(outer inner)))
                     1 0)))

    ;; -- Doc 186 P0/P1: char-table constructor/accessor layer.  `shared':
    ;; char-tables exist in stock Emacs too, and these are the doc's own
    ;; §6.2 parity pins -- construct + `char-table-p' (39), and `aref's
    ;; default-value fallback (40).
    (39 shared (if (char-table-p (make-char-table 'test)) 1 0))
    (40 shared (if (eq (aref (make-char-table 'test 'D) ?a) 'D) 1 0))

    ;; -- Doc 191 (hot code reload) Phase 1: live `defun' redefinition is
    ;; ordinary Elisp semantics, not a NeLisp-only claim, so both directions
    ;; belong in the shared corpus rather than only in the standalone-reader
    ;; smoke that pins the same case list end-to-end
    ;; (`nelisp-standalone--reader-defun-redefine-smoke').
    ;;
    ;; integration/wave4 merge 7/9: renumbered from this branch's own 39/40
    ;; to 41/42 -- feat/char-table-elisp-layer (merged 6/9) already claimed
    ;; 39/40 for its own §6.2 parity pins.  Per the integration playbook,
    ;; indices are never reused for a different form, so the char-table
    ;; pair keeps 39/40 and this pair moves up instead of either one being
    ;; dropped.  See this merge commit's body for the accepted-ledger
    ;; regeneration this renumbering required.
    ;;
    ;; 41: a call BY NAME always re-resolves and sees a later redefinition
    ;; (`(nelisp-parity-redef-fn)' -> 2), but a function object CAPTURED
    ;; before the redefinition (`captured', via `symbol-function') is a
    ;; snapshot and keeps running the old body (`(funcall captured)' -> 1)
    ;; -- Doc 191 §4's "interesting case", and the one it predicted would
    ;; NOT see the update.
    (41 shared (progn
                 (defun nelisp-parity-redef-fn () 1)
                 (let ((captured (symbol-function 'nelisp-parity-redef-fn)))
                   (defun nelisp-parity-redef-fn () 2)
                   (if (and (= (funcall captured) 1)
                            (= (nelisp-parity-redef-fn) 2))
                       1 0))))

    ;; 42: the opposite of 41's stale half -- a closure that calls the
    ;; redefined name BY NAME internally (rather than holding a captured
    ;; function object) re-resolves on every call, so it DOES see a later
    ;; redefinition.  Both 41 and 42 start from "a closure captured before
    ;; a reload"; which way it goes depends on whether the closure captured
    ;; the function object itself or only a call to it by name -- Doc 191
    ;; §4 keeps the two cases separate for exactly this reason.
    (42 shared (progn
                 (defun nelisp-parity-redef-target () 10)
                 (defun nelisp-parity-redef-make-closure ()
                   (lambda () (nelisp-parity-redef-target)))
                 (let ((closure (nelisp-parity-redef-make-closure)))
                   (let ((before (funcall closure)))
                     (defun nelisp-parity-redef-target () 20)
                     (if (and (= before 10) (= (funcall closure) 20))
                         1 0)))))

    ;; -- Doc 190 Phase A (bignums): a numeric literal one past
    ;; `most-positive-fixnum' is host-comparable -- real GNU Emacs (27+)
    ;; promotes it to a genuine bignum, `integerp'/comparison included,
    ;; the same contract Doc 190 Phase A gives NeLisp.  2305843009213693952
    ;; = 2^61 is `most-positive-fixnum' + 1 on BOTH substrates' 64-bit
    ;; builds (Doc 190 §1.1: matching Emacs's own 61-bit-magnitude fixnum
    ;; width bit for bit), so this is not a NeLisp-specific constant.
    ;;
    ;; 43: comparison across the fixnum/bignum boundary.
    (43 shared (if (> 2305843009213693952 most-positive-fixnum) 1 0))
    ;; 44: prin1/read round-trip preserves the exact value.
    (44 shared (if (equal 2305843009213693952
                          (car (read-from-string (prin1-to-string 2305843009213693952))))
                   1 0))

    ;; -- Doc 190 Phase B (bignums, arithmetic core, 2026-08-23): `+'/`-'/
    ;; `*' promotion and demotion are now host-comparable too -- real GNU
    ;; Emacs (27+) promotes a fixnum-boundary `+'/`-'/`*' overflow to a
    ;; bignum and demotes back to a fixnum once the magnitude re-fits, the
    ;; same contract Doc 190 Phase B gives NeLisp.  Values host-verified
    ;; (GNU Emacs 30.1, 2026-08-23).
    ;;
    ;; 45: exact promoted sum, `(+ most-positive-fixnum 1)'.
    (45 shared (if (= (+ most-positive-fixnum 1) 2305843009213693952) 1 0))
    ;; 46: exact bignum*bignum (genuine multi-limb on the NeLisp side --
    ;; most-positive-fixnum squared needs 3 32-bit limbs).
    (46 shared (if (= (* most-positive-fixnum most-positive-fixnum)
                      5316911983139663487003542222693990401)
                   1 0))
    ;; 47: demotion -- a bignum-producing op followed by one that brings
    ;; the magnitude back under 2^61 must equal (and, on the NeLisp side,
    ;; retag as) a plain fixnum again.  `fixnump' is host-only (not in
    ;; this runtime's predicate table, Doc 190 Phase A's own §6.1 scope),
    ;; so this entry checks the VALUE identity every substrate can
    ;; express; the NeLisp-only tag check (`bignump') lives in
    ;; `scripts/standalone-bignum-smoke.el' instead.
    (47 shared (if (= (- (+ most-positive-fixnum 1) 1) most-positive-fixnum) 1 0))
    ))

(provide 'nelisp-substrate-parity-corpus)

;;; nelisp-substrate-parity-corpus.el ends here
